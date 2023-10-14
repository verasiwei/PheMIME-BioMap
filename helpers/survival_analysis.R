library(PheGenHelper)
library(survival)
library(survminer)
library(future)
library(furrr)
library(tidyverse)
library(r2d3)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(DT)
library(igraph)
# library(bisbmHC)
library(glue)
# library(phewasHelper)
normalize_phecodes = function (codes) 
{
  sprintf("%06.2f", as.numeric(codes))
}

phecode_to_time = aws.s3::s3readRDS(bucket = "tbilab",object="/vis_tool/PheOmicsMultimorbidity/phecode_to_time_bioVU.rds")
phe_timeline_last = aws.s3::s3readRDS(bucket = "tbilab",object="/vis_tool/PheOmicsMultimorbidity/phe_timeline_last_bioVU.rds")
transcriptomics_dat = readRDS("/home/siwei/paper_draft_prepare/omics_multimorbidity/vumc_transcriptomics.rds")
transcriptomics_dat = transcriptomics_dat[,-7924]
initiate_database(type="phecode",phe_exclude=FALSE,phe_timeline=FALSE,demo=TRUE)
demo_dat = demo_dat %>%
  filter(grid %in% phe_timeline_last$grid) %>%
  arrange(grid)
aws.s3::s3saveRDS(x=demo_dat,bucket="tbilab",
                  object = "/vis_tool/PheOmicsMultimorbidity/demo_dat.rds",multipart = TRUE)

id_to_var = demo_dat %>%
  dplyr::select(grid, SEX, RACE) %>%
  left_join(.,transcriptomics_dat %>% rename(grid=id),by="grid") %>%
  dplyr::select(-grid)

plan(multicore,workers = 30)
start = Sys.time()
results <- furrr::future_map2_dfr(phecode_to_time$phecode,phecode_to_time$group_time,
             .options = furrr_options(globals = c("id_to_var","phecode_to_time")), function(x,y){

  dat = cbind(id_to_var,y)
  cox_res = furrr::future_map_dfr(3:ncol(id_to_var),
  .options = furrr_options(globals = c("dat")),function(i){
  cox_res = coxph(Surv(time,status) ~ ., data = dat[,c(1:2,i,ncol(dat)-1,ncol(dat))])
  cox_res = data.frame(variable = rownames(coef(summary(cox_res)))[9],
                       hr = coef(summary(cox_res))[9,2],
                       pvalue = coef(summary(cox_res))[9,5])
  cox_res                     
  })

  res = cox_res %>% mutate(phecode=x) %>% dplyr::select(phecode,variable,hr,pvalue)
  res

})
message("finish")
end = Sys.time()
# write_rds(results, "/home/siwei/cseToolkit/data/UKBB_regression.rds")
aws.s3::s3saveRDS(x=results,bucket="tbilab",
                  object = "/vis_tool/PheOmicsMultimorbidity/vumc_cox_transcriptomics.rds",multipart = TRUE)
end = Sys.time()
end-start




















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

### generate a nested dataset with each phecode ~ nested outcome (age/censored)
phe_dat_exclude_long <- aws.s3::s3readRDS(bucket = "tbilab",object="/vis_tool/PheOmicsMultimorbidity/phe_dat_exclude_long_bioVU.rds")
phe_timeline_event = aws.s3::s3readRDS(bucket = "tbilab",object="/vis_tool/PheOmicsMultimorbidity/phe_timeline_event_bioVU.rds")
phe_timeline_last = aws.s3::s3readRDS(bucket = "tbilab",object="/vis_tool/PheOmicsMultimorbidity/phe_timeline_last_bioVU.rds")

##individuals having this phecode
grid_all = data.frame(grid=unique(phe_dat_exclude_long$grid))
start=Sys.time()
phecode_to_time <- phe_dat_exclude_long %>% 
  group_by(phecode) %>%
  nest() %>%
  mutate(group_time = map2(phecode,data,function(x,y){
    #case = unique(phe_dat_exclude_long$grid[phe_dat_exclude_long$phecode == x$phecode])
    group_time = grid_all %>%
      mutate(status = ifelse(grid %in% y$grid,2,1)) %>%
      left_join(.,phe_timeline_event %>% 
                  filter(phecode == x) %>%
                  dplyr::select(grid,min_date,DOB),by="grid") %>%
      mutate(time = round(as.numeric(min_date-DOB)/365,2)) %>%
      dplyr::select(-DOB) %>% 
      left_join(.,phe_timeline_last, by="grid") %>%
      mutate(time = ifelse(is.na(time),as.numeric(max_date_all-DOB)/365,time)) %>%
      dplyr::select(status,time)   
    group_time
  }))
end=Sys.time()

aws.s3::s3saveRDS(x=phecode_to_time, bucket = "tbilab",object = "/vis_tool/PheOmicsMultimorbidity/phecode_to_time_bioVU.rds")




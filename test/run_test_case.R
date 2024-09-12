library(tidyverse)
# library(PheGenHelper)
library(phewasHelper)
library(future)

# phecode_nested = readRDS("/home/siwei/paper_draft_prepare/omics_multimorbidity/data/jak2_sampled_noncarrier_phe_nested.rds")
# id_to_demographics = readRDS("/home/siwei/paper_draft_prepare/omics_multimorbidity/data/jak2_sampled_noncarrier_id_to_demographics.rds")

##phenotype files
# jak2_dat = readRDS(paste0("/proj/PI_SAVONA/Projects/CHIP_PheWAS/.common/data/92k/","intensity_JAK2_92k.rds")) %>%
#   dplyr::filter(group == "JAK2 V617F non-carrier") %>%
#   dplyr::select(grid)
# initiate_database(phe_exclude = T, demo = T, phe_timeline = T)
# extract_grid(phe=F, phe_exclude = T,demo_dat, jak2_dat)
# phe_dat_exclude = phe_dat_exclude[phe_dat_exclude@Dimnames[[1]] %in% demo_dat$grid,]
# demo_dat = demo_dat[demo_dat$grid %in% phe_dat_exclude@Dimnames[[1]],]
# set.seed(777)
# demo_dat = demo_dat[sample(1:nrow(demo_dat),5000,replace = F),]
# phe_dat_exclude = phe_dat_exclude[phe_dat_exclude@Dimnames[[1]] %in% demo_dat$grid,]
# id_to_demographics = demo_dat %>%
#   arrange(grid) %>%
#   mutate(age = ifelse(isDead==0,(as.Date(Sys.Date())-as.Date(DOB))/365,(as.Date(DOD)-as.Date(DOB))/365)) %>%
#   dplyr::select(grid, age, EHRAge,SEX,RACE)

# phe_dat_exclude_long = wide_to_long(phe_dat_exclude,type="phecode") %>%
#   mutate(phecode = substring(phecode, 2, length(phecode))) %>%
#   mutate(phecode = normalize_phecodes(phecode)) %>%
#   arrange(grid)

# ##nested phenotype file: each row of phecode with index of patients who have this phecode
# phecode_nested = phe_dat_exclude_long %>%
#   arrange(grid) %>%
#   group_by(grid) %>%
#   mutate(int_id = cur_group_id()) %>%
#   ungroup() %>%
#   dplyr::select(-grid) %>%
#   group_by(phecode) %>%
#   nest()

options(future.globals.maxSize = 4000 * 1024 ^ 2) 
expand_combinations <- function(n, repeats = FALSE){
  
  rep_counts <- n:1
  
  if(!repeats){
    rep_counts <- rep_counts - 1
  }
  
  tibble(
    a_index = rep(1:n, times = rep_counts),
    b_index = purrr::flatten_int( purrr::map(rep_counts, ~{tail(1:n, .x)}) )
  )
}

shuffle_df <- function(df){
  df[sample(1:nrow(df)),]
}

set.seed(777)
phecode_combos <- expand_combinations(nrow(phecode_nested)) %>%
  shuffle_df() 

# id_to_demographics = demo_dat %>%
#   arrange(grid) %>%
#   mutate(age = ifelse(isDead==0,(as.Date(Sys.Date())-as.Date(DOB))/365,(as.Date(DOD)-as.Date(DOB))/365)) %>%
#   dplyr::select(grid, age, EHRAge,SEX,RACE)
# saveRDS(id_to_demographics,file = "/home/siwei/paper_draft_prepare/omics_multimorbidity/data/jak2_sampled_noncarrier_id_to_demographics.rds")
# saveRDS(phecode_nested,"/home/siwei/paper_draft_prepare/omics_multimorbidity/data/jak2_sampled_noncarrier_phe_nested.rds")
#================================run regression=============================#
start = Sys.time()
plan(multicore,workers = 50)
results <- furrr::future_pmap(
# purrr::pmap(
.options = furrr::furrr_options(globals = c("id_to_demographics","phecode_nested")),
  list(phecode_nested$data[phecode_combos$a_index],
       phecode_nested$data[phecode_combos$b_index],
       phecode_nested$phecode[phecode_combos$a_index],
       phecode_nested$phecode[phecode_combos$b_index]),
  purrr::safely(function(phecode_a_ids, phecode_b_ids,phecode_a,phecode_b) {
    
    id_to_demographics$phecode_a <- 0L
    id_to_demographics$phecode_a[phecode_a_ids$int_id] <- 1L
    
    id_to_demographics$phecode_b <- 0L
    id_to_demographics$phecode_b[phecode_b_ids$int_id] <- 1L
    
    
    a_to_b <-
      speedglm::speedglm(
        phecode_b ~ phecode_a + age + SEX + RACE + EHRAge,
        family = binomial(),
        data = id_to_demographics
      )
    
    b_to_a <-
      speedglm::speedglm(
        phecode_a ~ phecode_b + age + SEX + RACE + EHRAge,
        family = binomial(),
        data = id_to_demographics
      )
    
    bind_cols(phecode_a=phecode_a,phecode_b=phecode_b,dplyr::bind_cols(
      dplyr::select(
        broom::tidy(a_to_b)[2,],
        beta_a = estimate,
        std_err_a = std.error,
        z_a = statistic,
        p_val_a = p.value
      ),
      dplyr::select(
        broom::tidy(b_to_a)[2,],
        beta_b = estimate,
        std_err_b = std.error,
        z_b = statistic,
        p_val_b = p.value
      )
    ) %>%
      dplyr::mutate_if(is.factor, function(x){as.numeric(as.character(x))}) %>%
      dplyr::mutate(
        n_a = nrow(phecode_a_ids),
        n_b = nrow(phecode_b_ids),
        overlap = sum(id_to_demographics$phecode_a == 1 & id_to_demographics$phecode_b == 1)
      ))
  })
)
message("finish")
end = Sys.time()
end-start
write_rds(results, "/home/siwei/paper_draft_prepare/omics_multimorbidity/data/jak2_noncarrier_multimorbidity_regression.rds")


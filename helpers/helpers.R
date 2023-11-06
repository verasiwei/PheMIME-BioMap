makeTooltips <- function(phewas){
  columns <- colnames(phewas)
  
  phewas <- phewas %>% 
    mutate(tooltip = '')
  
  for(col in columns){
    # print(col)
    phewas <- phewas %>% 
      mutate(
        tooltip = paste0(
          tooltip,
          "<i> ", col, " </i>", !!(rlang::sym(col)), "</br>"
        )
      )
  }
  phewas
}

phecode_info = phecode_def %>%
  dplyr::select(phecode,description,category=group) %>% makeTooltips(.) %>%
  dplyr::select(name=description,tooltip)

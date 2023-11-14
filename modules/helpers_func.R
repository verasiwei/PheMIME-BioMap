### read and combine all phewas result from omics website
# filenames <- list.files ("/home/siwei/paper_draft_prepare/omics_multimorbidity/website_results", full.names = TRUE, pattern = ".csv$")
# res = purrr::map_dfr(filenames, function(x) {read_delim(x,delim = "\t",
#                                                         col_types = cols(
#                                                           Phenotype = col_character(),
#                                                           `Trait ID` = col_character(),
#                                                           Platform = col_character(),
#                                                           Type = col_character(),
#                                                           Description = col_character(),
#                                                           `Hazard Ratio|Effect Size` = col_character(),
#                                                           `FDR adjusted P-value` = col_double()
#                                                         ))})
# saveRDS(res,file = "omicspred_phewas_res.rds")
source("modules/load_library.R")

join_phecode_info = function (data_w_phecode, phecode_column = phecode, cols_to_join = c("description",
                                                                                         "category", "category_number", "phecode_index"))
{
  phecode_column_name <- rlang::quo_name(rlang::enquo(phecode_column))
  phecode_col_missing <- !(phecode_column_name %in% colnames(data_w_phecode))
  if (phecode_col_missing) {
    stop("Missing phecode column in data. Make sure phecode_column argument is correct.")
  }
  has_any_appended_cols <- cols_to_join %in% colnames(data_w_phecode)
  if (any(has_any_appended_cols)) {
    warning("Existing info columns in input. Joined info columns will be suffixed with '_info'.")
  }
  available_info_cols <- colnames(phecode_descriptions)
  bad_requests <- available_info_cols[!(cols_to_join %in% available_info_cols)]
  if (length(bad_requests) > 0) {
    stop(paste0("The request phecode information (", paste(bad_requests,
                                                           collapse = ","), (if (length(bad_requests) == 1)
                                                             ") is"
                                                             else "are"), " unavailable. Possible information values include: ",
                paste(available_info_cols, collapse = ", ")))
  }
  dplyr::left_join(data_w_phecode, dplyr::select(phecode_descriptions,
                                                 phecode, dplyr::one_of(cols_to_join)), suffix = c("",
                                                                                                   "_info"), by = rlang::set_names("phecode", phecode_column_name))
}

extract_code <- function(data, code){
  bind_rows(
    data %>%
      filter(a == code) %>%
      rename(description = b) %>%
      dplyr::select(-a),
    data %>%
      filter(b == code) %>%
      rename(description = a) %>%
      dplyr::select(-b)
  ) %>%
    left_join(.,phecode_def %>% dplyr::select(phecode,description),by="description") %>%
    join_phecode_info(cols_to_join = c("category", "phecode_index"))
}


get_phecode_info = function (codes, what = "description")
{
  joined <- dplyr::left_join(dplyr::tibble(phecode = codes),
                             phecode_descriptions, by = "phecode")
  if (what == "description") {
    return(joined$description)
  }
  else if (what == "category") {
    return(joined$category)
  }
  else if (what == "all") {
    return(dplyr::select(joined, phecode, description, category))
  }
  else {
    stop("what argument must be either 'description', 'category', or 'all'.")
  }
}

extract_and_diff <- function(pairs_df, code){
  pairs_df %>%
    extract_code(code)
  
  # %>%
  #   drop_na(z_avg_vandy, z_avg_mgh, z_avg_ukbb,z_combined_vandy_mgh_ukbb)
  # %>%
  #   mutate(difference = vandy - mgh) %>%
  #   arrange(difference)
}

scale_color_phecode <- function(){
  
  ggplot2::scale_color_manual(values = category_colors())
  
}

theme_phewas <- function(phecode_on_x_axis = TRUE){
  
  if(phecode_on_x_axis){
    t <- ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                        axis.text.x = ggplot2::element_blank(),
                        panel.grid.major.x = ggplot2::element_blank(),
                        panel.grid.minor.x = ggplot2::element_blank())
  } else {
    t <- ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                        axis.text.y = ggplot2::element_blank(),
                        panel.grid.major.y = ggplot2::element_blank(),
                        panel.grid.minor.y = ggplot2::element_blank())
  }
  
}

category_colors <- function(return_df = FALSE){
  
  categories <- c("neoplasms",               "dermatologic",          "endocrine/metabolic",
                  "mental disorders",        "respiratory",           "circulatory system",
                  "symptoms",                "hematopoietic",         "genitourinary",
                  "infectious diseases",     "sense organs",          "digestive",
                  "pregnancy complications", "injuries & poisonings", "musculoskeletal",
                  "congenital anomalies",    "neurological",          "other")
  
  # Hard coded colors
  colors <- c("#673770", "#C0717C", "#7FDCC0",
                       "#38333E", "#AD6F3B", "#D14285",
                       "#5E738F", "#8A7C64", "#689030",
                       "#DA5724", "#C84248", "#508578",
                       "#599861", "#CBD588", "#CE50CA",
                       "#D1A33D", "#3F4921", "#d9d9d9")
                       
  if(return_df){
    colors <- dplyr::tibble(
      category = categories,
      color = colors
    )
  } else {
    names(colors) <- categories
  }
  
  colors
}

fromList <- function(input){
  elements <- unique(unlist(input))
  data <- unlist(lapply(input, function(x){x <- as.vector(match(elements, x))}))
  data[is.na(data)] <- as.integer(0); data[data != 0] <- as.integer(1)
  data <- data.frame(matrix(data, ncol = length(input), byrow = F))
  data <- data[which(rowSums(data) !=0), ]
  names(data) <- names(input)
  return(data)
}

# Function to calculate intersection
calculate_intersection <- function(combination, upset_data) {

  cols <- upset_data %>%
    filter(if_all(combination,~.x == 1))
  if(ncol(cols)==1){
    cols = cols
  } else{
    cols = cols %>% filter(if_all(colnames(cols)[!colnames(cols) %in% combination],~.x==0))
  }
    
  intersect = nrow(cols) 
  return(intersect)
}





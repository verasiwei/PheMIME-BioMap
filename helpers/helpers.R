mirror_a_b <- function(pairs){
  bind_rows(
    pairs,
    pairs %>% dplyr::rename(a = b, b = a)
  ) 
}

calculate_phecode_similarities <- function(combined_associations){
  
  common_codes <- unique(c(combined_associations$a,combined_associations$b))
  
  # Filter the associations to only ones involving codes present in all systems/combined
  nested_associations <- combined_associations %>%
    filter(a %in% common_codes, b %in% common_codes) %>%
    dplyr::select(-contains("overlap")) %>% 
    rename_with(function(col_name){str_remove(col_name, "z_")}) %>% 
    mirror_a_b() %>% 
    mutate(b = furrr::future_map_int(b, ~which(common_codes == .x))) %>% 
    group_by(a) %>%
    nest() %>% 
    ungroup()
  
  phecode_to_associations <- set_names(nested_associations$data, nested_associations$a)
  
  get_similarity_score <- function(code_a, code_b){
    # Example codes if needed for debugging
    # code_a <- "295.10"
    # code_b <- "296.00"
    combined_pairs <- inner_join(
      phecode_to_associations[[code_a]],
      phecode_to_associations[[code_b]],
      by = "b",
      suffix = c("_a", "_b")
    )
    
    get_system_cor <- function(system){
      a_val <- combined_pairs[[paste0(system,"_a")]]
      b_val <- combined_pairs[[paste0(system,"_b")]]
      n_shared <- sum(!is.na(a_val) & !is.na(b_val))
      
      if(n_shared < 2){
        sim <- NA
      } else {
        sim <- cor(a_val, b_val, use = "complete.obs")
      }
      tibble(system = system, sim = sim, size = n_shared)
    }
    
    sim_results <- bind_rows(
      get_system_cor("strength")
    ) 
    
    sim_results$a <- code_a
    sim_results$b <- code_b
    
    sim_results
  }
  
  # Setup indices for all possible pairs of n values
  n <- length(phecode_to_associations)
  rep_counts <- (n:1) - 1
  
  
  plan(multicore,workers = 50)
  furrr::future_map2_dfr(
    common_codes[rep(1:n, times = rep_counts)],
    common_codes[unlist(lapply(rep_counts, function(x){utils::tail(1:n, x)}))],
    get_similarity_score
  )
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
    filter(if_all(combination,~.x == 1)) %>%
    filter(if_all(colnames(cols)[!colnames(cols) %in% combination],~.x==0))
  
   intersect = nrow(cols) 
   return(intersect)
}


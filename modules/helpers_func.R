##===========pathway analysis==============##

convert_id = function(dat){
  
  dat = purrr::map_dfr(unique(dat$phenotype),function(pheno){
    
    dat_sub = dat %>% 
      filter(phenotype == pheno) %>%
      distinct(orig_id,.keep_all = T)
    
    protein = bitr(dat_sub$orig_id[dat_sub$type=="protein"],fromType="UNIPROT", toType="ENTREZID", OrgDb="org.Hs.eg.db") %>%
      dplyr::rename(orig_id=UNIPROT) %>%
      distinct(ENTREZID,.keep_all = T)
    gene = bitr(dat_sub$orig_id[dat_sub$type=="gene"],fromType="ENSEMBL", toType="ENTREZID", OrgDb="org.Hs.eg.db") %>%
      dplyr::rename(orig_id=ENSEMBL) %>%
      distinct(ENTREZID,.keep_all = T)
    bind_rows(gene,protein) %>%
      distinct(ENTREZID,.keep_all = T) %>%
      left_join(.,dat_sub,by="orig_id") %>%
      filter(hazard_ratio!=Inf) %>%
      arrange(desc(hazard_ratio)) 
    
  })
  dat
  
}

enrichment_analysis = function(dat){
  
  if(nrow(dat)!=0){
    
    ## individual gene list
   plan(multisession,workers = 7) 
   res = furrr::future_map_dfr(unique(dat$phenotype),function(pheno){
     gene_list = dat %>% filter(phenotype==pheno) %>%
       distinct(orig_id,.keep_all = T) %>% 
       distinct(ENTREZID,.keep_all = T) %>%
       distinct(node,.keep_all = T) %>%
       arrange(desc(hazard_ratio)) %>%
       pull(hazard_ratio)
     names(gene_list) = dat %>% filter(phenotype==pheno) %>%
       distinct(orig_id,.keep_all = T) %>% 
       distinct(ENTREZID,.keep_all = T) %>%
       distinct(node,.keep_all = T) %>% 
       pull(ENTREZID)
     
     ## go enrichment analysis 
     res = gseGO(geneList   = gene_list,
                 OrgDb        = org.Hs.eg.db,
                 ont          = "CC",
                 minGSSize    = 100,
                 # maxGSSize    = 500,
                 pvalueCutoff = 1,
                 verbose      = FALSE)
     res_go = res@result
     
     ## kegg enrichment analysis
     res = gseKEGG(geneList     = gene_list,
                   organism     = 'hsa',
                   minGSSize    = 100,
                   pvalueCutoff = 1,
                   verbose      = FALSE)
     res_kegg = res@result
     
     ## wikipathway analysis
     # res = gseWP(gene_list,
     #             organism = "Homo sapiens",
     #             minGSSize    = 100,
     #             pvalueCutoff = 1,
     #             verbose      = FALSE)
     # res_wiki = res@result
     # 
     ## reactome pathway
     res <- gsePathway(gene_list,
                     minGSSize    = 100,
                     pvalueCutoff = 1,
                     pAdjustMethod = "BH",
                     verbose = FALSE)
     res_reactome = res@result
     
     ## disease
     res <- gseDO(gene_list,
                minGSSize     = 100,
                pvalueCutoff  = 1,
                pAdjustMethod = "BH",
                verbose       = FALSE)
     res_disease = res@result
     
     bind_rows(res_go,res_kegg,res_reactome,res_disease) %>%
       mutate(pvalue = round(pvalue,2),
              p.adjust = round(p.adjust,2),
              qvalue = round(qvalue,2),
              enrichmentScore = round(enrichmentScore,2),
              NES = round(NES,2)) %>%
       mutate(phenotype=pheno)
    }) 
   # %>%
     # distinct(Description,.keep_all = T) 
  
    if(nrow(res)!=0){
      
      ## percentage of genes under that pathway that originally from each disease phenotype
      res_freq = res %>% 
        # distinct(Description,.keep_all = T) %>%
        # filter(Description %in% unique(res$Description)) %>%
        group_by(phenotype) %>%
        dplyr::select(Description,core_enrichment) %>%
        mutate(core_enrichment = str_split(core_enrichment,"/")) %>%
        unnest(core_enrichment) %>%
        dplyr::rename(ENTREZID = core_enrichment) %>%
        ungroup() %>%
        left_join(.,dat %>% dplyr::select(phenotype,ENTREZID),by=c("phenotype","ENTREZID")) %>%
        filter(!is.na(phenotype))
      res_freq_list = res_freq %>%
        mutate(Description = factor(Description,levels = unique(Description))) %>%
        group_by(Description,phenotype) %>%
        summarise(n = n()) %>%
        mutate(freq = n / sum(n)) %>%
        dplyr::select(Description,freq,phenotype) %>%
        arrange(Description,phenotype) %>%
        group_split(Description,.keep=F) %>%
        map(.,function(x) {
          dat = x$freq
          names(dat) = x$phenotype
          dat
        })
      ##table data
      res_table = res %>%
        left_join(.,res_freq %>%
                    group_by(Description,phenotype) %>%
                    summarise(n = n()) %>%
                    mutate(freq = round(n / sum(n),2)) %>%
                    dplyr::select(Description,freq,phenotype), by=c("phenotype","Description")) %>%
        dplyr::select(ID,Description,freq,phenotype,enrichmentScore,pvalue,p.adjust,qvalue)
      
      ## Could I add the cluster analysis based on the similarity of phenotypes? which is to say according to the similarity among phenotype proportions for each biological term
      data_wide <- res_table %>%
        dplyr::select(Description, phenotype, freq) %>%
        pivot_wider(names_from = phenotype, values_from = freq, values_fill = list(freq = 0))
      data_matrix <- as.matrix(data_wide[, -1])
      rownames(data_matrix) <- data_wide$Description

      # Calculate KL divergence
      library(proxy)
      kl_divergence <- function(p, q) {
        p <- p + 1e-10
        q <- q + 1e-10
        sum(p * log(p / q))
      }
      dist_matrix <- as.matrix(dist(data_matrix, method = kl_divergence))
  
    # generate edges
      g <- graph_from_adjacency_matrix(dist_matrix, mode = "undirected", weighted = TRUE)
      res_network <- as_data_frame(g, what = "edges") %>% filter(weight != 0) %>% dplyr::select(-weight) %>% filter(from!=to)
      
      # Compute the distance matrix
      dist_matrix <- as.dist(dist_matrix)
      # Perform hierarchical clustering
      hclust_result <- hclust(dist_matrix,method = "ward.D2")
      ## cophenetic correlation
      cor_coef = cor(dist_matrix, cophenetic(hclust_result))
      ## silhouette width
      cutoffs = seq(min(hclust_result$height)+0.5,max(hclust_result$height)-0.5,1)
      sil_scores = purrr::map_dbl(cutoffs, function(cut){
        clusters = cutree(hclust_result,h=cut)
        mean(silhouette(clusters, dist_matrix)[,3])
      })
      sil_dat = data.frame(cutoffs, sil_scores)
      return(list(hclust_result,res_network,res_freq,res_freq_list,res_table,dist_matrix,cor_coef,sil_dat))
    } else{
      NULL
    }
  } else {
    NULL
  }
}


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

# Function to calculate intersection
intersection_nodes_unique <- function(combination, upset_data) {
  
  cols <- upset_data %>%
    filter(if_all(combination,~.x == 1))
  if(ncol(cols)==1){
    cols = cols
  } else{
    cols = cols %>% filter(if_all(colnames(cols)[!colnames(cols) %in% combination],~.x==0))
  }
  
  shared_nodes = rownames(cols)
  return(shared_nodes)
}

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











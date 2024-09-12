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
   res = purrr::map_dfr(unique(dat$phenotype),function(pheno){
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
     res = gseWP(gene_list,
                 organism = "Homo sapiens",
                 minGSSize    = 100,
                 pvalueCutoff = 1,
                 verbose      = FALSE)
     res_wiki = res@result
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
     
     ## msigdb?
     
     
     bind_rows(res_go,res_kegg,res_wiki,res_reactome,res_disease)
     
    })
    
   # # ## combined list
   # dat_gene_list = dat %>%
   #   group_by(node) %>%
   #   mutate(hazard_ratio_mean = mean(hazard_ratio)) %>%
   #   arrange(desc(hazard_ratio)) %>%
   #   ungroup() %>%
   #   distinct(ENTREZID,.keep_all = T) %>%
   #   distinct(node,.keep_all = T) %>%
   #   distinct(orig_id,.keep_all = T) %>%
   #   dplyr::select(hazard_ratio,ENTREZID)
   # gene_list = dat_gene_list$hazard_ratio
   # names(gene_list) = dat_gene_list$ENTREZID
  
    if(nrow(res)!=0){
      
      ## build pathway networks, edges connecting terms that have overlapping gene sets
      res_process = res %>%
        distinct(Description,.keep_all = T) %>%
        dplyr::select(Description,core_enrichment) %>%
        # mutate(index = row_number()) %>%
        pivot_longer(!Description, names_to = "variable",values_to = "element") %>%
        drop_na() %>%
        dplyr::select(-variable)
      res_network = res_process %>% 
        setNames(paste0(names(.), '_2')) %>% 
        tidyr::crossing(res_process) %>% 
        filter(Description != Description_2) %>%
        dplyr::select(a=Description,b=Description_2,element_a=element,element_b=element_2) %>%
        mutate(element_a = str_split(element_a,"/"),element_b = str_split(element_b,"/")) %>%
        rowwise() %>%
        mutate(overlap_n = length(intersect(element_a,element_b))) %>%
        filter(overlap_n!=0) %>%
        dplyr::select(a,b) %>%
        graph_from_data_frame(., directed=FALSE) %>%
        igraph::simplify(.) %>%
        as_data_frame
      
      vertices = res_network %>%
        dplyr::select(node=from) %>%
        bind_rows(
          res_network %>%
            dplyr::select(node=to)
        ) %>%
        distinct(node,.keep_all = T)
      
      ## percentage of genes under that pathway that originally from each disease phenotype
      res_freq = res %>% 
        distinct(Description,.keep_all = T) %>%
        dplyr::select(Description,core_enrichment) %>%
        mutate(core_enrichment = str_split(core_enrichment,"/")) %>%
        unnest(core_enrichment) %>%
        dplyr::rename(ENTREZID = core_enrichment) %>%
        left_join(.,dat %>% dplyr::select(phenotype,ENTREZID),by="ENTREZID") %>%
        filter(!is.na(phenotype))
      res_freq_list = res_freq %>%
        group_by(Description,phenotype) %>%
        summarise(n = n()) %>%
        mutate(freq = n / sum(n)) %>%
        dplyr::select(Description,freq,phenotype) %>%
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
                    mutate(freq = n / sum(n)) %>%
                    dplyr::select(Description,freq,phenotype), by=c("Description")) %>%
        dplyr::select(ID,Description,freq,phenotype,enrichmentScore,pvalue,p.adjust,qvalue)
      
      # Save data for JS visualization
      network_dat = list(
        nodes = vertices %>% dplyr::rename(id = node) %>% filter(id %in% res_freq$Description),
        edges = bind_rows(res_network) %>% dplyr::rename(source = from, target = to) %>% filter(source %in% res_freq$Description & target %in% res_freq$Description),
        freq = res_freq_list,
        res_table = res_table,
        extra = data.frame(nothing=Sys.time())
      )
      network_dat
    } else{
      NULL
    }
  } else {
    NULL
  }
}







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
# source("modules/load_library.R")

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



# trans_id = read_delim("transcriptomics_id.csv",delim="\t") %>%
#   dplyr::select(1:3) %>%
#   dplyr::rename(omicpred_id = `OMICSPRED ID`,variable=Gene) %>%
#   mutate(group="transcriptomics")
# 
# proteomics_soma_id = read_delim("proteomics_somalogic_id.csv",delim = "\t") %>%
#   dplyr::select(1,3,5) %>%
#   dplyr::rename(omicpred_id = `OMICSPRED ID`,variable=Protein) %>%
#   mutate(group="proteomics_soma")
# 
# proteomics_olink_id = read_delim("proteomics_olink_id.csv",delim = "\t") %>%
#   dplyr::select(1:2,4) %>%
#   dplyr::rename(omicpred_id = `OMICSPRED ID`,variable=Protein) %>%
#   mutate(group="proteomics_olink")
# 
# metabolomics_meta_id = read_delim("metabolomics_metabolon_id.csv",delim = "\t") %>%
#   dplyr::select(1:3) %>%
#   dplyr::rename(omicpred_id = `OMICSPRED ID`,variable=`Biochemical Name`) %>%
#   mutate(group="metabolomics_metabolon")
# 
# metabolomics_night_id = read_delim("metabolomics_nightingale_id.csv",delim = "\t") %>%
#   dplyr::select(1:3) %>%
#   dplyr::rename(omicpred_id = `OMICSPRED ID`,variable=`Biomarker Name`) %>%
#   mutate(group="metabolomics_nightingale")
# 
# all_id = bind_rows(trans_id %>% dplyr::rename(orig_id = `Ensembl ID`),
#                    proteomics_soma_id %>% dplyr::rename(orig_id=`UniProt ID`),
#                    proteomics_olink_id %>% dplyr::rename(orig_id=`UniProt ID`),
#                    metabolomics_meta_id %>% dplyr::rename(orig_id=`Metabolon ID`),
#                    metabolomics_night_id %>% dplyr::rename(orig_id=`Trait ID`))
# saveRDS(all_id,"data/all_id.rds")











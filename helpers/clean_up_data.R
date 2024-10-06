##clean datasets
phecodes <- phecode_def %>% dplyr::select(phecode,description) %>%
  dplyr::select(phecode) %>%
  join_phecode_info(cols_to_join = c("description", "category")) %>%
  dplyr::rename(Description=description)

# vumc
transcriptomics_dat = list.files(path="vumc_cox_transcriptomics", full.names=TRUE) %>% 
  map_dfr(readRDS) 
proteomics_dat = list.files(path="vumc_cox_proteomics", full.names=TRUE) %>% 
  map_dfr(readRDS)
metabolomics_dat = list.files(path="vumc_cox_metabolomics", full.names=TRUE) %>% 
  map_dfr(readRDS)

vumc_res = bind_rows(transcriptomics_dat %>% mutate(group="gene"),
                     proteomics_dat %>% mutate(group="protein"),
                     metabolomics_dat %>% mutate(group="metabolite")) %>%
           mutate(p_adjust = p.adjust(pvalue,"fdr")) %>%
           filter(p_adjust < 0.99) %>%
           # filter(pvalue < 0.05) %>% ### less strict rule
           mutate(variable = str_remove_all(variable,"`")) %>%
           dplyr::select(-pvalue) %>%
           mutate(pvalue = p_adjust)
          
# saveRDS(vumc_res, file="data/vumc_phewas_res_original_p_0.05.rds")
saveRDS(vumc_res, file="data/vumc_phewas_res_original.rds")
vumc_res_connect = vumc_res %>%
  mutate(phenotype=normalize_phecodes(phecode),hr=round(as.numeric(hr),2)) %>%
  left_join(., phecodes %>% dplyr::rename(phenotype=phecode),by="phenotype") %>%
  dplyr::select(variable,phenotype,connection_type=group,hr,pvalue,p_adjust,phecode_description=Description,phecode_category=category) %>%
  dplyr::rename(from = variable,to = phecode_description) %>%
  bind_rows(vumc_res %>%
              mutate(phenotype=normalize_phecodes(phecode)) %>%
              left_join(., phecodes %>% dplyr::rename(phenotype=phecode),by="phenotype") %>%
              dplyr::select(variable,phenotype,connection_type=group,hr,pvalue,p_adjust,phecode_description=Description,phecode_category=category) %>%
              dplyr::rename(from = phecode_description,to = variable)) %>%
  # mutate(hazard_ratio = str_extract(hr,"[^()]+",group=NULL)) %>%
  dplyr::select(from,to,phecode=phenotype,connection_type,hazard_ratio=hr,pvalue,phecode_category) %>%
  filter(!is.na(from) & !is.na(to)) %>%
  mutate(hazard_ratio=as.numeric(hazard_ratio))

res = readRDS("data/omicspred_phewas_res.rds")
omicspred_res_connect = res %>%
  dplyr::select(Phenotype,`Trait ID`,Description,Type,`Hazard Ratio|Effect Size`,`FDR adjusted P-value`) %>%
  dplyr::rename(phenotype = Phenotype, variable = Description,connection_type=Type,
         hr = `Hazard Ratio|Effect Size`, pvalue = `FDR adjusted P-value`) %>%
  mutate(phenotype=normalize_phecodes(phenotype)) %>%
  left_join(., phecodes %>% dplyr::rename(phenotype=phecode),by="phenotype") %>%
  dplyr::select(variable,phenotype,connection_type,hr,pvalue,phecode_description=Description,phecode_category=category) %>%
  dplyr::rename(from = variable,to = phecode_description) %>%
  bind_rows(res %>%
              dplyr::select(Phenotype,`Trait ID`,Description,Type,`Hazard Ratio|Effect Size`,`FDR adjusted P-value`) %>%
              dplyr::rename(phenotype = Phenotype, variable = Description,connection_type=Type,
                     hr = `Hazard Ratio|Effect Size`, pvalue = `FDR adjusted P-value`) %>%
              mutate(phenotype=normalize_phecodes(phenotype)) %>%
              left_join(., phecodes %>% dplyr::rename(phenotype=phecode),by="phenotype") %>%
              dplyr::select(variable,phenotype,connection_type,hr,pvalue,phecode_description=Description,phecode_category=category) %>%
              dplyr::rename(from = phecode_description,to = variable)) %>%
  mutate(hazard_ratio = str_extract(hr,"[^ ()]+",group=NULL)) %>%
  dplyr::select(from,to,phecode=phenotype,connection_type,hazard_ratio,hr,pvalue,phecode_category) %>%
  filter(!is.na(from) & !is.na(to)) %>%
  mutate(connection_type=case_when(connection_type=="Gene expression"~"gene",
                                   connection_type=="Protein"~"protein",
                                   connection_type=="Metabolite"~"metabolite"),
         hazard_ratio=as.numeric(hazard_ratio)) %>%
  dplyr::select(-hr)

all_res = bind_rows(omicspred_res_connect %>% mutate(institution="ukb"),
                    vumc_res_connect %>% mutate(institution="vumc"))
saveRDS(all_res, file="data/all_res_ukb_vumc_original.rds")
# nodes = all_res %>%
#   dplyr::select(node = Description,type=Type) %>%
#   distinct(node,.keep_all = T) %>%
#   bind_rows(phecodes %>% dplyr::select(node=Description) %>% mutate(type="phecode")) %>%
#   mutate(type=case_when(type=="gene"~"gene",
#                         type=="protein"~"protein",
#                         type=="metabolite"~"metabolite",
#                         type=="phecode"~"phecode")) %>%
#   filter(!is.na(node))
# all_res = bind_rows(res %>% mutate(institution="ukb"),
#                     vumc_res %>% mutate(institution="vumc"))

##prepare the upset plot
upset_dat = tidy_connect %>%
  filter(institution == "vumc")
upset_dat = upset_dat[1:(nrow(upset_dat)/2),] 

upset_dat_res = purrr::map(unique(upset_dat$to),function(i){
  
  group_to = upset_dat %>%
    filter(to == i) %>%
    dplyr::select(from) %>%
    distinct(.) %>%
    pull(from)
  
})
names(upset_dat_res) = unique(upset_dat$to)
saveRDS(upset_dat_res, file = "upset_phecode_vumc.rds")


##update tool data
tidy_connect_phe = tidy_connect %>%
  filter(institution == "ukb")
tidy_connect_phe = tidy_connect_phe[1:(nrow(tidy_connect_phe)/2),] 

tidy_connect_phe_res = purrr::map_dfr(unique(tidy_connect_phe$from),function(i){
  
  group_from = tidy_connect_phe %>%
    filter(from == i) %>%
    dplyr::select(to) %>%
    distinct(.) %>%
    pull(to)
  
  expand.grid(from=group_from,to=group_from) 
})

tidy_connect_phe_res = tidy_connect_phe_res %>%
  filter(from!=to) %>%
  graph_from_data_frame(., directed=FALSE) %>%
  simplify %>%
  as_data_frame

tidy_connect_phe_res = tidy_connect_phe_res %>%
  filter(from == "Type 2 diabetes" | to == "Type 2 diabetes")



### JAK2 use case
path = "/proj/PI_SAVONA/Projects/CHIP_PheWAS/.common/data/92k/"
phe.result.full <- readRDS(paste0(path,"biovu_phewas_firth_JAK2_461_1102_aUPD_types.rds"))
phe.result = phe.result.full %>% filter(covariate %in% c("other aUPD","overlap v617f aUPD","GGCC/GGCC","GGCC/TCTT")) %>%
  filter(pvalue<0.005) %>%
  dplyr::select(phecode,description,covariate,OR,pvalue,pvalue_adj,group) 
saveRDS(phe.result,file = "data/phe_res_jak2.rds")
jak2_connect = bind_rows(data.frame(from=phe.result$covariate,to=phe.result$description,phecode=phe.result$phecode,connection_type="phecode",
                                    hazard_ratio=phe.result$OR,pvalue=phe.result$pvalue,phecode_category=phe.result$group,institution="vumc"),
                         data.frame(from=phe.result$description,to=phe.result$covariate,phecode=phe.result$phecode,connection_type="phecode",
                                    hazard_ratio=phe.result$OR,pvalue=phe.result$pvalue,phecode_category=phe.result$group,institution="vumc"))
tidy_connect = bind_rows(tidy_connect,jak2_connect)
saveRDS(tidy_connect,file = "data/all_res_jak2.rds")



##========================================================================================================
##========================================================================================================
##========================================================================================================
##========================================================================================================
## built up pair-wise phenotypes dataframe that cover 1) # of (adjusted) significant results 2) # of shared individuals
phenotype = phecodes$Description
ukb_connect = tidy_connect %>%
  filter(institution == "ukb")
shared_phe_sig_ukb = ukb_connect %>%
  filter(pvalue < 0.05) %>%
  dplyr::select(from,to) %>%
  graph_from_data_frame(., directed=FALSE) %>%
  igraph::simplify(.) %>%
  as_data_frame %>%
  mutate(biomolecular = ifelse(from %in% phenotype,to,from),
         sel_phenotype = ifelse(from %in% phenotype,from,to)) %>%
  dplyr::select(sel_phenotype,biomolecular) %>%
  inner_join(., ukb_connect %>%
               filter(pvalue < 0.05) %>%
               dplyr::select(from,to) %>%
               graph_from_data_frame(., directed=FALSE) %>%
               igraph::simplify(.) %>%
               as_data_frame %>%
               mutate(biomolecular = ifelse(from %in% phenotype,to,from),
                      co_phenotype = ifelse(from %in% phenotype,from,to)) %>%
               dplyr::select(biomolecular,co_phenotype),by="biomolecular") %>%
  group_by(sel_phenotype,co_phenotype) %>%
  summarise(shared_biomoleculars = n()) %>%
  dplyr::arrange(desc(shared_biomoleculars)) %>%
  dplyr::rename("# of shared significant biomolecules (adjusted)"=shared_biomoleculars) %>%
  mutate(institution="ukb") 
# %>%
#   rowwise() %>%
#   mutate(
#     pair = paste(sort(c(sel_phenotype, co_phenotype)), collapse = "_")
#   ) %>%
#   ungroup() %>%
#   dplyr::distinct(pair, .keep_all = TRUE) %>%
#   dplyr::select(-pair)

vumc_connect = tidy_connect %>%
  filter(institution == "vumc")
shared_phe_sig_vumc = vumc_connect %>%
  filter(pvalue < 0.05) %>%
  dplyr::select(from,to) %>%
  graph_from_data_frame(., directed=FALSE) %>%
  igraph::simplify(.) %>%
  as_data_frame %>%
  mutate(biomolecular = ifelse(from %in% phenotype,to,from),
         sel_phenotype = ifelse(from %in% phenotype,from,to)) %>%
  dplyr::select(sel_phenotype,biomolecular) %>%
  inner_join(., vumc_connect %>%
               filter(pvalue < 0.05) %>%
               dplyr::select(from,to) %>%
               graph_from_data_frame(., directed=FALSE) %>%
               igraph::simplify(.) %>%
               as_data_frame %>%
               mutate(biomolecular = ifelse(from %in% phenotype,to,from),
                      co_phenotype = ifelse(from %in% phenotype,from,to)) %>%
               dplyr::select(biomolecular,co_phenotype),by="biomolecular") %>%
  group_by(sel_phenotype,co_phenotype) %>%
  summarise(shared_biomoleculars = n()) %>%
  dplyr::arrange(desc(shared_biomoleculars)) %>%
  dplyr::rename("# of shared significant biomolecules (adjusted)"=shared_biomoleculars) %>%
  mutate(institution="vumc") 
shared_phe_sig = bind_rows(shared_phe_sig_ukb,shared_phe_sig_vumc)

## built up pair-wise phenotypes dataframe that cover 2) # of exclusively shared individuals
library(PheGenHelper)
initiate_database(type="phecode",phe=TRUE,phe_exclude=FALSE,phe_timeline=FALSE,demo=TRUE)
grid = read_delim("data/cleanchr_qc_plink2.fam",delim = "\t",col_names = F) %>%
  dplyr::select(X1) %>%
  dplyr::rename(grid=X1)
extract_grid(phe=TRUE,phe_exclude=FALSE,demo_dat,grid)
phe_dat = data.frame(as.matrix(phe_dat))
colnames(phe_dat) = normalize_phecodes(substring(colnames(phe_dat),2))

shared_phe_nsamples_vumc = phe_dat %>% rownames_to_column("individual")
shared_phe_nsamples_vumc <- shared_phe_nsamples_vumc %>%
  pivot_longer(cols = -individual, names_to = "phecode", values_to = "has_phecode") %>%
  filter(has_phecode == TRUE)
shared_phe_nsamples_vumc <- shared_phe_nsamples_vumc %>%
  inner_join(shared_phe_nsamples_vumc, by = "individual") %>%
  filter(phecode.x != phecode.y) %>%
  group_by(phecode.x, phecode.y) %>%
  summarise(shared_count = n(), .groups = "drop")
shared_phe_nsamples_vumc = shared_phe_nsamples_vumc %>%
  dplyr::rename(sel_phenotype = phecode.x, co_phenotype = phecode.y) %>%
  left_join(.,phecode_def %>% dplyr::select(phecode,description) %>% dplyr::rename(sel_phenotype=phecode),by="sel_phenotype") %>%
  dplyr::select(-sel_phenotype) %>% dplyr::rename(sel_phenotype = description) %>%
  left_join(.,phecode_def %>% dplyr::select(phecode,description) %>% dplyr::rename(co_phenotype=phecode),by="co_phenotype") %>%
  dplyr::select(-co_phenotype) %>% dplyr::rename(co_phenotype = description) %>%
  dplyr::select(sel_phenotype,co_phenotype,"# of shared individuals"=shared_count) %>%
  dplyr::mutate(institution="vumc")

library(UKBBdata)
shared_phe_nsamples_ukb = aws.s3::s3readRDS(object="phenome/UKBB_ICD10CMtoPhecode_long_08122022.rds",
                                bucket = "ukb.tbilab") %>%
  dplyr::rename(individual = eid)
shared_phe_nsamples_ukb <- shared_phe_nsamples_ukb %>%
  inner_join(shared_phe_nsamples_ukb, by = "individual") %>%
  filter(phecode.x != phecode.y) %>%
  group_by(phecode.x, phecode.y) %>%
  summarise(shared_count = n(), .groups = "drop")
shared_phe_nsamples_ukb = shared_phe_nsamples_ukb %>%
  dplyr::rename(sel_phenotype = phecode.x, co_phenotype = phecode.y) %>%
  left_join(.,phecode_def %>% dplyr::select(phecode,description) %>% dplyr::rename(sel_phenotype=phecode),by="sel_phenotype") %>%
  dplyr::select(-sel_phenotype) %>% dplyr::rename(sel_phenotype = description) %>%
  left_join(.,phecode_def %>% dplyr::select(phecode,description) %>% dplyr::rename(co_phenotype=phecode),by="co_phenotype") %>%
  dplyr::select(-co_phenotype) %>% dplyr::rename(co_phenotype = description) %>%
  dplyr::select(sel_phenotype,co_phenotype,"# of shared individuals"=shared_count) %>%
  dplyr::mutate(institution="ukb")
shared_phe_nsamples = bind_rows(shared_phe_nsamples_ukb,shared_phe_nsamples_vumc)
shared_phe = shared_phe_nsamples %>%
  left_join(.,shared_phe_sig,by=c("sel_phenotype","co_phenotype","institution"))
saveRDS(shared_phe,file = "data/shared_phe.rds")
phecode_in_ukb = unique(c(tidy_connect$from[tidy_connect$institution=="ukb"],tidy_connect$to[tidy_connect$institution=="ukb"]))
phecode_in_ukb = phecode_in_ukb[phecode_in_ukb %in% phecode_def$description]
phecode_in_vumc = unique(c(tidy_connect$from[tidy_connect$institution=="vumc"],tidy_connect$to[tidy_connect$institution=="vumc"]))
phecode_in_vumc = phecode_in_vumc[phecode_in_vumc %in% phecode_def$description]
shared_phe = shared_phe %>% dplyr::filter((shared_phe$institution=="ukb"&shared_phe$sel_phenotype %in% phecode_in_ukb&shared_phe$co_phenotype %in% phecode_in_ukb) | 
                                            (shared_phe$institution=="vumc"&shared_phe$sel_phenotype %in% phecode_in_vumc&shared_phe$co_phenotype %in% phecode_in_vumc))
saveRDS(shared_phe,file = "data/shared_phe_phecodein.rds")

##AD and lung cancer
shared_phe_ad_lung = shared_phe %>%
  filter(sel_phenotype %in% c("Alzheimer's disease","Cancer of bronchus; lung")) %>%
  dplyr::select(sel_phenotype, co_phenotype, `# of shared individuals`,`# of shared significant biomolecules (adjusted)`,institution) %>%
  group_by(institution) %>%
  arrange(institution,sel_phenotype,desc(`# of shared individuals`)) %>%
  filter(!is.na(co_phenotype))
write.table(shared_phe_ad_lung,file = "data/shared_phenotype_AD_Lung.txt",sep = "\t",quote = F,col.names = T,row.names = F)

##========================================================================================================
##========================================================================================================
## the shared biomolecules between each disease pair
phenotype = phecodes$Description
shared_phe_ad = shared_phe %>%
  filter(sel_phenotype == "Alzheimer's disease")
ukb_connect = tidy_connect %>%
  filter(institution == "ukb")
vumc_connect = tidy_connect %>%
  filter(institution == "vumc")
shared_sig_bio_ad_ukb = ukb_connect %>%
  filter(pvalue < 0.05) %>%
  dplyr::select(from,to) %>%
  graph_from_data_frame(., directed=FALSE) %>%
  igraph::simplify(.) %>%
  as_data_frame %>%
  mutate(biomolecular = ifelse(from %in% phenotype,to,from),
         sel_phenotype = ifelse(from %in% phenotype,from,to)) %>%
  dplyr::select(sel_phenotype,biomolecular) %>%
  inner_join(., ukb_connect %>%
               filter(pvalue < 0.05) %>%
               dplyr::select(from,to) %>%
               graph_from_data_frame(., directed=FALSE) %>%
               igraph::simplify(.) %>%
               as_data_frame %>%
               mutate(biomolecular = ifelse(from %in% phenotype,to,from),
                      co_phenotype = ifelse(from %in% phenotype,from,to)) %>%
               dplyr::select(biomolecular,co_phenotype),by="biomolecular") %>%
  filter(sel_phenotype == "Alzheimer's disease" & co_phenotype %in% shared_phe_ad$co_phenotype) %>%
  mutate(institution="ukb") 

shared_sig_bio_ad_vumc = vumc_connect %>%
  filter(pvalue < 0.05) %>%
  dplyr::select(from,to) %>%
  graph_from_data_frame(., directed=FALSE) %>%
  igraph::simplify(.) %>%
  as_data_frame %>%
  mutate(biomolecular = ifelse(from %in% phenotype,to,from),
         sel_phenotype = ifelse(from %in% phenotype,from,to)) %>%
  dplyr::select(sel_phenotype,biomolecular) %>%
  inner_join(., vumc_connect %>%
               filter(pvalue < 0.05) %>%
               dplyr::select(from,to) %>%
               graph_from_data_frame(., directed=FALSE) %>%
               igraph::simplify(.) %>%
               as_data_frame %>%
               mutate(biomolecular = ifelse(from %in% phenotype,to,from),
                      co_phenotype = ifelse(from %in% phenotype,from,to)) %>%
               dplyr::select(biomolecular,co_phenotype),by="biomolecular") %>%
  filter(sel_phenotype == "Alzheimer's disease" & co_phenotype %in% shared_phe_ad$co_phenotype) %>%
  mutate(institution="vumc") 
shared_sig_bio_ad = bind_rows(shared_sig_bio_ad_ukb,shared_sig_bio_ad_vumc)

shared_sig_bio_ad = shared_sig_bio_ad %>%
  left_join(.,shared_phe_ad, by=c("sel_phenotype","co_phenotype","institution")) %>%
  arrange(desc(`# of shared significant biomolecules (adjusted)`)) %>%
  filter(!is.na(`# of shared significant biomolecules (adjusted)`))
##========================================================================================================
##========================================================================================================
## permutation test
tidy_connect_simple_vumc= tidy_connect %>%
  filter(institution == "vumc") %>%
  filter(pvalue<0.05) %>%
  rowwise() %>%
  mutate(sorted_from = min(from, to),
         sorted_to = max(from, to)) %>%
  ungroup() %>%
  distinct(sorted_from, sorted_to, .keep_all = TRUE) %>%
  dplyr::select(-sorted_from, -sorted_to) %>%
  ungroup()
tidy_connect_simple_ukb= tidy_connect %>%
  filter(institution == "ukb") %>%
  filter(pvalue<0.05) %>%
  rowwise() %>%
  mutate(sorted_from = min(from, to),
         sorted_to = max(from, to)) %>%
  ungroup() %>%
  distinct(sorted_from, sorted_to, .keep_all = TRUE) %>%
  dplyr::select(-sorted_from, -sorted_to) %>%
  ungroup()
tidy_connect_simple = bind_rows(tidy_connect_simple_ukb,tidy_connect_simple_vumc)
  
count_shared_biomolecules <- function(data,from) {
  data %>%
    filter(institution=="ukb") %>%
    inner_join(data %>% filter(institution=="ukb"), by = c(from)) %>%
    group_by(to.x, to.y) %>%
    summarise(shared_biomolecules = n(), .groups = 'drop') %>%
    dplyr::rename(sel_phenotype = to.x, co_phenotype = to.y) %>%
    mutate(institution="ukb") %>%
    bind_rows(., data %>%
                filter(institution=="vumc") %>%
                inner_join(data %>% filter(institution=="vumc"), by = c(from)) %>%
                group_by(to.x, to.y) %>%
                summarise(shared_biomolecules = n(), .groups = 'drop') %>%
                dplyr::rename(sel_phenotype = to.x, co_phenotype = to.y) %>%
                mutate(institution="vumc"))
}
observed_counts <- count_shared_biomolecules(tidy_connect_simple,"from")

set.seed(123)
# Step 2: Create a function to shuffle biomolecule associations within each institution
permute_biomolecules <- function(data, n_permutations = 1000,from) {
  perm_results <- purrr::map(1:n_permutations, function(x) {
    # Shuffle the biomolecule-to-phenotype association within each institution
    permuted_data <- data %>%
      group_by(institution) %>%
      mutate(shuffled_from = sample(from))  # Shuffle the 'from' column (biomolecules) within each institution
    
    permuted_data = count_shared_biomolecules(permuted_data,"shuffled_from")
    permuted_data = observed_counts %>%
      dplyr::select(-shared_biomolecules) %>%
      left_join(., permuted_data, by=c("institution","sel_phenotype","co_phenotype")) %>%
      pull(shared_biomolecules)
      
  })
  
  # Bind results from permutations
  perm_counts_df <- do.call(cbind,perm_results)
  
  # Step 3: Calculate p-values
  p_values <- apply(perm_counts_df, 1, function(perm_counts) {
    mean(perm_counts < observed_counts$shared_biomolecules)
  })
  
  observed_counts$p_value <- p_values
  observed_counts
}

test = shared_sig_bio_ad %>% 
  left_join(.,observed_counts %>% dplyr::filter(sel_phenotype=="Alzheimer's disease"), by=c("institution","sel_phenotype","co_phenotype"))
write.table(test, file="data/shared_sig_biomolecule_pvalue.txt",col.names = T,row.names = F,sep = "\t",quote = F)















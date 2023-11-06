##clean datasets
phecodes <- phecode_def %>% dplyr::select(phecode,description) %>%
  dplyr::select(phecode) %>%
  join_phecode_info(cols_to_join = c("description", "category")) %>%
  rename(Description=description)

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
           filter(p_adjust < 0.05) %>%
           mutate(variable = str_remove_all(variable,"`"))

vumc_res_connect = vumc_res %>%
  mutate(phenotype=normalize_phecodes(phecode),hr=round(as.numeric(hr),2)) %>%
  left_join(., phecodes %>% rename(phenotype=phecode),by="phenotype") %>%
  dplyr::select(variable,phenotype,connection_type=group,hr,pvalue,p_adjust,phecode_description=Description,phecode_category=category) %>%
  rename(from = variable,to = phecode_description) %>%
  bind_rows(vumc_res %>%
              mutate(phenotype=normalize_phecodes(phecode)) %>%
              left_join(., phecodes %>% rename(phenotype=phecode),by="phenotype") %>%
              dplyr::select(variable,phenotype,connection_type=group,hr,pvalue,p_adjust,phecode_description=Description,phecode_category=category) %>%
              rename(from = phecode_description,to = variable)) %>%
  # mutate(hazard_ratio = str_extract(hr,"[^()]+",group=NULL)) %>%
  dplyr::select(from,to,phecode=phenotype,connection_type,hazard_ratio=hr,pvalue,phecode_category) %>%
  filter(!is.na(from) & !is.na(to)) %>%
  mutate(hazard_ratio=as.numeric(hazard_ratio))

res = readRDS("data/omicspred_phewas_res.rds")
omicspred_res_connect = res %>%
  dplyr::select(Phenotype,`Trait ID`,Description,Type,`Hazard Ratio|Effect Size`,`FDR adjusted P-value`) %>%
  rename(phenotype = Phenotype, variable = Description,connection_type=Type,
         hr = `Hazard Ratio|Effect Size`, pvalue = `FDR adjusted P-value`) %>%
  mutate(phenotype=normalize_phecodes(phenotype)) %>%
  left_join(., phecodes %>% rename(phenotype=phecode),by="phenotype") %>%
  dplyr::select(variable,phenotype,connection_type,hr,pvalue,phecode_description=Description,phecode_category=category) %>%
  rename(from = variable,to = phecode_description) %>%
  bind_rows(res %>%
              dplyr::select(Phenotype,`Trait ID`,Description,Type,`Hazard Ratio|Effect Size`,`FDR adjusted P-value`) %>%
              rename(phenotype = Phenotype, variable = Description,connection_type=Type,
                     hr = `Hazard Ratio|Effect Size`, pvalue = `FDR adjusted P-value`) %>%
              mutate(phenotype=normalize_phecodes(phenotype)) %>%
              left_join(., phecodes %>% rename(phenotype=phecode),by="phenotype") %>%
              dplyr::select(variable,phenotype,connection_type,hr,pvalue,phecode_description=Description,phecode_category=category) %>%
              rename(from = phecode_description,to = variable)) %>%
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

nodes = all_res %>%
  dplyr::select(node = Description,type=Type) %>%
  distinct(node,.keep_all = T) %>%
  bind_rows(phecodes %>% dplyr::select(node=Description) %>% mutate(type="phecode")) %>%
  mutate(type=case_when(type=="gene"~"gene",
                        type=="protein"~"protein",
                        type=="metabolite"~"metabolite",
                        type=="phecode"~"phecode")) %>%
  filter(!is.na(node))

all_res = bind_rows(res %>% mutate(institution="ukb"),
                    vumc_res %>% mutate(institution="vumc"))

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

library(UpSetR)
upset(fromList(upset_dat_res),order.by = "freq")

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


























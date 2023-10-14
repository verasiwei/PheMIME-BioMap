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






source("modules/load_library.R")
source("modules/helpers_func.R")
load("data/phecode_def.rda")
load("data/phecode_descriptions.rda")
load("data/default_sel.rda")

all_id = readRDS("data/all_id.rds")
tidy_connect = readRDS("data/all_res_vumc_original_p_0.05.rds")
tidy_connect$from = str_replace_all(tidy_connect$from,", ","_")
tidy_connect$to = str_replace_all(tidy_connect$to,", ","_")
all_dat = readRDS("data/all_dat.rds")
phecodes = readRDS("data/phecodes.rds")
nodes = readRDS("data/nodes.rds")
# vumc_res = readRDS("data/vumc_phewas_res_original_p_0.05.rds")
# ukbb_res = readRDS("data/omicspred_phewas_res.rds")


data_network_initial = readRDS("data/data_network_initial.rds")
upset_results_initial = readRDS("data/upset_results_initial.rds")
marginalData_initial = readRDS("data/marginalData_initial.rds")
enrichment_network_initial = readRDS("data/enrichment_network_initial_update.rds")
enrichment_network_all_initial = readRDS("data/enrichment_network_all_initial.rds")
shared_phe = readRDS("data/shared_phe.rds")
sil_dat = readRDS("data/sil_dat.rds")
# data_network_initial$nodes$id = str_replace(data_network_initial$nodes$id,"-",".")
# data_network_initial$edges$source = str_replace(data_network_initial$edges$source,"-",".")
# data_network_initial$edges$target = str_replace(data_network_initial$edges$target,"-",".")
# data_heatmap_initial = readRDS("data/data_heatmap_initial.rds")
# data_heatmap_gene_initial = readRDS("data/data_heatmap_gene_initial.rds")
# data_heatmap_protein_initial = readRDS("data/data_heatmap_protein_initial.rds")
# data_heatmap_metabolite_initial = readRDS("data/data_heatmap_metabolite_initial.rds")
# data_heatmap_phecode_initial = readRDS("data/data_heatmap_phecode_initial.rds")
# data_network_depression = readRDS("data/data_network_depression.rds")
# res_cluster = readRDS("data/res_cluster.rds")
# vumc_res = readRDS("data/vumc_res.rds")
# vumc_score = aws.s3::s3read_using(read.csv,object = "s3://tbilab/vis_tool/PheOmicsMultimorbidity/vumc_res.csv")
# vumc_score = read_delim("vumc_res.csv",delim = ",") 
# vumc_score = aws.s3::save_object("s3://tbilab/vis_tool/PheOmicsMultimorbidity/vumc_res.csv") %>%
  # data.table::fread()

# phecode_in = data.frame(phecode=unique(normalize_phecodes(ukbb_res$Phenotype)),institution="ukb") %>%
#   bind_rows(data.frame(phecode=unique(normalize_phecodes(vumc_res$phecode)),institution="vumc"))
#   # unique(c(normalize_phecodes(ukbb_res$Phenotype),vumc_res$phecode))
# phecodes <- phecode_def %>% dplyr::select(phecode,description) %>%
#   dplyr::select(phecode) %>%
#   join_phecode_info(cols_to_join = c("description", "category")) %>%
#   dplyr::rename(Description=description) %>%
#   # filter(phecode %in% unique(phecode_in$phecode)) %>%
#   left_join(.,phecode_in,by="phecode") %>%
#   filter(!is.na(institution)) %>%
#   mutate(Description=str_replace_all(Description,", ","_"))%>%
#     dplyr::rename(code=phecode)
# saveRDS(phecodes,file = "data/phecodes.rds")
# # #jak2
# phecodes = bind_rows(phecodes,data.frame(phecode=c("other aUPD","overlap v617f aUPD","GGCC/GGCC","GGCC/TCTT"),
#                                          Description=c("other aUPD","overlap v617f aUPD","GGCC/GGCC","GGCC/TCTT"),
#                                          category=c("other aUPD","overlap v617f aUPD","GGCC/GGCC","GGCC/TCTT")))

# nodes = ukbb_res %>%
#   dplyr::select(node = Description,type=Type) %>%
#   distinct(node,.keep_all = T) %>%
#   bind_rows(phecodes %>% dplyr::select(node=Description,institution) %>% mutate(type="phecode") %>% filter(institution=="ukb")) %>%
#   mutate(type=case_when(type=="Gene expression"~"gene",
#                         type=="Protein"~"protein",
#                         type=="Metabolite"~"metabolite",
#                         type=="phecode"~"phecode"),
#          institution="ukb") %>%
#   bind_rows(vumc_res %>%
#               dplyr::select(node = variable,type=group) %>%
#               distinct(node,.keep_all = T) %>%
#               bind_rows(phecodes %>% dplyr::select(node=Description,institution) %>% mutate(type="phecode") %>% filter(institution=="vumc"))%>%
#               mutate(institution="vumc")) %>%
#   mutate(node = str_replace_all(node,", ","_"))
# # %>%
#   # filter(!is.na(node)) %>%
#   # distinct(node,.keep_all = T)
# saveRDS(nodes, file = "data/nodes.rds")

##jak2
# phe.result = readRDS("data/phe_res_jak2.rds")
# nodes = bind_rows(nodes,phe.result %>%
#                     dplyr::select(node=description) %>% mutate(type="phecode")) %>%
#   distinct(node,.keep_all = T)
# nodes = bind_rows(nodes,data.frame(node=c("other aUPD","overlap v617f aUPD","GGCC/GGCC","GGCC/TCTT"),
#                                    type=rep("phecode",4)))
  
# phecodes = phecodes %>%
#   dplyr::rename(code=phecode)
# 
# genes = nodes %>%
#   filter(type == "gene") %>%
#   dplyr::select(Description=node,institution) %>%
#   distinct(Description,.keep_all = T) %>%
#   mutate(code="gene")
# 
# proteins = nodes %>%
#   filter(type == "protein") %>%
#   dplyr::select(Description=node,institution) %>%
#   distinct(Description,.keep_all = T) %>%
#   mutate(code="protein")
# 
# metabolites = nodes %>%
#   filter(type == "metabolite") %>%
#   dplyr::select(Description=node,institution) %>%
#   distinct(Description,.keep_all = T) %>%
#   mutate(code="metabolite")
# 
# all_dat = bind_rows(phecodes %>% mutate(group="phecode"),
#                     genes %>% mutate(group="gene"),
#                     proteins %>% mutate(group="protein"),
#                     metabolites %>% mutate(group="metabolite"))
# saveRDS(all_dat,file = "data/all_dat.rds")







# ##prepare pre-selected data
# tidy_connect = res %>%
#   filter(!is.na(from) & !is.na(to)) %>%
#   mutate(connection_type=case_when(connection_type=="Gene expression"~"gene",
#                                    connection_type=="Protein"~"protein",
#                                    connection_type=="Metabolite"~"metabolite"))
# 
# ## 1st layer nodes connected with user selected phecode
# conn_first = tidy_connect %>%
#   filter(from %in% "Depression") %>%
#   dplyr::select(node = to) %>%
#   bind_rows(tidy_connect %>%
#               filter(to %in% "Depression") %>%
#               dplyr::select(node = from)) %>%
#   distinct(node) %>%
#   pull(node)
# ## phecode connected with molecular-level nodes
# conn_second = tidy_connect %>%
#   filter(from %in% conn_first)%>%
#   dplyr::select(node = to) %>%
#   bind_rows(tidy_connect %>%
#               filter(to %in% conn_first) %>%
#               dplyr::select(node = from)) %>%
#   distinct(node) %>%
#   pull(node)
# # conn = unique(c(current_code,conn_first,conn_second))
# tidy_connect = tidy_connect %>%
#   filter(from %in% conn_first | to %in% conn_first) %>%
#   dplyr::select(from,to) %>%
#   bind_rows(.,tidy_connect %>%
#               filter(from %in% conn_second | to %in% conn_second) %>%
#               dplyr::select(from,to)) %>%
#   graph_from_data_frame(., directed=FALSE) %>%
#   simplify %>%
#   as_data_frame
# 
# nodes = tidy_connect %>%
#   dplyr::select(node=from) %>%
#   bind_rows(
#     tidy_connect %>%
#       dplyr::select(node=to)
#   ) %>%
#   distinct(node,.keep_all = T) %>%
#   left_join(.,nodes,by="node") %>%
#   distinct(node,.keep_all = T) %>%
#   arrange(type) %>%
#   mutate(selected = ifelse(node %in% "Depression","yes","no"))
# 
# # Save data for JS visualization
# dat = list(
#   nodes = nodes %>% dplyr::rename(id = node),
#   edges = tidy_connect %>% dplyr::rename(source = from, target = to)
# )
# saveRDS(dat,"data/data_network_depression.rds")
# 
# 
# 
# 


# ### extract disease phenotypes that having shared/co-occurred biomoleculars nodes with the input disease phenotypes
# ##  biomolecular nodes that connected to the input phenotypes
# shared_bio = tidy_connect %>% dplyr::filter(from %in% test| to %in% test) %>%
#   filter(pvalue<0.05/10000) %>%
#   filter(institution == "ukb") %>%
#   dplyr::select(from,to) %>%
#   graph_from_data_frame(., directed=FALSE) %>%
#   igraph::simplify(.) %>%
#   as_data_frame %>%
#   mutate(biomolecular = ifelse(from %in% test,to,from),
#          select_phenotype = ifelse(from %in% test,from,to)) %>%
#   dplyr::select(select_phenotype,biomolecular)
# ##phenotypes shared the common biomolecular nodes with the input phenotypes
# shared_phe = tidy_connect %>%
#   filter(pvalue<0.05/10000) %>%
#   filter(institution == "ukb") %>%
#   dplyr::select(from,to) %>%
#   graph_from_data_frame(., directed=FALSE) %>%
#   igraph::simplify(.) %>%
#   as_data_frame %>%
#   filter((from %in% shared_bio$biomolecular)|
#            (to %in% shared_bio$biomolecular)) %>%
#   # filter(!(from %in% test | to %in% test)) %>%
#   mutate(co_phenotype = ifelse(from %in% shared_bio$biomolecular, to, from),
#          biomolecular = ifelse(from %in% shared_bio$biomolecular, from, to)) %>%
#   dplyr::select(co_phenotype,biomolecular)
# 
# shared_phe_ukb = shared_bio %>%
#   inner_join(.,shared_phe,by="biomolecular") %>%
#   group_by(select_phenotype,co_phenotype) %>%
#   summarise(shared_biomoleculars = n()) %>%
#   dplyr::arrange(desc(shared_biomoleculars)) %>%
#   dplyr::rename("# of shared biomolecules"=shared_biomoleculars) %>%
#   left_join(.,phecode_def %>% dplyr::select(phecode,description) %>% dplyr::rename(select_phenotype=description),by="select_phenotype") %>%
#   dplyr::rename(select_phecode=phecode) %>%
#   left_join(.,phecode_def %>% dplyr::select(phecode,description) %>% dplyr::rename(co_phenotype=description),by="co_phenotype") %>%
#   dplyr::rename(co_phecode=phecode) %>%
#   mutate(institution="ukb")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 

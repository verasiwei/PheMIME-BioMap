library('biomaRt')

### data clean and match by id with names
vumc_score = aws.s3::save_object("s3://tbilab/vis_tool/PheOmicsMultimorbidity/vumc_proteomics_somalogic.csv") %>%
  data.table::fread()
# vumc_score = read_delim("vumc_transcriptomics.csv",delim = ",") 
trans_id = read_delim("transcriptomics_id.csv",delim="\t") %>%
  dplyr::select(1:3) %>%
  rename(omicpred_id = `OMICSPRED ID`,variable=Gene) %>%
  mutate(group="transcriptomics")
# mart <- useDataset("hsapiens_gene_ensembl", useMart("ensembl"))
# G_list <- getBM(filters= "ensembl_gene_id", 
#                   attributes= c("ensembl_gene_id","hgnc_symbol"),values=trans_id$`Ensembl ID`,mart= mart) 
# trans_id = trans_id %>%
#   left_join(.,G_list %>% rename(`Ensembl ID`=ensembl_gene_id,gene=hgnc_symbol) %>%
#               mutate(gene=ifelse(gene=="",NA,gene)),by="Ensembl ID") %>%
#   dplyr::select(1:2,5,4)

proteomics_soma_id = read_delim("proteomics_somalogic_id.csv",delim = "\t") %>%
  dplyr::select(1:2,5) %>%
  rename(omicpred_id = `OMICSPRED ID`,variable=Protein) %>%
  mutate(group="proteomics_soma")

proteomics_olink_id = read_delim("proteomics_olink_id.csv",delim = "\t") %>%
  dplyr::select(1:2,4) %>%
  rename(omicpred_id = `OMICSPRED ID`,variable=Protein) %>%
  mutate(group="proteomics_olink")

metabolomics_meta_id = read_delim("metabolomics_metabolon_id.csv",delim = "\t") %>%
  dplyr::select(1:3) %>%
  rename(omicpred_id = `OMICSPRED ID`,variable=`Biochemical Name`) %>%
  mutate(group="metabolomics_metabolon")

metabolomics_night_id = read_delim("metabolomics_nightingale_id.csv",delim = "\t") %>%
  dplyr::select(1:3) %>%
  rename(omicpred_id = `OMICSPRED ID`,variable=`Biomarker Name`) %>%
  mutate(group="metabolomics_nightingale")

all_id = bind_rows(trans_id %>% rename(orig_id = `Ensembl ID`),
                   proteomics_soma_id %>% rename(orig_id=`SOMAscan ID`),
                   proteomics_olink_id %>% rename(orig_id=`UniProt ID`),
                   metabolomics_meta_id %>% rename(orig_id=`Metabolon ID`),
                   metabolomics_night_id %>% rename(orig_id=`Trait ID`))
saveRDS(all_id,"data/all_id.rds")


######################################
all_id = readRDS("data/all_id.rds")
vumc_score = aws.s3::save_object("s3://tbilab/vis_tool/PheOmicsMultimorbidity/vumc_metabolomics_metabolon.csv") %>%
  data.table::fread()
vumc_score2 = aws.s3::save_object("s3://tbilab/vis_tool/PheOmicsMultimorbidity/vumc_metabolomics_nightingale.csv") %>%
  data.table::fread()
vumc_score = as.data.frame(cbind(vumc_score,vumc_score2))
name = data.frame(omicpred_id=colnames(vumc_score))
name = name %>%
  left_join(.,all_id %>% filter(group=="metabolomics_metabolon" | group=="metabolomics_nightingale"),by="omicpred_id") %>%
  mutate(variable = ifelse(is.na(variable),orig_id,variable))
colnames(vumc_score) = name$variable
vumc_score = vumc_score[,!duplicated(colnames(vumc_score))]
fam = read_delim("cleanchr_qc_plink2.fam",delim = "\t",col_names = F)
dat = cbind(data.frame(id=fam$X1),vumc_score)
saveRDS(dat,file="vumc_metabolomics.rds")
dat = readRDS("vumc_transcriptomics.rds")
aws.s3::s3saveRDS(x=dat, bucket = "tbilab",object = "/vis_tool/PheOmicsMultimorbidity/vumc_transcriptomics.rds")

## conduct survival analysis for VUMC data
# library(PheGenHelper)
library(phewasHelper)
### read data
transcriptomics_dat = readRDS("vumc_transcriptomics.rds")
initiate_database(type="phecode",phe_exclude=TRUE,phe_timeline=TRUE,demo=TRUE)
grid <- data.frame(grid=transcriptomics_dat$id)
extract_grid(phe=FALSE,phe_exclude=TRUE,demo_dat,grid)

### generate efficient data format
### for each phenotype, 
### id_to_demographics: outcome:age of first occurrence of each phenotype/censored:last EHR record date/date of death-DOB (2017 version); variable: censored status + demographics + gene scores

### phe_dat_exclude to define case/control(censored) and then extract case/control(censored) with time to event info from phe_timeline_dat
phe_dat_exclude = phe_dat_exclude[phe_dat_exclude@Dimnames[[1]] %in% demo_dat$grid,]
demo_dat = demo_dat %>% filter(grid %in% phe_dat_exclude@Dimnames[[1]])
phe_dat_exclude_long = wide_to_long(phe_dat_exclude,type="phecode") %>% arrange(grid) %>%
                mutate(phecode = str_split(phecode, 'X', simplify = TRUE)[,2]) %>%
                mutate(phecode = normalize_phecodes(phecode))
aws.s3::s3saveRDS(x=phe_dat_exclude_long, bucket = "tbilab",object = "/vis_tool/PheOmicsMultimorbidity/phe_dat_exclude_long_bioVU.rds")
phe_timeline_event = phe_timeline_dat %>%
                     filter(grid %in% unique(phe_dat_exclude_long$grid)) %>%
                     mutate(phecode = normalize_phecodes(phecode)) %>%
                     mutate(time = as.Date(time,"%Y-%m-%d")) %>%
                     arrange(grid,time) %>%
                     group_by(phecode,grid) %>%
                     summarise(min_date=min(time)) %>% 
                     ungroup() %>%
                     left_join(.,demo_dat %>% 
                                  dplyr::select(grid,DOB) %>%
                                  mutate(DOB=as.Date(DOB,"%Y-%m-%d")),by="grid")
aws.s3::s3saveRDS(x=phe_timeline_event, bucket = "tbilab",object = "/vis_tool/PheOmicsMultimorbidity/phe_timeline_event_bioVU.rds")

phe_timeline_last = phe_timeline_dat %>%
                    filter(grid %in% unique(phe_dat_exclude_long$grid)) %>%
                     mutate(phecode = normalize_phecodes(phecode)) %>%
                     mutate(time = as.Date(time,"%Y-%m-%d")) %>%
                     arrange(grid,time) %>%
                     group_by(grid) %>%
                     summarise(max_date_all=max(time)) %>% 
                     ungroup() %>%
                     left_join(.,demo_dat %>% 
                                  dplyr::select(grid,DOB,DOD) %>%
                                  mutate(DOB=as.Date(DOB,"%Y-%m-%d"),
                                         DOD=as.Date(DOD,"%Y-%m-%d")),by="grid")
aws.s3::s3saveRDS(x=phe_timeline_last, bucket = "tbilab",object = "/vis_tool/PheOmicsMultimorbidity/phe_timeline_last_bioVU.rds")

### generate a nested dataset with each phecode ~ nested outcome (age/censored)
phe_dat_exclude_long <- aws.s3::s3readRDS(bucket = "tbilab",object="/vis_tool/PheOmicsMultimorbidity/phe_dat_exclude_long_bioVU.rds")
phe_timeline_dat = aws.s3::s3readRDS(bucket = "tbilab",object="/vis_tool/PheOmicsMultimorbidity/phe_timeline_dat_bioVU.rds")


##individuals having this phecode
grid_all = data.frame(grid=unique(phe_dat_exclude_long$grid))
start=Sys.time()
phecode_to_outcome <- phe_dat_exclude_long[1:1000,] %>% 
    group_by(phecode) %>%
    nest() %>%
    mutate(group_time = map2(phecode,data,function(x,y){
    #case = unique(phe_dat_exclude_long$grid[phe_dat_exclude_long$phecode == x$phecode])
    group_time = grid_all %>%
              mutate(group = ifelse(grid %in% y,1,0)) %>%
              left_join(.,phe_timeline_event %>% 
                            filter(phecode == x) %>%
                            dplyr::select(grid,min_date,DOB),by="grid") %>%
              mutate(time = round(as.numeric(min_date-DOB)/365,2)) %>%
              dplyr::select(-DOB) %>% 
              left_join(.,phe_timeline_last, by="grid") %>%
              mutate(time = ifelse(is.na(time),as.numeric(max_date_all-DOB)/365,time)) %>%
              dplyr::select(group,time)   
    group_time
    }))
end=Sys.time()
  




















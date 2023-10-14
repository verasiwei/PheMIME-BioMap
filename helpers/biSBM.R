## apply biSBM clustering to phecode~transcriptomics matrix


### graph-tool
### generate phecode by transciptomics matrix
# g = graph_from_data_frame(res %>% dplyr::select(from,to,hazard_ratio) %>% 
#                             rename(weight=hazard_ratio) %>%
#                             mutate(weight=as.numeric(weight)) %>%
#                             data.frame(),
#                           directed = F,vertices = nodes)
# g = simplify(g)
# adj <- get.adjacency(g,type = "both",attr = "weight")
cluster_res = run_sbm(res %>% dplyr::select(from,to,hazard_ratio) %>% 
                         rename(weight=hazard_ratio) %>%
                         mutate(weight=as.numeric(weight)))

### sbmr
library(sbmr)
dat_bisbm = new_sbm_network(edges = res,
                                  bipartite_edges = TRUE,
                                  edges_from_column = from,
                                  edges_to_column = to,
                                  random_seed = 777)
##find initial state
dat_bisbm = dat_bisbm  %>% 
  collapse_blocks(desired_n_blocks = 4,
                  num_mcmc_sweeps = 5,
                  sigma = 1.1)
##select the initial position
dat_bisbm <- dat_bisbm %>% 
  choose_best_collapse_state(heuristic = "delta_ratio", verbose = TRUE)
##run mcmc sweeps from the initial position
num_sweeps <- 10
dat_bisbm <- dat_bisbm %>%
  mcmc_sweep(num_sweeps = num_sweeps, 
             eps = 0.1, 
             track_pairs = TRUE)
res_cluster = dat_bisbm %>% 
  state()
saveRDS(res_cluster,file = "data/res_cluster.rds")









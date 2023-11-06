multipartite_network = function(id){
  ns = NS(id)
  fluidRow(
    column(width=9,
           column(6,actionButton(ns("return"), "Revert to entire network",style="font-size: 1.5rem;float:left;background-color:#D3D3D3;")),
           hr(),
           # conditionalPanel(
           #   condition = "input.close_network > 0",
           #   style = "display: none;background-color:#F2F3F6",
           #   ns=ns,
           withSpinner(r2d3::d3Output(ns("network"),width = "100%", height = "750px"),
                       hide.ui = FALSE)
           # )
           # ) #box
    ), #column
    
    column(width=3,
           box(
            width = 12,
            title = "",
            closable = F,
            status = "warning",
            solidHeader = FALSE,
            collapsible = TRUE,
            textOutput(ns("node_info"))
           )
    )
  ) #fluidrow
}

multipartite_networkServer = function(id,molecular,pvalue,update_network,reset_network,current_description,visualize_network,current_institution,current_data){
  moduleServer(id,
               function(input,output,session){
                 
                 ## update node information of user selected, received from omics_network.js
                 observe({
                   
                   clicked_node_id = input$clicked_node_id
                   if (!is.null(clicked_node_id)) {
                     
                     output$node_info = renderText({paste0("Clicked Node ID:", clicked_node_id, "\n")})
                     
                   }
                 })
                 
                 
                 ## update data for network visualization
                 data_for_network = reactiveVal(data_network_initial)
                 
                 ## observe visualization of the network
                 observeEvent(visualize_network(),{
                   
                   # ## 1st layer nodes connected with user selected phecode
                   # conn_first = tidy_connect %>%
                   #   filter(institution == current_institution()) %>%
                   #   filter(from %in% current_description()) %>%
                   #   dplyr::select(node = to) %>%
                   #   bind_rows(tidy_connect %>%
                   #               filter(institution == current_institution()) %>%
                   #               filter(to %in% current_description()) %>%
                   #               dplyr::select(node = from)) %>%
                   #   distinct(node) %>%
                   #   pull(node)
                   # 
                   # conn = unique(c(current_description(),conn_first))
                   tidy_connect_sub = tidy_connect %>%
                     filter(from %in% current_description() | to %in% current_description()) %>%
                     # filter(from %in% conn_first | to %in% conn_first) %>%
                     dplyr::select(from,to) %>%
                     graph_from_data_frame(., directed=FALSE) %>%
                     simplify %>%
                     as_data_frame %>%
                     rename(source=from,target=to)
                   
                   nodes = tidy_connect_sub %>%
                     dplyr::select(node=source) %>%
                     bind_rows(
                       tidy_connect_sub %>%
                         dplyr::select(node=target)
                     ) %>%
                     distinct(node,.keep_all = T) %>%
                     left_join(.,nodes,by="node") %>%
                     distinct(node,.keep_all = T) %>%
                     arrange(type) %>%
                     makeTooltips(.)
                   nodes = nodes %>%
                     mutate(selected = ifelse(node %in% current_description,"yes","no")) %>%
                     mutate(color=case_when(type=="gene" ~ "#689030",
                                            type=="protein" ~ "#5E738F",
                                            type=="metabolite" ~ "#AD6F3B",
                                            type=="phecode" ~ "#673770"),
                            size =case_when(type=="gene" ~ 0.1,
                                             type=="protein" ~ 0.1,
                                             type=="metabolite" ~ 0.1,
                                             type=="phecode" ~ 0.3),
                            selectable=ifelse(selected == "yes",TRUE,FALSE),
                            id=1:nrow(.),
                            index=1:nrow(.),
                            name = node,
                            inverted =ifelse(selected == "yes",TRUE,FALSE)) %>%
                     dplyr::select(index,name,color,size,selectable,id,inverted)
                   
                   tidy_connect_sub = tidy_connect_sub %>%
                     left_join(.,nodes %>% dplyr::select(name,id) %>% rename(source=name),by="source") %>%
                     dplyr::select(-source) %>%
                     dplyr::select(source=id,target) %>%
                     left_join(.,nodes %>% dplyr::select(name,id) %>% rename(target=name),by="target") %>%
                     dplyr::select(-target) %>%
                     dplyr::select(source,target=id)
                   # Save data for JS visualization
                   dat = list(
                     vertices = nodes,
                     edges = tidy_connect_sub
                     # ,
                     # extra = data.frame(nothing=Sys.time())
                   )
                   data_for_network(dat)
                 })
                 
                 ### observe return to network
                 observeEvent(input$return,{
                   
                   # # ## 1st layer nodes connected with user selected phecode 
                   # conn_first = tidy_connect %>%
                   #   filter(institution == current_institution()) %>%
                   #   filter(from %in% current_description()) %>%
                   #   dplyr::select(node = to) %>%
                   #   bind_rows(tidy_connect %>%
                   #               filter(institution == current_institution()) %>%
                   #               filter(to %in% current_description()) %>%
                   #               dplyr::select(node = from)) %>%
                   #   distinct(node) %>%
                   #   pull(node)
                   # conn = unique(c(current_description(),conn_first))
                   tidy_connect_sub = tidy_connect %>%
                     # filter(from %in% conn_first | to %in% conn_first) %>%
                     filter(from %in% current_description() | to %in% current_description()) %>%
                     dplyr::select(from,to) %>%
                     graph_from_data_frame(., directed=FALSE) %>% 
                     simplify %>%                                 
                     as_data_frame 
                   
                   nodes = tidy_connect_sub %>% 
                     dplyr::select(node=from) %>%
                     bind_rows(
                       tidy_connect_sub %>% 
                         dplyr::select(node=to)
                     ) %>% 
                     distinct(node,.keep_all = T) %>%
                     left_join(.,nodes,by="node") %>%
                     distinct(node,.keep_all = T) %>%
                     arrange(type) %>%
                     mutate(selected = ifelse(node %in% current_description(),"yes","no"))
                   
                   # Save data for JS visualization
                   dat = list(
                     nodes = nodes %>% dplyr::rename(id = node),
                     edges = tidy_connect_sub %>% dplyr::rename(source = from, target = to), 
                     extra = data.frame(nothing=Sys.time()) 
                   ) 
                   data_for_network(dat)
                   
                 })
                 
                 ### observe reset network after changing p-value cut-off or molecular levels
                 observeEvent(reset_network(),{
                   
                   # parent = res_cluster$parent[res_cluster$id %in% current_description()]
                   # connected_code = res_cluster$id[res_cluster$parent %in% parent]
                   
                   # # ## 1st layer nodes connected with user selected phecode 
                   # conn_first = tidy_connect %>%
                   #   filter(institution == current_institution()) %>%
                   #   filter(from %in% current_description()) %>%
                   #   dplyr::select(node = to) %>%
                   #   bind_rows(tidy_connect %>%
                   #               filter(institution == current_institution()) %>%
                   #               filter(to %in% current_description()) %>%
                   #               dplyr::select(node = from)) %>%
                   #   distinct(node) %>%
                   #   pull(node)
                   # conn = unique(c(current_description(),conn_first))
                   tidy_connect_sub = tidy_connect %>%
                     # filter(from %in% conn_first | to %in% conn_first) %>%
                     filter(from %in% current_description() | to %in% current_description()) %>%
                     dplyr::select(from,to) %>%
                     graph_from_data_frame(., directed=FALSE) %>% 
                     simplify %>%                                 
                     as_data_frame 
                   
                   nodes = tidy_connect_sub %>% 
                     dplyr::select(node=from) %>%
                     bind_rows(
                       tidy_connect_sub %>% 
                         dplyr::select(node=to)
                     ) %>% 
                     distinct(node,.keep_all = T) %>%
                     left_join(.,nodes,by="node") %>%
                     distinct(node,.keep_all = T) %>%
                     arrange(type) %>%
                     mutate(selected = ifelse(node %in% current_description(),"yes","no"))
                   
                   # Save data for JS visualization
                   dat = list(
                     nodes = nodes %>% dplyr::rename(id = node),
                     edges = tidy_connect_sub %>% dplyr::rename(source = from, target = to), 
                     extra = data.frame(nothing=Sys.time()) 
                   ) 
                   data_for_network(dat)
                 })
                 
                 ### observe update of p-value or molecular levels
                 observeEvent(update_network(),{
                   # parent = res_cluster$parent[res_cluster$id %in% current_description()]
                   # connected_code = res_cluster$id[res_cluster$parent %in% parent]
                   
                   tidy_connect_sub = tidy_connect %>%
                     filter(pvalue<isolate(pvalue())) %>%
                     filter(connection_type==isolate(molecular()) | connection_type=="phecode")
                   
                   # ## 1st layer nodes connected with user selected phecode 
                   # conn_first = tidy_connect_sub %>%
                   #   filter(institution == current_institution()) %>%
                   #   filter(from %in% current_description()) %>%
                   #   dplyr::select(node = to) %>%
                   #   bind_rows(tidy_connect_sub %>%
                   #               filter(institution == current_institution()) %>%
                   #               filter(to %in% current_description()) %>%
                   #               dplyr::select(node = from)) %>%
                   #   distinct(node) %>%
                   #   pull(node)
                   # conn = unique(c(current_description(),conn_first))
                   tidy_connect_sub = tidy_connect_sub %>%
                     # filter(from %in% conn_first | to %in% conn_first) %>%
                     filter(from %in% current_description() | to %in% current_description()) %>%
                     dplyr::select(from,to) %>%
                     graph_from_data_frame(., directed=FALSE) %>% 
                     simplify %>%                                 
                     as_data_frame 
                   
                   nodes = tidy_connect_sub %>% 
                     dplyr::select(node=from) %>%
                     bind_rows(
                       tidy_connect_sub %>% 
                         dplyr::select(node=to)
                     ) %>% 
                     distinct(node,.keep_all = T) %>%
                     left_join(.,nodes,by="node") %>%
                     distinct(node,.keep_all = T) %>%
                     arrange(type) %>%
                     mutate(selected = ifelse(node %in% current_description(),"yes","no"))
                   
                   # Save data for JS visualization
                   dat = list(
                     nodes = nodes %>% dplyr::rename(id = node),
                     edges = tidy_connect_sub %>% dplyr::rename(source = from, target = to), 
                     extra = data.frame(nothing=Sys.time()) 
                   ) 
                   data_for_network(dat)
                 })
                 
                 observeEvent(reset_network(),{
                   updateNumericInput(session,"pvalue",label=NULL,value=0.05,min=0,max=0.05)
                   updateSelectInput(session,"molecular",selected = NULL,label="",
                                     choices=list("","Gene"="gene","Protein"="protein","Metabolite"="metabolite"))
                 })
                 
                 observeEvent(input$run_network,{
                   updateNumericInput(session,"pvalue",label=NULL,value=0.05,min=0,max=0.05)
                   updateSelectInput(session,"molecular",selected = NULL,label="",
                                     choices=list("","Gene"="gene","Protein"="protein","Metabolite"="metabolite"))
                 })
                 
                 output$network = r2d3::renderD3({
                   # r2d3(
                   #   data=data_for_network(),
                   #   script = "inst/omics_network.js",
                   #   # dependencies = "inst/style.css",
                   #   options = list(r2d3.theme = list(background="#F2F3F6")),
                   #   container = "svg",
                   #   d3_version = "5"
                   # )
                   
                   dat %>%
                     jsonlite::toJSON() %>%
                     r2d3::r2d3(
                       script = here('inst/network_2d.js'),
                       container = 'canvas',
                       dependencies = "d3-jetpack",
                       d3_version = "4"
                       )
                   
                  
                     # dat %$%
                     #   network3d::network3d(
                     #     force_explorer = FALSE,
                     #     html_tooltip = TRUE,
                     #     vertices,
                     #     edges,
                     #     manybody_strength = -1.9,
                     #     max_iterations = 120,
                     #     edge_opacity = 0.2
                     #   )
                     
                     # r2d3::r2d3(
                     #   data = jsonlite::toJSON(dat),
                     #   script = "inst/index.js",
                     #   container = 'div',
                     #   d3_version = 4,
                     #   dependencies = c(
                     #     "d3-jetpack",
                     #     "inst/helpers.js",
                     #     "inst/helpers2.js"
                     #   ),
                     #   css = c(
                     #     "inst/helpers.css",
                     #     "inst/network.css",
                     #     "inst/common.css"
                     #   ),
                     #   options=list(highlighted_pattern=fake_network_options$highlighted_pattern)
                     # )
                     
                   # }
                 })
                 
               })
}
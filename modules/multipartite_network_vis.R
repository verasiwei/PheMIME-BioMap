multipartite_network = function(id){
  ns = NS(id)
  
  fluidRow(
    tags$head(
      tags$script(HTML(sprintf('
      function sendClickedNodeToShiny(selectedNodes) {
        const selectedNodeIDs = Array.from(selectedNodes, node => node.id);
        Shiny.onInputChange("%s", selectedNodeIDs);
      }
    ',ns("clicked_node_id"))
    ))
    ),
    
    column(width=8,
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
    
    column(width=4,
           fluidRow(
           box(
             width = 12,
             title = strong("Clicked Node Connections"),
             closable = F,
             status = "warning",
             solidHeader = FALSE,
             collapsible = TRUE,
             div(strong("Clicked Node ID",style="font-size: 1.3rem;color:black")),
             textOutput(ns("node_info")),
             hr(),
             ##same type node
             div(strong("Shared 2nd layer nodes connected to all selected nodes",style="font-size: 1.3rem;color:black")),
             DTOutput(ns("conn_second_table"))
           ),
           box(
             width = 12,
             title = strong("Multimorbidity Upset Plot"),
             closable = F,
             status = "warning",
             solidHeader = FALSE,
             collapsible = TRUE,
             plotOutput(ns("upset_plot"))
           )
           )
    )
  ) #fluidrow
}

multipartite_networkServer = function(id,molecular,pvalue,update_network,reset_network,current_description,visualize_network,current_institution,current_data){
  moduleServer(id,
               function(input,output,session){
                 # tidy_connect_update <- reactiveVal(tidy_connect)
                 # nodes_update <- reactiveVal(nodes)
                 # 
                 # observeEvent(upload_data_yes(),{
                 #   upload_connect = bind_rows(data.frame(from=upload_data()$covariate,to=upload_data()$description,phecode=upload_data()$phecode,connection_type="phecode",
                 #                                                        hazard_ratio=upload_data()$OR,pvalue=upload_data()$pvalue,phecode_category=upload_data()$group,institution="vumc"),
                 #                                             data.frame(from=upload_data()$description,to=upload_data()$covariate,phecode=upload_data()$phecode,connection_type="phecode",
                 #                                                        hazard_ratio=upload_data()$OR,pvalue=upload_data()$pvalue,phecode_category=upload_data()$group,institution="vumc"))
                 #   tidy_connect_update(bind_rows(tidy_connect_update(),upload_connect))
                 #   phecodes = bind_rows(phecodes,data.frame(phecode=c("other aUPD","overlap v617f aUPD","GGCC/GGCC","GGCC/TCTT"),
                 #                                            Description=c("other aUPD","overlap v617f aUPD","GGCC/GGCC","GGCC/TCTT"),
                 #                                            category=c("other aUPD","overlap v617f aUPD","GGCC/GGCC","GGCC/TCTT")))
                 #   nodes_update(bind_rows(nodes_update(),
                 #                     upload_data() %>%
                 #                       dplyr::select(node=description) %>% mutate(type="phecode"),
                 #                     data.frame(node=c("other aUPD","overlap v617f aUPD","GGCC/GGCC","GGCC/TCTT"),
                 #                                type=rep("phecode",4))) %>%
                 #     distinct(node,.keep_all = T)
                 #   
                 # })
                 
                 ## update node information of user selected, received from omics_network.js
                 observeEvent(input$clicked_node_id,{
                   
                   if (!is.null(input$clicked_node_id)) {
                     ##first layer connection nodes
                     
                    conn_nodes = purrr::map(str_split(input$clicked_node_id,","),function(x){
                       conn_first = tidy_connect %>%
                         filter(institution == current_institution()) %>%
                         filter(from %in% x) %>%
                         dplyr::select(node = to) %>%
                         bind_rows(tidy_connect %>%
                                     filter(institution == current_institution()) %>%
                                     filter(to %in% x) %>%
                                     dplyr::select(node = from)) %>%
                         distinct(node) %>%
                         pull(node)
                       ##second layer connection nodes (same type of nodes)
                       conn_second = tidy_connect %>%
                         filter(from %in% conn_first)%>%
                         dplyr::select(node = to) %>%
                         bind_rows(tidy_connect %>%
                                     filter(to %in% conn_first) %>%
                                     dplyr::select(node = from)) %>%
                         distinct(node) %>%
                         pull(node)
                       conn_second
                     })
                     shared_nodes = data.frame(ID=Reduce(intersect,conn_nodes))
                     
                     ##selected node ID
                     output$node_info <- renderText({input$clicked_node_id})
                     ##connected nodes information
                     output$conn_second_table <- renderDT({
                       datatable(shared_nodes,
                                 rownames = FALSE,
                                 #options = list(displayStart = start_index - 2),
                                 options = list(
                                   scrollX = "300px",
                                   scrollY = "300px"
                                 )
                       )
                     },server = FALSE)
                     
                     ##prepare the upset plot
                     upset_dat = tidy_connect %>%
                       filter(institution == current_institution()) %>%
                       filter(from %in% input$clicked_node_id | to %in% input$clicked_node_id)
                     upset_dat = upset_dat[1:(nrow(upset_dat)/2),] 
                     
                     upset_dat_res = purrr::map(unique(upset_dat$to),function(i){
                       
                       group_to = upset_dat %>%
                         filter(to == i) %>%
                         dplyr::select(from) %>%
                         distinct(.) %>%
                         pull(from)
                       
                     })
                     names(upset_dat_res) = unique(upset_dat$to)
                     if(length(upset_dat_res)!=1){
                       output$upset_plot = renderPlot({
                         upset(fromList(upset_dat_res),order.by = "freq")
                       })
                     }
                     
                   }
                 })
                   
               ## update data for network visualization
               data_for_network = reactiveVal(data_network_initial)

               ## observe visualization of the network
                 observeEvent(visualize_network(),{

                  # parent = res_cluster$parent[res_cluster$id %in% current_description()]
                  # connected_code = res_cluster$id[res_cluster$parent %in% parent]

                  # ## 1st layer nodes connected with user selected phecode
                  conn_first = tidy_connect %>%
                    filter(institution == current_institution()) %>%
                    filter(from %in% current_description()) %>%
                    dplyr::select(node = to) %>%
                    bind_rows(tidy_connect %>%
                               filter(institution == current_institution()) %>%
                               filter(to %in% current_description()) %>%
                               dplyr::select(node = from)) %>%
                    distinct(node) %>%
                    pull(node)
            
                  conn = unique(c(current_description(),conn_first))
                  tidy_connect_sub = tidy_connect %>%
                    filter(from %in% current_description() | to %in% current_description()) %>%
                    # filter(from %in% conn_first | to %in% conn_first) %>%
                    dplyr::select(from,to) %>%
                    # bind_rows(.,tidy_connect %>%
                    #             # filter(from %in% conn_second | to %in% conn_second) %>%
                    #             filter(from %in% connected_code | to %in% connected_code) %>%
                    #             dplyr::select(from,to)) %>%
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
                 
                 ### observe return to network
                 observeEvent(input$return,{
                   # parent = res_cluster$parent[res_cluster$id %in% current_description()]
                   # connected_code = res_cluster$id[res_cluster$parent %in% parent]
                   
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
                   tidy_connect_sub = tidy_connect %>%
                     filter(from %in% current_description() | to %in% current_description()) %>%
                     # filter(from %in% conn_first | to %in% conn_first) %>%
                     dplyr::select(from,to) %>%
                     # bind_rows(.,tidy_connect %>%
                     #             # filter(from %in% conn_second | to %in% conn_second) %>%
                     #             filter(from %in% connected_code | to %in% connected_code) %>%
                     #             dplyr::select(from,to)) %>%
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
                   # conn = unique(c(current_description(),conn_first))
                   tidy_connect_sub = tidy_connect %>%
                     filter(from %in% current_description() | to %in% current_description()) %>%
                     # filter(from %in% conn_first | to %in% conn_first) %>%
                     dplyr::select(from,to) %>%
                     # bind_rows(.,tidy_connect %>%
                     #             # filter(from %in% conn_second | to %in% conn_second) %>%
                     #             filter(from %in% connected_code | to %in% connected_code) %>%
                     #             dplyr::select(from,to)) %>%
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
                     filter(from %in% current_description() | to %in% current_description()) %>%
                     # filter(from %in% conn_first | to %in% conn_first) %>%
                     dplyr::select(from,to) %>%
                     # bind_rows(.,tidy_connect %>%
                     #             # filter(from %in% conn_second | to %in% conn_second) %>%
                     #             filter(from %in% connected_code | to %in% connected_code) %>%
                     #             dplyr::select(from,to)) %>%
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
                  # if(is.null(close_network())){
                  #   r2d3(
                  #     data=data_network_depression,
                  #     script = "inst/d3/omics_network.js",
                  #     # dependencies = "inst/style.css",
                  #     options = list(r2d3.theme = list(background="#F2F3F6")),
                  #     container = "svg",
                  #     d3_version = "5"
                  #   )
                  # } else{
                  # jsonlite::write_json(data_for_network(),path="inst/d3/original_data.json")
                    r2d3(
                    data=data_for_network(),
                    script = "inst/omics_network.js",
                    # dependencies = "inst/style.css",
                    options = list(r2d3.theme = list(background="#F2F3F6")),
                    container = "svg",
                    d3_version = "5"
                  )
                    # }
                })
                
               })
}
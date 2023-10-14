multipartite_network = function(id){
  ns = NS(id)
      fluidRow(
        column(width=12,
         # box(
         #  width = 12,
         #  title = "", 
         #  closable = F, 
         #  status = "warning", 
         #  solidHeader = FALSE, 
         #  collapsible = TRUE,
          # sidebar = boxSidebar(
          #   style="color:black",
          #   id=ns("test"),
          #   startOpen = T,
          #   width=25,
          #   height = "200px",
          #   background="#D3D3D3",
            # p(HTML("<b>Select molecular levels</b>"),span(shiny::icon("info-circle"),id = "info_molecular"),
            #   selectInput(ns('molecular'), label="",
            #               choices=list("","Gene"="gene","Protein"="protein","Metabolite"="metabolite"),selected = NULL),
            #   tippy::tippy_this(elementId = "info_molecular",
            #                     tooltip = "<span style='font-size:20px;'>Select gene/protein/metabolite<span>",
            #                     placement = "right")),
            # 
            # p(HTML("<b>Input cutoff p-value</b>"),span(shiny::icon("info-circle"),id = "info_pvalue"),
            #   numericInput(ns('pvalue'), label=NULL,min=0,max=0.05,value=0.05),
            #   tippy::tippy_this(elementId = "info_pvalue",
            #                     tooltip = "<span style='font-size:20px;'>Remove connections > p-value cutoff, maximum cutoff is 0.05<span>",
            #                     placement = "right")),
            # 
            # div(actionButton(ns("reset_network"), "Reset",style="float:right;background-color: #fff;"),
            #     actionButton(ns("update_network"), "Update",style="float:left;background-color: #fff;"))
          # ),
          # ,
         # tags$script(src = "inst/d3/omics_network.js"),
         # column(6,actionButton(ns("run_network"), "Visualize the multipartite network",style="font-size: 1.5rem;float:left;background-color:#D3D3D3;")),
         # column(3,actionButton(ns("isolate"), "Highlight nodes and neighbors",style="font-size: 1.1rem;float:left;background-color:#D3D3D3;")),
         # column(4,""),
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
      )
      )
      
}

multipartite_networkServer = function(id,molecular,pvalue,update_network,reset_network,current_description,visualize_network,current_institution,current_data){
  moduleServer(id,
               function(input,output,session){
                 
                 ## update data for network visualization
                 data_for_network = reactiveVal(data_network_initial)
                
                 # observeEvent(close_network(),{
                 #   data_for_network(list(nodes=NULL,edges=NULL))
                 #   })

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
                  # ## phecode connected with molecular-level nodes
                  # conn_second = tidy_connect %>%
                  #   filter(from %in% conn_first)%>%
                  #   dplyr::select(node = to) %>%
                  #   bind_rows(tidy_connect %>%
                  #               filter(to %in% conn_first) %>%
                  #               dplyr::select(node = from)) %>%
                  #   distinct(node) %>%
                  #   pull(node)
                  conn = unique(c(current_description(),conn_first))
                  tidy_connect = tidy_connect %>%
                    filter(from %in% conn_first | to %in% conn_first) %>%
                    # filter(from %in% connected_code | to %in% connected_code) %>%
                    dplyr::select(from,to) %>%
                    # bind_rows(.,tidy_connect %>%
                    #             # filter(from %in% conn_second | to %in% conn_second) %>%
                    #             filter(from %in% connected_code | to %in% connected_code) %>%
                    #             dplyr::select(from,to)) %>%
                    graph_from_data_frame(., directed=FALSE) %>%
                    simplify %>%
                    as_data_frame

                  nodes = tidy_connect %>%
                    dplyr::select(node=from) %>%
                    bind_rows(
                      tidy_connect %>%
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
                    edges = tidy_connect %>% dplyr::rename(source = from, target = to),
                    extra = data.frame(nothing=Sys.time())
                  )
                  data_for_network(dat)
               })
                 
                 ### observe return to network
                 observeEvent(input$return,{
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
                   tidy_connect = tidy_connect %>%
                     filter(from %in% conn_first | to %in% conn_first) %>%
                     # filter(from %in% connected_code | to %in% connected_code) %>%
                     dplyr::select(from,to) %>%
                     graph_from_data_frame(., directed=FALSE) %>% 
                     simplify %>%                                 
                     as_data_frame 
                   
                   nodes = tidy_connect %>% 
                     dplyr::select(node=from) %>%
                     bind_rows(
                       tidy_connect %>% 
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
                     edges = tidy_connect %>% dplyr::rename(source = from, target = to), 
                     extra = data.frame(nothing=Sys.time()) 
                   ) 
                   data_for_network(dat)
                   
                 })
                 
                 ### observe reset network after changing p-value cut-off or molecular levels
                 observeEvent(reset_network(),{
                   
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
                   tidy_connect = tidy_connect %>%
                     filter(from %in% conn_first | to %in% conn_first) %>%
                     # filter(from %in% connected_code | to %in% connected_code) %>%
                     dplyr::select(from,to) %>%
                     graph_from_data_frame(., directed=FALSE) %>% 
                     simplify %>%                                 
                     as_data_frame 
                   
                   nodes = tidy_connect %>% 
                     dplyr::select(node=from) %>%
                     bind_rows(
                       tidy_connect %>% 
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
                     edges = tidy_connect %>% dplyr::rename(source = from, target = to), 
                     extra = data.frame(nothing=Sys.time()) 
                   ) 
                   data_for_network(dat)
                 })
                 
                ### observe update of p-value or molecular levels
                 observeEvent(update_network(),{
                   # parent = res_cluster$parent[res_cluster$id %in% current_description()]
                   # connected_code = res_cluster$id[res_cluster$parent %in% parent]

                   tidy_connect = tidy_connect %>%
                       filter(pvalue<isolate(pvalue())) %>%
                       filter(connection_type==isolate(molecular()))
    
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
                   tidy_connect = tidy_connect %>%
                     filter(from %in% conn_first | to %in% conn_first) %>%
                     # filter(from %in% connected_code | to %in% connected_code) %>%
                     dplyr::select(from,to) %>%
                     graph_from_data_frame(., directed=FALSE) %>% 
                     simplify %>%                                 
                     as_data_frame 
                   
                   nodes = tidy_connect %>% 
                     dplyr::select(node=from) %>%
                     bind_rows(
                       tidy_connect %>% 
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
                     edges = tidy_connect %>% dplyr::rename(source = from, target = to), 
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
                    script = "inst/d3/omics_network.js",
                    # dependencies = "inst/style.css",
                    options = list(r2d3.theme = list(background="#F2F3F6")),
                    container = "svg",
                    d3_version = "5"
                  )
                    # }
                })
                
               })
}
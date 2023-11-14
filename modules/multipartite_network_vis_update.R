multipartite_network_UI = function(id){
  ns = NS(id)
  
  dashboardPage(
    dashboardHeader(
      ## header
      title = strong("Phe-Omics Multimorbidity Explorer"),
      titleWidth = 600,
      # titleWidth = 0,
      leftUi = tagList(dropdownBlock(
        id = "tuning_network",
        title = div(icon("sliders"),"Tuning Network",style="color:black;"),
        # icon = icon("sliders"),
        p(HTML("<b>Select molecular levels</b>"),span(shiny::icon("info-circle"),id = "info_molecular"),
          selectInput(ns('molecular'), label="",
                      choices=list("","Gene"="gene","Protein"="protein","Metabolite"="metabolite"),selected = NULL),
          tippy::tippy_this(elementId = "info_molecular",
                            tooltip = "<span style='font-size:20px;'>Select gene/protein/metabolite<span>",
                            placement = "right")),
        
        p(HTML("<b>Input cutoff p-value</b>"),span(shiny::icon("info-circle"),id = "info_pvalue"),
          numericInput(ns('pvalue'), label=NULL,min=0,max=0.05,value=0.05),
          tippy::tippy_this(elementId = "info_pvalue",
                            tooltip = "<span style='font-size:20px;'>Remove connections > p-value cutoff, maximum cutoff is 0.05<span>",
                            placement = "right")),
        
        div(actionButton(ns("reset_network"), "Reset",style="float:right;background-color: #fff;"),
            actionButton(ns("update_network"), "Update",style="float:left;background-color: #fff;"))
      ))
    ),
    dashboardSidebar(width = "0px"),
    dashboardBody(
      body = dashboardBody(
        setShadow(class = "dropdown-menu")
      ),
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
      ), #fluidrow
      tags$script("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';"),
      tags$head(
        tags$style(HTML('
       .content-wrapper,
       .right-side {
        background-color: #F2F3F6;
                  }
        /*main header*/
        .skin-blue .main-header .logo {
                              background-color: #F2F3F6;
                              color: #50595e;
                              }
        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #F2F3F6;
                              }
        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #F2F3F6;
                              }

        /* main sidebar */
        /* The toggle lines to collapse menu bar   */
        .skin-blue .main-header .navbar .sidebar-toggle {
                                color: #000000;
                                background-color: #F2F3F6;
                                }
        .skin-blue .main-sidebar {
                              background-color: #F2F3F6;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color:#859900;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #F2F3F6;
                              color: white;
                              }
        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #859900;
                              }
        /* toggle button when hovered  */
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #859900;
         }
         
       /* warning status color  */
  
         .box.box-solid.box-warning>.box-header {
                              color:black;
                              background:#2c3e50;
                              /*#dfdfdf;*/
                              }
         .box.box-solid.box-warning{
                              border-bottom-color:white;
                              border-left-color:white;
                              border-right-color:white;
                              border-top-color:white;
                              /*#2c3e50*/
                              background:#fff;
                              }
          .box.box-warning>.box-header {
                              color:#000000;
                              background:white;
                    }
          .box.box-warning{
                              border-bottom-color:white;
                              border-left-color:white;
                              border-right-color:white;
                              border-top-color:white;
                              background:white;
                              }')))
    )
  )

}

multipartite_networkServer = function(input,output,session,current_description,current_institution,visualize_network){
                 ns <- session$ns
                 ## update node information of user selected, received from omics_network.js
                 observeEvent(input$clicked_node_id,{
                   
                   if (!is.null(input$clicked_node_id)) {
                     ##first layer connection nodes
                     
                     conn_nodes = purrr::map(str_split(input$clicked_node_id,","),function(x){
                       conn_first = tidy_connect %>%
                         filter(institution == current_institution) %>%
                         filter(from %in% x) %>%
                         dplyr::select(node = to) %>%
                         bind_rows(tidy_connect %>%
                                     filter(institution == current_institution) %>%
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
                       filter(institution == current_institution) %>%
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
                 observeEvent(visualize_network,{
                   
                   # parent = res_cluster$parent[res_cluster$id %in% current_description]
                   # connected_code = res_cluster$id[res_cluster$parent %in% parent]
                   
                   # ## 1st layer nodes connected with user selected phecode
                   conn_first = tidy_connect %>%
                     filter(institution == current_institution) %>%
                     filter(from %in% current_description) %>%
                     dplyr::select(node = to) %>%
                     bind_rows(tidy_connect %>%
                                 filter(institution == current_institution) %>%
                                 filter(to %in% current_description) %>%
                                 dplyr::select(node = from)) %>%
                     distinct(node) %>%
                     pull(node)
                   
                   conn = unique(c(current_description,conn_first))
                   tidy_connect_sub = tidy_connect %>%
                     filter(from %in% current_description | to %in% current_description) %>%
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
                     mutate(selected = ifelse(node %in% current_description,"yes","no"))
                   
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
                   # parent = res_cluster$parent[res_cluster$id %in% current_description]
                   # connected_code = res_cluster$id[res_cluster$parent %in% parent]
                   
                   # ## 1st layer nodes connected with user selected phecode 
                   # conn_first = tidy_connect_sub %>%
                   #   filter(institution == current_institution) %>%
                   #   filter(from %in% current_description) %>%
                   #   dplyr::select(node = to) %>%
                   #   bind_rows(tidy_connect_sub %>%
                   #               filter(institution == current_institution) %>%
                   #               filter(to %in% current_description) %>%
                   #               dplyr::select(node = from)) %>%
                   #   distinct(node) %>%
                   #   pull(node)
                   # conn = unique(c(current_description,conn_first))
                   tidy_connect_sub = tidy_connect %>%
                     filter(from %in% current_description | to %in% current_description) %>%
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
                     mutate(selected = ifelse(node %in% current_description,"yes","no"))
                   
                   # Save data for JS visualization
                   dat = list(
                     nodes = nodes %>% dplyr::rename(id = node),
                     edges = tidy_connect_sub %>% dplyr::rename(source = from, target = to), 
                     extra = data.frame(nothing=Sys.time()) 
                   ) 
                   data_for_network(dat)
                   
                 })
                 
                 ### observe reset network after changing p-value cut-off or molecular levels
                 observeEvent(input$reset_network,{
                   
                   # parent = res_cluster$parent[res_cluster$id %in% current_description]
                   # connected_code = res_cluster$id[res_cluster$parent %in% parent]
                   
                   # ## 1st layer nodes connected with user selected phecode 
                   # conn_first = tidy_connect %>%
                   #   filter(institution == current_institution) %>%
                   #   filter(from %in% current_description) %>%
                   #   dplyr::select(node = to) %>%
                   #   bind_rows(tidy_connect %>%
                   #               filter(institution == current_institution) %>%
                   #               filter(to %in% current_description) %>%
                   #               dplyr::select(node = from)) %>%
                   #   distinct(node) %>%
                   #   pull(node)
                   # conn = unique(c(current_description,conn_first))
                   tidy_connect_sub = tidy_connect %>%
                     filter(from %in% current_description | to %in% current_description) %>%
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
                     mutate(selected = ifelse(node %in% current_description,"yes","no"))
                   
                   # Save data for JS visualization
                   dat = list(
                     nodes = nodes %>% dplyr::rename(id = node),
                     edges = tidy_connect_sub %>% dplyr::rename(source = from, target = to), 
                     extra = data.frame(nothing=Sys.time()) 
                   ) 
                   data_for_network(dat)
                 })
                 
                 ### observe update of p-value or molecular levels
                 observeEvent(input$update_network,{
                   # parent = res_cluster$parent[res_cluster$id %in% current_description]
                   # connected_code = res_cluster$id[res_cluster$parent %in% parent]
                   
                   tidy_connect_sub = tidy_connect %>%
                     filter(pvalue<isolate(input$pvalue)) %>%
                     filter(connection_type==isolate(input$molecular) | connection_type=="phecode")
                   
                   # ## 1st layer nodes connected with user selected phecode 
                   # conn_first = tidy_connect_sub %>%
                   #   filter(institution == current_institution) %>%
                   #   filter(from %in% current_description) %>%
                   #   dplyr::select(node = to) %>%
                   #   bind_rows(tidy_connect_sub %>%
                   #               filter(institution == current_institution) %>%
                   #               filter(to %in% current_description) %>%
                   #               dplyr::select(node = from)) %>%
                   #   distinct(node) %>%
                   #   pull(node)
                   # conn = unique(c(current_description,conn_first))
                   tidy_connect_sub = tidy_connect_sub %>%
                     filter(from %in% current_description | to %in% current_description) %>%
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
                     mutate(selected = ifelse(node %in% current_description,"yes","no"))
                   
                   # Save data for JS visualization
                   dat = list(
                     nodes = nodes %>% dplyr::rename(id = node),
                     edges = tidy_connect_sub %>% dplyr::rename(source = from, target = to), 
                     extra = data.frame(nothing=Sys.time()) 
                   ) 
                   data_for_network(dat)
                 })
                 
                 observeEvent(input$reset_network,{
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
                 
               
}
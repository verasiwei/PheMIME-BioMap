multipartite_network_UI = function(id,app_ns){
  ns <- . %>% NS(id)() %>% app_ns()
  fluidRow(
    tags$head(
      tags$script(HTML(sprintf('
                   function sendClickedNodeToShiny(selectedNodes) {
                   const selectedNodeIDs = Array.from(selectedNodes, node => node.id);
                   Shiny.onInputChange("%s", selectedNodeIDs);
                   }
                   function sendPreSelectNodeToShiny(preselectedNodes) {
                   const preselectedNodeIDs = Array.from(preselectedNodes, node => node.id);
                   Shiny.onInputChange("%s", preselectedNodeIDs);
                   }',
                   ns("clicked_node_id"),
                   ns("preselected_node_id"))
      ))
    ),
    column(12,offset = 0, style='padding:0px;',
           box(title = strong("Multipartite Network"),
                   width = 12,
                   solidHeader=F,
               style='overflow-y: auto; padding-right: 0px;padding-top: 0px;padding-bottom: 0px;padding-left: 5px;height:70vh;',
               
               fluidRow(
                 column(6,actionButton(ns("return"), "Revert to entire network",style="font-size: 1.5rem;float:left;background-color:#D3D3D3;")),
                 column(width=12,
                        # hr(),
                        withSpinner(r2d3::d3Output(ns("network"),width = "100%"),
                                    hide.ui = FALSE)
                       ) #column
                      )#fluidrow
              ) #box
           )
    # , #column
    
  ) ##fluidRow
  
}

multipartite_network_Server = function(input,output,session,current_description,current_institution,visualize_network){
  ns <- session$ns
  
  # nodes_event <- reactive({
  #   list(input$clicked_node_id, input$preselected_node_id)
  # })
  
  
  ## update data for network visualization
  data_for_network = reactiveVal(data_network_initial)
  
  ## observe visualization of the network
  observeEvent(visualize_network,{
  
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
    # conn = unique(c(current_description,conn_first))
    tidy_connect_sub = tidy_connect %>%
      filter(from %in% current_description | to %in% current_description) %>%
      # filter(from %in% conn_first | to %in% conn_first) %>%
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
   
    ## 1st layer nodes connected with user selected phecode
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
    # conn = unique(c(current_description,conn_first))
    tidy_connect_sub = tidy_connect %>%
      filter(from %in% current_description | to %in% current_description) %>%
      # filter(from %in% conn_first | to %in% conn_first) %>%
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
      mutate(selected = ifelse(node %in% current_description,"yes","no"))
    
    # Save data for JS visualization
    dat = list(
      nodes = nodes %>% dplyr::rename(id = node),
      edges = tidy_connect_sub %>% dplyr::rename(source = from, target = to), 
      extra = data.frame(nothing=Sys.time()) 
    ) 
    data_for_network(dat)
    
    #===========================================
    ##upset update
    ##prepare the upset plot
    upset_dat = tidy_connect %>%
      filter(institution == current_institution) %>%
      filter(from %in% input$preselected_node_id | to %in% input$preselected_node_id)
    upset_dat = upset_dat[1:(nrow(upset_dat)/2),] 
    
    upset_dat_res = purrr::map(unique(input$preselected_node_id),function(i){
      
      group_to = upset_dat %>%
        filter(to == i) %>%
        dplyr::select(from) %>%
        distinct(.) %>%
        pull(from)
      group_from = upset_dat %>%
        filter(from == i) %>%
        dplyr::select(to) %>%
        distinct(.) %>%
        pull(to)
      c(group_to,group_from)
      
    })
    names(upset_dat_res) = input$preselected_node_id
    if(length(upset_dat_res)!=1){
      output$upset_plot = renderPlot({
        upset(fromList(upset_dat_res),order.by = "freq")
      })
    }
    
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
      options = list(r2d3.theme = list(background="white")),
      container = "svg",
      d3_version = "5"
    )
    # }
  })
  
  return(
    reactive({
        list(
          clicked_node_id = input$clicked_node_id,
          preselected_node_id = input$preselected_node_id
            )
    })
  )
  
}





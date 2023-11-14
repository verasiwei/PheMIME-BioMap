shared_info_UI = function(id,app_ns){
  ns <- . %>% NS(id)() %>% app_ns()
  
  tagList(
    box(title = strong("Shared Mechanism"),offset = 0, 
            width = 12,
            solidHeader=F,
        style='overflow-y: auto; padding-right: 0px;padding-top: 0px;padding-bottom: 0px;padding-left: 5px;height:30vh;',
        div(strong("Selected Nodes:",style="font-size: 1.3rem;color:black")),
        textOutput(ns("node_info")),
        hr(),
        fluidRow(
          column(6,
                 ##1st layer shared 
                 # div(
                 div(strong("Universal Connectivity: Shared Nodes Among Selected Nodes",style="font-size: 1.3rem;color:black")),
                 DTOutput(ns("conn_first_table"))
                 ),
          
          column(6,
                 # div(
                 div(strong("Synchronized Nodes Linking All Selected Nodes",style="font-size: 1.3rem;color:black")),
                 DTOutput(ns("conn_second_table"))
                 )
    )
  )
  )
}

shared_info_Server <- function(input,output,session,current_phecode,current_description,current_institution,visualize_network,clicked_node_id,preselected_node_id) {
  
  ## update node information of user selected, received from omics_network.js
  # observeEvent(clicked_node_id,{
  
  if(is.null(clicked_node_id)) {
    ##1st layer connected nodes
    conn_nodes = purrr::map(str_split(preselected_node_id,","),function(x){
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
      conn_first
    })
    shared_nodes_first = data.frame(ID=Reduce(intersect,conn_nodes))
    
    ##second layer connection nodes
    shared_nodes_second = tidy_connect %>%
      filter(institution == current_institution) %>%
      filter(from %in% shared_nodes_first$ID) %>%
      dplyr::select(node = to) %>%
      bind_rows(tidy_connect %>%
                  filter(institution == current_institution) %>%
                  filter(to %in% shared_nodes_first$ID) %>%
                  dplyr::select(node = from)) %>%
      distinct(node) %>%
      rename(ID=node)
    
    ##selected node ID
    code_id <- reactive(glue("{preselected_node_id};"))
    output$node_info <- renderText(glue("{code_id()}"))
    ##1st layer connected nodes information
    output$conn_first_table <- renderDT({
      datatable(shared_nodes_first,
                rownames = FALSE,
                #options = list(displayStart = start_index - 2),
                options = list(
                  scrollX = "300px",
                  scrollY = "300px",
                  pageLength = 30, lengthChange = FALSE
                )
      )
    },server = FALSE)
    
    ##2nd layer connected nodes information
    output$conn_second_table <- renderDT({
      datatable(shared_nodes_second,
                rownames = FALSE,
                #options = list(displayStart = start_index - 2),
                options = list(
                  scrollX = "300px",
                  scrollY = "300px",
                  pageLength = 30, lengthChange = FALSE
                )
      )
    },server = FALSE)
  } else if(!is.null(clicked_node_id)) {
    
    ##1st layer connected nodes
    conn_nodes = purrr::map(str_split(clicked_node_id,","),function(x){
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
      conn_first
    })
    shared_nodes_first = data.frame(ID=Reduce(intersect,conn_nodes))
    
    ##second layer connection nodes
    shared_nodes_second = tidy_connect %>%
      filter(institution == current_institution) %>%
      filter(from %in% shared_nodes_first$ID) %>%
      dplyr::select(node = to) %>%
      bind_rows(tidy_connect %>%
                  filter(institution == current_institution) %>%
                  filter(to %in% shared_nodes_first$ID) %>%
                  dplyr::select(node = from)) %>%
      distinct(node) %>%
      rename(ID=node)
    
    ##selected node ID
    code_id <- reactive(glue("{clicked_node_id};"))
    output$node_info <- renderText(glue("{code_id()}"))
    ##1st layer connected nodes information
    output$conn_first_table <- renderDT({
      datatable(shared_nodes_first,
                rownames = FALSE,
                #options = list(displayStart = start_index - 2),
                options = list(
                  scrollX = "300px",
                  scrollY = "300px",
                  pageLength = 30, lengthChange = FALSE
                )
      )
    },server = FALSE)
    
    ##2nd layer connected nodes information
    output$conn_second_table <- renderDT({
      datatable(shared_nodes_second,
                rownames = FALSE,
                #options = list(displayStart = start_index - 2),
                options = list(
                  scrollX = "300px",
                  scrollY = "300px",
                  pageLength = 30, lengthChange = FALSE
                )
      )
    },server = FALSE)
  }
      
}
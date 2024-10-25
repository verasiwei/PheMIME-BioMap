multipartite_network_UI = function(id,app_ns){
  ns <- . %>% NS(id)() %>% app_ns()
  fluidRow(
    useShinyjs(),  
    shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
    tags$head(
      tags$script(HTML(sprintf('
                   function sendClickedNodeToShiny(selectedNodes) {
                   const selectedNodeIDs = Array.from(selectedNodes, node => node.id);
                   // Include a timestamp or a counter to ensure Shiny detects the change
    const dataToSend = {
        selectedNodeIDs: selectedNodeIDs,
        timestamp: new Date().getTime() // Use a timestamp to force Shiny to recognize the change
    };
                   Shiny.onInputChange("%s", dataToSend);
                   }
                   
                   function sendPreSelectNodeToShiny(preselectedNodes) {
                   const preselectedNodeIDs = Array.from(preselectedNodes, node => node.id);
                   Shiny.onInputChange("%s", preselectedNodeIDs);
                   }',
                               ns("clicked_node_id"),
                               ns("preselected_node_id"))
      )),
    ),
    column(12,offset = 0, style='padding:0px;',
           shinydashboardPlus::box(title = strong("Bipartite Network"),
               width = 12,
               solidHeader=F,
               enable_sidebar = T,
               style='overflow-y: auto; padding-right: 0px;padding-top: 15px;padding-bottom: 0px;padding-left: 5px;height:100vh;',
               
               sidebar = boxSidebar(
                 width = 25,
                 startOpen = F,
                 background = "white",
                 p(HTML("<b>Select molecular levels</b>"),span(shiny::icon("info-circle"),id = "info_molecular"),
                   selectInput(ns('molecular'), label="",
                               choices=list("","Gene"="gene","Protein"="protein","Metabolite"="metabolite"),multiple = T,selected = c("gene","protein","metabolite")),
                   tippy::tippy_this(elementId = "info_molecular",
                                     tooltip = "<span style='font-size:20px;'>Select gene/protein/metabolite<span>",
                                     placement = "right")),
                 
                 p(HTML("<b>Input cutoff p-value</b>"),span(shiny::icon("info-circle"),id = "info_pvalue"),
                   numericInput(ns('pvalue'), label=NULL,min=0,max=0.05,value=0.001),
                   tippy::tippy_this(elementId = "info_pvalue",
                                     tooltip = "<span style='font-size:20px;'>Remove connections > p-value cutoff, maximum cutoff is 0.05<span>",
                                     placement = "right")),
                 div(actionButton(ns("updatepm"), "Update",style="float:left;background-color: #fff;"))
               ),
               
               fluidRow(
                 # column(3,actionButton(ns("first_layer"), "1st-layer network",class="buttonstyle"
                 #                       # style="font-size: 1.5rem;float:left;background-color:#D3D3D3;"
                 # )),
                 # column(3,actionButton(ns("second_layer"), "2nd-layer network",class="buttonstyle")),
                 # column(3,actionButton(ns("return"), "Revert to pre-selected network",class="buttonstyle")),
                 column(1,""),
                 column(2,actionButton(ns("updateupset"), "Update upset",class="buttonstyle")),
                 column(2,actionButton(ns("updatepathway"), "Update pathway analysis",class="buttonstyle")),
                 column(width=12,style="padding-bottom:0px;margin-bottom:0px",
                        # hr(),
                        withSpinner(r2d3::d3Output(ns("network"),width = "100%",height="60vh"),
                                    hide.ui = FALSE)
                 ),
                 tags$br(),
                 column(12,div(class = "subtitle","Selected Nodes:",style="font-size: 1.5rem;color:black;padding-top:10px;padding-left:5px;")),
                 column(12,textOutput(ns("node_info"))),
                 tags$br(),
                 column(12,
                        div(class = "subtitle","Biomolecules Table"),
                        column(12,downloadButton(class="buttonstyle",ns("download_shared"), "Download Table")),
                        # div(
                        # column(5,actionButton(ns("showsharedtab"),"Shared Biomolecules Table",class="buttonstyle")),
                        # column(7,conditionalPanel(
                        #   condition = "input.update_network > 0",  # Shows when update_network is clicked
                        #   actionButton(ns("exclusivetab"), "Exclusively Shared Biomolecules Table", class = "buttonstyle")
                        # )),
                        # column(7,actionButton(ns("exclusivetab"),"Exlusively Shared Biomolecules Table",class="buttonstyle")),
                        column(5,uiOutput(ns("shared_ui"))),
                        column(7,uiOutput(ns("exclusive_ui"))),
                        # column(6, hidden(downloadButton(class="buttonstyle",ns("download_table"), "Download Results"))),
                        column(12,textOutput(ns("table_info"))),
                        column(12,style = "overflow-y: auto; height: 200px;",withSpinner(DTOutput(ns("conn_table")),
                                    hide.ui = FALSE))
                 )
                 # column(6,
                 #        ##1st layer shared
                 #        # div(
                 #        div(class = "subtitle","Shared Nodes Connected to All Selected Nodes"),
                 #        downloadButton(class="buttonstyle",ns("download_table1"), "Download Results"),
                 #        withSpinner(DTOutput(ns("conn_first_table")),
                 #                    hide.ui = FALSE)
                 # ),
                 # column(6,
                 #        # div(
                 #        div(class = "subtitle","Exclusively Shared Nodes Among Selected Nodes (UpSet Selection)"),
                 #        downloadButton(class="buttonstyle",ns("download_table2"), "Download Results"),
                 #        withSpinner(DTOutput(ns("conn_second_table")),
                 #                    hide.ui = FALSE)
                 # )
               )#fluidrow
           ) #box
    )
    # , #column
    
  ) ##fluidRow
  
}

multipartite_network_Server = function(input,output,session,current_description,current_institution,visualize_network,current_phecode,update_network,update_clicked_id,update_info_all,shared_nodes_id_unique,upset_data){
  ns <- session$ns
  
  # Reactive value to track clicked nodes
  clicked_nodes <- reactiveVal(NULL)
  ## update data for network visualization
  data_for_network <- reactiveVal(data_network_initial)
  sharednodes <- reactiveVal(NULL)
  # shinyjs::hide("exclusivetab")
  #=================================tuning selection============================
  #=============================================================================
  #=============================================================================
  pvalue_react <- reactive(1)
  molecular_react <- reactiveValues(
    table_data = data.frame(type=c("gene","protein","metabolite"))
    )
  
  observeEvent(input$molecular,{
    molecular_react$table_data = data.frame(type=input$molecular)
  })
  observeEvent(input$pvalue,{
    pvalue_react <- (
      input$pvalue
    )
  })
  #=================================updates from upset plot=====================
  #=============================================================================
  #=============================================================================
  # Initially hide the button
  exclusive_button <- reactiveVal(FALSE)
  shared_button <- reactiveVal(FALSE)
  # Render UI conditionally based on the reactive trigger
  output$exclusive_ui <- renderUI({
    if (exclusive_button()) {
      actionButton(ns("exclusivetab"), "Exclusively Shared Biomolecules Table", class = "buttonstyle")
    }
  })
  # Render UI conditionally based on the reactive trigger
  output$shared_ui <- renderUI({
    if (shared_button()) {
      actionButton(ns("sharedtab"), "Shared Biomolecules Table", class = "buttonstyle")
    }
  })
  # # Reset the trigger when the button is clicked
  observeEvent(input$exclusivetab, {
    exclusive_button(FALSE)  # Hide the button after it's clicked
  })
  observeEvent(input$sharedtab, {
    shared_button(FALSE)  # Hide the button after it's clicked
  })
  
  observeEvent(update_network(),{
 
    exclusive_button(TRUE)
    updated_ids <- update_clicked_id()
    clicked_nodes(updated_ids)
    shared_nodes_unique <- shared_nodes_id_unique()
    
    # Ensure updated_ids is always a list/array
    if (length(updated_ids) == 1) {
      updated_ids <- list(updated_ids)
    }
    
    session$sendCustomMessage(type = 'updateD3Selection', message = list(updated_ids=updated_ids, 
                                                                         shared_nodes_unique=shared_nodes_unique))
  
  })

  #=======observe event of the clicked nodes update from network================
  #=============================================================================
  #=============================================================================
  observeEvent(input$clicked_node_id,{
    sharednodes(NULL)
    clicked_node_ids <- clicked_nodes()
    updated_ids <- update_clicked_id()
    if(length(input$clicked_node_id$selectedNodeIDs)==0){
      clicked_nodes(NULL)
    # } else if(!isTRUE(all.equal(clicked_node_ids,input$clicked_node_id$selectedNodeIDs))) {
    } else {
      clicked_nodes(input$clicked_node_id$selectedNodeIDs)
    }
  
    output$node_info <- renderText(glue("{clicked_nodes()};"))
    output$table_info <- renderText(NULL)
    
    if(!isTRUE((all.equal(clicked_node_ids,updated_ids))) | is.null(updated_ids)){
      exclusive_button(FALSE) 
      shared_button(TRUE)
    } else {
      exclusive_button(TRUE) 
      shared_button(FALSE)
    }
  })

  ## observe visualization of the network
  observeEvent(c(visualize_network,update_info_all()),{
    ##1st layer nodes connected to the user selection
    tidy_connect_sub = tidy_connect %>%
      filter(institution %in% current_institution()) %>%
      filter(from %in% current_description() | to %in% current_description()) %>%
      dplyr::filter(pvalue <= pvalue_react()) %>%
      dplyr::filter(connection_type %in% molecular_react$table_data$type) %>%
      # filter(from %in% conn_first | to %in% conn_first) %>%
      dplyr::select(from,to) %>%
      graph_from_data_frame(., directed=FALSE) %>%
      igraph::simplify(.) %>%
      as_data_frame
    
    vertices_first = tidy_connect_sub %>%
      dplyr::select(node=from) %>%
      bind_rows(
        tidy_connect_sub %>%
          dplyr::select(node=to)
      ) %>%
      distinct(node,.keep_all = T)
    
    vertices = bind_rows(vertices_first) %>%
      left_join(.,nodes %>% filter(institution==current_institution()),by="node") %>%
      distinct(node,.keep_all = T) %>%
      arrange(type) %>%
      mutate(selected = ifelse(node %in% current_description(),"yes","no")) 
  
    # Save data for JS visualization
    dat = list(
      nodes = vertices %>% dplyr::rename(id = node),
      edges = bind_rows(tidy_connect_sub) %>% dplyr::rename(source = from, target = to),
      extra = data.frame(nothing=Sys.time())
    )
    data_for_network(dat)
    
  })
  
  # ### observe revert to pre-selected
  # observeEvent(input$return,{
  #   
  #   # shinyjs::js$refresh_page()
  #   ##1st layer nodes connected to the user selection
  #   tidy_connect_sub = tidy_connect %>%
  #     filter(institution %in% current_institution()) %>%
  #     filter(from %in% current_description() | to %in% current_description()) %>%
  #     #filter(pvalue <= pvalue_react()) %>%
  #     #filter(connection_type %in% molecular_react$table_data$type) %>%
  #     # filter(from %in% conn_first | to %in% conn_first) %>%
  #     dplyr::select(from,to) %>%
  #     graph_from_data_frame(., directed=FALSE) %>%
  #     igraph::simplify(.) %>%
  #     as_data_frame
  # 
  #   vertices_first = tidy_connect_sub %>%
  #     dplyr::select(node=from) %>%
  #     bind_rows(
  #       tidy_connect_sub %>%
  #         dplyr::select(node=to)
  #     ) %>%
  #     distinct(node,.keep_all = T)
  # 
  #   vertices = bind_rows(vertices_first) %>%
  #     left_join(.,nodes %>% filter(institution==current_institution()),by="node") %>%
  #     distinct(node,.keep_all = T) %>%
  #     arrange(type) %>%
  #     mutate(selected = ifelse(node %in% current_description(),"yes","no"))
  #   
  #   # Save data for JS visualization
  #   dat = list(
  #     nodes = vertices %>% dplyr::rename(id = node),
  #     edges = bind_rows(tidy_connect_sub) %>% dplyr::rename(source = from, target = to),
  #     extra = data.frame(nothing=Sys.time())
  #   )
  #   data_for_network(dat)
  #   clicked_nodes(NULL)
  #   # Send a custom message to JavaScript to reset clicked_node_id
  #   session$sendCustomMessage('resetClickedNode', list())
  # })
  # 
  # observeEvent(input$updatepm,{
  #   tidy_connect_sub = tidy_connect %>%
  #     filter(institution %in% current_institution()) %>%
  #     filter(from %in% current_description() | to %in% current_description()) %>%
  #     dplyr::filter(pvalue <= pvalue_react()) %>%
  #     dplyr::filter(connection_type %in% molecular_react$table_data$type) %>%
  #     # filter(from %in% conn_first | to %in% conn_first) %>%
  #     dplyr::select(from,to) %>%
  #     graph_from_data_frame(., directed=FALSE) %>%
  #     igraph::simplify(.) %>%
  #     as_data_frame
  #   
  #   vertices_first = tidy_connect_sub %>%
  #     dplyr::select(node=from) %>%
  #     bind_rows(
  #       tidy_connect_sub %>%
  #         dplyr::select(node=to)
  #     ) %>%
  #     distinct(node,.keep_all = T)
  #   
  #   vertices = bind_rows(vertices_first) %>%
  #     left_join(.,nodes %>% filter(institution==current_institution()),by="node") %>%
  #     distinct(node,.keep_all = T) %>%
  #     arrange(type) %>%
  #     mutate(selected = ifelse(node %in% current_description(),"yes","no"))
  #   
  #   dat = list(
  #     nodes = vertices %>% dplyr::rename(id = node),
  #     edges = bind_rows(tidy_connect_sub) %>% dplyr::rename(source = from, target = to),
  #     extra = data.frame(nothing=Sys.time())
  #   )
  #   data_for_network(dat)
  # })
  
  output$network = r2d3::renderD3({
    
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
  
  app_data <- reactiveValues(
    update_upset = NULL,
    return_preselected = NULL,
    update_pathway = NULL
  )
  observeEvent(input$updateupset,{
    app_data$update_upset = 1
  })
  observeEvent(input$updatepathway,{
    app_data$update_pathway = 1
  })
  observeEvent(input$return_preselected,{
    app_data$return_preselected = 1
  })
  
  
  #=======================================================================shared info module=========================================================================#
  #==================================================================================================================================================================#
  #==================================================================================================================================================================#
  # shared_nodes_second = reactiveValues(tab_dat=data.frame(ID=c(),type=c()))
  
  observeEvent(input$sharedtab,{
    isolate({
    clicked_node_ids <- clicked_nodes()
    # isolate({
    if(!is.null(clicked_node_ids)){
    ##1st layer connected nodes
    conn_nodes = purrr::map(str_split(clicked_node_ids,","),function(x){
      conn_first = tidy_connect %>%
        filter(institution == current_institution()) %>%
        filter(from %in% x) %>%
        dplyr::select(node = to) %>%
        bind_rows(tidy_connect %>%
                    dplyr::filter(institution == current_institution()) %>%
                    dplyr::filter(to %in% x) %>%
                    dplyr::select(node = from)) %>%
        distinct(node) %>%
        pull(node)
      conn_first
    })
    shared_tab = data.frame(ID=Reduce(intersect,conn_nodes))
    if(nrow(shared_tab)!=0){
      shared_tab = shared_tab %>% left_join(.,nodes %>% dplyr::rename(ID=node),by="ID") %>% dplyr::filter(institution == current_institution())
    } else{
      shared_tab = shared_tab
    }

    ## add pvalue, etc.
    shared_tabb = shared_tab %>%
      left_join(., tidy_connect %>% group_by(institution) %>%
                  dplyr::filter((from %in% clicked_node_ids & to %in% shared_tab$ID) |
                                             (to %in% clicked_node_ids & from %in% shared_tab$ID)) %>%
                  dplyr::select(from,to,hazard_ratio,pvalue,phecode_category,institution) %>%
                  ungroup() %>%
                  # bind_rows(
                  #   tidy_connect %>% filter((from %in% clicked_node_ids & to %in% shared_tab$ID) |
                  #                             (to %in% clicked_node_ids & from %in% shared_tab$ID)) %>%
                  #     dplyr::select(to,from,hazard_ratio,pvalue)
                  # ) %>%
                  dplyr::select(ID=from,phenotype=to,hazard_ratio,pvalue,institution),by=c("institution","ID")) %>%
      # distinct(.,.keep_all = T) %>%
      # group_by(ID) %>%
      # mutate(hazard_ratio_mean = round(mean(hazard_ratio),2),
             # pvalue_mean = round(mean(pvalue),2)) %>%
      # ungroup() %>%
      # distinct(ID,.keep_all = T) %>%
      # dplyr::select(-hazard_ratio,-pvalue,-institution) %>%
      dplyr::rename(hr=hazard_ratio,pval=pvalue) %>%
      mutate(pval = round(pval,3))

    # ##selected node ID
    # code_id <- reactiveVal(glue("{clicked_nodes()};"))
    # output$node_info <- renderText(glue("{code_id()}"))
    ## table info
    output$table_info <- renderText("Shared Nodes Connected to All Selected Nodes")
    sharednodes(shared_tabb)
    # shared_button(FALSE)  # Hide the button after it's clicked
    }
    })
  })
  
  ### exclusively shared biomolecules table
  observeEvent(input$exclusivetab,{
    updated_ids <- update_clicked_id()
    if(!is.null(updated_ids)){
    shared_nodes_id_unique_ids <- shared_nodes_id_unique()
    exclusive_tab = data.frame(ID=shared_nodes_id_unique_ids)
    if(nrow(exclusive_tab)!=0){
      exclusive_tab = exclusive_tab %>% left_join(.,nodes %>% dplyr::rename(ID=node),by="ID") %>% filter(institution == current_institution())
    } else{
      exclusive_tab = exclusive_tab
    }

    ## add pvalue, etc.
    # updated_ids <- update_clicked_id()
    exclusive_tabb = exclusive_tab %>%
      left_join(., tidy_connect %>% group_by(institution) %>% 
                  filter((from %in% updated_ids & to %in% exclusive_tab$ID) |
                                             (to %in% updated_ids & from %in% exclusive_tab$ID)) %>%
                  dplyr::select(from,to,hazard_ratio,pvalue,phecode_category,institution) %>%
                  ungroup() %>%
                  # bind_rows(
                  #   tidy_connect %>% filter((from %in% updated_ids & to %in% shared_tab$ID) |
                  #                             (to %in% updated_ids & from %in% shared_tab$ID)) %>%
                  #     dplyr::select(to,from,hazard_ratio,pvalue)
                  # ) %>%
                  dplyr::select(ID=from,phenotype=to,hazard_ratio,pvalue,institution),by=c("institution","ID")) %>%
      # distinct(.,.keep_all = T) %>%
      # group_by(ID) %>%
      # mutate(hazard_ratio_mean = mean(hazard_ratio),
             # pvalue_mean = mean(pvalue)) %>%
      # ungroup() %>%
      # distinct(ID,.keep_all = T) %>%
      # dplyr::select(-hazard_ratio,-pvalue)
      dplyr::rename(hr=hazard_ratio,pval=pvalue) %>%
      mutate(pval = round(pval,3))

    # ##selected node ID
    # code_id <- reactiveVal(glue("{clicked_nodes()};"))
    # output$node_info <- renderText(glue("{code_id()}"))

    sharednodes(exclusive_tabb)
    ## table info
    output$table_info <- renderText("Exclusively Shared Nodes Among Selected Nodes")
    }
  
  })
  ##1st layer connected nodes information
  output$conn_table <- renderDT({
    datatable(sharednodes(),
              rownames = FALSE,
              #options = list(displayStart = start_index - 2),
              options = list(
                scrollX = TRUE,
                scrollY = TRUE,
                pageLength = 10,
                lengthChange = FALSE,
                # Enable deferred rendering for large datasets
                deferRender = TRUE,
                # Only show the first page and load more when needed
                dom = 'tp'
              ),
              selection = list(mode = "single")
    )
  },server = FALSE)
  
  output$download_shared <- downloadHandler(
    filename = function() {
      paste0("biomolecules_table",Sys.time(),".csv")
    },
    content = function(file) {
      write.csv(sharednodes(), file, row.names = FALSE,
                col.names = T,quote = F)
    }
  )
  
  return(
    reactive({
      list(
        clicked_node_id = clicked_nodes(),
        preselected_node_id = input$preselected_node_id,
        update_upset = input$updateupset,
        return_preselected = input$return,
        update_pathway = input$updatepathway
      )
    })
  )
  
}
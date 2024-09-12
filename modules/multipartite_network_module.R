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
                   }
                   
                   Shiny.addCustomMessageHandler("resetClickedNode", function(message) {
                                 Shiny.setInputValue("%s", null);
                   })',
                               ns("clicked_node_id"),
                               ns("preselected_node_id"),
                               ns("clicked_node_id"))
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
                 div(actionButton(ns("update_p_m"), "Update",style="float:left;background-color: #fff;"))
               ),
               
               fluidRow(
                 column(3,actionButton(ns("first_layer"), "1st-layer network",class="buttonstyle"
                                       # style="font-size: 1.5rem;float:left;background-color:#D3D3D3;"
                 )),
                 # column(3,actionButton(ns("second_layer"), "2nd-layer network",class="buttonstyle")),
                 column(3,actionButton(ns("return"), "Revert to pre-selected network",class="buttonstyle")),
                 column(1,""),
                 column(2,actionButton(ns("update_upset"), "Update upset",class="buttonstyle")),
                 column(2,actionButton(ns("update_pathway"), "Update pathway analysis",class="buttonstyle")),
                 column(width=12,style="padding-bottom:0px;margin-bottom:0px",
                        # hr(),
                        withSpinner(r2d3::d3Output(ns("network"),width = "100%",height="60vh"),
                                    hide.ui = FALSE)
                 ),
                 tags$br(),
                 column(12,div(class = "subtitle","Selected Nodes:",style="font-size: 1.5rem;color:black;padding-top:10px;padding-left:5px;")),
                 column(12,textOutput(ns("node_info"))),
                 tags$br(),
                 column(6,
                        ##1st layer shared 
                        # div(
                        div(class = "subtitle","1st Layer: Shared Nodes Among Selected Nodes"),
                        downloadButton(class="buttonstyle",ns("download_table1"), "Download Results"),
                        withSpinner(DTOutput(ns("conn_first_table")),
                                    hide.ui = FALSE)
                 ),
                 column(6,
                        # div(
                        div(class = "subtitle","2nd Layer: Nodes Connected to 1st Layer Nodes"),
                        downloadButton(class="buttonstyle",ns("download_table2"), "Download Results"),
                        withSpinner(DTOutput(ns("conn_second_table")),
                                    hide.ui = FALSE)
                 )
               )#fluidrow
           ) #box
    )
    # , #column
    
  ) ##fluidRow
  
}

multipartite_network_Server = function(input,output,session,current_description,current_institution,visualize_network,current_phecode,update_network,update_clicked_id,update_info_all,shared_nodes_id_unique){
  ns <- session$ns
  
  # Reactive value to track clicked nodes
  clicked_nodes <- reactiveVal(starting_description)
  
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
  observeEvent(update_network(),{

    updated_ids <- update_clicked_id()
    
    # clicked_nodes(updated_ids)
    
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
    clicked_node_ids <- clicked_nodes()
    if(length(input$clicked_node_id$selectedNodeIDs)==0){
      clicked_nodes(NULL)
    } else if(!isTRUE(all.equal(clicked_node_ids,input$clicked_node_id$selectedNodeIDs))) {
      clicked_nodes(input$clicked_node_id$selectedNodeIDs)
    }
  })
  
  ## update data for network visualization
  data_for_network = reactiveVal(data_network_initial)
  
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
  
  ### observe revert to pre-selected
  observeEvent(input$return,{
    
    # shinyjs::js$refresh_page()
    ##1st layer nodes connected to the user selection
    tidy_connect_sub = tidy_connect %>%
      filter(institution %in% current_institution()) %>%
      filter(from %in% current_description() | to %in% current_description()) %>%
      #filter(pvalue <= pvalue_react()) %>%
      #filter(connection_type %in% molecular_react$table_data$type) %>%
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
    clicked_nodes(NULL)
    # Send a custom message to JavaScript to reset clicked_node_id
    session$sendCustomMessage('resetClickedNode', list())
    
  })
  
  observeEvent(input$update_p_m,{
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
    
    dat = list(
      nodes = vertices %>% dplyr::rename(id = node),
      edges = bind_rows(tidy_connect_sub) %>% dplyr::rename(source = from, target = to),
      extra = data.frame(nothing=Sys.time())
    )
    data_for_network(dat)
  })
  
  ### observe 1st layer network
  observeEvent(input$first_layer,{
    
    if(length(input$clicked_node_id$selectedNodeIDs)==0){
      clicked_nodes(NULL)
    } else {
      clicked_nodes(input$clicked_node_id$selectedNodeIDs)
    }

      if(!is.null(clicked_nodes())){
      ##1st layer nodes connected to the user selection
      tidy_connect_sub = tidy_connect %>%
        filter(institution %in% current_institution()) %>%
        filter(from %in% unique(clicked_nodes()) | to %in% unique(clicked_nodes())) %>%
        filter(pvalue <= pvalue_react()) %>%
        filter(connection_type %in% molecular_react$table_data$type) %>%
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
        mutate(selected = ifelse(node %in% c(clicked_nodes()),"yes","no"))
      
     
    # Save data for JS visualization
    dat = list(
      nodes = vertices %>% dplyr::rename(id = node),
      edges = tidy_connect_sub %>% dplyr::rename(source = from, target = to), 
      extra = data.frame(nothing=Sys.time()) 
    ) 
    data_for_network(dat)
      }
    
    observeEvent(input$update_p_m,{
      ##1st layer nodes connected to the user selection
      tidy_connect_sub = tidy_connect %>%
        filter(institution %in% current_institution()) %>%
        filter(from %in% unique(clicked_nodes()) | to %in% unique(clicked_nodes())) %>%
        filter(pvalue <= pvalue_react()) %>%
        filter(connection_type %in% molecular_react$table_data$type) %>%
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
        mutate(selected = ifelse(node %in% c(clicked_nodes()),"yes","no"))
      
      
      # Save data for JS visualization
      dat = list(
        nodes = vertices %>% dplyr::rename(id = node),
        edges = tidy_connect_sub %>% dplyr::rename(source = from, target = to), 
        extra = data.frame(nothing=Sys.time()) 
      ) 
      data_for_network(dat)
    })
  })
  
  ## observe visualization of the network
  observeEvent(input$second_layer,{
    if(length(input$clicked_node_id$selectedNodeIDs)==0){
      clicked_nodes(NULL)
    } else {
      clicked_nodes(input$clicked_node_id$selectedNodeIDs)
    }
   
      if(!is.null(clicked_nodes())){
      ##1st layer nodes connected to the user selection
      tidy_connect_sub = tidy_connect %>%
        filter(institution %in% current_institution()) %>%
        filter(from %in% unique(clicked_nodes()) | to %in% unique(clicked_nodes())) %>%
        filter(pvalue <= pvalue_react()) %>%
        filter(connection_type %in% molecular_react$table_data$type) %>%
        # filter(from %in% conn_first | to %in% conn_first) %>%
        dplyr::select(from,to) %>%
        graph_from_data_frame(., directed=FALSE) %>%
        igraph::simplify(.) %>%
        as_data_frame
      ##2nd layer nodes connected to the 1st layer nodes
      tidy_connect_second = tidy_connect %>%
        filter(institution %in% current_institution()) %>%
        filter(!(from %in% unique(c(current_description(),clicked_nodes())))) %>%
        filter(!(to %in% unique(c(current_description(),clicked_nodes())))) %>%
        filter(from %in% unique(c(tidy_connect_sub$from,tidy_connect_sub$to)) | to %in% unique(c(tidy_connect_sub$from,tidy_connect_sub$to))) %>%
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
      vertices_second = tidy_connect_second %>%
        filter(!from %in% vertices_first$node) %>%
        dplyr::select(node=from) %>%
        bind_rows(
          tidy_connect_second %>%
            filter(!to %in% vertices_first$node) %>%
            dplyr::select(node=to)
        ) %>%
        distinct(node,.keep_all = T)
      
      vertices = bind_rows(vertices_first,vertices_second) %>%
        left_join(.,nodes %>% filter(institution==current_institution()),by="node") %>%
        distinct(node,.keep_all = T) %>%
        arrange(type) %>%
        mutate(selected = ifelse(node %in% clicked_nodes(),"yes","no")) 
      
    
    # Save data for JS visualization
    dat = list(
      nodes = vertices %>% dplyr::rename(id = node),
      edges = bind_rows(tidy_connect_sub,tidy_connect_second) %>% dplyr::rename(source = from, target = to),
      extra = data.frame(nothing=Sys.time())
    )
    data_for_network(dat)
      }
    
    observeEvent(input$update_p_m,{
      ##1st layer nodes connected to the user selection
      tidy_connect_sub = tidy_connect %>%
        filter(institution %in% current_institution()) %>%
        filter(from %in% unique(clicked_nodes()) | to %in% unique(clicked_nodes())) %>%
        filter(pvalue <= pvalue_react()) %>%
        filter(connection_type %in% molecular_react$table_data$type) %>%
        # filter(from %in% conn_first | to %in% conn_first) %>%
        dplyr::select(from,to) %>%
        graph_from_data_frame(., directed=FALSE) %>%
        igraph::simplify(.) %>%
        as_data_frame
      ##2nd layer nodes connected to the 1st layer nodes
      tidy_connect_second = tidy_connect %>%
        filter(institution %in% current_institution()) %>%
        filter(!(from %in% unique(c(current_description(),clicked_nodes())))) %>%
        filter(!(to %in% unique(c(current_description(),clicked_nodes())))) %>%
        filter(from %in% unique(c(tidy_connect_sub$from,tidy_connect_sub$to)) | to %in% unique(c(tidy_connect_sub$from,tidy_connect_sub$to))) %>%
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
      vertices_second = tidy_connect_second %>%
        filter(!from %in% vertices_first$node) %>%
        dplyr::select(node=from) %>%
        bind_rows(
          tidy_connect_second %>%
            filter(!to %in% vertices_first$node) %>%
            dplyr::select(node=to)
        ) %>%
        distinct(node,.keep_all = T)
      
      vertices = bind_rows(vertices_first,vertices_second) %>%
        left_join(.,nodes %>% filter(institution==current_institution()),by="node") %>%
        distinct(node,.keep_all = T) %>%
        arrange(type) %>%
        mutate(selected = ifelse(node %in% clicked_nodes(),"yes","no")) 
      
      
      # Save data for JS visualization
      dat = list(
        nodes = vertices %>% dplyr::rename(id = node),
        edges = bind_rows(tidy_connect_sub,tidy_connect_second) %>% dplyr::rename(source = from, target = to),
        extra = data.frame(nothing=Sys.time())
      )
      data_for_network(dat)
      
    })
  })
  
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
  observeEvent(input$update_upset,{
    app_data$update_upset = 1
  })
  observeEvent(input$update_pathway,{
    app_data$update_pathway = 1
  })
  observeEvent(input$return_preselected,{
    app_data$return_preselected = 1
  })
  
  
  #=======================================================================shared info module=========================================================================#
  #==================================================================================================================================================================#
  #==================================================================================================================================================================#
  shared_nodes_first = reactiveValues(tab_dat=data.frame(ID=c(),type=c()))
  shared_nodes_second = reactiveValues(tab_dat=data.frame(ID=c(),type=c()))
  
  ##1st layer connected nodes information
  output$conn_first_table <- renderDT({
    datatable(shared_nodes_first$tab_dat,
              rownames = FALSE,
              #options = list(displayStart = start_index - 2),
              options = list(
                scrollX = "300px",
                scrollY = "300px",
                pageLength = 30, lengthChange = FALSE
              ),
              selection = list(mode = "single")
    )
  },server = FALSE)
  
  ##2nd layer connected nodes information
  output$conn_second_table <- renderDT({
    datatable(shared_nodes_second$tab_dat,
              rownames = FALSE,
              #options = list(displayStart = start_index - 2),
              options = list(
                scrollX = "300px",
                scrollY = "300px",
                pageLength = 30, lengthChange = FALSE
              ),
              selection = list(mode = "single")
    )
  },server = FALSE)
  
  observeEvent(clicked_nodes(),{
    
    ##1st layer connected nodes
    conn_nodes = purrr::map(str_split(clicked_nodes(),","),function(x){
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
      conn_first
    })
    shared_nodes_first_tab = data.frame(ID=Reduce(intersect,conn_nodes)) 
    if(nrow(shared_nodes_first_tab)!=0){
      shared_nodes_first_tab = shared_nodes_first_tab %>% left_join(.,nodes %>% dplyr::rename(ID=node),by="ID") %>% filter(institution == current_institution())
    } else{
      shared_nodes_first_tab = shared_nodes_first_tab
    }
      
    ##second layer connection nodes
    shared_nodes_second_tab = tidy_connect %>%
      filter(institution == current_institution()) %>%
      filter(from %in% shared_nodes_first$ID) %>%
      dplyr::select(node = to,connection_type) %>%
      bind_rows(tidy_connect %>%
                  filter(institution == current_institution()) %>%
                  filter(to %in% shared_nodes_first$ID) %>%
                  dplyr::select(node = from,connection_type)) %>%
      distinct(node,.keep_all = T) %>%
      dplyr::rename(ID=node,type=connection_type) %>%
      mutate(type = ifelse(ID %in% phecode_def$description,"phenotype",type))
    
    ## add pvalue, etc.
    shared_nodes_first_tab = shared_nodes_first_tab %>%
      left_join(., tidy_connect %>% filter((from %in% clicked_nodes() & to %in% shared_nodes_first_tab$ID) | 
                                             (to %in% clicked_nodes() & from %in% shared_nodes_first_tab$ID)) %>%
                  dplyr::select(from,to,hazard_ratio,pvalue,phecode_category) %>%
                  bind_rows(
                    tidy_connect %>% filter((from %in% clicked_nodes() & to %in% shared_nodes_first_tab$ID) | 
                                              (to %in% clicked_nodes() & from %in% shared_nodes_first_tab$ID)) %>%
                      dplyr::select(to,from,hazard_ratio,pvalue)
                  ) %>%
                  dplyr::select(ID=from,hazard_ratio,pvalue),by="ID") %>%
      distinct(.,.keep_all = T) %>%
      group_by(ID) %>%
      mutate(hazard_ratio_mean = mean(hazard_ratio),
             pvalue_mean = mean(pvalue)) %>%
      ungroup() %>%
      distinct(ID,.keep_all = T) %>%
      dplyr::select(-hazard_ratio,-pvalue)
    
    
    shared_nodes_second_tab = shared_nodes_second_tab %>%
      left_join(., tidy_connect %>% filter((from %in% clicked_nodes() & to %in% shared_nodes_first_tab$ID) | 
                                             (to %in% clicked_nodes() & from %in% shared_nodes_first_tab$ID)) %>%
                  dplyr::select(from,to,hazard_ratio,pvalue,phecode_category) %>%
                  bind_rows(
                    tidy_connect %>% filter((from %in% clicked_nodes() & to %in% shared_nodes_first_tab$ID) | 
                                              (to %in% clicked_nodes() & from %in% shared_nodes_first_tab$ID)) %>%
                      dplyr::select(to,from,hazard_ratio,pvalue)
                  ) %>%
                  dplyr::select(ID=from,hazard_ratio,pvalue),by="ID") %>%
      distinct(.,.keep_all = T) %>%
      group_by(ID) %>%
      mutate(hazard_ratio_mean = mean(hazard_ratio),
             pvalue_mean = mean(pvalue)) %>%
      ungroup() %>%
      distinct(ID,.keep_all = T) %>%
      dplyr::select(-hazard_ratio,-pvalue)
    
    ##selected node ID
    code_id <- reactiveVal(glue("{clicked_nodes()};"))
    output$node_info <- renderText(glue("{code_id()}"))
    
    shared_nodes_first$tab_dat = shared_nodes_first_tab
    shared_nodes_second$tab_dat = shared_nodes_second_tab
    
    ##1st layer connected nodes information
    output$conn_first_table <- renderDT({
      datatable(shared_nodes_first$tab_dat,
                rownames = FALSE,
                #options = list(displayStart = start_index - 2),
                options = list(
                  scrollX = "300px",
                  scrollY = "300px",
                  pageLength = 30, lengthChange = FALSE
                ),
                selection = list(mode = "single")
      )
    },server = FALSE)
    
    ##2nd layer connected nodes information
    output$conn_second_table <- renderDT({
      datatable(shared_nodes_second$tab_dat,
                rownames = FALSE,
                #options = list(displayStart = start_index - 2),
                options = list(
                  scrollX = "300px",
                  scrollY = "300px",
                  pageLength = 30, lengthChange = FALSE
                ),
                selection = list(mode = "single")
      )
    },server = FALSE)
    
    ## user select node from the table of shared nodes among selected nodes
    current_shared_first <- reactiveVal(NULL)
    observeEvent(input$conn_first_table_rows_selected,{
      current_shared_first(shared_nodes_first$tab_dat$ID[input$conn_first_table_rows_selected])
    })
    
    observeEvent(input$close,{removeModal()})
    
    download_nodes_first <- reactive(shared_nodes_first$tab_dat)
    download_nodes_second <- reactive(shared_nodes_second$tab_dat)
    output$download_table1 <- downloadHandler(
      filename = function() {
        paste0("associated_mechanism",Sys.time(),".csv")
      },
      content = function(file) {
        write.csv(download_nodes_first(), file, row.names = FALSE,
                  col.names = T,quote = F)
      }
    )
    
    output$download_table2 <- downloadHandler(
      filename = function() {
        paste0("shared_mechanism",Sys.time(),".csv")
      },
      content = function(file) {
        write.csv(download_nodes_second(), file, row.names = FALSE,
                  col.names = T,quote = F)
      }
    )
    
  })
  
  return(
    reactive({
      list(
        clicked_node_id = clicked_nodes(),
        preselected_node_id = input$preselected_node_id,
        update_upset = input$update_upset,
        return_preselected = input$return,
        update_pathway = input$update_pathway
      )
    })
  )
  
}
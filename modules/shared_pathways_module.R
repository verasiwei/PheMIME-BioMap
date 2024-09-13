shared_pathways_UI = function(id,app_ns){
  ns <- . %>% NS(id)() %>% app_ns()
  
  fluidRow(
    column(12,offset = 0, style='padding:0px;',
           
           box(title = strong("Shared Biological Pathways"),
               width = 12,
               solidHeader=F,offset = 0,style='padding:0px;',
               style='overflow-y: auto; padding-right: 0px;padding-top: 0px;padding-bottom: 0px;padding-left: 5px;height:100vh;',
               
               fluidRow(
                 shinyWidgets::progressBar(
                   id = ns("pathwayProgress"),
                   value = 0,
                   total = 100,
                   display_pct = TRUE
                 ),
                 column(12,div(class = "subtitle","Selected Nodes:",style="font-size: 1.5rem;color:black;padding-left: 5px;padding-top:8px;")),
                 column(12,textOutput(ns("node_info"))),
                 column(5,
                        sliderInput(ns("hclust_cutoff"), "Select Dendrogram Cut-off Height:",
                                      min = 0, 
                                      max = 10, 
                                      value = 0.2, 
                                      step = 0.1),
                        withSpinner(plotOutput(ns("dendrogram_plot"))),
                        div(class = "subtitle","Clustering Measurements",style="font-size: 1.2rem;color:black;padding-left: 5px;padding-top:8px;"),
                        withSpinner(textOutput(ns("cor_phecoef"))),
                        withSpinner(plotOutput(ns("silhouette_plot")))),
                 column(7,
                        downloadButton(class="buttonstyle",ns("download_table"), "Download Results"),
                        # actionButton(ns("update_pathway"), "Update pathway analysis",class="buttonstyle"),
                        withSpinner(r2d3::d3Output(ns("pathway_network"),width = "100%",height="70vh")),
                        tags$br(),
                        div(div(class = "subtitle","Table of Shared Biological Terms",style="font-size: 1.5rem;color:black;padding-left: 5px;padding-top:8px;")),
                        withSpinner(DTOutput(ns("pathway_table"))))
               )
           )
    )
  ) ##fluidRow
}

shared_pathways_Server <- function(input,output,session,current_phecode,current_description,current_institution,clicked_node_id,return_preselected,update_pathway_react,update_info_all) {
  ns <- session$ns
  enrichment_res_react = reactiveVal(enrichment_network_initial$network_dat)
  res_table = reactiveVal(enrichment_network_initial$network_dat$res_table)
  dendrogram = reactiveVal(enrichment_network_initial$dendrogram_dat)
  cor_coef_react = reactiveVal(0.9326)
  sil_dat_react = reactiveVal(sil_dat)
  enrichment_res_all_react = reactiveVal(enrichment_network_all_initial)
  # update_pathway_react = reactiveVal(input$update_pathway)

  ##selected node ID
  code_id <- reactiveVal(glue("Below is an example. Please select nodes in the bipartite network and initialize pathway analysis, it may take some time."))
  output$node_info <- renderText(glue("{code_id()}"))
  observeEvent(clicked_node_id(),{
      code_id <- reactiveVal(glue("{clicked_node_id()};"))
      output$node_info <- renderText(glue("{code_id()}"))
  })

  observeEvent(update_pathway_react(),{
    # Save selected nodes before update
  selected_nodes_backup <- clicked_node_id()
    # if(!is.null(clicked_node_id())) {
      # withProgress(
      #   message = "It is running, please wait...",
      #   value = 0,{
      enrichment_res_react(NULL) ##to tell r2d3 there is an update, remove the previous r2d3, but did not plot new r2d3 since it is still running within oberveEvent
      dendrogram(NULL)
      sil_dat_react(NULL)
      res_table
          ##1st layer connected nodes
          # Example progress step
          shinyWidgets::updateProgressBar(session, id = "pathwayProgress", value = 10)
      conn_nodes = purrr::map_dfr(str_split(selected_nodes_backup,","),function(x){
        conn_first = tidy_connect %>%
          filter(institution == current_institution()) %>%
          filter(from %in% x) %>%
          dplyr::select(node = to,hazard_ratio) %>%
          bind_rows(tidy_connect %>%
                      filter(institution == current_institution()) %>%
                      filter(to %in% x) %>%
                      dplyr::select(node = from,hazard_ratio)) %>%
          distinct(node,.keep_all = T) %>%
          mutate(phenotype = x)
      })
      ## multiple gene/protein lists for each disease phenotype
      ### need to get an averaged hazard ratio,
      ### enrichment analysis on each list or on the combined list? need to test whether some terms found enriched in one individual list but not in the combined list?
      datt = conn_nodes %>%
        left_join(.,all_id %>% dplyr::rename(node = variable) %>% dplyr::select(node,orig_id) %>% filter(!is.na(node)) %>% distinct(node,.keep_all = T), by="node") %>%
        left_join(.,nodes %>% filter(institution==current_institution()),by="node")
      datt = convert_id(datt)
      # Example progress step
      shinyWidgets::updateProgressBar(session, id = "pathwayProgress", value = 30)
      # isolate({incProgress(amount=1/10)})
      enrichment_res = enrichment_analysis(datt)
      # Example progress step
      shinyWidgets::updateProgressBar(session, id = "pathwayProgress", value = 100)
      ##dendrogram data
      dendrogram_dat = enrichment_res[[1]]
      ##network data
      clusters <- data.frame(node = unique(enrichment_res[[3]]$Description),cluster=cutree(dendrogram_dat, h = input$hclust_cutoff))
      ##cophenetic coefficient
      cor_coef = enrichment_res[[7]]
      ##silhouette
      sil_dat = enrichment_res[[8]]
      vertices = data.frame(node = unique(enrichment_res[[3]]$Description))
      vertices = vertices %>%
        left_join(.,clusters,by="node")
      
      network_dat = list(
        nodes = vertices %>% dplyr::rename(id = node) %>% filter(id %in% enrichment_res[[3]]$Description),
        edges = enrichment_res[[2]] %>% dplyr::rename(source = from, target = to) %>% filter(source %in% enrichment_res[[3]]$Description & target %in% enrichment_res[[3]]$Description),
        freq = enrichment_res[[4]],
        res_table = enrichment_res[[5]],
        res_freq = enrichment_res[[3]],
        res_network = enrichment_res[[2]],
        extra = data.frame(nothing=Sys.time())
      )
    # })
      
      enrichment_res_all_react(enrichment_res)
      enrichment_res_react(network_dat)
      res_table(network_dat$res_table)
      dendrogram(dendrogram_dat)
      cor_coef_react(cor_coef)
      sil_dat_react(sil_dat)
    # }
  })
  
  observeEvent(input$hclust_cutoff,{
      ##dendrogram data
      dendrogram_dat = enrichment_res_all_react()[[1]]
      ##network data
      clusters <- data.frame(node = unique(enrichment_res_all_react()[[3]]$Description),cluster=cutree(dendrogram_dat, h = input$hclust_cutoff))
      ##cophenetic coefficient
      cor_coef = enrichment_res_all_react()[[7]]
      ##silhouette
      sil_dat = enrichment_res_all_react()[[8]]
      vertices = data.frame(node = unique(enrichment_res_all_react()[[3]]$Description))
      vertices = vertices %>%
        left_join(.,clusters,by="node")

    network_dat = list(
      nodes = vertices %>% dplyr::rename(id = node) %>% filter(id %in% enrichment_res_all_react()[[3]]$Description),
      edges = enrichment_res_all_react()[[2]] %>% dplyr::rename(source = from, target = to) %>% filter(source %in% enrichment_res_all_react()[[3]]$Description & target %in% enrichment_res_all_react()[[3]]$Description),
      freq = enrichment_res_all_react()[[4]],
      res_table = enrichment_res_all_react()[[5]],
      res_freq = enrichment_res_all_react()[[3]],
      res_network = enrichment_res_all_react()[[2]],
      extra = data.frame(nothing=Sys.time())
    )

    enrichment_res_react(network_dat)
    res_table(network_dat$res_table)
    dendrogram(dendrogram_dat)
    cor_coef_react(cor_coef)
    sil_dat_react(sil_dat)
  })
  
  ## cophenetic coefficient
  output$cor_phecoef <- renderText(glue("Cophenetic Correlation:","{cor_coef_react()}"))
  
  ## silhouette
  output$silhouette_plot <- renderPlot({
    # withProgress(
    #   message = "It is running, please wait...",
    #   value = 0,{
    if(!is.null(sil_dat_react())){
    ggplot(sil_dat_react(),aes(x=cutoffs, y=sil_scores)) +
      geom_point(size=1) +
      geom_line()+
      theme_minimal() +
      ylim(c(min(sil_dat_react()$sil_scores)-0.1,1))+
      scale_x_continuous(
        breaks = seq(0,60,2)
      )+
      labs(title = "Silhouette Score", y = "Mean of Silhouette Score", color = "Height")+
      theme(axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 7,angle = 20))
    }
  # })
    }, height = 400,width = 500)

  
  ## dendrogram tree
  output$dendrogram_plot <- renderPlot({
    # withProgress(
    #   message = "It is running, please wait...",
    #   value = 0,{
    # Convert hclust result to dendrogram
    if(!is.null(dendrogram())){
    dend <- as.dendrogram(dendrogram())
    clusters <- cutree(dendrogram(), h = input$hclust_cutoff)
    # Add cluster colors to dendrogram
    colors = unique(phecode_def$color)
    colors = c(colors[!is.na(colors)],grDevices::colors()[sample(1:length(grDevices::colors()), 50)])
    dend <- color_branches(dend, k = length(unique(clusters)),col = colors)
    # Extract dendrogram data for ggplot2
    ddata <- dendro_data(dend, type = "rectangle")
    # Create a data frame for labels with cluster colors
    labels_df <- data.frame(
      label = ddata$labels$label,
      x = ddata$labels$x,
      y = ddata$labels$y,
      cluster = factor(clusters[order.dendrogram(dend)])
    )
    
    # Plot dendrogram
    ggplot(segment(ddata)) +
      geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_text(data = labels_df, aes(x = x, y = y, label = label, color = cluster),hjust=1,size = 3) +
      scale_colour_manual(values=colors)+
      geom_hline(yintercept = input$hclust_cutoff, linetype = "dashed", color = "red") +
      coord_flip() +
      scale_y_reverse(expand = c(0.2, 0)) +
      theme_minimal() +
      labs(title = "Cluster Dendrogram", y = "Height", color = "Cluster") +
      theme(legend.position = "bottom", axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank())
    }
      # })
  }, height = 400,width = 500)
  
  ## pathway network
  output$pathway_network = r2d3::renderD3({
    r2d3(
      data=enrichment_res_react(),
      script = "inst/pathway_network.js",
      # dependencies = "inst/style.css",
      options = list(r2d3.theme = list(background="white")),
      container = "svg",
      d3_version = "5"
    )
  })
  
  output$pathway_table <- renderDT({
    datatable(data = res_table(),
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













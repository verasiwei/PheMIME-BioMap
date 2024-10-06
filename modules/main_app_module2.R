main_app_UI = function(id){
  ns = NS(id)
  
  fluidRow(
    tags$head(
      tags$style(HTML('
        .container-fluid {
          padding-left: 0 !important;
          padding-right: 0 !important;
          margin-right: 0 !important;
          margin-left: 00 !important;
        }
        .tight-column {
          padding-left: 0 !important;
          padding-right: 0 !important;
          margin-left: 0 !important;
          margin-right: 0 !important;
        }
        .first-column {
          padding-right: 0 !important;
          padding-left: 5 !important;
          margin-left: 0 !important;
          margin-right: 0 !important;
        }
        .last-column {
          padding-right: 0 !important;
          padding-left: 5 !important;
          margin-left: 0 !important;
          margin-right: 0 !important;
        }
      '))
    ),
    
    titlePanel(
      div(h2("PheMIME-BioNet",style='padding-top:0px;padding-bottom:0px;padding-left:10px'),style='margin-top:-15px;margin-bottom:-5px'),
      windowTitle = "Network Analysis and Visualization of Shared Biomolecular Mechanisms in Disease Multimorbidity"
    ),
    fluidRow(
      column(
        width = 3,
        class = "tight-column",  # Add this class to your column
        info_panel_UI("info_panel",ns)
      ),
      column(
        width = 9,
        class = "tight-column",
        fluidRow(
          column(
            width = 7,
            class = "first-column",  # Add this class to your column
            multipartite_network_UI("multipartite_network",ns)
            # ,
            # shared_info_UI("shared_info",ns)
          ),  
          column(
            width = 5,
            class = "last-column",
            upset_plot_UI("upset_plot",ns)
          ),
          column(width = 12,
                 class = "first-column",
                 shared_pathways_UI("shared_pathways",ns))
          # ,
          # column(
          #   width = 5,
          #   class = "last-column",  # Add this class to your column
          #   shared_info_UI("shared_info",ns)
          # )
        )
      )
    )
  )
}


main_app_Server = function(input,output,session,current_phecode,current_description,current_institution,current_row, visualize_network){
  ns <- session$ns
  app_data <- reactiveValues(
    current_phecode = NULL,
    current_description = NULL,
    current_institution = NULL,
    current_row = NULL,
    visualize_network = FALSE,
    clicked_node_id = NULL,
    preselected_node_id = NULL,
    update_upset = NULL,
    return_preselected = NULL,
    update_network = NULL,
    update_clicked_id = NULL,
    update_pathway = NULL,
    update_info_all = NULL,
    upset_data = NULL
  )
  
  current_description_react <- reactiveValues(val=current_description)
  current_institution_react <- reactiveValues(val=current_institution)
  current_phecode_react <- reactiveValues(val=current_phecode)
  current_row_react <- reactiveValues(val=current_row)
  
  
  ##info
  observe({
    info <- callModule(
      info_panel_Server, "info_panel",
      current_phecode = reactive(current_phecode_react$val),
      current_description = reactive(current_description_react$val),
      current_institution = reactive(current_institution_react$val),
      current_row = reactive(current_row_react$val),
      visualize_network = visualize_network
    )
    
    ## update reactive values if updates from info
    observeEvent(info(), {
      current_description_react$val <- info()$current_description
      current_institution_react$val <- info()$current_institution
      current_phecode_react$val <- info()$current_phecode
      current_row_react$val <- info()$current_row
      app_data$visualize_network <- info()$visualize_network
      app_data$update_info_all <- info()$update_info_all
    })
  })
  
  ##upset plot
  observe({
    upset_plot <- callModule(
      upset_plot_Server, "upset_plot",
      current_phecode = reactive(current_phecode_react$val),
      current_description = reactive(current_description_react$val),
      current_institution = reactive(current_institution_react$val),
      # current_data = all_data$current_data,
      visualize_network = visualize_network,
      clicked_node_id = reactive(app_data$clicked_node_id),
      preselected_node_id = app_data$preselected_node_id,
      update_upset = reactive(app_data$update_upset),
      return_preselected = reactive(app_data$return_preselected),
      update_info_all = reactive(app_data$update_info_all)
    )
    
    observeEvent(upset_plot(),{
      
      app_data$update_network <- upset_plot()$update_network
      app_data$update_clicked_id <- upset_plot()$update_clicked_node
      app_data$shared_nodes_id_unique <- upset_plot()$shared_nodes_id_unique
      app_data$upset_data <- upset_plot()$upset_data
    })
  })
  
  ## network
  observe({
    network <- callModule(
      multipartite_network_Server, "multipartite_network",
      current_description = reactive(current_description_react$val),
      current_institution = reactive(current_institution_react$val),
      # current_data = all_data$current_data,
      visualize_network = reactive(app_data$visualize_network),
      current_phecode = reactive(current_phecode_react$val),
      update_network = reactive(app_data$update_network),
      update_clicked_id = reactive(app_data$update_clicked_id),
      update_info_all = reactive(app_data$update_info_all),
      shared_nodes_id_unique = reactive(app_data$shared_nodes_id_unique),
      upset_data = reactive(app_data$upset_data)
      
    )
    
    observeEvent(network(),{
      # network_data <- network()
      app_data$clicked_node_id <- network()$clicked_node_id
      app_data$preselected_node_id <- network()$preselected_node_id
      app_data$update_upset <- network()$update_upset
      app_data$return_preselected <- network()$return_preselected
      app_data$update_pathway <- network()$update_pathway
    })
  })
  
  ##shared info
  observe({
    shared_info_table <- callModule(
     shared_info_Server, "shared_info",
     current_phecode = reactive(current_phecode_react$val),
     current_description = reactive(current_description_react$val),
     current_institution = reactive(current_institution_react$val),
      # current_data = all_data$current_data,
      visualize_network = visualize_network,
      clicked_node_id = reactive(app_data$clicked_node_id),
      preselected_node_id = app_data$preselected_node_id,
      return_preselected = reactive(app_data$return_preselected),
     update_info_all = reactive(app_data$update_info_all)
    )
  })
  
  ##biological pathways
  observe({
    shared_pathways_vis <- callModule(
      shared_pathways_Server, "shared_pathways",
      current_phecode = reactive(current_phecode_react$val),
      current_description = reactive(current_description_react$val),
      current_institution = reactive(current_institution_react$val),
      # current_data = all_data$current_data,
      # visualize_network = visualize_network,
      clicked_node_id = reactive(app_data$clicked_node_id),
      # preselected_node_id = app_data$preselected_node_id,
      return_preselected = reactive(app_data$return_preselected),
      update_pathway_react = reactive(app_data$update_pathway),
      update_info_all = reactive(app_data$update_info_all)
    )
  })
  
}

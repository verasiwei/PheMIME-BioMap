upset_plot_UI = function(id,app_ns){
  ns <- . %>% NS(id)() %>% app_ns()
  
  fluidRow(
    tags$head(
      tags$script(HTML(sprintf('
                                // Function to send clicked pattern information to Shiny
                     function sendClickedPatternToShiny(pattern) {
                           Shiny.onInputChange("%s", pattern);
                    }',
                               ns("clickedPattern"))
      )),
    ),
    column(12,offset = 0, style='padding:0px;',
           shinydashboardPlus::box(title = strong("Multimorbidity Upset Plot"),
                                   width = 12,
                                   solidHeader=F,
                                   style='overflow-y: auto; padding-right: 0px;padding-top: 15px;padding-bottom: 0px;padding-left: 5px;height:100vh;',
                                   
                                   fluidRow(
                                     # column(6,actionButton(ns("update_upset"), "Update Upset Plot",style="font-size: 1.5rem;float:left;background-color:#D3D3D3;")),
                                     column(3,actionButton(ns("highlight_network"), "Highlight in Bipartite Network",class="buttonstyle")),
                                     column(width=12,
                                            withSpinner(r2d3::d3Output(ns("upset_plot"),width = "100%",height = "800px"),
                                                        hide.ui = FALSE)
                                     )
                                   )
           )
    )
  ) ##fluidRow
  
}

upset_plot_Server <- function(input,output,session,current_phecode,current_description,current_institution,visualize_network,clicked_node_id,preselected_node_id,update_upset,return_preselected,update_info_all) {
  ns <- session$ns
  
  upset_results_react <- reactiveVal(upset_results_initial)
  marginalData_react <- reactiveVal(marginalData_initial)
  update_click_id <- reactiveVal(NULL)
  upset_data_react <- reactiveVal(NULL)
  shared_nodes_unique_react <- reactiveVal(NULL)
  
  observeEvent(input$highlight_network,{
    
    # Ensure input$clickedPattern is a list or vector
    clicked_patterns <- input$clickedPattern
    
    # Iterate over each clicked pattern
    clicked_nodes_list = c()
    shared_nodes_unique_list = c()
    for (pattern in clicked_patterns) {
      
      # if (str_detect(pattern, "/")) {
        update_click <- str_split(pattern, "/")[[1]]
        shared_nodes_unique <- intersection_nodes_unique(update_click, upset_data_react())
      # } else {
        # update_click <- str_split(pattern, "/")[[1]]
        # shared_nodes_unique <- intersection_nodes_unique(pattern, upset_data)
      # }
      clicked_nodes_list = c(clicked_nodes_list, update_click)
      shared_nodes_unique_list = c(shared_nodes_unique_list, shared_nodes_unique)
    }
    update_click_id(unique(clicked_nodes_list))
    shared_nodes_unique_react(shared_nodes_unique_list)
    
    # if(!is.null(input$clickedPattern)){
    #   if(sum(str_detect(input$clickedPattern,"/"))>=1){
    #     update_click <- str_split_1(input$clickedPattern,"/")
    #     update_click_id(update_click)
    #     shared_nodes_unique = intersection_nodes_unique(update_click, upset_data_react())
    #     shared_nodes_unique_react(shared_nodes_unique)
    #   } else{
    #     update_click_id(input$clickedPattern)
    #     shared_nodes_unique = intersection_nodes_unique(input$clickedPattern, upset_data_react())
    #     shared_nodes_unique_react(shared_nodes_unique)
    #   }
    #   
    # }
    
  })
  
  observeEvent(c(visualize_network, update_info_all()),{
    ##prepare the upset plot
    upset_dat = tidy_connect %>%
      filter(institution %in% current_institution()) %>%
      filter(from %in% unique(c(current_description())) | to %in% unique(c(current_description()))) %>%
      # filter(from %in% conn_first | to %in% conn_first) %>%
      dplyr::select(from,to) %>%
      graph_from_data_frame(., directed=FALSE) %>%
      igraph::simplify(.) %>%
      as_data_frame
    
    show_upset_node = unique(c(current_description()))
    ## list of molecule nodes associated with each disease phenotype
    upset_dat_res = purrr::map(show_upset_node,function(i){
      
      ## codes connect to other phenotypes except the i
      codes_in_others = c(
        upset_dat %>%
          filter(to %in% show_upset_node[!show_upset_node %in% i]) %>%
          dplyr::select(from) %>%
          distinct(.) %>%
          pull(from),
        
        upset_dat %>%
          filter(from %in% show_upset_node[!show_upset_node %in% i]) %>%
          dplyr::select(to) %>%
          distinct(.) %>%
          pull(to)
      )
      
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
    names(upset_dat_res) = show_upset_node
    ## convert list to intersection dataframe with columns of disease phenotypes and rows of each molecule nodes
    upset_data = fromList(upset_dat_res)
    if(!is.null(dim(upset_data))){
      rownames(upset_data) = unique(unlist(upset_dat_res))
    } else {
      colname = names(upset_data)[1]
      upset_data = data.frame(upset_data)
      colnames(upset_data) = colname
      rownames(upset_data) = unique(unlist(upset_dat_res))
    }
    ## list of all combinations
    upset_results <- list()
    for(i in 1:ncol(upset_data)){
      combinations = combn(colnames(upset_data),i,simplify = F)
      for (comb in combinations) {
        intersection <- calculate_intersection(comb, upset_data)
        upset_results[[paste(comb, collapse = "/")]] <- intersection
      }
    }
    
    upset_results = upset_results %>%
      flatten() %>%
      keep(~all(.!=0)) %>%
      map_df(.,as_tibble,.id = "pattern") %>%
      dplyr::rename(count=value) %>%
      mutate(code = str_count(pattern,"/")+1) %>%
      arrange(desc(count))
      
    
    marginalData = upset_results %>%
      filter(code == 1) %>%
      dplyr::select(-code) %>%
      dplyr::rename(code=pattern) %>%
      jsonlite::toJSON()
    
    upset_data_react(upset_data)
    upset_results_react(upset_results)
    marginalData_react(marginalData)
  })
  
  
  observeEvent(return_preselected(),{
    ##prepare the upset plot
    upset_dat = tidy_connect %>%
      filter(institution %in% current_institution()) %>%
      filter(from %in% unique(c(current_description())) | to %in% unique(c(current_description()))) %>%
      # filter(from %in% conn_first | to %in% conn_first) %>%
      dplyr::select(from,to) %>%
      graph_from_data_frame(., directed=FALSE) %>%
      igraph::simplify(.) %>%
      as_data_frame
    
    show_upset_node = unique(c(current_description()))
    
    upset_dat_res = purrr::map(show_upset_node,function(i){
      
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
    names(upset_dat_res) = show_upset_node
    ##convert list to intersection matrix
    upset_data = fromList(upset_dat_res)
    if(!is.null(dim(upset_data))){
      rownames(upset_data) = unique(unlist(upset_dat_res))
    } else {
      colname = names(upset_data)[1]
      upset_data = data.frame(upset_data)
      colnames(upset_data) = colname
      rownames(upset_data) = unique(unlist(upset_dat_res))
    }
    
    upset_results <- list()
    for(i in 1:ncol(upset_data)){
      combinations = combn(colnames(upset_data),i,simplify = F)
      for (comb in combinations) {
        intersection <- calculate_intersection(comb, upset_data)
        upset_results[[paste(comb, collapse = "/")]] <- intersection
      }
    }
    
    upset_results = upset_results %>%
      flatten() %>%
      keep(~all(.!=0)) %>%
      map_df(.,as_tibble,.id = "pattern") %>%
      dplyr::rename(count=value) %>%
      mutate(code = str_count(pattern,"/")+1) %>%
      arrange(code,desc(count)) 
    
    marginalData = upset_results %>%
      filter(code == 1) %>%
      dplyr::select(-code) %>%
      dplyr::rename(code=pattern) %>%
      jsonlite::toJSON()
    
    upset_results_react(upset_results)
    marginalData_react(marginalData)
  })
  
  observeEvent(update_upset(),{
    # if(update_upset == T){
    ##prepare the upset plot
    if(!is.null(clicked_node_id())){
      upset_dat = tidy_connect %>%
        filter(institution %in% current_institution()) %>%
        filter(from %in% unique(c(clicked_node_id())) | to %in% unique(c(clicked_node_id()))) %>%
        # filter(from %in% conn_first | to %in% conn_first) %>%
        dplyr::select(from,to) %>%
        graph_from_data_frame(., directed=FALSE) %>%
        igraph::simplify(.) %>%
        as_data_frame
      
      show_upset_node = unique(c(clicked_node_id()))
      
      upset_dat_res = purrr::map(show_upset_node,function(i){
        
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
      names(upset_dat_res) = show_upset_node
      ##convert list to intersection matrix
      upset_data = fromList(upset_dat_res)
      if(!is.null(dim(upset_data))){
        rownames(upset_data) = unique(unlist(upset_dat_res))
      } else {
        colname = names(upset_data)[1]
        upset_data = data.frame(as.matrix(upset_data))
        colnames(upset_data) = colname
        rownames(upset_data) = unique(unlist(upset_dat_res))
      }
      
      upset_results <- list()
      for(i in 1:ncol(upset_data)){
        combinations = combn(colnames(upset_data),i,simplify = F)
        for (comb in combinations) {
          intersection <- calculate_intersection(comb, upset_data)
          upset_results[[paste(comb, collapse = "/")]] <- intersection
        }
      }
      
      upset_results = upset_results %>%
        flatten() %>%
        keep(~all(.!=0)) %>%
        map_df(.,as_tibble,.id = "pattern") %>%
        dplyr::rename(count=value) %>%
        mutate(code = str_count(pattern,"/")+1) %>%
        arrange(code,desc(count))
      
      marginalData = upset_results %>%
        filter(code == 1) %>%
        dplyr::select(-code) %>%
        dplyr::rename(code=pattern) %>%
        jsonlite::toJSON()
      upset_results_react(upset_results)
      marginalData_react(marginalData)
    }
  })
  
  output$upset_plot <- r2d3::renderD3({
    
    r2d3(
      data=upset_results_react(),
      script = "inst/upset_plot.js",
      # dependencies = "inst/style.css",
      options = list(r2d3.theme = list(background="white"),marginalData=marginalData_react()),
      container = "svg",
      dependencies = "d3-jetpack",
      d3_version = "4"
    )
  })
  
  return(
    reactive({
      list(
        update_network = input$highlight_network,
        update_clicked_node = update_click_id(),
        shared_nodes_id_unique = shared_nodes_unique_react(),
        upset_data = upset_data_react()
      )
    })
  )
  
} 
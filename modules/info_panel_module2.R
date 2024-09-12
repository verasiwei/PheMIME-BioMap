info_panel_UI = function(id,app_ns){
  ns <- . %>% NS(id)() %>% app_ns()
  
  tagList(
    
    box(title = strong("Information Panel"),
        offset = 0, 
        style='overflow-y: auto; padding-right: 0px;padding-top: 0px;padding-bottom: 0px;padding-left: 5px;height:203vh;',
        width = 12,
        solidHeader=F,
        DTOutput(ns("shared_phenotype_tab")),
        div("Current selections:",style = "font-size:1.7rem;color: black;
                                                padding-left: 10px;
                                                display: flex;align-items:
                                                center;justify-content: space-evenly;"),
        div(span(textOutput(ns("current_code_label"),inline = TRUE),style = "font-size:1.7rem;color:black;"),
            style = "padding-left: 10px;
                     display: flex;align-items:
                     center;justify-content: space-evenly;"),
        div(actionButton(ns("visualize"), "Update Analysis",class="buttonstyle1"))
        # hr(),
        # div(strong("Correlation with major systems"),style="font-size: 1.5rem;color:black"),
        # hr()
    )
    
  )
}

info_panel_Server <- function(input,output,session,current_phecode,current_description,current_institution,visualize_network) {
  
  # code_id <- reactive(glue("{current_phecode}:","{current_description};"))
  # output$current_code_label <- renderText(
  #   glue("{code_id()}")
  #   # cat(cat(paste0(current_phecode,":",current_description,"\n")))
  # )
  
  current_institution_react <- reactiveVal(current_institution)
  current_phecode_react <- reactiveVal(current_phecode)
  current_description_react <- reactiveVal(current_description)

  index <- reactiveVal(NULL)
  ## send the updates to the main module
  app_data <- reactiveValues(
    current_phecode = NULL,
    current_description = NULL,
    current_institution = NULL,
    # current_data = NULL,
    visualize_network = FALSE,
    update_info_all = FALSE
    
  )
  ## pre real-time disease co-occurence table
  dat_co <- reactiveValues(table_data = shared_phe %>% filter(institution==current_institution_react()) %>% filter(select_phenotype %in% current_description_react()))
  
  ## update selection table and co phenotype table
  observeEvent(input$institution,{
    
    dat_co$table_data = shared_phe %>% filter(institution==current_institution_react()) %>% filter(select_phenotype %in% current_description_react())
    
  })
  
  ## user select specific phenotypes
  output$shared_phenotype_tab <- renderDT({
    
    datatable(dat_co$table_data,
              # caption = "Select Disease Phenotype or Biomolecule of Interest",
              # rownames = FALSE,
              #options = list(displayStart = start_index - 2),
              options = list(
                scrollX = "300px",
                scrollY = "300px",
                pageLength = 30, lengthChange = FALSE
              ),    
              selection = list(mode = 'multiple')
    )
  },server = TRUE)
  
  ## update the current selections based on the user's selection
  observeEvent(input$shared_phenotype_tab_rows_selected,{
    
    current_description_react(unique(c(current_description,unique(dat_co$table_data$select_phenotype),dat_co$table_data$co_phenotype[input$shared_phenotype_tab_rows_selected])))
    current_phecode_react(unique(c(current_phecode,unique(dat_co$table_data$select_phecode),dat_co$table_data$co_phecode[input$shared_phenotype_tab_rows_selected])))
  
  })
  
  ##after user select specific code table, update the react variables
  code_id <- reactive(glue("{current_phecode_react()}:", "{current_description_react()}\n"))
  output$current_code_label <- renderText(glue("{code_id()}"))
  # visualize_network <- reactive({input$visualize})
  observeEvent(input$visualize, {
    # removeModal()
    if(length(current_phecode)==0){
      showModal(modalDialog(
        tags$h2('Please upload or select disease phenotypes or biomolecules'),
        footer=tagList(
          modalButton('Return')
        )
      ))
    } else {
      app_data$current_phecode = current_phecode_react()
      app_data$current_description = current_description_react()
      app_data$current_institution = current_institution_react()
      # app_data$current_data = current_data()
      app_data$visualize_network = TRUE
      app_data$update_info_all = TRUE
    }
    
  })
  #reset selections if user select reset

  return(
    reactive({
      if(app_data$visualize_network){
        list(
          current_phecode = app_data$current_phecode,
          current_description = app_data$current_description, 
          current_institution = app_data$current_institution,
          # current_data = app_data$current_data,
          visualize_network = app_data$visualize_network,
          update_info_all = app_data$update_info_all
        )  
      } else {
        NULL
      }
    })
  )
}








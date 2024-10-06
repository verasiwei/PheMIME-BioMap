info_panel_UI = function(id,app_ns){
  ns <- . %>% NS(id)() %>% app_ns()
  
  tagList(
    
    box(title = strong("Information Panel"),
        offset = 0, 
        style='overflow-y: auto; padding-right: 0px;padding-top: 0px;padding-bottom: 0px;padding-left: 5px;height:203vh;',
        width = 12,
        solidHeader=F,
        ## select disease phenotype of interest
        div(class = "subtitle","Update Disease Phenotypes of Interest",style="padding-top: 20px;font-size: 2rem;color:black;float:left"),
        DTOutput(ns("code_selection_tab")),
        hr(),
        div(class = "subtitle","Update Disease Phenotypes That Highly Co-occur with the Selected Disease Phenotypes",style="padding-top: 20px;font-size: 2rem;color:black;float:left"),
        DTOutput(ns("shared_phenotype_tab")),
        div("Current selections:",style = "font-size:1.7rem;color: black;
                                                padding-left: 10px;
                                                display: flex;align-items:
                                                center;justify-content: space-evenly;"),
        div(span(textOutput(ns("current_code_label"),inline = TRUE),style = "font-size:1.7rem;color:black;"),
            style = "padding-left: 10px;
                     display: flex;align-items:
                     center;justify-content: space-evenly;"),
        div(actionButton(ns("update"), "Update Analysis",class="buttonstyle1"))
        # hr(),
        # div(strong("Correlation with major systems"),style="font-size: 1.5rem;color:black"),
        # hr()
    )
    
  )
}

info_panel_Server <- function(input,output,session,current_phecode,current_description,current_institution,current_row,visualize_network) {
  
  current_institution_react <- reactiveVal(current_institution())
  current_phecode_react <- reactiveVal(current_phecode())
  current_description_react <- reactiveVal(current_description())
  current_row_react <- reactiveVal(current_row())
  
  ## send the updates to the main module
  app_data <- reactiveValues(
    current_phecode = NULL,
    current_description = NULL,
    current_institution = NULL,
    current_row = NULL,
    visualize_network = FALSE,
    update_info_all = FALSE
    
  )
  ## pre disease phenotype of interest selection table
  dat <- reactiveValues(table_data = all_dat %>% filter(group=="phecode") %>% filter(institution==current_institution()))
  ## pre real-time disease co-occurence table
  dat_co <- reactiveValues(table_data = shared_phe %>% filter(institution==current_institution()) %>% filter(sel_phenotype %in% current_description()))
  
  ## update selection table and co phenotype table
  observeEvent(current_description_react(),{

    dat$table_data = all_dat %>% filter(group=="phecode") %>% filter(institution==current_institution_react())
    dat_co$table_data = shared_phe %>% filter(institution==current_institution_react()) %>% filter(sel_phenotype %in% current_description_react())

  })
  # 
  ## update the current selections based on the user's selection
  observeEvent(c(input$code_selection_tab_rows_selected,input$shared_phenotype_tab_rows_selected),{

    current_description_react(unique(c(dat$table_data$Description[input$code_selection_tab_rows_selected],
                                 dat_co$table_data$co_phenotype[input$shared_phenotype_tab_rows_selected])))
    current_phecode_react(unique(c(dat$table_data$code[input$code_selection_tab_rows_selected],
                             dat_co$table_data$co_phecode[input$shared_phenotype_tab_rows_selected])))

    add_row = which(dat$table_data$Description %in% current_description_react())
    current_row_react(add_row)

  })
  
  ## user select specific phenotypes/biomolecules
  output$code_selection_tab <- renderDT({
    
    datatable(dat$table_data,
              # caption = "Select Disease Phenotype or Biomolecule of Interest",
              # rownames = FALSE,
              #options = list(displayStart = start_index - 2),
              options = list(
                scrollX = "300px",
                scrollY = "300px",
                pageLength = 30, lengthChange = FALSE
              ),    
              selection = list(mode = 'multiple',selected=current_row_react())
    )
  },server = TRUE)
  
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
  
  
  ##after user select specific code table, update the react variables
  code_id <- reactive(glue("{current_phecode_react()}:", "{current_description_react()}\n"))
  output$current_code_label <- renderText(glue("{code_id()}"))
  # visualize_network <- reactive({input$visualize})
  observeEvent(input$update, {
    # removeModal()
    if(length(current_phecode_react())==0){
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
      app_data$current_row = current_row_react()
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
          current_row = app_data$current_row,
          visualize_network = app_data$visualize_network,
          update_info_all = app_data$update_info_all
        )  
      } else {
        NULL
      }
    })
  )
}








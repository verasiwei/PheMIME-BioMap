server <- function(input, output, session) {
  
  loaded_data <- callModule(
    data_loading, "data_loading",session=session
  )
  
  output$ui <- renderUI({
    no_data <- is.null(loaded_data())
    if(no_data){
      data_loading_UI('data_loading')
    }else{
      main_app_UI('main_app')
    }
  })
  
  observeEvent(loaded_data(), {
    all_data <- loaded_data()
    app <- callModule(
      main_app_Server, "main_app",
      current_phecode = all_data$current_phecode,
      current_description = all_data$current_description, 
      current_institution = all_data$current_institution,
      current_row = all_data$current_row,
      visualize_network = all_data$visualize_network
    )
  })
}
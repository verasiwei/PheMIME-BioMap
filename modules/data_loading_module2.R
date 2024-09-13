starting_code <- c("250.20","250.70","272.10","278.10","401.10","594.10","296.20")
starting_description = c("Type 2 diabetes", "Diabetic retinopathy", "Hyperlipidemia", "Obesity", "Essential hypertension", "Calculus of kidney", "Depression")
starting_row = c(73,78,83,95,163,274,118)
# Used in data table to both select correct row and navigate table to that row
# start_index <- which(phecodes$code %in% starting_code)
data_loading_UI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    useShinydashboard(),
    # use a gradient in background
    setBackgroundColor(
      color = c("#F2F3F6"),
      gradient = "linear",
      direction = "bottom"
    ),
    titlePanel(
      div(h2("PheBioNet",style='padding-top:0px;padding-bottom:0px;padding-left:10px'),style='margin-top:-15px;margin-bottom:-5px'),
      windowTitle = "Network Analysis and Visualization of Shared Biomolecular Mechanisms in Disease Multimorbidity"
    ),
    
    # hr(),
    sidebarLayout(
      sidebarPanel(
        width=6,
        style = "background-color: white;height:137vh",
        selectInput(ns("institution"),label = div(class = "subtitle","Selecting Institution",style="font-size: 2rem;color:black;float:left"),
                    choices = list("UKB"="ukb","VUMC"="vumc"),selected = "ukb"),
        hr(),
        ## select disease phenotype of interest
        div(class = "subtitle","Select Disease Phenotypes of Interest",style="padding-top: 20px;font-size: 2rem;color:black;float:left"),
        DTOutput(ns("code_selection")),
        hr(),
        actionButton(ns("bring_top"), "Bring Selected Rows Top",class="buttonstyle1"),
        # selectizeInput(ns("Description"), NULL, 
        #                choices = all_dat %>% filter(group=="phecode") %>% pull(Description) %>% unique(), 
        #                selected=starting_description,
        #                multiple=TRUE,options=list(create=FALSE)),
        # hr(),
        div("Current selections:",style = "font-size:1.7rem;color: black;
                                                padding-left: 10px;
                                                display: flex;align-items:
                                                center;justify-content: space-evenly;"),
        div(span(textOutput(ns("current_code_label"),inline = TRUE),style = "font-size:1.7rem;color:black;"),
            style = "padding-left: 10px;
                     display: flex;align-items:
                     center;justify-content: space-evenly;"),
        hr(),
        div(actionButton(ns("reset_selection"), "Reset Selection",class="buttonstyle1"),
            actionButton(ns("visualize"), "Visualize Bipartite Network",class="buttonstyle1")
        ),
      ),
      ## upset plot with disease of interest
      mainPanel(style = "background-color: white;height:137vh;",width=6,
                div(class = "subtitle","Adding Disease Phenotypes from PheMIME",style="padding-top: 20px;font-size: 2rem;color:black;float:left"),
                ## upload results from PheMIME
                fileInput(ns("upload"), label = NULL,multiple = TRUE),
                hr(),
                div(class = "subtitle","Adding Disease Phenotypes That Highly Co-occur with the Selected Disease Phenotypes",style="padding-top: 20px;font-size: 2rem;color:black;float:left"),
                DTOutput(ns("shared_phenotype")),
                hr()
      )
    ) #full panel
  )
}

data_loading <- function(input, output, session) {
  current_institution <- reactiveVal("ukb")
  current_phecode <- reactiveVal(starting_code)
  current_description <- reactiveVal(starting_description)
  current_row <- reactiveVal(starting_row)
  upload_data <- reactiveValues(table_data=data.frame(phecode=c(),description=c()))
  index <- reactiveVal(NULL)
  ## send the updates to the main module
  app_data <- reactiveValues(
    current_phecode = NULL,
    current_description = NULL,
    current_institution = NULL,
    # current_data = NULL,
    visualize_network = FALSE
  )
  ## pre disease phenotype of interest selection table
  dat <- reactiveValues(table_data = all_dat %>% filter(group=="phecode") %>% filter(institution=="ukb"))
  ## pre real-time disease co-occurence table
  dat_co <- reactiveValues(table_data = shared_phe %>% filter(institution=="ukb") %>% filter(select_phenotype %in% starting_description))
  
  ## update selection table and co phenotype table
  observeEvent(c(input$institution,current_description()),{
    
    dat$table_data = all_dat %>% filter(group=="phecode") %>% filter(institution==current_institution())
    dat_co$table_data = shared_phe %>% filter(institution==current_institution()) %>% filter(select_phenotype %in% current_description())
    
  })
  
  observeEvent(input$upload,{
    
    file <- input$upload
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "xlsx", "Please upload a xlsx file"))
    
    nodes_data = read_excel(file$datapath) %>%
      dplyr::select(phecode,description)
    upload_data$table_data=nodes_data
    
    nodes_data = upload_data$table_data[upload_data$table_data$description %in% all_dat$Description[all_dat$institution==current_institution()],]
    
    if(nrow(nodes_data)==0){
      showModal(modalDialog(
        tags$h2('There are no significant associations with these codes in the knowledge-base.'),
        footer=tagList(
          modalButton('Return')
        )
      ))
    } else{
      current_phecode(unique(c(current_phecode(),nodes_data$phecode)))
      current_description(unique(c(current_description(),nodes_data$description)))
    }
  })
  
  ## user select specific phenotypes/biomolecules
  output$code_selection <- renderDT({
    
    datatable(dat$table_data,
              # caption = "Select Disease Phenotype or Biomolecule of Interest",
              # rownames = FALSE,
              #options = list(displayStart = start_index - 2),
              options = list(
                scrollX = "300px",
                scrollY = "300px",
                pageLength = 30, lengthChange = FALSE
              ),    
              selection = list(mode = 'multiple',selected=current_row())
    )
  },server = TRUE)
  
  ## user select specific phenotypes
  output$shared_phenotype <- renderDT({
    
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
  observeEvent(input$code_selection_rows_selected,{
    
    current_description(unique(c(dat$table_data$Description[input$code_selection_rows_selected],dat_co$table_data$co_phenotype[input$shared_phenotype_rows_selected])))
    current_phecode(unique(c(dat$table_data$code[input$code_selection_rows_selected],dat_co$table_data$co_phecode[input$shared_phenotype_rows_selected])))
    
  })
  ## update the current selections based on the user's selection
  observeEvent(input$shared_phenotype_rows_selected,{
    
    current_description(unique(c(dat$table_data$Description[input$code_selection_rows_selected],
                                 unique(dat_co$table_data$select_phenotype),
                                 dat_co$table_data$co_phenotype[input$shared_phenotype_rows_selected],upload_data$table_data$description)))
    current_phecode(unique(c(dat$table_data$code[input$code_selection_rows_selected],
                             unique(dat_co$table_data$select_phecode),
                             dat_co$table_data$co_phecode[input$shared_phenotype_rows_selected],upload_data$table_data$phecode)))
    
    add_row = which(dat$table_data$Description %in% current_description())
    current_row(add_row)
  })
 
  
  ## bring the selection to the top option
  observeEvent(input$bring_top,{
    
    selected_rows <- unique(c(input$code_selection_rows_selected,index()))
    # selected_rows <- input$code_selection_rows_selected
    
    row_order <- order(
      seq_along(dat$table_data[[1]]) %in% selected_rows,
      decreasing = TRUE
    )
    dat$table_data <- dat$table_data[row_order,]
    
    proxy <- DT::dataTableProxy("code_selection")
    DT::replaceData(proxy,dat$table_data)
    DT::selectRows(proxy,seq_along(selected_rows))
    
  })
  
  ##after user select specific code table, update the react variables
  code_id <- reactive(glue("{current_phecode()}:", "{current_description()}\n"))
  output$current_code_label <- renderText(glue("{code_id()}"))
  # visualize_network <- reactive({input$visualize})
  observeEvent(input$visualize, {
    # removeModal()
    if(length(current_phecode())==0){
      showModal(modalDialog(
        tags$h2('Please upload or select disease phenotypes or biomolecules'),
        footer=tagList(
          modalButton('Return')
        )
      ))
    } else {
      app_data$current_phecode = current_phecode()
      app_data$current_description = current_description()
      app_data$current_institution = current_institution()
      # app_data$current_data = current_data()
      app_data$visualize_network = TRUE
    }
    
  })
  #reset selections if user select reset
  observeEvent(input$reset_selection,{
    current_description(NULL)
    current_phecode(NULL)
    output$code_selection <- renderDT({

      datatable(dat$table_data,
                rownames = FALSE,
                #options = list(displayStart = start_index - 2),
                options = list(
                  scrollX = "300px",
                  scrollY = "300px"
                ),    
                selection = list(mode = 'multiple')
      )},server = FALSE)
    
    output$shared_phenotype <- renderDT({
      
      datatable(dat_co$table_data,
                rownames = FALSE,
                #options = list(displayStart = start_index - 2),
                options = list(
                  scrollX = "300px",
                  scrollY = "300px"
                ),    
                selection = list(mode = 'multiple')
      )},server = FALSE)
    
  })
  
  return(
    reactive({
      if(app_data$visualize_network){
        list(
          current_phecode = app_data$current_phecode,
          current_description = app_data$current_description, 
          current_institution = app_data$current_institution,
          # current_data = app_data$current_data,
          visualize_network = app_data$visualize_network
        )  
      } else {
        NULL
      }
    })
  )
  
}



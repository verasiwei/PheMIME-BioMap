data_loading_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          uiOutput(ns('preloaded_data')),
          actionButton(ns('preLoadedData'), 'Use preloaded UK Biobank or VUMC data'),
          hr(),
          h3('Load your data'),
          fileInput(ns("phewas"), "Phewas summary statistics file",
                    accept = ACCEPTED_FORMATS
                    
          ),
          shinyjs::hidden(
            actionButton(ns("goToApp"), "Enter App")
          )
        )
        # ,
        # mainPanel(
        #   includeMarkdown(here("www/data_instructions.md"))
        # )
      )
    )
  )
}

data_loading <- function(input, output, session) {
  #----------------------------------------------------------------
  # Reactive Values based upon user input
  #----------------------------------------------------------------
  print('running loading module!')
  app_data <- reactiveValues(
    tidy_connect = NULL
  )

  ## upload UK Biobank Data
  preloaded_data <- list.files(here('data/preloaded'), pattern = 'rs')

  output$preloaded_data <- renderUI({
    selectInput(session$ns("dataset_selection"), "Select a pre-loaded dataset:",
                preloaded_snps
    )
  })

  observeEvent(input$genome, {

    tryCatch({
      good_genome_file <- read_csv(input$genome$datapath) %>%
        checkGenomeFile()

      app_data$snp_name <- good_genome_file$snp_name
      app_data$genome_raw <- good_genome_file$data
    },
    error = function(message){
      print(message)
      showModal(modalDialog(
        p("There's something wrong with the format of your genome data. Make sure the file has two columns. One with the title IID with unique id and one with the title of your snp containing copies of the minor allele."),
        strong('Error message:'),
        code(message),
        title = "Data format problem",
        easyClose = TRUE
      ))
    })
  })

  observeEvent(input$phewas, {

    tryCatch({
      app_data$phewas_raw <- read_csv(input$phewas$datapath) %>% checkPhewasFile()
    },
    error = function(message){
      print(message)
      showModal(modalDialog(
        p("There's something wrong with the format of your results data."),
        strong('Error message:'),
        code(message),
        title = "Data format problem",
        easyClose = TRUE
      ))
    })
  })

  observeEvent(input$phenome, {
    tryCatch({
      app_data$phenome_raw <- read_csv(input$phenome$datapath) %>% checkPhenomeFile()
    },
    error = function(message){
      print(message)
      showModal(modalDialog(
        p("There's something wrong with the format of your phenome data."),
        strong('Error message:'),
        code(message),
        title = "Data format problem",
        easyClose = TRUE
      ))
    })
  })
  #----------------------------------------------------------------
  # Data Loading Logic
  #----------------------------------------------------------------
  # Watches for all files to be loaded and then triggers.
  observe({
    req(app_data$phewas_raw, app_data$genome_raw, app_data$phenome_raw)

    withProgress(message = 'Loading data', value = 0, {
      # read files into R's memory
      incProgress(1/4, detail = "Reading in uploaded files")

      phenome <- app_data$phenome_raw
      genome  <- app_data$genome_raw
      phewas  <- app_data$phewas_raw

      # first spread the phenome data to a wide format
      incProgress(2/4, detail = "Processing phenome data")
      individual_data <- phenome %>%
        mutate(value = 1) %>%
        spread(code, value, fill = 0)

      # Next merge with genome data
      incProgress(3/4, detail = "Merging phenome and genome data")
      individual_data <- individual_data %>%
        left_join(genome, by = 'IID') %>%
        mutate(snp = ifelse(is.na(snp), 0, snp))

      # These are codes that are not shared between the phewas and phenome data. We will remove them
      # from either.
      phenome_cols <- colnames(individual_data)
      bad_codes <- setdiff(phenome_cols %>% head(-1) %>% tail(-1), unique(phewas$code))
      app_data$phewas_data <- phewas %>% # remove bad codes from phewas
        filter(!(code %in% bad_codes))

      # remove bad codes from individual data
      app_data$individual_data<- individual_data[,-which(phenome_cols %in% bad_codes)]

      # Color palette for phecode categories
      app_data$category_colors <- makeDescriptionPalette(app_data$phewas_data)

      # Sending to app
      incProgress(4/4, detail = "Sending to application!")

      # shinyjs::show("goToApp")
      app_data$data_loaded <- TRUE
    }) # end progress messages
  })

  observeEvent(input$goToApp,{
    app_data$data_loaded <- TRUE
  })

  observeEvent(input$preLoadedData,{
    base_dir <- glue('data/preloaded/{input$dataset_selection}') %>% here()

    app_data$phewas_raw <- glue('{base_dir}/phewas_results.csv') %>% read_csv()
    app_data$phenome_raw <-  here('data/preloaded/id_to_code.csv') %>% read_csv()


    genome_file <- glue('{base_dir}/id_to_snp.csv') %>%
      read_csv() %>%
      checkGenomeFile()
    app_data$snp_name <- genome_file$snp_name
    app_data$genome_raw <- genome_file$data
  })

  return(
    reactive({
      if(app_data$data_loaded){
        list(
          individual_data = app_data$individual_data,
          category_colors = app_data$category_colors,
          phewas_data = app_data$phewas_data,
          snp_name = app_data$snp_name
        )
      } else {
        NULL
      }
    })
  )
}



source("modules/load_library.R")
source("modules/helpers_func.R")
source("modules/data_loading.R")
# source("modules/multipartite_network_vis_update.R")
source('modules/data_loading_module2.R')
source('modules/main_app_module2.R')
source('modules/info_panel_module2.R')
source('modules/multipartite_network_module.R')
source('modules/shared_info_module.R')
source('modules/upset_plot_module2.R')
source("modules/shared_pathways_module.R")
options(repos = BiocManager::repositories())

starting_code <- c("250.20","250.70","272.10","278.10","401.10","594.10","296.20")
# starting_code <- c("other aUPD","overlap v617f aUPD","GGCC/GGCC","GGCC/TCTT")
starting_description = c("Type 2 diabetes", "Diabetic retinopathy", "Hyperlipidemia", "Obesity", "Essential hypertension", "Calculus of kidney", "Depression")
# starting_description = c("other aUPD","overlap v617f aUPD","GGCC/GGCC","GGCC/TCTT")
starting_row = c(73,78,83,95,163,274,118)
# starting_row = c(672,673,674,675)
# Used in data table to both select correct row and navigate table to that row
start_index <- which(phecodes$code %in% starting_code)
ui <- shinyUI(
  fluidPage(
    # titlePanel(
    #   title = "Phe-Omics Multimorbidity Explorer"
    # ),
    # hr(),
    mainPanel(
      width = 12,
      includeCSS("inst/custom.css"),
      shinyjs::useShinyjs(debug = TRUE),
      uiOutput("ui")
    )
    # ,
    # skin = 'black'
  )
)
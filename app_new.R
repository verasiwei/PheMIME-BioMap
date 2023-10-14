# setwd(here::here())
source("modules/load_library.R")
source("modules/helpers_func.R")
source("modules/data_loading.R")
source("modules/multipartite_network_vis.R")
source("modules/heatmap.R")
# molecular_nodes = unique(nodes$node)
starting_code <- c("296.20")
# Used in data table to both select correct row and navigate table to that row
start_index <- which(phecodes$code %in% starting_code)


ui = dashboardPage(
  
  dashboardHeader(
    ## header
    title = strong("Phe-Omics Multi-Institutional Multimorbidity Explorer"),
    titleWidth = 600,
    leftUi = tagList(dropdownBlock(
        id = "tuning_network",
        title = div(icon("sliders"),"Tuning Network",style="color:black;"),
        # icon = icon("sliders"),
        p(HTML("<b>Select molecular levels</b>"),span(shiny::icon("info-circle"),id = "info_molecular"),
          selectInput('molecular', label="",
                      choices=list("","Gene"="gene","Protein"="protein","Metabolite"="metabolite"),selected = NULL),
          tippy::tippy_this(elementId = "info_molecular",
                            tooltip = "<span style='font-size:20px;'>Select gene/protein/metabolite<span>",
                            placement = "right")),
        
        p(HTML("<b>Input cutoff p-value</b>"),span(shiny::icon("info-circle"),id = "info_pvalue"),
          numericInput('pvalue', label=NULL,min=0,max=0.05,value=0.05),
          tippy::tippy_this(elementId = "info_pvalue",
                            tooltip = "<span style='font-size:20px;'>Remove connections > p-value cutoff, maximum cutoff is 0.05<span>",
                            placement = "right")),
        
        div(actionButton("reset_network", "Reset",style="float:right;background-color: #fff;"),
            actionButton("update_network", "Update",style="float:left;background-color: #fff;"))
      ))
  ),
  
  dashboardSidebar(
    width = 300,
    tags$style(
      "#sidebarItemExpanded {
            overflow: auto;
            max-height: 100vh;
        }"
    ),
    fluidRow(
      ##select institution
      column(width=12,
             box(width=12,
                 status="warning",solidHeader = FALSE,
                 title = strong("Select Institution",style="font-size: 1.7rem;color:black;float:left"),
                 ##select institution
                 selectInput("institution", label = NULL,
                             choices = list("UKB"="ukb","VUMC"="vumc")),
                 hr(),
                 ##select phecode/molecular centric
                 div(strong("Interactive Data Selection",style="font-size: 1.7rem;color:black")),
                 DTOutput("data_selection"),
                 actionButton("show_data", "Show Tables of Selection",style="float:right;background-color: #D3D3D3;")
                 )
             ),
      hr(),
      column(width=12,
             div("Current selections:",style = "font-size:1.7rem;color: black;
                                                padding-left: 10px;
                                                display: flex;align-items:
                                                center;justify-content: space-evenly;"),
             div(span(textOutput("current_code_label",inline = TRUE),style = "font-size:1.7rem;color:black;"),
               style = "padding-left: 10px;
                     display: flex;align-items:
                     center;justify-content: space-evenly;")
             # ,
      
      )
     
    )),
  dashboardBody(
    body = dashboardBody(
      setShadow(class = "dropdown-menu")
    ),
    fluidRow(
      
      ##network visualization
      column(width=8,
             multipartite_network("network_vis")
             )
      ,
      ##heatmap between selection of nodes
      column(width=4,
             heatmap("heatmap_selections")
             )
    ),
    
    tags$head(
    tags$style(HTML('
       .content-wrapper,
       .right-side {
        background-color: #F2F3F6;
                  }
        /*main header*/
        .skin-blue .main-header .logo {
                              background-color: #F2F3F6;
                              color: #50595e;
                              }
        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #F2F3F6;
                              }
        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #F2F3F6;
                              }

        /* main sidebar */
        /* The toggle lines to collapse menu bar   */
        .skin-blue .main-header .navbar .sidebar-toggle {
                                color: #000000;
                                background-color: #F2F3F6;
                                }
        .skin-blue .main-sidebar {
                              background-color: #F2F3F6;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color:#859900;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #F2F3F6;
                              color: white;
                              }
        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #859900;
                              }
        /* toggle button when hovered  */
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #859900;
         }
         
       /* warning status color  */
  
         .box.box-solid.box-warning>.box-header {
                              color:black;
                              background:#2c3e50;
                              /*#dfdfdf;*/
                              }
         .box.box-solid.box-warning{
                              border-bottom-color:white;
                              border-left-color:white;
                              border-right-color:white;
                              border-top-color:white;
                              /*#2c3e50*/
                              background:#fff;
                              }
          .box.box-warning>.box-header {
                              color:#000000;
                              background:white;
                    }
          .box.box-warning{
                              border-bottom-color:white;
                              border-left-color:white;
                              border-right-color:white;
                              border-top-color:white;
                              background:white;
                              }')))
  )
)

server <- function(input, output, session) {
  current_institution <- reactive({input$institution})
  current_data <- reactiveVal("Phecode")
  current_phecode <- reactiveVal(c("250.20","272.10"))
  current_description <- reactiveVal(c("Type 2 diabetes","Hyperlipidemia"))
  current_row <- reactiveVal(c(113,144))
  observeEvent(input$data_selection_rows_selected,{
    current_data(c("Phecode","Gene","Protein","Metabolite")[input$data_selection_rows_selected])
  })
  
  observeEvent(input$code_selection_rows_selected,{
    # sel <- current_data()
    if(identical(current_data(),"Phecode")){
      dat = all_dat %>% filter(group=="phecode")
    } else if(identical(current_data(),"Gene")){
      dat = all_dat %>% filter(group=="gene")
    } else if(identical(current_data(),"Protein")){
      dat = all_dat %>% filter(group=="protein")
    } else if(identical(current_data(),"Metabolite")){
      dat = all_dat %>% filter(group=="metabolite")
    } 
    # else if(identical(current_data(),c("Phecode","Gene"))){
    #   dat = all_dat %>% filter(group=="phecode" | group=="gene")
    # } else if(identical(current_data(),c("Phecode","Protein"))){
    #   dat = all_dat %>% filter(group=="phecode" | group=="protein")
    # } else if(identical(current_data(),c("Phecode","Metabolite"))){
    #   dat = all_dat %>% filter(group=="phecode" | group=="metabolite")
    # } else if(all(current_data()==c("Gene","Protein"))){
    #   dat = all_dat %>% filter(group=="gene" | group=="protein")
    # } else if(all(current_data()==c("Gene","Metabolite"))){
    #   dat = all_dat %>% filter(group=="gene" | group=="metabolite")
    # } else if(all(current_data()==c("Protein","Metabolite"))){
    #   dat = all_dat %>% filter(group=="protein" | group=="metabolite")
    # } else if(all(current_data()==c("Phecode","Gene","Protein","Metabolite"))){
    #   dat = all_dat 
    # } else if(all(current_data()==c("Phecode","Gene","Protein"))){
    #   dat = all_dat %>% filter(group=="phecode" | group=="gene" | group=="protein")
    # } else if(all(current_data()==c("Phecode","Gene","Metabolite"))){
    #   dat = all_dat %>% filter(group=="phecode" | group=="gene" | group=="metabolite")
    # } else if(all(current_data()==c("Phecode","Protein","Metabolite"))){
    #   dat = all_dat %>% filter(group=="phecode" | group=="metabolite" | group=="protein")
    # } else if(all(current_data()==c("Gene","Metabolite","Protein"))){
    #   dat = all_dat %>% filter(group=="metabolite" | group=="gene" | group=="protein")
    # }
    current_row(input$code_selection_rows_selected)
    current_description(dat$Description[input$code_selection_rows_selected])
    current_phecode(dat$code[input$code_selection_rows_selected])
  }) #phecode description
  
  code_id <- reactive(glue("{current_phecode()} {current_description()}"))
  output$current_code_label <- renderText(glue("{code_id()}"))
  
  #reset selections
  observeEvent(input$reset_selection,{
    current_description(NULL)
    current_phecode(NULL)
    output$code_selection <- renderDT({
      if(identical(current_data(),"Phecode")){
        dat = all_dat %>% filter(group=="phecode")
      } else if(identical(current_data(),"Gene")){
        dat = all_dat %>% filter(group=="gene")
      } else if(identical(current_data(),"Protein")){
        dat = all_dat %>% filter(group=="protein")
      } else if(identical(current_data(),"Metabolite")){
        dat = all_dat %>% filter(group=="metabolite")
      }
      datatable(dat,
                rownames = FALSE,
                #options = list(displayStart = start_index - 2),
                options = list(
                  scrollX = "300px",
                  scrollY = "300px"
                ),    
                selection = list(mode = 'multiple')
      )},server = FALSE)
    
  })
  
  ##tuning selection
  molecular <- reactive({
    input$molecular
  })
  pvalue <- reactive({
    input$pvalue
  })
  update_network <- reactive({
    input$update_network
  })
  reset_network <- reactive({
    input$reset_network
  })
  
  ## select data centric of interest
  selected_rows <- reactiveVal(NULL)
  output$data_selection <- renderDT({
    dat_sel = data.frame(Category = c("Phecode","Gene","Protein","Metabolite"))
    datatable(dat_sel,
              rownames = FALSE,colnames = "",
              options = list(dom="t"),
              # caption = strong("Interactive Data Selection",style="font-size: 1.7rem;"),
              selection = list(mode = "single",selected = c(1)))
  },server = FALSE)
  
  ## datatable to let users select

  observeEvent(input$show_data,{
    current_phecode(NULL)
    current_description(NULL)
    showModal(modalDialog(DTOutput("code_selection"),
                          footer = div(
                            # actionButton("done","Select"),
                                       actionButton("reset_selection", "Reset selections"),
                                       actionButton("visualize","Visualize the multipartite network"))
                          ))
  })
  # close_network <- reactiveVal(NULL)
  visualize_network <- reactive({input$visualize})
  observeEvent(input$visualize, {
    removeModal()
    # showModal(modalDialog(div("Click Visualize Button to Update the Network")))
  })
  
  output$code_selection <- renderDT({
    if(identical(current_data(),"Phecode")){
      dat = all_dat %>% filter(group=="phecode")
    } else if(identical(current_data(),"Gene")){
      dat = all_dat %>% filter(group=="gene")
    } else if(identical(current_data(),"Protein")){
      dat = all_dat %>% filter(group=="protein")
    } else if(identical(current_data(),"Metabolite")){
      dat = all_dat %>% filter(group=="metabolite")
    }
    datatable(dat,
              rownames = FALSE,
              #options = list(displayStart = start_index - 2),
              options = list(
                scrollX = "300px",
                scrollY = "300px"
              ),    
              selection = list(mode = 'multiple',selected=current_row())
    )},server = FALSE)
  
  multipartite_networkServer("network_vis",molecular,pvalue,update_network,reset_network,current_description,visualize_network,current_institution,current_data)
  heatmapServer("heatmap_selections",current_description,visualize_network,current_institution)
}

shinyApp(ui, server)

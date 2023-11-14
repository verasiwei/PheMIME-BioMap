info_panel_UI = function(id,app_ns){
  ns <- . %>% NS(id)() %>% app_ns()
  
  tagList(
    
    box(title = strong("Information Panel"),
        offset = 0, 
        style='overflow-y: auto; padding-right: 0px;padding-top: 0px;padding-bottom: 0px;padding-left: 5px;height:106vh;',
        width = 12,
        solidHeader=F,
        div(strong("Pre-selected Nodes"),style="font-size: 1.5rem;color:black"),
        textOutput(ns("preselected_nodes")),
        hr(),
        div(strong("User Manual"),style="font-size: 1.5rem;color:black"),
        div(strong("Multipartite Network"),style="font-size:1.3rem;color:black"),
        p("Interact with the network plot by selecting the nodes of interest; these clicked nodes will be highlighted in red for easy identification. Additionally, any other nodes that are connected to all of the selected nodes will be accentuated in their respective category colors. This feature not only distinguishes your selections but also visually emphasizes their connections within the network."),
        div(strong("Upset Plot"),style="font-size:1.3rem;color:black"),
        p("The upset plot is interactive with the multipartite network plot. When nodes in the network plot are selected, their intersections are dynamically represented in the upset plot."),
        div(strong("Information Table"),style="font-size:1.3rem;color:black"),
        p("The information table is designed to interact dynamically with the multipartite network plot. It highlights the universal connectivity by displaying nodes that are directly connected to the nodes selected by the user. Additionally, it features synchronized nodes, which represent nodes linked to common shared nodes that are connected with all the nodes selected by the user. This interactivity provides a comprehensive view of both direct and indirect relationships within the network.")
    )
    
  )
}

info_panel_Server <- function(input,output,session,current_phecode,current_description,current_institution,visualize_network) {
  
  code_id <- reactive(glue("{current_phecode}:","{current_description};"))
  output$preselected_nodes <- renderText(
    glue("{code_id()}")
    # cat(cat(paste0(current_phecode,":",current_description,"\n")))
    )
}








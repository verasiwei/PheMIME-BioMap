heatmap = function(id){
  ns = NS(id)
  fluidRow(
    column(width=12,
           box(
             width = 12,
             title = "Pre-Selected Nodes Multimorbidity Strength", 
             closable = F, 
             status = "warning", 
             solidHeader = FALSE, 
             collapsible = TRUE,
            div(style = 'overflow-y: scroll;height:750px;overflow-x: scroll;',
                div("gene"),
             div(withSpinner(plotlyOutput(ns("heatmap_gene_plot"),width = "100%"),
                                hide.ui = FALSE)),
             div("protein"),
             div(withSpinner(plotlyOutput(ns("heatmap_protein_plot"),width = "100%"),
                             hide.ui = FALSE)),
             div("metabolite"),
             div(withSpinner(plotlyOutput(ns("heatmap_metabolite_plot"),width = "100%"),
                             hide.ui = FALSE)),
             div("phecode"),
             div(withSpinner(plotlyOutput(ns("heatmap_phecode_plot"),width = "100%"),
                             hide.ui = FALSE))
            )
           ))
  ) #fluidrow
}

heatmapServer = function(id,current_description,visualize_network,current_institution){
  moduleServer(id,
               function(input,output,session){
                 
                 ## update data for heatmap
                 data_for_heatmap_gene = reactiveVal(data_heatmap_gene_initial)
                 data_for_heatmap_protein = reactiveVal(data_heatmap_protein_initial)
                 data_for_heatmap_metabolite = reactiveVal(data_heatmap_metabolite_initial)
                 data_for_heatmap_phecode = reactiveVal(data_heatmap_phecode_initial)
       
                 ## observe visualization of the network
                 observeEvent(visualize_network(),{
                   
                   # ## 1st layer nodes connected with user selected phecode
                   conn_first = tidy_connect %>%
                     filter(institution == current_institution()) %>%
                     filter(from %in% current_description()) %>%
                     dplyr::select(node = to,connection_type) %>%
                     bind_rows(tidy_connect %>%
                                 filter(institution == current_institution()) %>%
                                 filter(to %in% current_description()) %>%
                                 dplyr::select(node = from,connection_type)) %>%
                     distinct(node,.keep_all = T) 
                   
                   heatmap_dat = purrr::map_dfr(current_description(),function(i){
                     
                     data.frame(y = conn_first$node,connection_type=conn_first$connection_type) %>%
                       left_join(.,tidy_connect %>%
                                   filter(institution == current_institution()) %>%
                                   filter(from %in% i) %>%
                                   dplyr::select(node = to,hazard_ratio) %>%
                                   bind_rows(tidy_connect %>%
                                               filter(institution == current_institution()) %>%
                                               filter(to %in% i) %>%
                                               dplyr::select(node = from,hazard_ratio)) %>%
                                   distinct(node,.keep_all=T) %>%
                                   mutate(x=i) %>% 
                                   rename(y=node),by="y") %>%
                       mutate(hazard_ratio = ifelse(is.na(x),0,hazard_ratio)) %>%
                       mutate(x=ifelse(is.na(x),i,x)) %>%
                       dplyr::select(rows = x,cols = y,hazard_ratio,connection_type)
                     
                   })
                   heatmap_dat = heatmap_dat %>%
                     mutate(text=paste0("x: ",rows,"\n","y: ", cols, "\n", "hazard ratio: ", round(hazard_ratio,2))) %>%
                     arrange(rows,desc(hazard_ratio))
                   heatmap_dat$cols = factor(heatmap_dat$cols,levels = unique(heatmap_dat$cols))
                   
                   
                   ### if user select molecular-level nodes
                   if(sum(current_description() %in% nodes$node[nodes$type=="phecode"]==FALSE)>=1){
                     
                     type = unique(heatmap_dat$connection_type)
                     heatmap_phecode = heatmap_dat %>% filter(connection_type==type)
                     if(nrow(heatmap_phecode)!=0){
                       heatmap_phecode = heatmap_phecode
                     } else{
                       heatmap_phecode = data.frame(cbind(rows=current_description(),cols="No connection",hazard_ratio=0)) %>% 
                         mutate(hazard_ratio=as.numeric(hazard_ratio),connection_type="phecode",
                                text=paste0("x: ",rows,"\n","y: ", cols, "\n", "hazard ratio: ", round(hazard_ratio,2)))
                     }
                     
                     heatmap_gene = data.frame(cbind(rows=current_description(),cols="No connection",hazard_ratio=0)) %>% 
                       mutate(hazard_ratio=as.numeric(hazard_ratio),connection_type="gene",
                              text=paste0("x: ",rows,"\n","y: ", cols, "\n", "hazard ratio: ", round(hazard_ratio,2)))
                     heatmap_protein = data.frame(cbind(rows=current_description(),cols="No connection",hazard_ratio=0)) %>% 
                       mutate(hazard_ratio=as.numeric(hazard_ratio),connection_type="protein",
                              text=paste0("x: ",rows,"\n","y: ", cols, "\n", "hazard ratio: ", round(hazard_ratio,2)))
                     heatmap_metabolite = data.frame(cbind(rows=current_description(),cols="No connection",hazard_ratio=0)) %>% 
                       mutate(hazard_ratio=as.numeric(hazard_ratio),connection_type="metabolite",
                              text=paste0("x: ",rows,"\n","y: ", cols, "\n", "hazard ratio: ", round(hazard_ratio,2)))
                     
                   } else {
                     heatmap_gene = heatmap_dat %>% filter(connection_type=="gene")
                     heatmap_protein = heatmap_dat %>% filter(connection_type=="protein")
                     heatmap_metabolite = heatmap_dat %>% filter(connection_type=="metabolite")
                     heatmap_phecode = data.frame(cbind(rows=current_description(),cols="No connection",hazard_ratio=0)) %>% 
                       mutate(hazard_ratio=as.numeric(hazard_ratio),connection_type="phecode",
                              text=paste0("x: ",rows,"\n","y: ", cols, "\n", "hazard ratio: ", round(hazard_ratio,2)))
                     
                     if(nrow(heatmap_gene)!=0){
                       heatmap_gene = heatmap_gene
                     } else{
                       heatmap_gene = data.frame(cbind(rows=current_description(),cols="No connection",hazard_ratio=0)) %>% 
                         mutate(hazard_ratio=as.numeric(hazard_ratio),connection_type="gene",
                                text=paste0("x: ",rows,"\n","y: ", cols, "\n", "hazard ratio: ", round(hazard_ratio,2)))
                     }
                     
                     if(nrow(heatmap_protein)!=0){
                       heatmap_protein = heatmap_protein
                     } else{
                       heatmap_protein = data.frame(cbind(rows=current_description(),cols="No connection",hazard_ratio=0)) %>% 
                         mutate(hazard_ratio=as.numeric(hazard_ratio),connection_type="protein",
                                text=paste0("x: ",rows,"\n","y: ", cols, "\n", "hazard ratio: ", round(hazard_ratio,2)))
                     }
                     
                     if(nrow(heatmap_metabolite)!=0){
                       heatmap_metabolite = heatmap_metabolite
                     } else{
                       heatmap_metabolite = data.frame(cbind(rows=current_description(),cols="No connection",hazard_ratio=0)) %>% 
                         mutate(hazard_ratio=as.numeric(hazard_ratio),connection_type="metabolite",
                                text=paste0("x: ",rows,"\n","y: ", cols, "\n", "hazard ratio: ", round(hazard_ratio,2)))
                     }
                     
                   }
                   
                   data_for_heatmap_gene(heatmap_gene)
                   data_for_heatmap_protein(heatmap_protein)
                   data_for_heatmap_metabolite(heatmap_metabolite)
                   data_for_heatmap_phecode(heatmap_phecode)
                 })
                 
                 
                 output$heatmap_gene_plot <- renderPlotly({
                   ggplotly(ggplot(data_for_heatmap_gene(),aes(x=rows,y=cols,fill=hazard_ratio,text=text))+
                              geom_tile()+
                              scale_fill_gradient(low="white",high="#689030",guide = "none")+
                              theme(axis.text.x = element_text(angle=90),
                                    axis.text.y = element_blank(),
                                    axis.title.x = element_blank(),
                                    axis.title.y = element_blank(),
                                    legend.position = "none")
                            ,tooltip = "text")%>% config(displayModeBar = F)
                   
                 })
                 
                 output$heatmap_protein_plot <- renderPlotly({
                   ggplotly(ggplot(data_for_heatmap_protein(),aes(x=rows,y=cols,fill=hazard_ratio,text=text))+
                              geom_tile()+
                              scale_fill_gradient(low="white",high="#5E738F",guide = "none")+
                              theme(axis.text.x = element_text(angle=90),
                                    axis.text.y = element_blank(),
                                    axis.title.x = element_blank(),
                                    axis.title.y = element_blank(),
                                    legend.position = "none")
                            ,tooltip = "text")%>% config(displayModeBar = F)
                   
                 })
                 
                 output$heatmap_metabolite_plot <- renderPlotly({
                   ggplotly(ggplot(data_for_heatmap_metabolite(),aes(x=rows,y=cols,fill=hazard_ratio,text=text))+
                              geom_tile()+
                              scale_fill_gradient(low="white",high="#AD6F3B",guide = "none")+
                              theme(axis.text.x = element_text(angle=90),
                                    axis.text.y = element_blank(),
                                    axis.title.x = element_blank(),
                                    axis.title.y = element_blank(),
                                    legend.position = "none")
                            ,tooltip = "text")%>% config(displayModeBar = F)
                   
                 })
                 
                 output$heatmap_phecode_plot <- renderPlotly({
                   ggplotly(ggplot(data_for_heatmap_phecode(),aes(x=rows,y=cols,fill=hazard_ratio,text=text))+
                              geom_tile()+
                              scale_fill_gradient(low="white",high="#673770",guide = "none")+
                              theme(axis.text.x = element_text(angle=90),
                                    axis.text.y = element_blank(),
                                    axis.title.x = element_blank(),
                                    axis.title.y = element_blank(),
                                    legend.position = "none")
                            ,tooltip = "text")%>% config(displayModeBar = F)
                   
                 })
                
                 })
                 
               }




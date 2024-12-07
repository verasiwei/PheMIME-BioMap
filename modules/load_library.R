library(tidyverse)
library(r2d3)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(DT)
library(igraph)
# library(bisbmHC)
library(glue)
library(heatmaply)
# library(d3heatmap)
library(hrbrthemes)
library(plotly)
library(UpSetR)
library(shinyjs)
library(clusterProfiler)
library(org.Hs.eg.db)
library(ReactomePA)
library(DOSE)
library(phewasHelper)
library(reticulate)
library(ggdendro)
library(dendextend)
library(readxl)
library(furrr)
library(cluster)
normalize_phecodes = function (codes) 
{
  sprintf("%06.2f", as.numeric(codes))
}


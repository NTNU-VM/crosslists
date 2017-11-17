library(shiny)
library(xlsx)
library(sp)
require(ggmap)
require(gridExtra)
require(rgdal)

fluidPage(
  titlePanel("Processing Vascular plant crosslists"),
    sidebarLayout(sidebarPanel(
    
    helpText("1. Browse to select Excel file where crosslist data entered"),
       fileInput('file1', 'Choose XLSX File',
              accept = c('text/xlsx')),
    helpText("2. Check metadata, taxa list, location on maps and Darwin core table"),
    helpText("3. Click to download Darwin Core file as a .csv"),
  downloadButton('dwc_download',label='Download DwC file'),
  imageOutput('img1')  
  ),
  mainPanel( tabsetPanel(
    tabPanel("TaxaList", tableOutput("taxalist")),
    tabPanel("MetaData", tableOutput("metadat")),
    tabPanel("Map", plotOutput("map")), 
    tabPanel("DwC", tableOutput("dwc"))
  )
))
)


library(shiny)
library(xlsx)
#library(openxlsx)
library(sp)
require(ggmap)
require(gridExtra)
require(rgdal)


function(input, output) {

output$img1 <- renderImage({
    list(src='images/Institutt_for_naturhistorie-eng-2.png',
         height = 100,
         width = 400,
         style = "margin:10px 10px")
  },deleteFile = F)
  
  
  observe({
  inFile <- input$file1
  
  if (is.null(inFile))
    return(NULL)

  crosslist=
      read.xlsx(
        inFile$datapath,
        sheetName = 'Species',
        endrow = 84,
        stringsAsFactors = F,
        encoding = "UTF-8"
      )

metadata_t <-
      read.xlsx(
        inFile$datapath,
        sheetName = 'MetaData',
        stringsAsFactors = F,
        header = F,
        encoding = 'UTF-8'
      )

    #Transpose metadata
    metadata <- data.frame(t(metadata_t[, 2]), stringsAsFactors = F)
    names(metadata) <- metadata_t$X1
    metadata <- data.frame(metadata, check.names = T)
 
 
    
       
output$metadat<-renderTable(metadata)
 

  # Processes the taxa names ------------------------------------------------


  df1 <-
    data.frame(cbind(
      'taxa' = stack(subset(crosslist, select = seq(1, 48, by = 2)))[, 1],
      'presence' = stack(subset(crosslist, select = seq(2, 48, by =
                                                          2)))[, 1]
    ))

  df2 <- df1[!is.na(df1$taxa), ]


  #Replace * with sbsp
  #Replace # with var
  df2$taxa <- gsub("[*]", 'subsp.', df2$taxa)
  df2$taxa <- gsub("[#]", 'var.', df2$taxa)


  #This dline removes lines that do not contain at least one lower case letter
  #This removes class names (all uppercase, and NAs and '...' etc)
  df3 <- df2[grep("[a-z]", df2$taxa), ]

  #Genus names have a capital letter
  genusnames <- df3[grep("[A-Z]", df3$taxa), ]
  speciesnames <- df3[grep("[A-Z]", df3$taxa, invert = T), ]

  #Trim leading space from species names
  trim.leading <- function (x)
    sub("^\\s+", "", x)
  speciesnames[, 1] <- trim.leading(speciesnames[, 1])

  speciesnames[, 1] <- gsub("c ", " ", speciesnames[, 1])


  #Need to include species epitphet in subspecies and variety names
  speciesnames$fullnames2 <- c()
  for (i in 1:length(speciesnames$taxa)) {
    if (grepl("subsp", speciesnames$taxa[i]) == T |
        grepl("var", speciesnames$taxa[i]) == T)
      print(i)
  }

  subsvar <-
    speciesnames[(grepl("subsp", speciesnames$taxa) == T |
                    grepl("var", speciesnames$taxa) == T) , ]
  speconly <-
    speciesnames[(grepl("subsp", speciesnames$taxa) == F &
                    grepl("var", speciesnames$taxa) == F) , ]

  speconly$taxa <- trim.leading(speconly$taxa)
  subsvar$taxa <- trim.leading(subsvar$taxa)

  for (i in 1:length(speciesnames$taxa)) {
    a <-
      as.numeric(rownames(speciesnames[i, ])) - as.numeric(rownames(genusnames))
    if (grepl("subsp", speciesnames$taxa[i]) == T |
        grepl("var", speciesnames$taxa[i]) == T)
    {
      b <-
        as.numeric(rownames(speciesnames[i, ])) - as.numeric(rownames(speconly))
      speciesnames$fullnames2[i] <-
        paste(genusnames$taxa[which.min(a[a > 0])],
              speconly$taxa[which.min(b[b > 0])],
              speciesnames$taxa[i],
              sep = ' ')
    }
    else
    {
      (
        speciesnames$fullnames2[i] <-
          paste(genusnames$taxa[which.min(a[a > 0])],
                speciesnames$taxa[i], sep = ' ')
      )
    }
  }

  #Need to find single genus names and match to full binomial
  #Species name from TAXAREG
  taxareg <-
    read.csv(
      'data/TAXAREG.csv',
      header = T,
      encoding = 'UTF-8'
    )
  taxaregspplist <-
    as.character(taxareg$GNAVN[taxareg$GYLDIG == 'TRUE' &
                                 taxareg$TAXATYPE == 'A'])

  #Split up names and select genus and spp
  taxareggen <- do.call(rbind, (strsplit(taxaregspplist, ' ')))[, 1:2]

  #Make a dataframe for appending monogeneric species

  singlegenus <- data.frame(taxa = c(),
                            presence = c(),
                            fullnames = c())

  for (i in 1:nrow(genusnames)) {
    a <- as.numeric(rownames(genusnames))
    ifelse(a[i + 1] - a[i] == 1,
           singlegenus <- rbind(singlegenus, genusnames[i, ]),
           print(i))
  }
  singlegenus <- rbind(singlegenus, genusnames[nrow(genusnames), ])
  #Match genus name with binomial name
  for (i in 1:nrow(singlegenus)) {
    singlegenus$fullnames2[i] <-
      taxaregspplist[match(singlegenus[i, 1], taxareggen)]
  }
  #Handle alternative spellings of Honkenya/Honckenya
  singlegenus$fullnames2[singlegenus$taxa == 'Honkenya'] <-
    'Honckenya peploides'

  #Join together
  full_list <- rbind(singlegenus, speciesnames)
  #Back to originalorder
  full_list$index <- as.numeric(rownames(full_list))
  full_list <- full_list[order(full_list$index), ]
  taxa_list <- full_list[, c(3, 2)]

  # Listing species present -------------------------------------------------

  #Now list species present
  spp_occurences<-taxa_list[!is.na(taxa_list$presence),]

names(spp_occurences)<-c('Taxa','Occurence')
 


 # Handling location and other meta data ----------------------------------------------

  #Date
  date<-as.Date(paste(metadata$Day,metadata$Month,metadata$Year,sep='/'),'%d/%m/%Y')

  #list of recorders
  recorders<-c(metadata$Filled.out.by.1.,metadata$Filled.out.by.2,metadata$Filled.out.by.3,metadata$Filled.out.by.4,metadata$Filled.out.by.5)
  #Remove NA recorders
  recs<-recorders[!is.na(recorders)]

#Coordinates
  #Latlong coords
  if(metadata$Location.type=='latlon'){
    coords<-c(as.numeric(metadata$Longitude),as.numeric(metadata$Latitude))
    spdf<-SpatialPoints(as.data.frame(t(coords)),CRS("+proj=longlat +datum=WGS84"))
 }
  #Convert coordinates if not latlong
  if(metadata$Location.type=='UTM'){
    crsutm<-as.character(paste("+proj=utm +zone=",metadata$UTM.sone.WGS.1984,sep=""))
    coordsutm<-SpatialPoints(as.data.frame(t(c(as.numeric(metadata$Easting),as.numeric(metadata$Northing)))),CRS(crsutm))
    spdf<-spTransform(coordsutm,CRS("+proj=longlat +datum=WGS84"))
  }

  dwc<-data.frame(
    #Record level
    institutionCode=rep('NTNU University Museum',times=nrow(spp_occurences)),
    datasetName=rep('Vascular plant crosslists',times=nrow(spp_occurences)),
    basisOfRecord=rep('HumanObservation',times=nrow(spp_occurences)),
    #Occurrrence
    recordedBy=rep(paste(recs,sep='|'),times=nrow(spp_occurences)),
    organismQuantity=spp_occurences$Occurence,
    organismQuantityType=rep('Description of relatie abundance',times=nrow(spp_occurences)),
    #Make GUUI for occurrence ID?
    occurrenceID=rep(paste('Crosslist',date,metadata$Lokalitet,spp_occurences$fullnames2,sep='_')),#,times=nrow(spp_occurences)),
    #Event
    eventid<-rep(paste('NTNU_University_Museum_Crosslist',date,metadata$Lokalitet,sep='_'),times=nrow(spp_occurences)),
    day=rep(metadata$Day,times=nrow(spp_occurences)),
    month=rep(metadata$Month,times=nrow(spp_occurences)),
    year=rep(metadata$Year,times=nrow(spp_occurences)),
    habitat=rep(metadata$Vegetation.type.habitat,times=nrow(spp_occurences)),
    sampleSizeValue=rep(metadata$Sampled.area,times=nrow(spp_occurences)),
    sampleSizeUnit=rep('square metre',times=nrow(spp_occurences)),
    #Location
    country=rep('Norway',times=nrow(spp_occurences)),
    county=rep(metadata$Fylke,times=nrow(spp_occurences)),
    municipality=rep(metadata$Kommune,times=nrow(spp_occurences)),
    location=rep(metadata$Lokalitet,times=nrow(spp_occurences)),
    decimalLatitude=rep(spdf@coords[,2],times=nrow(spp_occurences)),
    decimalLongitude=rep(spdf@coords[,1],times=nrow(spp_occurences)),
    coordinateUncertaintyInMeters=rep(metadata$Location.uncertainty,times=nrow(spp_occurences)),
    verbatimLocality=rep(metadata$Decription.of.locality,times=nrow(spp_occurences)),
    minimumElevationInMeters=rep(metadata$Elevation.lower,times=nrow(spp_occurences)),
    maximumElevationInMeters=rep(metadata$Elevation.upper,times=nrow(spp_occurences)),
    #Taxon
    scientificName=spp_occurences$Taxa
  )

  dfcoord<-as.data.frame(spdf@coords) 
  map<-ggmap(get_map(dfcoord,maptype='hybrid',zoom=17))+
    geom_point(aes_string(dfcoord$V1,dfcoord$V2),col='red',size=3,alpha=0.5)+
    ggtitle('Check locality provided')+theme(plot.title = element_text(lineheight=.8, face="bold"))
  mapf<-ggmap(get_map(dfcoord,maptype='hybrid',zoom=8))+
    geom_point(aes_string(dfcoord$V1,dfcoord$V2),col='red',size=3,alpha=0.5)+
    ggtitle('Check locality provided')+theme(plot.title = element_text(lineheight=.8, face="bold"))
  

  
###############################################################
#Outputs
 
output$taxalist<-renderTable(spp_occurences)  
  
  

output$metadat<-renderTable({
    df1<-data.frame(Fylke=metadata$Fylke,
                    Kommune=metadata$Kommune,
                    Lokalitet=metadata$Lokalitet,
                    Recorders=recs,
                    'Date (YYYY-MM-DD)' =as.character(date))
    df1
          })

    
output$dwc <- renderTable(dwc,digits=5)

   
output$dwc_download <- downloadHandler(
          filename =function(){paste(
            paste('Crosslist',date,metadata$Lokalitet,sep='_'),
          '.csv',sep="")},
          content=function(file){
          write.csv(dwc,file)})#Note that download button does not work in RStudio viewer. Open in browser
   
 output$map<-renderPlot({
     grid.arrange(mapf,map,ncol=2)
     })

 
 }) 
} 
  

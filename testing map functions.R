#OSM in R


#show some of the maps available
#nm <- c("osm", "maptoolkit-topo", "bing", "stamen-toner",
#        "stamen-watercolor", "esri", "esri-topo",
#        "nps", "apple-iphoto", "skobbler")
#par(mfrow=c(3,4))

# map of Korea
map <- openmap(c(43.46886761482925,119.94873046875),
               c(33.22949814144951,133.9892578125),
               minNumTiles=10,type="osm")





# map from central Norway
bb <- center_bbox(coords[1], coords[2], 150000, 150000)
bb2 <- as.vector(bb)

plot(openmap(bb2[c(2,1)], bb2[c(4,3)],
             minNumTiles=4,type="osm"))
#plot(openmap(bb2[c(2,1)], bb2[c(4,3)],
#             zoom = 10,type="osm"))
plot(openmap(bb2[c(2,1)], bb2[c(4,3)],
             minNumTiles=10,type="bing"))







# adding singe point
subscr<-data.frame(lat=coords[1],
                   lon=coords[2])


map <- openmap(c(80,-100), c(0,100))
plot(map)


coordinates(subscr)<-~lat+lon
proj4string(subscr)<-CRS("+init=epsg:4326")
points(spTransform(subscr,osm()), cex=5)





# leaflet method - very good
library( leaflet )
library( magrittr )

subscr<-data.frame(lat=c(coords[2]),
                   lon=c(coords[1]))

leaflet() %>% addTiles() %>% 
  addCircleMarkers(data = subscr,
                   lat = ~lat, lng = ~lon,
                   color = "blue")


leaflet() %>% addTiles(group = "OSM",
                       options = providerTileOptions(minZoom = 2, maxZoom = 100)) %>% 
  addCircleMarkers(data = subscr,
                   lat = ~lat, lng = ~lon,
                   color = "blue")


# also, I could check out mapView

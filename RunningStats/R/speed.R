plotSpeed <- function(tr, df)
{
  
  minlon <- tr@sp@bbox[1,1]
  minlat <- tr@sp@bbox[2,1]
  maxlon <- tr@sp@bbox[1,2]
  maxlat <- tr@sp@bbox[2,2]
  
  mapImageData <- get_map(location = c(minlon, minlat, maxlon, maxlat),
                          #color = "color", # or bw
                          source = "stamen",
                          maptype = "toner",
                          zoom = 13)
    
 plot <- ggmap(mapImageData,
        extent = "panel",
        ylab = "Latitude",
        xlab = "Longitude",
        legend = "right") 

 plot <- plot +  geom_point(aes_string(x = "lon", 
                               y = "lat",
                               color = "speed"),
                           size = 5,
                           data = df)
              

 plot <- plot + scale_colour_gradient("Speed [km/h]", low="red")
 return(plot)
}

plotSpe <- function(tr)
{
  df <- as.data.frame(tr@sp@coords)
  h <- rbind(tr@connections, c(0,0,0,0))
  df["speed"] <- h$speed * 3.6
  plotSpeed(tr, df)
}




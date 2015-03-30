require(trajectories)
require(spacetime)
require(OpenStreetMap)
require(ggplot2)
require(plotKML)
require(signal)
require(ggmap)
require(rjson)


getTr <- function (file) {
  tr <- readGPX (file)
  tr <- tr$tracks[[1]][[1]]
  crs <- CRS ("+proj=longlat")
  sp <- SpatialPoints (tr[,1:2], crs)
  t <- as.POSIXct(strptime(tr$time, "%Y-%m-%dT%H:%M:%SZ"))
  stidf <- STIDF (sp, t, data.frame (tr$ele))
  tr <- as (stidf, "Track")
  Track(tr)
}

tracklength <- function(x) {
  l <- length(x) - 1
  distance <- 0
  
  for(i in 1:l) { 
    x1 <-x[i,]
    x2 <- x[i+1,]
    gap <- (spDists(x1, x2, longlat = TRUE)) * 1000
    distance <- distance + gap
  }
  as.numeric(distance)
}

getStats <- function (tr)
{
  StartTime <- index(tr@time[1])
  EndTime <- index(tr@time[length(tr@time)])
  Date <- as.Date(StartTime)
  st <- strftime(StartTime, format="%H:%M")
  et <- strftime(EndTime, format="%H:%M")
  TrackLength <- tracklength(tr@sp)
  sub <- as.numeric(EndTime) - as.numeric(StartTime)
  Duration <- round(sub/60, 2)
  ElevationMin <- min(as.numeric(as.vector(tr@data$tr.ele)))
  ElevationMax <- max(as.numeric(as.vector(tr@data$tr.ele)))
  minkm <- round (Duration/TrackLength * 1000, 2) 
  maxSpeed <- round(max(tr@connections$speed) * 3.6, 2)
  weather <- getWeatherInfo(tr)
  
  df = data.frame(Date, st,et,TrackLength,Duration, minkm, maxSpeed)
  names(df) <- c("Date", "Start", "End", "Meters", "Minutes", "min/km", "SpeedMax[km/h]")
  if (length(weather) > 0) {
    df <- merge(df, weather)
  }    
  df
}

plotMap <- function(tr, x, y)
{
  l <- toString(round(tracklength(tr@sp)/1000,2))
  s <- as.numeric(index(tr@time[length(tr@time)])) - as.numeric(index(tr@time[1]))
  t <- round(s/60, 2)
  
  x <- x
  y <- y
  
  minlon <- tr@sp@bbox[1,1]
  minlat <- tr@sp@bbox[2,1]
  maxlon <- tr@sp@bbox[1,2]
  maxlat <- tr@sp@bbox[2,2]
  
  mapImageData <- get_map(location = c(minlon, minlat, maxlon, maxlat),
                          #color = "color", # or bw
                          source = "stamen",
                          maptype = "toner",
                          zoom = 13)
  
  pathcolor <- "#F8971F"
  
  plot <- ggmap(mapImageData,
        extent = "panel",
        ylab = "Latitude",
        xlab = "Longitude",
        legend = "right") 
  plot <- plot + geom_path(aes(x = x, 
                  y = y), 
                  colour = "black",
                  size = 2) +
    geom_path(aes(x = x, 
                  y = y),
              colour = pathcolor,
              size = 1.4) 
  plot <- plot +  labs(x = "Longitude", y = "Latitude") 
  plot <- plot +  ggtitle(paste(c(l, "km in", t, "minutes"), collapse = " "))
return(plot)
}

localMaxima <- function(x) {
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

localMinima <- function(x) {
  y <- diff(c(.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

v <- vector()
slope <- function (x,y, elevation, coords){
  l <- round(tracklength(coords[x:y]), 2)
  sl <- as.numeric(elevation[x])-as.numeric(elevation[y])
  p <- sl/l * 100
  p <- round(p * (-1), 2)
  v <- c(l, p)
  v #%
}

calculateSlope <- function (tr) {
  elevation <- as.vector(tr@data$tr.ele)
  coords <- tr@sp
    
  existingDF <- data.frame() 
  x <- 1:length(elevation)
  
  lowpass.spline <- smooth.spline(x,elevation, spar = 0.6) 
  s_values <- lowpass.spline["y"]
  values <- do.call(c, s_values) 

  minimas <- as.integer(localMinima(values))
  maximas <- as.integer(localMaxima(values))
  l_min <- length(minimas)
  l_max <- length(maximas)
  
  if (maximas[1] < minimas[1]) {
    if (maximas[l_max] > minimas[l_min])
    {
      #print("Anfang: Max, Ende: Max")
      for (i in 1:(length(maximas)-1)) {
        slope1 <- slope(maximas[i], minimas[i], elevation, coords)
        existingDF = rbind(existingDF,slope1)
        slope2 <- slope(minimas[i], maximas[i+1], elevation, coords)
        existingDF = rbind(existingDF,slope2)
      }
    }
    else {
      #print("Anfang: Max, Ende: Min")
      for (i in 1:length(maximas)) {
        slope1 <- slope(maximas[i], minimas[i], elevation, coords)
        existingDF = rbind(existingDF,slope1)
          if (i < length(maximas))
          {
            slope2 <- slope(minimas[i], maximas[i+1], elevation, coords)
            existingDF = rbind(existingDF,slope2)
          }
      }
    }
  }
  else {
    if (minimas[1] < maximas[1]) {
      if (maximas[l_max] < minimas[l_min])
      {
        #print("Anfang: Min, Ende: Min")
        for (i in 1:(length(minimas)-1)) {
          slope1 <- slope(minimas[i], maximas[i], elevation, coords)
          existingDF = rbind(existingDF,slope1)
          slope2 <- slope(maximas[i], minimas[i+1], elevation, coords)
          existingDF = rbind(existingDF,slope2)
        }
      }
      else {
        #print("Anfang: Min, Ende: Max")
        for (i in 1:length(maximas)) {
          slope1 <- slope(minimas[i], maximas[i], elevation, coords)
          existingDF = rbind(existingDF,slope1)
          if (i < length(maximas))
          {
            slope2 <- slope(maximas[i], minimas[i+1], elevation, coords)
            existingDF = rbind(existingDF,slope2)
          }
        }
      }
    }
  }
  names(existingDF) <- c("distance [metres]", "slope [%]")
  existingDF
}

##
plotEleSpeed <- function (tr){
  elevation <- as.vector(tr@data$tr.ele)
  time <- index(tr@time)
  xlab <- toString(as.Date(time[1]))
  min_e <- min(as.numeric(elevation))
  max_e <- max(as.numeric(elevation))
  
  ymin <- min(as.numeric(elevation))
  ymax <- max(as.numeric(elevation))
  
  h <- tr@connections
  med <- median(tr@connections$speed)
  h <- rbind(h, c(0, 0, med, 0))
  #speed <- h$speed
  lowpass.spline <- smooth.spline(time, h$speed, spar = 0.6) 
  s_values <- lowpass.spline["y"]
  speed <- do.call(c, s_values) 
  
  
  ylim <- c(ymin, ymax)
  
  par(mar=c(5,4,4,5)+.1)
  plot(time, elevation, ylim=ylim, type="l", col="black", lwd=3, xlab = xlab, ylab="")  
  par(new=TRUE)
  plot(time, speed, type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="", lwd=3)
  axis(4)
  mtext("Speed [m/s]", side=4, line=3, col="blue")
  mtext("Elevation [m]", side=2, line=3, col="black")
}










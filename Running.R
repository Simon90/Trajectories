require(trajectories)
require(spacetime)
require(OpenStreetMap)
require(ggplot2)
require(plotKML)
require(signal)

##############################
# - Einbindung OpenWeatherMap 
# 
# getStats (anfang, end, gesamtlänge, gesamtdauer, min-speed, max-speed, average-speed, 
#           tiefster-punkt, höchster-punkt, average-höhe, klimatische Daten)
# 
# getKmStats (anfang, end, gesamtlänge, gesamtdauer, min-speed, max-speed, average-speed, 
#             tiefster-punkt, höchster-punkt, average-höhe, klimatische Daten)
# - generalize/aggregate ? 
# 
# plotTrack (Intervall - Speed, eventuell Map)
# 
# ValidationSpeed (Ausreißer, Mittelwert, ...)
# ReplaceOutliers (Ausreißer durch Median etc. ersetzen, ...)
###############################

ddir <- "dat/2014-08-14-Running.gpx"
ddir2 <- "dat/2014-08-17-Running.gpx"

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

Tr <- getTr(ddir)
Tr2 <- getTr(ddir2)

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

v <- vector()
slope <- function (x,y, elevation, coords){
  l <- tracklength(coords[x:y])
  sl <- as.numeric(elevation[x])-as.numeric(elevation[y])
  p <- sl/l * 100
  p <- p * (-1)
  v <- c(l, p)
  v #%
}

calculateSlope <- function (elevation, coords) {
  existingDF <- data.frame()
  elevation <- as.vector(elevation) 
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
      print("Anfang: Max, Ende: Max")
      for (i in 1:(length(maximas)-1)) {
        slope1 <- slope(maximas[i], minimas[i], elevation, coords)
        existingDF = rbind(existingDF,slope1)
        slope2 <- slope(minimas[i], maximas[i+1], elevation, coords)
        existingDF = rbind(existingDF,slope2)
      }
    }
    else {
      print("Anfang: Max, Ende: Min")
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
        print("Anfang: Min, Ende: Min")
        for (i in 1:(length(minimas)-1)) {
          slope1 <- slope(minimas[i], maximas[i], elevation, coords)
          existingDF = rbind(existingDF,slope1)
          slope2 <- slope(maximas[i], minimas[i+1], elevation, coords)
          existingDF = rbind(existingDF,slope2)
        }
      }
      else {
        print("Anfang: Min, Ende: Max")
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
  existingDF
}

elevation <- as.vector(Tr@data$tr.ele)
calculateSlope(elevation,Tr@sp)





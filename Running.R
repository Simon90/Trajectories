require(trajectories)
require(spacetime)
require(OpenStreetMap)
require(ggplot2)
require(plotKML)
require(signal)

ddir <- "dat/2014-08-14-Running.gpx"

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

- Einbindung OpenWeatherMap 

getStats (anfang, end, gesamtlänge, gesamtdauer, min-speed, max-speed, average-speed, 
               tiefster-punkt, höchster-punkt, average-höhe, klimatische Daten)

getKmStats (anfang, end, gesamtlänge, gesamtdauer, min-speed, max-speed, average-speed, 
            tiefster-punkt, höchster-punkt, average-höhe, klimatische Daten)
            - generalize/aggregate ? 

plotTrack (Intervall - Speed, eventuell Map)

ValidationSpeed (Ausreißer, Mittelwert, ...)
ReplaceOutliers (Ausreißer durch Median etc. ersetzen, ...)
Smoothing !? - Höhenunterschiede ausgleichen

localMaxima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
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
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

e <- localMinima(b)
length(d)
length(e)

[1,2,3,4]
[2,2,4,5,5]
s1 <- d[1]
s2 <- e[1]

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

slope <- function (x,y, elevation, coords){
  print(x)
  print(y)
  l <- tracklength(coords[x:y])
  sl <- elevation[x]-elevation[y]
  return (sl/(l)) * 100 #%
}

calculateSlope <- function (elevation, coords) {
  vec <- vector()
  elevation <- as.vector(elevation) 
  x <- 1:length(elevation)
  
  lowpass.spline <- smooth.spline(x,elevation, spar = 0.6) 
  s_values <- lowpass.spline["y"]
  print(s_values)
  values <- do.call(c, s_values) 

  minimas <- as.integer(localMinima(values))
  maximas <- as.integer(localMaxima(values))
  print(minimas[1])
  
  if (maximas[1] < minimas[1]) {
    for (i in 1:length(maximas)-1) {
      #print(minimas[1])
      slope1 <- slope(maximas[i], minimas[i], elevation, coords)
      #slope1 <- slope(1, 172, elevation, coords)
      vec <- c(vec, slope1)
      #slope2 <- slope(minimas[i], maximas[i+1], elevation, coords)
      #vec <- c(vec, slope2)
    }
  }
  vec
}

elevation <- as.vector(Tr@data$tr.ele)
calculateSlope(elevation,Tr@sp@coords)


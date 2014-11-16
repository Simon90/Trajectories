require(trajectories)
require(spacetime)
require(maptools)
require(OpenStreetMap)
require(ggplot2)
require (rgdal)
require (maptools)
require(plotKML)
require(map3d)
require(sp)

plotEleSpeed <- function (elevation, speed, ylim){
  x <- 1:length(speed)
  par(mar=c(5,4,4,5)+.1)
  plot(x,elevation,type="l",col="red", ylim=ylim)
  par(new=TRUE)
  plot(x, speed,,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
  axis(4)
  mtext("speed",side=4,line=3)
  legend("topleft",col=c("red","blue"),lty=1,legend=c("Elevation","Speed"))
}


## Holger's Cycle Tracks

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

ddir <- "dat/gpx-tracks/"
files <- list.files (ddir)
files <- as.character (sapply (files, function (x) paste (ddir, x, sep="")))

rides1 <- list ()
for (f in files) {
  cat (f, "\n")
  rides1 <- c (rides1, getTr (f))
}

holger <- TracksCollection (list (rides1=Tracks (rides1)))

# Holger Track 1: 5 max speed values
h1data <- holger@tracksCollection$rides1@tracks$Track1
h1conn <- h1data@connections
h1conn <- rbind(h1conn, c(0, 0, 0, 0))
h1speed <- h1conn$speed
h1elevation <- as.vector(h1data@data$tr.ele)

h1 <- sort(h1data$speed)
c(h1[(length(h1)-5):length(h1)])

plotEleSpeed(h1elevation, h1speed, c(60,300))


# Holger Track 2: 5 max speed values
h2data <- holger@tracksCollection$rides1@tracks$Track2
h2conn <- h2data@connections
h2conn <- rbind(h2conn, c(0, 0, 0, 0))
h2speed <- h2conn$speed
h2elevation <- as.vector(h2data@data$tr.ele)

h2 <- sort(h2data$speed)
c(h2[(length(h2)-5):length(h2)])

plotEleSpeed(h2elevation, h2speed, c(40,115))

# Holger Track 3: 5 max speed values
h3data <- holger@tracksCollection$rides1@tracks$Track3
h3conn <- h3data@connections
h3conn <- rbind(h3conn, c(0, 0, 0, 0))
h3speed <- h3conn$speed
h3elevation <- as.vector(h3data@data$tr.ele)

h3 <- sort(h3data$speed)
c(h3[(length(h3)-5):length(h3)])

plotEleSpeed(h3elevation, h3speed, c(50,170))

# Holger Track 4: 5 max speed values
h4data <- holger@tracksCollection$rides1@tracks$Track4
h4conn <- h4data@connections
h4conn <- rbind(h4conn, c(0, 0, 0, 0))
h4speed <- h4conn$speed
h4elevation <- as.vector(h4data@data$tr.ele)

h4 <- sort(h4data$speed)
c(h4[(length(h4)-5):length(h4)])

plotEleSpeed(h4elevation, h4speed, c(45,300))

# Holger Track 5: 5 max speed values
h5data <- holger@tracksCollection$rides1@tracks$Track5
h5conn <- h5data@connections
h5conn <- rbind(h5conn, c(0, 0, 0, 0))
h5speed <- h5conn$speed
h5elevation <- as.vector(h5data@data$tr.ele)

h5 <- sort(h5data$speed)
c(h5[(length(h5)-5):length(h5)])

plotEleSpeed(h5elevation, h5speed, c(400,700))

# Holger Track 6: 5 max speed values
h6data <- holger@tracksCollection$rides1@tracks$Track6
h6conn <- h6data@connections
h6conn <- rbind(h6conn, c(0, 0, 0, 0))
h6speed <- h6conn$speed
h6elevation <- as.vector(h6data@data$tr.ele)

h6 <- sort(h6data$speed)
c(h6[(length(h6)-5):length(h6)])

plotEleSpeed(h6elevation, h6speed, c(42,110))


## Mark's Cycle Tracks

getMTr <- function (file) {
  tr <- readGPX (file)
  tr <- tr$tracks[[1]][[1]]
  crs <- CRS ("+proj=longlat")
  sp <- SpatialPoints (tr[,1:2], crs)
  t <- as.POSIXct(strptime(tr$time, "%Y-%m-%dT%H:%M:%OSZ"))
  stidf <- STIDF (sp, t, data.frame (tr$ele))
  tr <- as (stidf, "Track")
  Track(tr)
}


ddir <- "dat/gpx-tracks-mp/"
files <- list.files (ddir)
files <- as.character (sapply (files, function (x) paste (ddir, x, sep="")))
rides2 <- list ()
for (f in files) {
  cat (f, "\n")
  rides2 <- c (rides2, getMTr (f))
}

mark <- TracksCollection (list (rides2=Tracks (rides2)))

# Mark Track 1: 5 max speed values
m1data <- mark@tracksCollection$rides2@tracks$Track1
m1conn <- m1data@connections
m1conn <- rbind(m1conn, c(0, 0, 0, 0))
m1speed <- m1conn$speed
m1elevation <- as.vector(m1data@data$tr.ele)

m1 <- sort(m1data$speed)
c(m1[(length(m1)-5):length(m1)])

plotEleSpeed(m1elevation, m1speed, c(55,80))

# Mark Track 2: 5 max speed values
m2data <- mark@tracksCollection$rides2@tracks$Track2
m2conn <- m2data@connections
m2conn <- rbind(m2conn, c(0, 0, 0, 0))
m2speed <- m2conn$speed
m2elevation <- as.vector(m2data@data$tr.ele)

m2 <- sort(m2data$speed)
c(m2[(length(m2)-5):length(m2)])

plotEleSpeed(m2elevation, m2speed, c(55,80))

# Mark Track 3: 5 max speed values
m3data <- mark@tracksCollection$rides2@tracks$Track3
m3conn <- m3data@connections
m3conn <- rbind(m3conn, c(0, 0, 0, 0))
m3speed <- m3conn$speed
m3elevation <- as.vector(m3data@data$tr.ele)

m3 <- sort(m3data$speed)
c(m3[(length(m3)-5):length(m3)])

plotEleSpeed(m3elevation, m3speed, c(59,190))


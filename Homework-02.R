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
h1 <- sort(holger@tracksCollection$rides1@tracks$Track1$speed)
c(h1[(length(h1)-5):length(h1)])

# Holger Track 2: 5 max speed values
h2 <- sort(holger@tracksCollection$rides1@tracks$Track2$speed)
c(h2[(length(h2)-5):length(h2)])

# Holger Track 3: 5 max speed values
h3 <- sort(holger@tracksCollection$rides1@tracks$Track3$speed)
c(h3[(length(h3)-5):length(h3)])

# Holger Track 4: 5 max speed values
h4 <- sort(holger@tracksCollection$rides1@tracks$Track4$speed)
c(h4[(length(h4)-5):length(h4)])

# Holger Track 5: 5 max speed values
h5 <- sort(holger@tracksCollection$rides1@tracks$Track5$speed)
c(h5[(length(h5)-5):length(h5)])

# Holger Track 6: 5 max speed values
h6 <- sort(holger@tracksCollection$rides1@tracks$Track6$speed)
c(h6[(length(h6)-5):length(h6)])


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
m1 <- sort(mark@tracksCollection$rides2@tracks$Track1$speed)
c(m1[(length(m1)-5):length(m1)])

# Mark Track 2: 5 max speed values
m2 <- sort(mark@tracksCollection$rides2@tracks$Track2$speed)
c(m2[(length(m2)-5):length(m2)])

# Mark Track 3: 5 max speed values
m3 <- sort(mark@tracksCollection$rides2@tracks$Track3$speed)
c(m3[(length(m3)-5):length(m3)])


require(trajectories)
require(spacetime)
require(OpenStreetMap)
require(ggplot2)
require(plotKML)
require(signal)
require(ggmap)
require(rjson)


## Import
ddir1 <- "dat/2014-08-14-Running.gpx"
ddir2 <- "dat/2015-02-01-Running.gpx"

Tr1 <- getTr(ddir1)
Tr2 <- getTr(ddir2)

## GetStats
getStats(Tr1)
getStats(Tr2)

## Plot with Basic Map
df <- as.data.frame(Tr2@sp@coords)
x <- df[,1]
y <- df[,2]
plotMap(Tr2, x, y)

df <- as.data.frame(Tr1@sp@coords)
x <- df[,1]
y <- df[,2]
plotMap(Tr1, x, y)

## Plot Elevation with Speed 
plotEleSpeed (Tr1)
plotEleSpeed (Tr2)

## Slope
calculateSlope(Tr1)
calculateSlope(Tr2)

## Speed
plotSpeed(Tr2)



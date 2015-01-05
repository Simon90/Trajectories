#require(trajectories)
#require(spacetime)
#require(OpenStreetMap)
#require(ggplot2)
#require(plotKML)

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
Tr

- Einbindung OpenWeatherMap 

getStats (anfang, end, gesamtlänge, gesamtdauer, min-speed, max-speed, average-speed, 
               tiefster-punkt, höchster-punkt, average-höhe, klimatische Daten)

getKmStats (anfang, end, gesamtlänge, gesamtdauer, min-speed, max-speed, average-speed, 
            tiefster-punkt, höchster-punkt, average-höhe, klimatische Daten)
            - generalize/aggregate ? 

plotTrack (Intervall - Speed, eventuell Map)

ValidationSpeed (Ausreißer, Mittelwert, ...)
ReplaceOutliers (Ausreißer durch Median etc. ersetzen, ...)


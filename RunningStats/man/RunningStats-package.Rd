\name{RunningStats-package}
\alias{RunningStats-package}
\alias{RunningStats}
\docType{package}
\title{
Plotting Running Tracks and Statistics
}
\description{
This package provides statistics for running tracks in the GPX-Format. It provides plot functions for maps and elevation profiles and accesses the OpenWeatherMap API for historical weather data.

}
\details{
\tabular{ll}{
Package: \tab RunningStats\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-03-30\cr
License: \tab GPL (>= 2)\cr
}

}
\author{
Simon Schoemaker <simon.schoemaker@googlemail.com>
 Daniel Schumacher <daniel.schumacher@wwu.de>

Maintainer: Simon Schoemaker <simon.schoemaker@googlemail.com>
}
\keyword{ trajectories }
\seealso{
  \code{\link[openweathermap.org]{}} 
}
\examples{
\dontrun{
  tr <- getTr(file)
  df <- as.data.frame(Tr2@sp@coords)
  x <- df[,1]
  y <- df[,2]
  plotMap(tr, x, y)
  getStats(tr)
  plotEleSpeed(tr)
  }  
}

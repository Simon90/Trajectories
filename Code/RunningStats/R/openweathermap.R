queryWeatherData <- function(id, time){
  url <- paste("http://api.openweathermap.org/data/2.5/history/city?id=",id,"&type=hour&start=",time,"&cnt=1&units=metric", sep= "")
  data <- fromJSON(readLines(url))
  data
}

getMainInfo <- function(data){
  main <- data$list[[1]]$main
  main["temp"] = as.numeric(main["temp"]) - 273.15
  main["temp_min"] = as.numeric(main["temp_min"]) - 273.15
  main["temp_max"] = as.numeric(main["temp_max"]) - 273.15
  main
}

getValue <- function(data, attr){
  x <- data[attr]
  x <- unname(x)
  x
}
  
getCityId <- function(lat, lon){
  url <- paste("http://api.openweathermap.org/data/2.5/find?lat=",lat,"&lon=",lon,"&cnt=1", sep="")
  data <- fromJSON(readLines(url))
  id <- (data$list[[1]]$id)
  id
}

#' Get Weather Information
#' @description
#' The function queries weather data for the Track from OpenWeatherMap.org
#' @param Track-class
#' @return DataFrame
#' @examples
#' \dontrun{
#' getWeatherInfo(track)
#' }
getWeatherInfo <- function (track){
  df = data.frame()
  lat <- track@sp@coords[1,]["lat"]
  lon <- track@sp@coords[1,]["lon"]
  time <- as.numeric(index(track@time[length(track@time)]))
  id <- getCityId(lat, lon)
  data <- queryWeatherData(id, time)
  if (length(data$list) < 1) {
    stop ('It is only possible to query data from OpenWeatherMap for the past 30 days')
  }
  main <- getMainInfo(data)
  temp <- getValue(main, "temp")
  humidity <- getValue(main, "humidity")
  pressure <- getValue(main, "pressure")
  windspeed <- getValue(data$list[[1]]$wind, "speed")
  weather_description <- data$list[[1]]$weather[[1]]$description
  df = data.frame(temp, humidity, weather_description, windspeed, pressure)
  names(df) <- c("Temperature[?C]", "Humidity[%]", "Weather-Description", "Windspeed[m/s]", "Pressure[hPa]")
  df  
}








queryWeatherData <- function(id, time){
  url <- paste("http://api.openweathermap.org/data/2.5/history/city?id=",id,"&type=hour&start=",time,"&cnt=1&units=metric", collapse = "")
  data_str <- paste(url, collapse = "")
  data <- fromJSON(data_str)
  data
}

getMainInfo <- function(data){
  main <- data$list[[1]]$main
  main["temp"] = main["temp"] - 273.15
  main["temp_min"] = main["temp_min"] - 273.15
  main["temp_max"] = main["temp_max"] - 273.15
  main
}

getValue <- function(data, attr){
  x <- data[attr]
  x <- unname(x)
  x
}
  
getCityId <- function(lat, lon){
  url <- paste("http://api.openweathermap.org/data/2.5/find?lat=", lat, "&lon=",-2.15, "&cnt=1", collapse = "")
  data <- fromJSON(url)
  id <- (data$list[[1]]$id)
  id
}

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
  names(df) <- c("Temperature[°C]", "Humidity[%]", "Weather-Description", "Windspeed[m/s]", "Pressure[hPa]")
  df  
}






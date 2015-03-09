require(RJSONIO)

getData <- function(id, time){
  url <- paste("http://api.openweathermap.org/data/2.5/history/city?id=",id,"&type=hour&start=",time,"&cnt=1&units=metric", collapse = "")
  data_str <- paste(url, collapse = "")
  data <- fromJSON(data_str)
  data
}

getMainInfo <- function(data){
  main <- data$list[[2]]$main
  main
}

getValueFromMain <- function(main, type){
  x <- main[type]
  x <- unname(x)
  x
}
  
getCityId <- function(lat, lon){
  url <- paste("http://api.openweathermap.org/data/2.5/find?lat=", lat, "&lon=",-2.15, "&cnt=1", collapse = "")
  data <- fromJSON(url)
  id <- (data$list[[1]]$id)
  id
}

getClimaticInfo <- function (lat, lon, time, att){
  id <- getCityId(lat, lon)
  data <- getData(id, time)
  main <- getMainInfo(data)
  getValueFromMain(main, att)
}

getClimaticInfo(51,7, 1369728000, "humidity")
getClimaticInfo(51,7, 1369728000, "temp")



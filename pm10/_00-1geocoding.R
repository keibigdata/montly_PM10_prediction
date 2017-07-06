library(RCurl)
library(RJSONIO)
setwd("D:/01 Study/08 pm/data/대기오염물질")
addressInfo <- read.csv("monitoring_station_csv.csv", stringsAsFactors=FALSE)
vecAddr <- addressInfo$ADDRESS

dfCoord <- data.frame()
dfFail <- data.frame()

getCoor <- function(addr){
  url <- paste('https://maps.googleapis.com/maps/api/geocode/json?address=',addr,'&key=AIzaSyALGG7xOXPZ7iO6BiWLm0b890gV7aMGVNQ',sep="")
  web <- getURL(url)
  raw <-fromJSON(web)
  if(raw$status=="OK"){
    new <- data.frame(NAME=name, ADDRESS=addr,LAT=raw$results[[1]]$geometry$location[[1]], LNG=raw$results[[1]]$geometry$location[[2]])
    dfCoord <<- rbind(dfCoord, new)
  } else {
    print(name)
    new <- data.frame(NAME=name, ADDRESS=addr)
    dfFail <<- rbind(dfFail, new)
    # print(name)
    # getCoor(cutAddr(addr))
  }
}

for(inx in 1:length(vecAddr)){
  addr <- vecAddr[inx]
  if(addr!=""){
    name <- addressInfo[inx,"NAME"]
    newAddr <- gsub(" ", "+",addr)
    getCoor(newAddr)  
  }
}

write.csv(dfCoord, "geocoding.csv")
write.csv(dfFail, "fail.csv")
library(ncdf4)
library(chron)
library(lubridate)
library(dplyr)
library(lattice)
library(ProgGUIinR)
library(Hmisc)
library(parallel)
library(stringr)

cluster <- makeCluster(getOption("cl,cores",4))
cluster

dir <- paste("D:\\Data\\USA\\Tropomi\\Ozone\\Ozone_Column\\")

file <- sort(list.files(dir))

tmp <- NULL
mp <- 8
j <- 1

for (i in 1:length(file)){
  raw <- nc_open(paste(dir,file[i],sep=""))
  lon_t <- ncvar_get(raw,"PRODUCT/longitude")
  lat_t <- ncvar_get(raw,"PRODUCT/latitude")
  m <- as.numeric(str_sub(paste(file[i]),25,26))
  o3_t <- ncvar_get(raw,"PRODUCT/ozone_total_vertical_column")
  
  longitude <- as.vector(lon_t)
  latitude <- as.vector(lat_t)
  st_o3 <- as.vector(o3_t)
  
  month <- rep(m,length(lat_t))
  
  tmp0 <- cbind.data.frame(longitude,latitude,st_o3)
  tmp1 <- filter(tmp0,longitude<(-83),longitude>(-85.5),latitude<34.75,latitude>32.75)
  tmp <- rbind(tmp1,tmp)
  
  if(m!=mp){
    tmp <- na.omit(tmp)
    write.csv(tmp,paste("T:\\eohprojs\\CDC_climatechange\\Wenhao\\LIB\\Atlanta\\","Atlanta_Monthly_",mp,".csv",sep=""),row.names=F)
    tmp <- NULL
    print(j)
  }
  mp <- m 
  print(i)
}


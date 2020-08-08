library(dplyr)
library(ncdf4)

setwd('/aqua/Wenhao')

year <- 2018
month <- "05"
path <- "/aqua/Wenhao/HRRR_vertical/nc/"
#define day as i in the paramether
for (i in 1:31){
  day <- sprintf("%02d",i)
  print(day)
  daily <- NULL
  for (j in 18:23){
    hour <- sprintf("%02d",j)
    file <- paste(path,year,"_",month,"/",year,month,day,"/hrrr.t",hour,"z.wrfsfcf00.nc",sep="")
    tmp <- nc_open(file)
    temp <- nc_open(file)
    lat <- as.vector(ncvar_get(temp,"gridlat_0"))
    lon <- as.vector(ncvar_get(temp,"gridlon_0"))
    coor <- as.data.frame(cbind(1:length(lat),lat,lon))
    coor_CA <- coor[lat>32,]
    coor_CA <- coor_CA[lat<43,]
    coor_CA <- coor_CA[lon>-126,]
    coor_CA <- coor_CA[lon<(-113),]
    num <- coor_CA$V1
    long <- lon[num]
    lati <- lat[num]
    maxuvv <- as.vector(ncvar_get(temp,"MAXUVV_P8_2L108_GLC0_max"))[num]
    maxdvv <- as.vector(ncvar_get(temp,"MAXDVV_P8_2L108_GLC0_max"))[num]
    dzdt <- as.vector(ncvar_get(temp,"DZDT_P8_2L104_GLC0_avg"))[num]
    mxuphl <- as.vector(ncvar_get(temp,"MXUPHL_P8_2L103_GLC0_max"))[num]
    ltng <- as.vector(ncvar_get(temp,"LTNG_P0_L10_GLC0"))[num]

    result <- data.frame(lati,long,maxuvv,maxdvv,dzdt,mxuphl,ltng)
    
    write.csv(result,paste("/aqua/Wenhao/HRRR_vertical/csv/",year,month,day,"/CA_hour",hour,".csv",sep=""))
    print(j)
  }qlo
  daily <- rbind(daily,result)
  print(i)
}

year <- 2018
month <- "05"
path <- "/aqua/Wenhao/HRRR_vertical/nc/"
#define day as i in the paramether
for (i in 1:31){
  day <- sprintf("%02d",i)
  print(day)
  daily <- NULL
  for (j in 18:23){
    hour <- sprintf("%02d",j)
    file <- paste(path,year,"_",month,"/",year,month,day,"/hrrr.t",hour,"z.wrfsfcf00.nc",sep="")
    tmp <- nc_open(file)
    temp <- nc_open(file)
    lat <- as.vector(ncvar_get(temp,"gridlat_0"))
    lon <- as.vector(ncvar_get(temp,"gridlon_0"))
    coor <- as.data.frame(cbind(1:length(lat),lat,lon))
    coor_CA <- coor[lat>32,]
    coor_CA <- coor_CA[lat<43,]
    coor_CA <- coor_CA[lon>-126,]
    coor_CA <- coor_CA[lon<(-113),]
    num <- coor_CA$V1
    long <- lon[num]
    lati <- lat[num]
    maxuvv <- as.vector(ncvar_get(temp,"MAXUVV_P8_2L108_GLC0_max"))[num]
    maxdvv <- as.vector(ncvar_get(temp,"MAXDVV_P8_2L108_GLC0_max"))[num]
    dzdt <- as.vector(ncvar_get(temp,"DZDT_P8_2L104_GLC0_avg"))[num]
    mxuphl <- as.vector(ncvar_get(temp,"MXUPHL_P8_2L103_GLC0_max"))[num]
    ltng <- as.vector(ncvar_get(temp,"LTNG_P0_L10_GLC0"))[num]

    result <- data.frame(lati,long,maxuvv,maxdvv,dzdt,mxuphl,ltng)
    assign(paste(hour,j,sep=""),result)
    write.csv(result,paste("/aqua/Wenhao/HRRR_vertical/csv/",year,month,day,"/CA_hour",hour,".csv",sep=""))
  }
  daily <- rbind(daily,result)
  print(i)
}


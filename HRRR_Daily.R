library(dplyr)
library(chron)

dir <- paste("D:\\hrrr_csv\\")

year <- 2018
month <- 11

for (i in 1:30){
  date <- paste(dir,year,sprintf("%02d",month),sprintf("%02d",i),"\\",sep="")
  for (j in 10:15){
    assign(paste("h",j,sep=""),read.csv(paste(date,"CA_hour",j+8,".csv",sep="")))
    print(j)
  }
  latitude <- h10[,1]
  longitude <- h10[,2]
  result <- data.frame(latitude,longitude)
  for (j in 3:length(colnames(h10))){
    variable <- colnames(h10)[j]
    mean <- (h10[,j]+h11[,j]+h12[,j]+h13[,j]+h14[,j]+h15[,j])/6
    assign(paste(variable),mean)
    result <- cbind.data.frame(result,get(paste(variable)))
    print(j)
  }
  colnames(result)<-colnames(h10)
  write.csv(result,paste("D:\\hrrr_daily\\2018",sprintf("%02d",month),sprintf("%02d",i),".csv",sep=""),row.names = F)
}


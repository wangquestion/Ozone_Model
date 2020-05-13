require(plyr)
require(dplyr)
require(randomForest)
require(stringr)
require(foreign)
require(xlsx)
require(readxl)
require(h2o)
require(ggplot2)
require(pls)
library(nlme)
library(lme4)
library(randomForest)
library(pls)
library(colorRamps)
library(aqfig)
library(foreign)
library(reshape2)
library(grid)
library(plyr)
library(Rmisc)
####Database combine and then run formally
pre.ground <- read.csv("/Volumes/Wenhao_eSSD 2/Pre_analysis/ground.csv")
pre.ID.match <- read.csv("/Volumes/Wenhao_eSSD 2/Pre_analysis/GridID_Ground_Match_Count1.csv")
Grid_ID <- 1:nrow(pre.ground)

for (i in 1:nrow(pre.ID.match)){
  Grid_ID[pre.ground$SiteNumber==pre.ID.match$station_ID[i]] <- pre.ID.match$TARGET_FID[i]
  print(i)
}

ready.ground <- data.frame(Grid_ID,pre.ground)
ready.ground$Date <- as.Date(ready.ground$Date)

###HRRR
pre.HRRR <- read.csv("/Volumes/Wenhao_eSSD 2/Pre_analysis/HRRR_Gridin.csv")
ready.HRRR <- pre.HRRR
colnames(ready.HRRR)[2] <- "Grid_ID"
ready.HRRR$Date <- as.Date(pre.HRRR$Date)

###Tropomi
pre.ozone <- read.csv("/Volumes/Wenhao_eSSD 2/Pre_analysis/ozone_column.csv")
pre.no2 <- read.csv("/Volumes/Wenhao_eSSD 2/Pre_analysis/trops_NO2.csv")
ready.ozone <- pre.ozone
ready.no2 <- pre.no2
ready.ozone$Date <- as.Date(ready.ozone$Date)
ready.no2$Date <- as.Date(ready.no2$Date)
pre.tropomi <- merge(ready.ozone,ready.no2,by=c("ID","Date"))
ready.tropomi<- pre.tropomi
colnames(ready.tropomi)[c(1,3,4)] <- c("Grid_ID","TROPOMI_O3","TROPOMI_NO2")

###Population
Y2015 <- read_excel("/Volumes/Wenhao_eSSD 2/LandUSE2/Population_Count/Population_2015.xlsx")
Y2015 <- Y2015[,c(2,5)]
Y2020 <- read_excel("/Volumes/Wenhao_eSSD 2/LandUSE2/Population_Count/Population_2020.xlsx")
Y2020 <- Y2020[,c(2,10)]
colnames(Y2015) <- c("Grid_ID","population")
colnames(Y2020) <- c("Grid_ID","population")
population <- Y2020$population-((Y2020$population-Y2015$population)/5)
Grid_ID <- Y2015$Grid_ID
ready.population <- data.frame(Grid_ID,population) 

###Land Use
pre.land.use <- read.dbf("/Volumes/Wenhao_eSSD 2/LandUSE2/dbf/fishnet_10km_CA_SUM2.dbf")
pre.land.use <- pre.land.use[,-c(1,3,13,14)]
colnames(pre.land.use)[1] <- "Grid_ID"
pre.bind.land.use <- merge(ready.population,pre.land.use,by=("Grid_ID"))

ready.bind.land.use <-NULL

#####Mulitiple the landuse by date
str_date <- as.Date("2018-05-01")
end_date <- as.Date("2019-05-01")
date_series <- as.Date(str_date:end_date,origin="1970-01-01")
for ( i in 1:length(date_series)){
  Date <- rep(date_series[i],nrow(pre.bind.land.use))
  temp <- cbind(pre.bind.land.use,Date)
  ready.bind.land.use <- rbind.data.frame(ready.bind.land.use,temp) 
  print(date_series[i])
}

ready.bind.land.use$Date <- as.Date(ready.bind.land.use$Date)
colnames(ready.bind.land.use) <- c("Grid_ID","population","elevation","NLCD_water","NLCD_developed",
                                   "NLCD_barren","NLCD_forest","NLCD_shrubland","NLCD_herbaceous",
                                   "NLCD_cultivation","NLCD_wetland","Date")

###Predictors
full.dataset <- merge(ready.bind.land.use,merge(ready.HRRR,ready.tropomi,by=c("Date","Grid_ID")),by=c("Date","Grid_ID")) 
summary(full.dataset)
pre.dataset <- full.dataset
pre.dataset <- pre.dataset[,-c(69,70)]

###Dataset standardization
list <-  1:length(colnames(pre.dataset))
list <- list[-c(1,2)]
for(i in list){
  col <- pre.dataset[,i]
  mean <- mean(na.omit(col))
  std <- sd(na.omit(col))
  pre.dataset[,i] <- (col-mean)/std
  print(i)
}

for(i in list){
  print(mean(na.omit(pre.dataset[,i])))
  print(i)
}
summary(pre.dataset)
class(pre.dataset$Date)
ready.dataset <- left_join(pre.dataset,ready.ground,by=c("Date","Grid_ID"))
ready.training.set.MD8 <- ready.dataset[!is.na(ready.dataset$MD8),]
ready.training.set.AVG <- ready.dataset[!is.na(ready.dataset$Daily_average),]

cor(ready.training.set.AVG$MD8,ready.training.set.AVG$Daily_average)

###Modeling
rf.md8.10.1000 <- randomForest(formula = MD8~gust+ugrd_10m+vgrd_10m+vgrd_250+tmp
                               +ugrd_250+pres+hgt+mstav+spfh+rh+sfcr+gflux+vgtvp
                               +cape+cin+mcdc+vddsf+hpbl+population+elevation+NLCD_water
                               +NLCD_developed+NLCD_barren+NLCD_forest
                               +TROPOMI_O3+TROPOMI_NO2
                               ,data=ready.training.set.MD8,mtry=10,ntree=1000,importance=T)

rf.avg.10.1000 <- randomForest(formula = Daily_average~gust+ugrd_10m+vgrd_10m+vgrd_250+tmp
                               +ugrd_250+pres+hgt+mstav+spfh+rh+sfcr+gflux+vgtvp
                               +cape+cin+mcdc+vddsf+hpbl+population+elevation+NLCD_water
                               +NLCD_developed+NLCD_barren+NLCD_forest
                               +TROPOMI_O3+TROPOMI_NO2
                               ,data=ready.training.set.AVG,mtry=10,ntree=1000,importance=T)

varImpPlot(rf.md8.10.1000,type=1,main = "Random Forest Importance Rank")
varImpPlot(rf.avg.10.1000)
?varImpPlot
reg.md8 <- data.frame(rf.md8.10.1000$predicted,rf.md8.10.1000$y)
colnames(reg.md8) <- c("Prediction","Ground_Monitor")
scatter.md8 <- ggplot(reg.md8,aes(x=Prediction,y=Ground_Monitor))+geom_point(shape=19,color="grey")+geom_smooth(method=lm)

reg.avg <- data.frame(rf.avg.10.1000$predicted,rf.avg.10.1000$y)
colnames(reg.avg) <- c("Prediction","Ground_Monitor")
scatter.avg <- ggplot(reg.avg,aes(x=Prediction,y=Ground_Monitor))+geom_point(shape=19,color="grey")+geom_smooth(method=lm)

lm.md8.10.100 <- lm(Ground_Monitor~Prediction,reg.md8)
lm.avg.10.100 <- lm(Ground_Monitor~Prediction,reg.avg)

result.md8 <- predict(rf.md8.10.1000,newdata = ready.dataset)
estimate.md8 <- cbind.data.frame(ready.dataset$Date,ready.dataset$Grid_ID,result.md8)
colnames(estimate.md8) <- c("Date","Grid_ID","estimate.md8")
estimate.md8.by.date <- group_by(estimate.md8,Date)
daily.time.series.estimate <- summarise(estimate.md8.by.date,daily.mean.=mean(estimate.md8))

ground.by.date <- group_by(ready.ground,Date)
daily.time.series.ground <- summarise(ground.by.date,ground.md8.daily.mean = mean(MD8))
daily.time.series <- merge(daily.time.series.estimate,daily.time.series.ground,by="Date")
colnames(daily.time.series) <- c("Date","Modeling","Ground Monitors")

daily.time.series.reshape <- melt(daily.time.series,id="Date")
daily.time.series.reshape$value <- daily.time.series.reshape$value*1E3

colnames(daily.time.series.reshape)[2] <- "Method"


p <- ggplot(data=daily.time.series.reshape,aes(x=Date,y=value,colour=Method))+geom_line()
p + ylab("Average of MD8 (ppb)") + theme_set(theme_bw()) + theme(panel.grid.major=element_line(colour=NA))
cor.test(daily.time.series$Ground,daily.time.series$Modeling)

result.md8.ground <- merge(estimate.md8,ready.ground,by=c("Date","Grid_ID"))[,c(1,3,6)]
result.md8.summarise <- summarise(group_by(result.md8.ground,Date),Estimate = mean (estimate.md8) ,Ground = mean(MD8))
result.md8.ground.reshaped <- melt(result.md8.summarise,id="Date")
plot.md8.ground.stationwide <- ggplot(data=result.md8.ground.reshaped,aes(x=Date,y=value,colour=variable)) + geom_line() + ylab("Average of MD8 (ppb)")
cor.test(result.md8.summarise$Estimate,result.md8.summarise$Ground)

####Estimating map creating "estimate.md8"
estimate.md8.by.grid <- group_by(estimate.md8,Grid_ID)
estimate.md8.by.grid.yearly.mean <- summarise(estimate.md8.by.grid,yearly.mean.md8 = mean (estimate.md8))
write.csv(estimate.md8.by.grid.yearly.mean,"/Volumes/Wenhao_eSSD 2/Estimate/estimate_yearly_mean_md8.csv",row.names = F)

####Do the estimate by season
estimate.md8.ozone.season <- estimate.md8[estimate.md8$Date<"2018-11-01",]
estimate.md8.ozone.season.by.grid <- group_by(estimate.md8.ozone.season,Grid_ID)
estimate.md8.ozone.season.mean.by.grid <- summarise(estimate.md8.ozone.season.by.grid,season.mean.md8 = mean (estimate.md8))

estimate.md8.non.ozone.season <- estimate.md8[estimate.md8$Date>="2018-11-01",]
estimate.md8.non.ozone.season.by.grid <- group_by(estimate.md8.non.ozone.season,Grid_ID)
estimate.md8.non.ozone.season.mean.by.grid <- summarise(estimate.md8.non.ozone.season.by.grid,mean.md8 = mean (estimate.md8))

write.csv(estimate.md8.ozone.season.mean.by.grid,"/Volumes/Wenhao_eSSD 2/Estimate/estimate_ozone_season_mean_md8.csv",row.names = F)
write.csv(estimate.md8.non.ozone.season.mean.by.grid,"/Volumes/Wenhao_eSSD 2/Estimate/estimate_non_ozone_season_mean_md8.csv",row.names = F)

##CV calculation
#####10-fold CV
CV_segmnets <- cvsegments(nrow(ready.training.set.MD8),10)
num <- 1:nrow(ready.training.set.MD8)
training.set.num <- cbind(num,ready.training.set.MD8)

result <- NULL

for(i in 1:10){
  testing <- training.set.num[CV_segmnets[i]$V,]
  training <- training.set.num[!(training.set.num$num %in% testing$num),]
  
  rf.temp <- randomForest(formula = MD8~gust+ugrd_10m+vgrd_10m+vgrd_250+tmp
                          +ugrd_250+pres+hgt+mstav+spfh+rh+sfcr+gflux+vgtvp
                          +cape+cin+mcdc+vddsf+hpbl+population+elevation+NLCD_water
                          +NLCD_developed+NLCD_barren+NLCD_forest
                          +TROPOMI_O3+TROPOMI_NO2
                          ,data=training,mtry=10,ntree=1000,importance=T)
  print(Sys.time())
  temp.predict <- predict(rf.temp,testing)
  temp.i <- data.frame(testing,predict=temp.predict)
  result <- rbind(result,temp.i)
  print(i)
}

result$MD8 <- 1E3*result$MD8 
result$predict <- 1E3*result$predict

lm.10.fold <-lm(result$MD8~result$predict)
summary(lm.10.fold)

sactter.10.fold <- ggplot(result,aes(x=predict,y=MD8))+geom_point(shape=19,color="grey")+geom_smooth(method="lm")

###Temporal CV
str_date <- as.Date("2018-05-01")
end_date <- as.Date("2019-04-30")
date_series <- as.Date(str_date:end_date,origin="1970-01-01")
available_number <- 1:length(date_series)

Temporal.CV_segment <- cvsegments(length(date_series),10)
temporal.result <- c()

for (i in 1:10){
  day.testing <- date_series[Temporal.CV_segment[i]$V]
  day.training <- date_series[!(date_series %in% day.testing)]
  testing <- ready.training.set.MD8[ready.training.set.MD8$Date %in% day.testing,]
  training <- ready.training.set.MD8[ready.training.set.MD8$Date %in% day.training,]
  
  rf.temp <- randomForest(formula = MD8~gust+ugrd_10m+vgrd_10m+vgrd_250+tmp
                          +ugrd_250+pres+hgt+mstav+spfh+rh+sfcr+gflux+vgtvp
                          +cape+cin+mcdc+vddsf+hpbl+population+elevation+NLCD_water
                          +NLCD_developed+NLCD_barren+NLCD_forest
                          +TROPOMI_O3+TROPOMI_NO2
                          ,data=training,mtry=10,ntree=1000,importance=T)
  
  print(Sys.time())
  temp.predict <- predict(rf.temp,testing)
  temp.i <- data.frame(testing,predict=temp.predict)
  temporal.result <- rbind(temporal.result,temp.i)
  print(i)
}

temporal.result$MD8 <- 1E3*temporal.result$MD8 
temporal.result$predict <- 1E3*temporal.result$predict

lm.temporal <-lm(temporal.result$MD8~temporal.result$predict)
summary(lm.temporal)

sactter.temporal <- ggplot(temporal.result,aes(x=predict,y=MD8))+geom_point(shape=19,color="grey")+geom_smooth(method="lm")
sactter.temporal

####Spatial CV
grid.list <-  pre.ID.match$TARGET_FID
spatial.CV_segment <- cvsegments(length(grid.list),10)
spatial.result <- c()

for (i in 1:10){
  grid.testing <- grid.list[spatial.CV_segment[i]$V]
  grid.training <- grid.list[!(grid.list %in% grid.testing )]
  testing <- ready.training.set.MD8[ready.training.set.MD8$Grid_ID %in% grid.testing,]
  training <- ready.training.set.MD8[ready.training.set.MD8$Grid_ID %in% grid.training,]
  
  rf.temp <- randomForest(formula = MD8~gust+ugrd_10m+vgrd_10m+vgrd_250+tmp
                          +ugrd_250+pres+hgt+mstav+spfh+rh+sfcr+gflux+vgtvp
                          +cape+cin+mcdc+vddsf+hpbl+population+elevation+NLCD_water
                          +NLCD_developed+NLCD_barren+NLCD_forest
                          +TROPOMI_O3+TROPOMI_NO2
                          ,data=training,mtry=10,ntree=1000,importance=T)
  
  print(Sys.time())
  temp.predict <- predict(rf.temp,testing)
  temp.i <- data.frame(testing,predict=temp.predict)
  spatial.result <- rbind(spatial.result,temp.i)
  print(i)
}

spatial.result$MD8 <- 1E3*spatial.result$MD8 
spatial.result$predict <- 1E3*spatial.result$predict

lm.spatial <-lm(spatial.result$MD8~spatial.result$predict)
summary(lm.spatial)

lm.spatial <-plot(spatial.result$predict,spatial.result$MD8)

sactter.spatial<- ggplot(spatial.result,aes(x=predict,y=MD8))+geom_point(shape=19,color="grey")+geom_smooth(method="lm")


###Analyse the reason in low spatial R-square
correlation <- 1:length(grid.list)
for (i in 1:length(grid.list)){
  print(i)
  temp.set <- spatial.result[spatial.result$Grid_ID==grid.list[i],]
  correlation[i] <- cor(temp.set$predict,temp.set$MD8)
}

cor.by.grid <- data.frame(grid.list,correlation)

R_under_50 <- cor.by.grid[cor.by.grid$correlation<=0.5,]

grid.cor <- read.csv("/Volumes/Wenhao_eSSD 2/Fishnet.csv")

under50.grid.cor <- grid.cor[grid.cor$OID %in% R_under_50$grid.list,]

write.csv(under50.grid.cor,"/Volumes/Wenhao_eSSD 2/grid_under_50.csv")

###Estimate By Season
#meteorologyic Summer
estimate.m.summer <- estimate.md8[(estimate.md8$Date>="2018-06-01") & (estimate.md8$Date<"2018-09-01"),]
estimate.m.summer.by.grid <- group_by(estimate.m.summer,Grid_ID)
estimate.m.summer.mean.by.grid <- summarise(estimate.m.summer.by.grid,season.mean.md8 = mean (estimate.md8))
write.csv(estimate.m.summer.mean.by.grid ,"/Volumes/Wenhao_eSSD 2/Estimate/estimate_summer_mean_md8.csv",row.names = F)

estimate.m.fall <- estimate.md8[(estimate.md8$Date>="2018-09-01") & (estimate.md8$Date<"2018-12-01"),]
estimate.m.fall.by.grid <- group_by(estimate.m.fall,Grid_ID)
estimate.m.fall.mean.by.grid <- summarise(estimate.m.fall.by.grid,season.mean.md8 = mean (estimate.md8))
write.csv(estimate.m.fall.mean.by.grid ,"/Volumes/Wenhao_eSSD 2/Estimate/estimate_fall_mean_md8.csv",row.names = F)

estimate.m.winter <- estimate.md8[(estimate.md8$Date>="2018-12-01") & (estimate.md8$Date<"2019-03-01"),]
estimate.m.winter.by.grid <- group_by(estimate.m.winter,Grid_ID)
estimate.m.winter.mean.by.grid <- summarise(estimate.m.winter.by.grid,season.mean.md8 = mean (estimate.md8))
write.csv(estimate.m.winter.mean.by.grid ,"/Volumes/Wenhao_eSSD 2/Estimate/estimate_winter_mean_md8.csv",row.names = F)

estimate.m.spring <- estimate.md8[(estimate.md8$Date>="2019-03-01") & (estimate.md8$Date<"2019-06-01"),]
estimate.m.spring.by.grid <- group_by(estimate.m.spring,Grid_ID)
estimate.m.spring.mean.by.grid <- summarise(estimate.m.spring.by.grid,season.mean.md8 = mean (estimate.md8))
write.csv(estimate.m.spring.mean.by.grid ,"/Volumes/Wenhao_eSSD 2/Estimate/estimate_spring_mean_md8.csv",row.names = F)

estimate.m.spring.may.include <- estimate.md8[(estimate.md8$Date>="2019-03-01") | (estimate.md8$Date<"2018-06-01"),]
estimate.m.spring.may.include.by.grid <- group_by(estimate.m.spring.may.include,Grid_ID)
estimate.m.spring.may.include.mean.by.grid <- summarise(estimate.m.spring.may.include.by.grid,season.mean.md8 = mean (estimate.md8))
write.csv(estimate.m.spring.may.include.mean.by.grid ,"/Volumes/Wenhao_eSSD 2/Estimate/estimate_spring.may.include_mean_md8.csv",row.names = F)

###Multiple Plot
P1 <- sactter.10.fold+scale_y_continuous(expand = c(0,0)) + xlab("") + ylab("Observed Ozone Concentration (ppb)") +labs(title = "Overall")
P2 <- sactter.spatial+scale_y_continuous(expand = c(0,0)) + xlab("Predicted Ozone Concentration (ppb)") + ylab("") +labs(title = "Spatial")
P3 <- sactter.temporal+scale_y_continuous(expand = c(0,0)) + xlab("") + ylab("") + labs(title = "Temporal")

multiplot(P1,P2,P3,cols = 3)

mean(estimate.md8$estimate.md8)
mean(pre.ground$MD8)
mean(estimate.md8.ozone.season$estimate.md8)
mean(estimate.md8.non.ozone.season$estimate.md8)

100*nrow(estimate.md8[estimate.md8$estimate.md8>=0.070,])/nrow(estimate.md8)
boxplot(estimate.md8$estimate.md8)
summary(estimate.md8$estimate.md8)

result.with.station <- data.frame(result$SiteName,result$MD8,result$predict)
levels(result.with.station$result.SiteName)
colnames(result.with.station) <- c("Station","Ground","Prediction")
result.with.station.by.group <- group_by(result.with.station,Station)
result.with.station.mean.by.group <- summarize(result.with.station.by.group,Ground=mean(Ground),Prediction=mean(Prediction))
cor.test(result.with.station.mean.by.group$Ground,result.with.station.mean.by.group$Prediction)

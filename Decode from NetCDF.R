library(dplyr)
library(ncdf4)

year <- 2018
month <- "10"
path <- "/maia/Wenhao/HRRR/output/"

#define day as i in the paramether
for (i in 1:10){
  day <- sprintf("%02d",i)
  print(day)
  for (j in 0:23){
    hour <- sprintf("%02d",j)
    file <- paste(path,year,month,day,"/hrrr.t",hour,"z.wrfsfcf00.nc",sep="")
    tmp <- nc_open(file)
    
    ###Read The NC file
    temp <- nc_open(file)
    
    ###Create Criterion for the filter location
    lat <- as.vector(ncvar_get(temp,"gridlat_0"))
    lon <- as.vector(ncvar_get(temp,"gridlon_0"))
    coor <- as.data.frame(cbind(1:length(lat),lat,lon))
    coor_CA <- coor[lat>32,]
    coor_CA <- coor_CA[lat<43,]
    coor_CA <- coor_CA[lon>-126,]
    coor_CA <- coor_CA[lon<(-113),]
    num <- coor_CA$V1
    rot_angle <- as.vector(ncvar_get(temp,"gridrot_0"))[num]
    long <- lon[num]
    lati <- lat[num]
    
    ###get data by the filter criterion
    vis <- as.vector(ncvar_get(temp,"VIS_P0_L1_GLC0"))[num]
    gust <- as.vector(ncvar_get(temp,"WIND_P8_L103_GLC0_max"))[num]
    ugrd_height <- ncvar_get(temp,"VGRD_P0_L103_GLC0")
    vgrd_height <- ncvar_get(temp,"UGRD_P0_L103_GLC0")
    ugrd_10m <- as.vector(ugrd_height[,,1])[num]
    ugrd_80m <- as.vector(ugrd_height[,,2])[num]
    vgrd_10m <- as.vector(vgrd_height[,,1])[num]
    vgrd_80m <- as.vector(vgrd_height[,,2])[num]
    levels <- ncvar_get(temp,"lv_ISBL1")
    ugrd_levels <- ncvar_get(temp,"UGRD_P0_L100_GLC0")
    vgrd_levels <- ncvar_get(temp,"VGRD_P0_L100_GLC0")
    for(k in 1:length(levels)){
      assign(paste("ugrd_",levels[k]/100,sep=""),as.vector(ugrd_levels[,,k])[num])
      assign(paste("vgrd_",levels[k]/100,sep=""),as.vector(vgrd_levels[,,k])[num])
    }
    pres <- as.vector(ncvar_get(temp,"PRES_P0_L1_GLC0"))[num]
    hgt <- as.vector(ncvar_get(temp,"HGT_P0_L1_GLC0"))[num]
    tmp <- as.vector(ncvar_get(temp,"TMP_P0_L1_GLC0"))[num]
    asnow <- as.vector(ncvar_get(temp,"ASNOW_P8_L1_GLC0_acc"))[num]
    mstav <- as.vector(ncvar_get(temp,"MSTAV_P0_L106_GLC0"))[num]
    snowc <- as.vector(ncvar_get(temp,"SNOWC_P0_L1_GLC0"))[num]
    snod <- as.vector(ncvar_get(temp,"SNOD_P0_L1_GLC0"))[num]
    tmp_2m <- as.vector(ncvar_get(temp,"TMP_P0_L103_GLC0"))[num]
    pot <- as.vector(ncvar_get(temp,"POT_P0_L103_GLC0"))[num]
    spfh <- as.vector(ncvar_get(temp,"SPFH_P0_L103_GLC0"))[num]
    dpt <- as.vector(ncvar_get(temp,"DPT_P0_L103_GLC0"))[num]
    rh <- as.vector(ncvar_get(temp,"RH_P0_L103_GLC0"))[num]
    wind <- as.vector(ncvar_get(temp,"WIND_P8_L103_GLC0_max"))[num]
    cpofp <- as.vector(ncvar_get(temp,"CPOFP_P0_L1_GLC0"))[num]
    prate <- as.vector(ncvar_get(temp,"PRATE_P0_L1_GLC0"))[num]
    apcp <- as.vector(ncvar_get(temp,"APCP_P8_L1_GLC0_acc"))[num]
    sfcr <- as.vector(ncvar_get(temp,"SFCR_P0_L1_GLC0"))[num]
    fricv <- as.vector(ncvar_get(temp,"FRICV_P0_L1_GLC0"))[num]
    shtfl <- as.vector(ncvar_get(temp,"SHTFL_P0_L1_GLC0"))[num]
    lhtfl <- as.vector(ncvar_get(temp,"LHTFL_P0_L1_GLC0"))[num]
    gflux <- as.vector(ncvar_get(temp,"GFLUX_P0_L1_GLC0"))[num]
    vgtvp <- as.vector(ncvar_get(temp,"VGTYP_P0_L1_GLC0"))[num]
    cape <- as.vector(ncvar_get(temp,"CAPE_P0_L1_GLC0"))[num]
    cin <- as.vector(ncvar_get(temp,"CIN_P0_L1_GLC0"))[num]
    lcdc <- as.vector(ncvar_get(temp,"LCDC_P0_L214_GLC0"))[num]
    mcdc <- as.vector(ncvar_get(temp,"MCDC_P0_L224_GLC0"))[num]
    hcdc <- as.vector(ncvar_get(temp,"HCDC_P0_L234_GLC0"))[num]
    tcdc <- as.vector(ncvar_get(temp,"TCDC_P0_L10_GLC0"))[num]
    dswrf <- as.vector(ncvar_get(temp,"DSWRF_P0_L1_GLC0"))[num]
    dlwrf <- as.vector(ncvar_get(temp,"DLWRF_P0_L1_GLC0"))[num]
    uswrf <- as.vector(ncvar_get(temp,"USWRF_P0_L1_GLC0"))[num]
    ulwrf <- as.vector(ncvar_get(temp,"ULWRF_P0_L1_GLC0"))[num]
    vbdsf <- as.vector(ncvar_get(temp,"VBDSF_P0_L1_GLC0"))[num]
    vddsf <- as.vector(ncvar_get(temp,"VDDSF_P0_L1_GLC0"))[num]
    hpbl <- as.vector(ncvar_get(temp,"HPBL_P0_L1_GLC0"))[num]
    land <- as.vector(ncvar_get(temp,"LAND_P0_L1_GLC0"))[num]
    icec <-as.vector(ncvar_get(temp,"ICEC_P0_L1_GLC0"))[num]
    
    result <- data.frame(lati,long,rot_angle,vis,gust,ugrd_10m,ugrd_80m,vgrd_10m,vgrd_80m,
                         vgrd_250,vgrd_300,vgrd_500,vgrd_700,vgrd_850,vgrd_925,vgrd_1000,
                         ugrd_250,ugrd_300,ugrd_500,ugrd_700,ugrd_850,ugrd_925,ugrd_1000,
                         pres,hgt,tmp,asnow,mstav,snowc,snod,tmp_2m,pot,spfh,dpt,rh,wind,
                         cpofp,prate,apcp,sfcr,fricv,shtfl,lhtfl,gflux,vgtvp,cape,cin,lcdc,
                         mcdc,hcdc,tcdc,dswrf,dlwrf,uswrf,ulwrf,vbdsf,vddsf,hpbl,land,icec)
    
    write.csv(result,paste("/Volumes/Wenhao/HRRR/csv/",year,month,day,"/CA_hour",hour,".csv",sep=""))
    print(j)
  }
  print(i)
}


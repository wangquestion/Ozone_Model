ssh -X -Y #Address for the remote cluster
#Shell Script for the convert to nc file 

##set the date range and the route for the data
##the HRRR data was 2D files and store by day in 24 hours a day

#start=`date -d "2018-11-05" +%Y%m%d`
#end=`date -d "2018-11-30" +%Y%m%d`
#route0=/maia/Wenhao/HRRR/Data/2018/
#output=/maia/Wenhao/HRRR/output/

#Create dir for the output
#start=`date -d "2018-01-01" +%Y%m%d`
#end=`date -d "2019-12-31" +%Y%m%d`

##Set the output dir for the c
#outnc=/maia/Wenhao/HRRR/csv/

while [ ${start} -le ${end} ]
do
  echo ${start}
  path=$outnc${start}"/"
  echo $path
  start=`date -d "1 day ${start}" +%Y%m%d`
  mkdir -p $path
done


#Main code for the convert from grib2 data to the netCDF data
while [ ${start} -le ${end} ]
do
  path=$route0${start}"/"
  path_output=$output${start}"/"
  for ((i=0;i<=23;i++))
  do
  	echo $i
  	number=`echo ${i}|awk '{printf("%02d\n",$0)}'`
  	filename="hrrr.t"$number"z.wrfsfcf00.grib2"
  	url=$path$filename
  	#convert the grib2 file to the netCDF format
  	ncl_convert2nc $url -o $path_output
  done
  echo ${start}
  start=`date -d "1 day ${start}" +%Y%m%d`
done




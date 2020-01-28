#Shell script for the download of HRRRR

url1='https://pando-rgw01.chpc.utah.edu/hrrr/sfc/2018'
url2='/hrrr.t'
url3='z.wrfsfcf00.grib2'

#Set the route for the storing for the HRRR raw data
route=/Users/wenhao/Desktop/data/HRRR/

cd $route
#Month Loop
for((i=1;i<=12;i++))
do
	printf -v month "%02d" "${i}"
	for ((j=1;j<=31;j++))
	do
		printf -v day "%02d" "${j}"
		for ((k=0;k<=23;k++))
		do
			printf -v hour "%02d" "${k}"
			url="${url1}${month}${day}${url2}${hour}${url3}"
			wget -c -x $url -P /home/wwan322/HRRR/
		done
	done
done

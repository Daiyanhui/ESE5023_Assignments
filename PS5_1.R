#Note
#I got inspired by Yiwen and I looked up some data formats on the website and searched for some methods to analyze them by using R.
library("sp")
library("raster")
library("sf")
library("rgdal")

# 1.1
wind_2.5m_11 <- raster("wc2.1_2.5m_wind/wc2.1_2.5m_wind_11.tif")
srad_2.5m_11 <- raster("wc2.1_2.5m_srad/wc2.1_2.5m_srad_11.tif")
prec_2.5m_11 <- raster("wc2.1_2.5m_prec/wc2.1_2.5m_prec_11.tif")

China_map_crop <- readOGR("C:\\Users\\Daidai\\Documents\\ESE5023\\China_map", "bou2_4p") 


# Crop the raster with china map
Wind_Nov_crop <- crop(wind_2.5m_11, China_map_crop)
Srad_Nov_crop <- crop(srad_2.5m_11, China_map_crop)
Prec_Nov_crop <- crop(prec_2.5m_11, China_map_crop)


#1.2
plot(Wind_Nov_crop,  main="Wind speed in Nov.")
plot(Srad_Nov_crop,  main="Srad speed in Nov.")
plot(Prec_Nov_crop,  main="prec speed in Nov.")


#1.3 
#select interest spot
Crop_point <- c(120,130,10,20)
# Crop the raster
Wind_Nov_crop <- crop(wc_wind_2.5m_11, Crop_point)

# Plot DEM
plot(Wind_Nov_crop, main="High Wind speed in Nov.")
Wind_Nov_crop_crop<-crop(Wind_Nov_crop, China_map_crop)


#1.4 
Crop_point_high_srad <- c(75,120,10,25)

Crop_point_low_prec <- c(75,120,15,40)

Srad_Nov_crop <- crop(Srad_Nov_crop, Crop_box_high_srad)
Prec_Nov_crop <- crop(Srad_Nov_crop, Crop_box_low_prec)

plot(Prec_Nov_crop, main="High Srad and low precipitation in Nov.")
Wind_Nov_crop_crop<-crop(Prec_Nov_crop, China_map_crop)

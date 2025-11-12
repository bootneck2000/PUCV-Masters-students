#PUCV Antarctic Masters degree course lectures. 
#This ensures reproducibility of code.  Do this section only once.
install.packages("renv", repos = "https://rstudio.r-universe.dev")
renv::init()
renv::install(c("tidyverse", "sf", "terra", "raster", "stringr", "lubridate"))

##LECTURE 1: Telemetry device data download, basic processing----
library(tidyverse)
library(sf)
library(terra)
library(raster)
library(lubridate)
renv::snapshot()
data = read_tsv("./data/Raw gps/KI-6_S1.txt",
                col_names=(c("date", "lat", "lon", "blah", "blah", "blah", "blah", "blah"))) %>%
  dplyr::mutate(id = "KI-6") %>%
  dplyr::select(id, date, lon, lat)

data$date = lubridate::dmy_hms(data$date)

data_sf = data %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  sf::st_union() %>%      # Combine the points into a MULTIPOINT
  sf::st_cast("LINESTRING")

plot(st_geometry(data_sf))

#EXERCISES----
#bring in the KI-6_S1.csv file and estimate the max and mean dive depths for this individual.
#The CSV file has accelerometer data in it, this is why it is so big.
#You will need to read the data in, then, as a pipe (%>%), filter the data to 
#take only those data with dive values that are not "NA" values.
#Then perform some basic R stats (mean, max, total number of dives) on the Depth column.
#Bonus: how long was the animal at sea for ?

d = readr::read_delim("./data/Raw gps/KI-6_S1.csv",
                      delim = ";")

dt = d %>%
  dplyr::filter(Depth > 0)


##LECTURE 2: R and QGIS spatial basics----

library(tidyverse)
library(sf)
library(terra)
library(raster)
library(lubridate)
renv::snapshot()
#try this
download.file("https://download.pangaea.de/dataset/937574/files/IBCSO_v2_bed.nc",
              destfile = "./data/learn to download/bathy.nc")

#pull SST RASTER data from the web
# the website i use for this example is the ERRDAP site here: https://coastwatch.pfeg.noaa.gov/erddap/index.html
# you can do this on ANY environmental data portal, so long as you are able to build distinct URL weblinks for each file.  
# Otherwise, look at FPT servers and use an FTP file system like Filezilla (https://filezilla-project.org/) to manually 
# download a list of environmental datasets

url = "https://coastwatch.noaa.gov/erddap/files/noaacwBLENDEDsstDLDaily/2024/20240101000000-OSPO-L4_GHRSST-SSTfnd-Geo_Polar_DABlended-GLOB-v02.0-fv01.0.nc"

# 1. Define start and end date
start_date <- as.Date("2024-01-01")
end_date   <- as.Date("2024-01-06")

# 2. Create sequence of dates
date_seq <- seq.Date(start_date, end_date, by = "day")
date_strs <- format(date_seq, "%Y%m%d")
# 3. Format dates as "YYYYMMDD"
date_strs <- format(date_seq, "%Y%d%m")

url.list = list()
destination = list()

for(i in 1:length(date_strs)){
  url.list[[i]] = paste0("https://coastwatch.noaa.gov/erddap/files/noaacwBLENDEDsstDLDaily/2024/",date_strs[[i]],"000000-OSPO-L4_GHRSST-SSTfnd-Geo_Polar_DABlended-GLOB-v02.0-fv01.0.nc")
  temp = date_strs[[i]]
  destination[[i]] = paste0("./data/learn to download/SST test/",temp)
  download.file(url = url.list[[i]], 
                destfile = destination[[i]],
                mode = "wb")
}

#bring subset in for basic manipulation of SST RASTER data-
dir = "./data/learn to download/SST raw"
setwd(dir)
box = c(xmin = 275, 
        xmax = 310,
        ymin = -70, 
        ymax = -50)

er = terra::ext(box)

#OR
dev.new(noRStudioGD = TRUE)
er = draw(x="extent", col = "blue", n=4) #- but you need one raster plotted first in order to set the extent object.
sst_raw = list.files()

sst_rast = list()
for (i in 1:5){
  temp =  terra::rast(sst_raw[[i]],
                      lyrs = 1)
  sst_rast[[i]] = terra::crop(temp, er)
}

#i = 1
#temp =  terra::rast(sst_raw[[i]],
# lyrs = 1)
#terra::image(temp)

terra::image(sst_rast[[1]])
terra::crs(sst_rast[[1]], proj = TRUE, describe = TRUE)

sst_rast_list = terra::rast(sst_rast)

sst_rast_mean = app(sst_rast_list, mean)

terra::writeRaster(sst_rast_mean, 
                   filename = "./output/Mean SST for some rasters.tif")

#manipulate or examine shapefiles and write out for import into QGIS

coast = terra::vect("./QGIS baselayers/add_coastline_high_res_polygon_v7.2/add_coastline_high_res_polygon_v7.2.shp")

plot(coast)

er2 = draw(x="extent", col = "red")

WAP = terra::crop(coast, er2)

terra::writeVector(WAP, file = "./QGIS baselayers/add_coastline_high_res_polygon_v7.2/WAP coastline.shp")

#make contours from raster then write to shapefile for import to QGIS
sst_rast_mean = terra::rast("./output/Mean SST for some rasters.tif")
range(sst_rast_mean) #range to calculate the number of levels for contouring
sst_rast_mean_contour = terra::as.contour(sst_rast_mean, nlevels = 10)
terra::writeVector(sst_rast_mean_contour, "./output/Mean SST Contours.shp")

#create a simple lines dataset, simulating animal tracking data, and write to a shapefile for import into QGIS"
#EXAMPLE 1 - single track, text file, messy
d = readr::read_delim("./data/Raw gps/KI-6_S1.txt",
                      col_names = c("date", 
                                    "lat",
                                    "lon",
                                    "x",
                                    "y",
                                    "z",
                                    "a",
                                    "b")) %>%
  dplyr::select(date, lon, lat) 
d$date = lubridate::dmy_hms(d$date)

d_sf = d %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(do_union = F) %>%
  sf::st_cast(to = "LINESTRING")
plot(st_geometry(d_sf))

sf::st_write(d_sf, "./output/D14_tracks.shp")

#EXAMPLE 2: multiple tracks, structured and clean data

d = readr::read_csv("./data/Raw gps/Mac Point tracking data.csv")

d = d %>%
  dplyr::group_by(id) %>%
  sf::st_as_sf(coords=c("lon", "lat"), crs = 4326) %>%
  dplyr::summarize(do_union = FALSE) %>%
  sf::st_cast(.,'LINESTRING')
#plotting option #1
ggplot(d) +
  geom_sf(aes(color = factor(id)))
#plotting option #2 in QGIS
sf::st_write(d, "./output/penguin tracks Macaroni Point.shp")
####EXERCISES----

#1) bring in GEBCO, create bounding box, crop to WAP, write out to tif. 

bathy = terra::rast("IBCSO_v2_bed.nc")
dev.new(noRStudioGD = TRUE)
terra::image(bathy)

er2 = draw(x = "extent", colour = "red")

WAPbathy = terra::crop(bathy, er2)
terra::image(WAPbathy)
bathy_contours = terra::as.contour(WAPbathy, levels = c(-100,
                                                        -500,
                                                        -600,
                                                        -800,
                                                        -900,
                                                        -1000,
                                                        -2000,
                                                        -3000))

writeRaster(WAPbathy, "Wap bathy.tif")
writeVector(bathy_contours, "wap bathy contours.shp")
#2) create contours of depth (choose nlevels based on bathy range of cropped WAP), write to shapefie
#3) Create a shapefile of tracking data, with each animal ID as a different colour, and overlay onto QGIS cropped bathymetry and contours

####----
#Solution 1:
bathy = terra::rast("IBCSO_v2_bed.nc")
terra::image(bathy)
ext = terra::draw(x = "extent", col = "blue")
wap_bathy = terra::crop(bathy, ext)
terra::image(wap_bathy)
terra::writeRaster(wap_bathy, "WAP bathy.tif")
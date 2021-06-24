library(tidyverse)
library(lubridate)
library(grid)
library(sharpshootR)
library(geofacet)
require(wql)
library(magrittr)
library(viridis)
library(scales)
library(sf)
library(ggspatial)
library(plotrix)
library(ggrepel)

#Set workspace
setwd("~/GitHub/geotiff_extract")


#####Load shapefile used to subset raster/tiff
#Shapefile from Mahardja et al. 2021 EDSM paper, convert to lat and long to make it more intuitive
EDSM_polygon<-st_read(file.path("Shapefiles","EDSM_Subregions_053019.shp"))%>% st_transform(crs=4326)

#Read in Delta files
DeltaSubregionsWater <- st_read("D:/Projects/Pascale's Striped Bass - Salmon Study/Map Files/DJFMP Water bodies","DeltaSubregionsWater")

#Create the map figure
edsm_map<-ggplot() + theme_bw()+
  geom_sf(data = DeltaSubregionsWater, fill = 'cyan2', lwd = 0.5, color='cyan2') + 
  geom_sf(data = EDSM_polygon,fill=NA, lwd = 0.5, color='black') +
 # geom_sf(data=df.SP,color="red",size=2)+ geom_label_repel(data=df.SP, aes(x=Longitude,y=Latitude,label=station_id),nudge_x = c(-0.025, -0.025, 0.025, -0.025, -0.025, 0.025, 0.1),nudge_y = c(0.025,-0.025, 0.025, 0.025, 0.025, -0.025, -0.025),segment.alpha=0.7,color="blue")+
 # coord_sf(xlim = c(-122.3, -121.5), ylim = c(37.8, 38.5),crs=crsLONGLAT)  +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_y = unit(1.0, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "tr", width_hint = 0.5)+
  theme(plot.tag= element_text(size=20, color="black"), axis.text.x = element_text(size=16, color="black"),axis.text.y = element_text(size=16, color="black"),axis.title.x=element_blank(),axis.title.y=element_blank())
edsm_map

# Print map
tiff(filename="Figure_EDSM_Map.tiff", 
     type="cairo",
     units="in", 
     width=12*1, 
     height=11*1, 
     pointsize=18, 
     res=450, compression="lzw")
edsm_map
dev.off()
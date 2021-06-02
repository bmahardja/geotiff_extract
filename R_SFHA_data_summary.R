#==========================================================================#
#  Script created by Brian Mahardja on June 2nd, 2021     #
#  To pull data from geotiff files produced by RMA for evaluating summer-fall habitat action scenarios   #
#==========================================================================#

#Set workspace
setwd("D:/Projects/Structured Decision Making/2021-05-14 - Example files from RMA models")

library(stringr)
library(rgdal)
library(sf)
library(tidyverse)
#'terra' is a better package than 'raster' for large spatial files - also a Robert Hijmans package
library(terra)

#####Load custom function

#Function from online to extract numbers from a character string
#http://stla.github.io/stlapblog/posts/Numextract.html
Numextract <- function(string){
  unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)))
}

#####Load shapefile used to subset raster/tiff
#Shapefile from Mahardja et al. 2021 EDSM paper, convert to lat and long to make it more intuitive
EDSM_polygon<-st_read(file.path("Shapefiles","EDSM_Subregions_053019.shp"))%>% st_transform(crs=4326)


#####
# Create the list of all the tif files in the directory. Can change to "hsit-avgmon_rma"
list_tif <- dir(pattern = "*.tif") 

#Create dataframe repository for results
df <- data.frame(nrow=1:length(list_tif), filename=NA, value=NA, action=NA, year=NA, month=NA)

#Loop to read each file one by one and store them into the data frame
for (k in 1:length(list_tif)){
  temp_data <- rast(x = list_tif[k]) #temp = temporary
  temp_data <- aggregate(temp_data, fact=10) #reduce resolution of raster by a factor of 10
  temp_data <- as.points(temp_data) %>% #Convert to vector/points
    #The coordinate system we used is a “Pseudo-Mercator” system which allows the tiled
    #pngs to be used with background map layers in the Shiny App. 
    #The EPSG code of the coordinate system is 3857. 
    st_as_sf(crs=3857) %>% #use sf to convert to spatial points
    st_transform(crs=4326) %>% #convert to lat and long
    st_filter(EDSM_polygon) #filter with EDSM polygon
  temp_data<-as.data.frame(temp_data) #convert to data frame
  
  df$filename[k] <- list_tif[k]  #Name of file
  df$value[k] <- mean(temp_data[,1]) #Summarize value
  df$action[k] <- sub("\\_.*", "", df$filename[k]) #Extract name of action/scenario
  df$year[k] <- Numextract(df$filename[k])[1] #Year value
  df$month[k] <- Numextract(df$filename[k])[2] #Month value
  
  message(paste0("Finished run ", k, "/",length(list_tif))) #Message to let you know where the run is at (e.g. run 1 out of x)
  
}

#Export data frame out into a csv file
write.csv(df,file = "HSI_results.csv",row.names = F)


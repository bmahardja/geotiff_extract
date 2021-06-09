#==========================================================================#
#  Script created by Brian Mahardja on June 8th, 2021     #
#  To pull data from geotiff files produced by RMA for evaluating summer-fall habitat action scenarios   #
#  And summarize based on Rose et al. IBM regions   #
#==========================================================================#

#Set workspace
setwd("D:/Projects/Structured Decision Making/2021-06-07 - RMA model HSI calculation")

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
#Read the shapefile for IBM subregions
#Note the filepath denotes a subfolder from the working directory titled "Shapefiles"
IBM_polygon<-st_read(file.path("Shapefiles","subregions.shp"))%>% st_transform(crs=4326)

#####
#Change workspace to wherever the tiff files are
setwd("D:/Projects/Structured Decision Making/2021-06-07 - RMA model HSI calculation/RMA model results - HSI")

# Create the list of all the tif files in the directory. Can change to "hsit-avgmon_rma"
list_tif <- dir(pattern = "1930.+hsi-avgmon")
#For "&" ("and"), if you know the order, use regex like 20130801.+USD or .+20130801.+USD.+. 
#The .+ matches any characters in-between (and possibly before/after)


#Create dataframe repository for results
datalist <- list()

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
    st_join(IBM_polygon, join=st_intersects) %>% #Add IBM polgyon and regions
    filter(!is.na(SUBREGION)) #Filter out areas outside the IBM
  temp_data<-as.data.frame(temp_data) #convert to data frame
  temp_data$SUBREGION<-as.factor(temp_data$SUBREGION) #Change subregion to factor instead of character
  
  temp_df<- temp_data %>% group_by(SUBREGION) %>% summarise(across(1, mean, .names="value"))
    
  temp_df$filename <- list_tif[k]  #Name of file
  temp_df$action <- sub("\\_.*", "", temp_df$filename) #Extract name of action/scenario
  temp_df$year <- Numextract(temp_df$filename)[1] #Year value
  temp_df$month <- Numextract(temp_df$filename)[2] #Month 
  temp_df$run_number <- k 
  
  datalist[[k]] <- temp_df #Store data
  
  message(paste0("Finished run ", k, "/",length(list_tif))) #Message to let you know where the run is at (e.g. run 1 out of x)
  
}

#Final data frame
df = do.call(rbind, datalist)

#Export data frame out into a csv file
write.csv(df,file = "Temperature.results_mean_summary.csv",row.names = F)


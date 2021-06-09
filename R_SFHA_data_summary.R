#==========================================================================#
#  Script created by Brian Mahardja on June 2nd, 2021     #
#  To pull data from geotiff files produced by RMA for evaluating summer-fall habitat action scenarios   #
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
#Shapefile from Mahardja et al. 2021 EDSM paper, convert to lat and long to make it more intuitive
EDSM_polygon<-st_read(file.path("Shapefiles","EDSM_Subregions_053019.shp"))%>% st_transform(crs=4326)


#####
#Change workspace
setwd("D:/Projects/Structured Decision Making/2021-06-07 - RMA model HSI calculation/RMA model results - HSI")

# Create the list of all the tif files in the directory. Can change to "hsit-avgmon_rma"
list_tif <- dir(pattern = "1979.+hsit-avgmon") 
#For "&" ("and"), if you know the order, use regex like 20130801.+USD or .+20130801.+USD.+. 
#The .+ matches any characters in-between (and possibly before/after)

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

#Switch work directory again
setwd("D:/Projects/Structured Decision Making/2021-06-07 - RMA model HSI calculation")

#Export data frame out into a csv file
write.csv(df,file = "HSIT_results_1979.csv",row.names = F)







###############################################
###############################################
###############################################
#For summarizing data by year

setwd("D:/Projects/Structured Decision Making/2021-06-07 - RMA model HSI calculation")

# Create the list of the relevant csv files in the directory. 
list_csv <- dir(pattern = "HSI_.+.csv") 
#HSI
HSI_annual_sum_data<-do.call(rbind,lapply(list_csv,read.csv))
#Remove June since we're concerned with July-Oct
HSI_annual_sum_data<- HSI_annual_sum_data %>% filter(month>6)
#Add July for ndfa and ndfa+smscg with no-action data
HSI_annual_sum_data <- HSI_annual_sum_data %>% expand(action, month, year) %>% full_join(HSI_annual_sum_data) %>%
  filter(!(grepl('smscg', action)&year==1930)) #take out smscg scenario for dry year (1930)

#Fill July NA with no action month for NDFA
HSI_annual_sum_data[is.na(HSI_annual_sum_data$value)&HSI_annual_sum_data$year==1930&HSI_annual_sum_data$action=="ndfa",c("value")] <- HSI_annual_sum_data[HSI_annual_sum_data$year==1930&HSI_annual_sum_data$action=="no+action"&HSI_annual_sum_data$month==7,c("value")]
HSI_annual_sum_data[is.na(HSI_annual_sum_data$value)&HSI_annual_sum_data$year==1940&HSI_annual_sum_data$action=="ndfa",c("value")] <- HSI_annual_sum_data[HSI_annual_sum_data$year==1940&HSI_annual_sum_data$action=="no+action"&HSI_annual_sum_data$month==7,c("value")]
HSI_annual_sum_data[is.na(HSI_annual_sum_data$value)&HSI_annual_sum_data$year==1979&HSI_annual_sum_data$action=="ndfa",c("value")] <- HSI_annual_sum_data[HSI_annual_sum_data$year==1979&HSI_annual_sum_data$action=="no+action"&HSI_annual_sum_data$month==7,c("value")]
HSI_annual_sum_data[is.na(HSI_annual_sum_data$value)&HSI_annual_sum_data$year==1986&HSI_annual_sum_data$action=="ndfa",c("value")] <- HSI_annual_sum_data[HSI_annual_sum_data$year==1986&HSI_annual_sum_data$action=="no+action"&HSI_annual_sum_data$month==7,c("value")]
#Fill July NA with SMSCG month for NDFA+SMSCG

HSI_annual_sum_data[is.na(HSI_annual_sum_data$value)&HSI_annual_sum_data$year==1940&HSI_annual_sum_data$action=="ndfa+smscg",c("value")] <- HSI_annual_sum_data[HSI_annual_sum_data$year==1940&HSI_annual_sum_data$action=="smscg"&HSI_annual_sum_data$month==7,c("value")]
HSI_annual_sum_data[is.na(HSI_annual_sum_data$value)&HSI_annual_sum_data$year==1979&HSI_annual_sum_data$action=="ndfa+smscg",c("value")] <- HSI_annual_sum_data[HSI_annual_sum_data$year==1979&HSI_annual_sum_data$action=="smscg"&HSI_annual_sum_data$month==7,c("value")]
HSI_annual_sum_data[is.na(HSI_annual_sum_data$value)&HSI_annual_sum_data$year==1986&HSI_annual_sum_data$action=="ndfa+smscg",c("value")] <- HSI_annual_sum_data[HSI_annual_sum_data$year==1986&HSI_annual_sum_data$action=="smscg"&HSI_annual_sum_data$month==7,c("value")]


#Export filled out data frame out into a csv file
write.csv(HSI_annual_sum_data,file = "HSI.results_all_data_filled.csv",row.names = F)

#Sum by year
HSI_annual_sum_data<-HSI_annual_sum_data %>% group_by(action,year) %>% summarise(annual_mean_value=mean(value)) %>%
  mutate(WaterYearType= case_when(year == 1930 ~ "Dry",
                                  year == 1940 ~ "Above Normal",
                                  year == 1979 ~ "Below Normal",
                                  year == 1986 ~ "Wet"))
#Export data frame out into a csv file
write.csv(HSI_annual_sum_data,file = "HSI.results_mean_summary.csv",row.names = F)



##################################
#HSIT
list_csv <- dir(pattern = "HSIT_.+.csv") 

HSIT_annual_sum_data<-do.call(rbind,lapply(list_csv,read.csv))

#Remove June since we're concerned with July-Oct
HSIT_annual_sum_data<- HSIT_annual_sum_data %>% filter(month>6)
#Add July for ndfa and ndfa+smscg with no-action data
HSIT_annual_sum_data <- HSIT_annual_sum_data %>% expand(action, month, year) %>% full_join(HSIT_annual_sum_data) %>%
  filter(!(grepl('smscg', action)&year==1930)) #take out smscg scenario for dry year (1930)

#Fill July NA with no action month for NDFA
HSIT_annual_sum_data[is.na(HSIT_annual_sum_data$value)&HSIT_annual_sum_data$year==1930&HSIT_annual_sum_data$action=="ndfa",c("value")] <- HSIT_annual_sum_data[HSIT_annual_sum_data$year==1930&HSIT_annual_sum_data$action=="no+action"&HSIT_annual_sum_data$month==7,c("value")]
HSIT_annual_sum_data[is.na(HSIT_annual_sum_data$value)&HSIT_annual_sum_data$year==1940&HSIT_annual_sum_data$action=="ndfa",c("value")] <- HSIT_annual_sum_data[HSIT_annual_sum_data$year==1940&HSIT_annual_sum_data$action=="no+action"&HSIT_annual_sum_data$month==7,c("value")]
HSIT_annual_sum_data[is.na(HSIT_annual_sum_data$value)&HSIT_annual_sum_data$year==1979&HSIT_annual_sum_data$action=="ndfa",c("value")] <- HSIT_annual_sum_data[HSIT_annual_sum_data$year==1979&HSIT_annual_sum_data$action=="no+action"&HSIT_annual_sum_data$month==7,c("value")]
HSIT_annual_sum_data[is.na(HSIT_annual_sum_data$value)&HSIT_annual_sum_data$year==1986&HSIT_annual_sum_data$action=="ndfa",c("value")] <- HSIT_annual_sum_data[HSIT_annual_sum_data$year==1986&HSIT_annual_sum_data$action=="no+action"&HSIT_annual_sum_data$month==7,c("value")]
#Fill July NA with SMSCG month for NDFA+SMSCG

HSIT_annual_sum_data[is.na(HSIT_annual_sum_data$value)&HSIT_annual_sum_data$year==1940&HSIT_annual_sum_data$action=="ndfa+smscg",c("value")] <- HSIT_annual_sum_data[HSIT_annual_sum_data$year==1940&HSIT_annual_sum_data$action=="smscg"&HSIT_annual_sum_data$month==7,c("value")]
HSIT_annual_sum_data[is.na(HSIT_annual_sum_data$value)&HSIT_annual_sum_data$year==1979&HSIT_annual_sum_data$action=="ndfa+smscg",c("value")] <- HSIT_annual_sum_data[HSIT_annual_sum_data$year==1979&HSIT_annual_sum_data$action=="smscg"&HSIT_annual_sum_data$month==7,c("value")]
HSIT_annual_sum_data[is.na(HSIT_annual_sum_data$value)&HSIT_annual_sum_data$year==1986&HSIT_annual_sum_data$action=="ndfa+smscg",c("value")] <- HSIT_annual_sum_data[HSIT_annual_sum_data$year==1986&HSIT_annual_sum_data$action=="smscg"&HSIT_annual_sum_data$month==7,c("value")]

#Export filled out data frame out into a csv file
write.csv(HSIT_annual_sum_data,file = "HSIT.results_all_data_filled.csv",row.names = F)

#Sum by year
HSIT_annual_sum_data<-HSIT_annual_sum_data %>% group_by(action,year) %>% summarise(annual_mean_value=mean(value)) %>%
  mutate(WaterYearType= case_when(year == 1930 ~ "Dry",
                                  year == 1940 ~ "Above Normal",
                                  year == 1979 ~ "Below Normal",
                                  year == 1986 ~ "Wet"))
#Export data frame out into a csv file
write.csv(HSIT_annual_sum_data,file = "HSIT.results_mean_summary.csv",row.names = F)

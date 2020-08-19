library(tidyverse)
library(bupaR)
library(edeaR)
library(lubridate)
library(plyr)
library(dplyr)
library(sp)
library(spatstat)
library(raster)
library(sp)
library(naniar)
library(janitor)
library(ranger)
library(rsample)
library(rgdal)
library(naniar)
library(maptools)

filtered_log <- PO_log #BPIC 19 event log


case_by_date <- filtered_log %>% group_by(date(event.time.timestamp)) %>% dplyr::summarize(count=n())
filtered_log <- PO_log %>% mutate(date=date(event.time.timestamp)) %>% left_join(case_by_date,by=c("date" = "date(event.time.timestamp)"))
# Add a couple of temporal feature to event log
filtered_log$cdate <- floor(unclass(as.POSIXct(as.POSIXct(paste(filtered_log$event.time.timestamp), format="%Y-%m-%d")))/86400)
filtered_log$doy <- as.integer(strftime(as.POSIXct(paste(filtered_log$event.time.timestamp), format="%Y-%m-%d"), format = "%j"))


filtered_log <- filtered_log %>% dplyr::select(case.Purchasing.Document,event.concept.name,event.time.timestamp,RemTime,ElapsedTime,count,cdate,doy,x,y)

filtered_log <- filtered_log %>% mutate_if(is.character,as.factor)
#extract spatial training & test set for both spatial/non-spatial models
final_log <- filtered_log[1:2000,]

set.seed(1353)
log_split <- initial_split(final_log, prop = 0.1)

# Extract the testing dataframe
testing_data <- log_split %>% testing() 
training_data <- log_split %>% training()

# Create a SpatialPoints object for the event locations
coords <- SpatialPoints(data.frame(training_data$x,training_data$y))
coords_test <- SpatialPoints(data.frame(testing_data$x,testing_data$y))

#Assign CRS to the spatial points object
proj4string(coords) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(coords_test) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#Transform to UTM CRS from long-lat
coords_UTM <- spTransform(coords, CRS("+proj=utm +zone=32 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
coords_test_UTM <- spTransform(coords_test, CRS("+proj=utm +zone=32 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) 

#Create a spatial dataframe from event log
spdf <- SpatialPointsDataFrame(coords_UTM,training_data)
spdf_test <- SpatialPointsDataFrame(coords_test_UTM,testing_data)

# create the study window .i.e. location process was executed
bbox <- spdf@bbox
bbox[,"min"] <- bbox[,"min"]-4000
bbox[,"max"] <- bbox[,"max"]+4000
pe2km <- plotKML::vect2rast(spdf, cell.size=4000, bbox=bbox)
pe2km$mask = 1
pe2km <- as(pe2km["mask"], "SpatialPixelsDataFrame")

#calculate the buffer distances 
sp.dist0 <- GSIF::buffer.dist(spdf["throughput_time_case"], pe2km, as.factor(1:nrow(spdf@data)))
#sp.dist1 <- GSIF::buffer.dist(spdf[cols], pe2km, as.factor(1:nrow(spdf@data)))

#sp.dist0_nd <- GSIF::buffer.dist(spdf_nd["RemTime"], spxdf_nd[1], as.factor(1:nrow(spdf_nd)))
ov.sp  <- over(spdf["throughput_time_case"],sp.dist0)

#ov.sp1 <- over(spdf["RemTime"],sp.dist1)
sp.dn0 <- paste(names(sp.dist0),collapse="+")
#sp.dn1 <- paste(cols,collapse="+")
#sp.dn2 <- paste(sp.dn0,"+",sp.dn1)

sp.fm1 <- as.formula(paste("RemTime ~ ",sp.dn0)) #

#ov.thruput <- over(spdf["RemTime"], spxdf[1:2])
sp.rm <- do.call(cbind, list(spdf@data["throughput_time_case"],ov.sp)) #,

# create the spatial model & predict
pars.tpt = list(mtry=27, min.node.size=2, sample.fraction=0.9930754, num.trees=150, importance = "impurity", seed=1) # how did they work out these parameter?
m1.tpt <- ranger(sp.fm1, sp.rm[complete.cases(sp.rm),],mtry=27, min.node.size=2, sample.fraction=0.9930754, num.trees=150, importance = "impurity", seed=1, quantreg=FALSE)
m1.tpt 


pe2km$RemTime <- predict(m1.tpt,sp.dist0@data,type="response")$predictions

sp.test <- over(spdf_test, pe2km["RemTime"])

# Test the model 
testing_data <- testing_data %>% mutate(mlp_pred = pred_mlp,gbm_pred=pred_gbm)

testing_data <- testing_data %>% mutate(spat_pred=sp.test$RemTime,spat_pred_abserr=abs(RemTime-spat_pred)) 
#test_data <- test_data %>% mutate(sp_pred=spat_pred-ElapsedTime,spat_pred_abserr=abs(RemTime-sp_pred))

# Note: The non-spatial models need to be created prior to running this section of code (see )
#testing_data <- testing_data %>% mutate(gbm_pred=pred_gbm,gbm_pred_abserr=abs(RemTime-gbm_pred),spat_gbm_pred=(gbm_pred+spat_pred)/2,spat_gbm_pred_abserr=abs(RemTime-spat_gbm_pred)) 
test_data <- test_data %>% mutate(g_pred=gbm_pred-ElapsedTime,gbm_pred_abserr=abs(RemTime-g_pred),spat_gbm_pred=(g_pred+sp_pred)/2,spat_gbm_pred_abserr=abs(RemTime-spat_gbm_pred))

#testing_data <- testing_data %>% mutate(mlp_pred=pred_mlp,mlp_pred_abserr=abs(RemTime-pred_mlp),spat_mlp_pred=(mlp_pred+spat_pred)/2,spat_mlp_pred_abserr=abs(RemTime-spat_mlp_pred)) 
test_data <- test_data %>% mutate(m_pred=mlp_pred-ElapsedTime,mlp_pred_abserr=abs(RemTime-m_pred),gbm_mlp_pred=(m_pred+g_pred)/2,gbm_mlp_pred_abserr=abs(RemTime-gbm_mlp_pred)) 

#testing_data <- testing_data %>% mutate(mlp_pred=pred_mlp,gbm_pred=pred_gbm,mlp_gbm_pred=(mlp_pred+gbm_pred)/2,mlp_gbm_pred_abserr=abs(RemTime-mlp_gbm_pred))


#spatial prediction error
testing_data %>% 
  filter(!is.na(spat_pred)) %>% 
  summarise(mae=mean(spat_pred_abserr),sd_ae=sd(spat_pred_abserr))

#gbm prediction error
testing_data %>% 
  filter(!is.na(gbm_pred)) %>% 
  summarise(mae=mean(gbm_pred_abserr),sd_ae=sd(gbm_pred_abserr))

#gbm + spat prediction error
testing_data %>% 
  filter(!is.na(spat_pred)) %>% 
  summarise(mae=mean(spat_gbm_pred_abserr),sd_ae=sd(spat_gbm_pred_abserr))

#mlp prediction error
testing_data %>% 
  filter(!is.na(mlp_pred)) %>% 
  summarise(mae=mean(mlp_pred_abserr),sd_ae=sd(mlp_pred_abserr))

#gbm + spat prediction error
testing_data %>% 
  filter(!is.na(spat_pred)) %>% 
  summarise(mae=mean(spat_mlp_pred_abserr),sd_ae=sd(spat_mlp_pred_abserr))

#gbm + mlp prediction error
test_data %>% 
  filter(!is.na(g_pred)) %>% 
  summarise(mae=mean(gbm_mlp_pred_abserr),sd_ae=sd(gbm_mlp_pred_abserr)) 

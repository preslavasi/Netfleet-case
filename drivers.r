rm(list=ls())
#setwd("C:\\Users\\SonyaIlieva\\Desktop\\UNI\\R\\Summer semester\\NETERA")
#setwd("D:\\Preslava\\uni\\Business Analytics\\Boryana Saturday\\NETERA")
setwd("C:\\Users\\Daria\\Desktop\\business analytics\\Business analytics 2 sem\\Netera case")

load("joined_data.RData")
# can_dd=read.csv("can_bus_events.csv", na.string=c("","NA"," ","?","\\N"),stringsAsFactors = F)
# gps_dd=read.csv("gps_events.csv",na.string=c("","NA"," ","?","\\N"),stringsAsFactors = F)
# 
# colnames(gps_dd)=c("gps_event_id","device_id","latitude","longitude","speed","azimuth","altitude","current_work_hours","mileage","power_voltage","gps_utc_time", "satellites")
# names(gps_dd)
# colnames(can_dd)=c("id","device_id","total_distance","total_fuel_used","engine_rpm","vehicle_speed","engine_coolant_temperature","engine_temperature","accelerator_pedal_pressure","e.range",
#                    "total_engine_hours"," total_driving_time"," total_engine_idle_time","total_idle_fuel_used","detailed_fuel_low","detailed_driver_seatbelt","detailed_air_conditioning","detailed_cruise_control",
#                    "detailed_brake_pedal","detailed_handbrake","detailed_central_lock","detailed_reverse_gear","detailed_running_lights","detailed_low_beams","detailed_high_beams","detailed_rear_fog_lights","detailed_front_fog_lights","detailed_doors","detailed_trunk","doors_driver_door","doors_passenger_door","doors_rear_left_door","doors_rear_right_door",
#                    "doors_trunk","doors_hood","gps_utc_time","trip_id"," fuel_level_litres","fuel_level_percentage","gps_event_id")
# 
# #Merge the two datasets
# library(dplyr)
# can_gps=left_join(gps_dd,can_dd,by=c("device_id","gps_event_id","gps_utc_time"))

#Remove observations which have NAs in trip_id column
sum(is.na(can_gps$trip_id))
can_gps=can_gps[!(is.na(can_gps$trip_id)==T),]

# Checking classes and missing values ----
DD=data.frame(vn=names(can_gps),vc=sapply(can_gps,class),vna=colSums(is.na(can_gps))/nrow(can_gps))

# Remove variables with more than 20% missing values for can_dd
names(can_gps[names(can_gps) %in% DD$vn[DD$vna>=0.2]])
#[1] "engine_coolant_temperature" "engine_temperature"        
#[3] "accelerator_pedal_pressure" " fuel_level_litres"  
can_gps=can_gps[,!names(can_gps) %in% DD$vn[DD$vna>=0.2]]
DD=DD[-which(DD$vna>=0.2),]
names(can_gps)

#Replacing the missing calues with 0
sum(is.na(can_gps$device_id))
can_gps[is.na(can_gps)]=0

#Fixing the types of the variables
library(lubridate)
can_gps$gps_utc_time[1:5]
can_gps$gps_utc_time=ymd_hms(can_gps$gps_utc_time)
class(can_gps$gps_utc_time)

#GPS speed underestimates the exact speed - remove this variable
can_gps=can_gps[,-(5)]
DD=DD[-c(5),]

#Factor variables
unique(can_gps$detailed_fuel_low)
ll=list()
for (i in 1:ncol(can_gps)){
  ll[[i]]=unique(can_gps[[i]])
}
#(22:43) - factor variables
names(can_gps[,c(10,22:43)])

#During one trip these events can happen several times - we will aggregate the data and find the average numbers of these events
#length(unique(can_gps$device_id))
#length(unique(can_gps$trip_id))
device_aggr=aggregate(can_gps[,-c(1,2,10,11,12,43)], by=list(can_gps$device_id), FUN="mean")
names(device_aggr)[1]<-"device_id"
options("scipen"=100, "digits"=4)
DD_aggr=data.frame(vmax=apply(device_aggr,2,max,na.rm=T),vmin=apply(device_aggr,2,min,na.rm=T),vmean=apply(device_aggr,2,mean,na.rm=T))
DD_aggr$vdiff=DD_aggr$vmax-DD_aggr$vmean


# Have a look at correlation of features without the factor variables
windows()
library(psych)
cor.plot(device_aggr,numbers=T, las=2, main="Correlation matrix of the aggregated variables",cex=0.3,cex.axis=0.8) 

#Group the variebles into the 4 main categories/factors
aux_non_economy=device_aggr[grep("total_idle_fuel_used|total_distance|total_fuel_used|mileage|total_engine_hours|total_driving_time|total_engine_idle_time|detailed_air_conditioning|detailed_trunk|doors_driver_door|doors_passenger_door|doors_rear_left_door|doors_rear_right_door|detailed_doors|altitude|detailed_brake_pedal",names(device_aggr))]
aux_danger=device_aggr[grep("vehicle_speed|engine_rpm|detailed_high_beams|detailed_low_beams|detailed_rear_fog_lights|detailed_front_fog_lights|doors_hood|detailed_running_lights",names(device_aggr))]
aux_non_ecology=device_aggr[grep(".fuel|total_distance|total_driving_time|total_engine_hours|detailed_brake_pedal",names(device_aggr))]
aux_parking=device_aggr[grep("detailed_reverse_gear",names(device_aggr))]

#Calculate the weights
a1=alpha(aux_non_economy,check.keys = T)
a1$keys
a2=alpha(aux_danger,check.keys = T)
a2$keys
a3=alpha(aux_non_ecology,check.keys = T)
a3$keys

a1$total#G6(smc) 0.9568
a2$total#G6(smc)0.7749  
a3$total#G6(smc) 0.8513

#aux_danger=device_aggr[grep("vehicle_speed|engine_rpm|detailed_high_beams|detailed_running_lights|detailed_low_beams|detailed_brake_pedal|detailed_handbrake",names(device_aggr))]
#aux_danger_other_conditions=device_aggr[grep("doors_hood|detailed_trunk|doors_trunk|detailed_rear_fog_lights|detailed_front_fog_lights|detailed_running_lights|detailed_high_beams|detailed_low_beams",names(device_aggr))]
#a3=alpha(aux_danger,check.keys = T)
#a3$total#G6(smc) 0.7710589
#a4=alpha(aux_danger_other_conditions,check.keys = T)
#a4$total#G6(smc) 0.8548306


factors_drivers=as.data.frame(rowSums(device_aggr[,names(device_aggr) %in% c("total_idle_fuel_used","total_distance","total_fuel_used","mileage","total_engine_hours","total_driving_time","total_engine_idle_time","detailed_air_conditioning","detailed_trunk","doors_driver_door","doors_passenger_door","doors_rear_left_door","doors_rear_right_door","detailed_doors","altitude","total_brake_pedal")]))/16
names(factors_drivers)[1]<-"Non_Economy"
factors_drivers$Non_Ecology=rowSums(device_aggr[,names(device_aggr) %in% c(".fuel","total_distance","total_driving_time","total_engine_hours","total_brake_pedal")])/7
factors_drivers$Danger=rowSums(device_aggr[,names(device_aggr) %in% c("vehicle_speed","engine_rpm","detailed_high_beams","detailed_low_beams","detailed_rear_fog_lights","detailed_front_fog_lights","doors_hood","detailed_running_lights")])/8
factors_drivers$Parking=device_aggr[,names(device_aggr) %in% c("detailed_reverse_gear")]

#factors_drivers$F4=rowSums(device_aggr[,names(device_aggr) %in% c("doors_hood","detailed_trunk","doors_trunk","detailed_rear_fog_lights","detailed_front_fog_lights","detailed_running_lights","detailed_high_beams","detailed_low_beams")])/8

# use 3/4 of the available observations for training purposes
#set.seed(2)
#smp_size <- floor(0.75 * nrow(factors_drivers))
#train_ind <- sample(seq_len(nrow(factors_drivers)), size = smp_size)
#train <- factors_drivers[train_ind, ]
#test <- factors_drivers[-train_ind, ]

#k-means
km.out2=kmeans(factors_drivers,4,nstart=20)
km.out2
km.out2$cluster
km.out2$centers
windows()
plot(factors_drivers, col=(km.out2$cluster +1), main="K-Means ClusteringResults with K=4",  pch=20, cex=2)


#Inspect the devices with highest speed
windows()
boxplot(can_gps$vehicle_speed, boxwex=0.1, main="Vehicle_speed",col="blue", boxwex=0.1)

max_speed=max(can_gps$vehicle_speed)#206
can_gps$device_id[can_gps$vehicle_speed==max_speed]#[1] 64

device_aggr_new=can_gps[can_gps$vehicle_speed!=0,]
device_aggr_new1=aggregate(device_aggr_new[,-c(1,2,10,11,12,43)], by=list(device_aggr_new$device_id), FUN="mean")
max_speed_aggr=max(device_aggr_new1$vehicle_speed)#92
names(device_aggr_new1)[1]<-"device_id"
device_aggr_new1$device_id[device_aggr_new1$vehicle_speed==max_speed_aggr]#[1] 95

#Inspect outliers for economy and ecology
windows()
boxplot(device_aggr$total_fuel_used, boxwex=0.1, main="Total fuel used",col="blue", boxwex=0.1)
max_fuel=max(device_aggr$total_fuel_used)#2 397 647
device_aggr$device_id[device_aggr$total_fuel_used==max_fuel]#[1] 15

windows()
boxplot(device_aggr$total_idle_fuel_used, boxwex=0.1, main="Total idle fuel used",col="blue", boxwex=0.1)
max_idle_fuel=max(device_aggr$total_idle_fuel_used)#561775
device_aggr$device_id[device_aggr$total_idle_fuel_used==max_idle_fuel]#[1] 35

#Inspect parking outliers
windows()
boxplot(device_aggr$detailed_reverse_gear, boxwex=0.1, main="Total movements of reverse gear",col="blue", boxwex=0.1)
max_reverse_gear=max(device_aggr$detailed_reverse_gear)#17.65%
device_aggr$device_id[device_aggr$detailed_reverse_gear==max_reverse_gear]#[1] 64

#Finding second outlier
aggr_for_gear=device_aggr[!device_aggr$device_id==64,]
max_reverse_gear2=max(aggr_for_gear$detailed_reverse_gear)#13.82%
aggr_for_gear$device_id[aggr_for_gear$detailed_reverse_gear==max_reverse_gear2]#[1] 169

# Classification--------
#Ecology----
#rm(aux_ecology)
#aux_non_ecology=device_aggr[grep(".fuel|total_distance|total_driving_time|total_engine_hours|detailed_brake_pedal",names(device_aggr))]
aux_non_ecology$device_id=device_aggr$device_id
aux_non_ecology$response=0
library(ISLR)
library(MASS)

aux_non_ecology$Sums=rowSums(aux_non_ecology[,1:7])
summary(aux_non_ecology$Sums)
aux_non_ecology$response=ifelse(aux_non_ecology$Sums>=220139,1,0)
names(aux_non_ecology)
attach(aux_non_ecology)

#Logistic regression application--
eq1=glm(response~total_distance+total_fuel_used+total_engine_hours+total_idle_fuel_used+detailed_fuel_low+detailed_brake_pedal,
        data=aux_non_ecology,family=binomial)
summary(eq1)

# forecasting in-sample probabilities using eq1
eq1Pr=predict.glm(eq1, newdata=aux_non_ecology, type = "response")
aux_non_ecology$predicted=eq1Pr
summary(eq1Pr)
aux_non_ecology$response=ifelse(aux_non_ecology$predicted>=0.51,1,0)
sum(aux_non_ecology$response) #37device_id behave "non-ecologically"
aux_non_ecology$lat=device_aggr$latitude
aux_non_ecology$lng=device_aggr$longitude

#Ecological data---
#https://www.r-graph-gallery.com/4-tricks-for-working-with-r-leaflet-and-shiny/
# Load libraries
install.packages("shiny")
library(shiny)
library(leaflet)

# Make data with several positions
data_red=data.frame(LONG=aux_non_ecology$lng[aux_non_ecology$response==1], LAT=aux_non_ecology$lat[aux_non_ecology$response==1], PLACE=paste("Non-eco Device_id",aux_non_ecology$device_id[aux_non_ecology$response==1]))
data_green=data.frame(LONG=aux_non_ecology$lng[aux_non_ecology$response==0], LAT=aux_non_ecology$lat[aux_non_ecology$response==0], PLACE=paste("Eco Device_id",aux_non_ecology$device_id[aux_non_ecology$response==0]))

# Initialize the leaflet map:
leaflet() %>% 
  setView(lng=23, lat=42, zoom=8 ) %>%
  # Initialize the leaflet map:
  leaflet() %>% 
  setView(lng=23, lat=42, zoom=8 ) %>%
  
  # Add two tiles
  addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group="background 2") %>%
  
  # Add 2 marker groups
  addCircleMarkers(data=data_red, lng=~LONG , lat=~LAT, radius=5 , color="black",  fillColor="red", stroke = TRUE, fillOpacity = 0.8, group="Red") %>%
  addCircleMarkers(data=data_green, lng=~LONG , lat=~LAT, radius=5 , color="black",  fillColor="green", stroke = TRUE, fillOpacity = 0.8, group="Blue") %>%
  
  # Add the control widget
  addLayersControl(overlayGroups = c("Non-eco","Eco") , baseGroups = c("background 2"), options = layersControlOptions(collapsed = FALSE))

#Economy----
#rm(aux_non_economy)
#aux_non_economy=device_aggr[grep("total_idle_fuel_used|total_distance|total_fuel_used|mileage|total_engine_hours|total_driving_time|total_engine_idle_time|detailed_air_conditioning|detailed_trunk|doors_driver_door|doors_passenger_door|doors_rear_left_door|doors_rear_right_door|detailed_doors|altitude",names(device_aggr))]
aux_non_economy$device_id=device_aggr$device_id
aux_non_economy$response=0
library(ISLR)
library(MASS)

aux_non_economy$Sums=rowSums(aux_non_economy[,1:16])
summary(aux_non_economy$Sums)
aux_non_economy$response=ifelse(aux_non_economy$Sums>=293874,1,0)
names(aux_non_economy)
attach(aux_non_economy)

#Logistic regression application--
eq2=glm(response~total_idle_fuel_used+total_distance+total_fuel_used+mileage+total_engine_hours+ detailed_air_conditioning+ detailed_trunk+  doors_driver_door+ doors_passenger_door + doors_rear_left_door+ doors_rear_right_door+  
            detailed_doors+ altitude +detailed_brake_pedal ,data=aux_non_economy,family=binomial)
summary(eq2)

# forecasting in-sample probabilities using eq1
eq2Pr=predict.glm(eq2, newdata=aux_non_economy, type = "response")
aux_non_economy$predicted=eq2Pr
summary(eq2Pr)
aux_non_economy$response=ifelse(aux_non_economy$predicted>=0.51,1,0)
sum(aux_non_economy$response) #39 device_id behave "uneconomical"
aux_non_economy$lat=device_aggr$latitude
aux_non_economy$lng=device_aggr$longitude

#Plotting results for economy--
library(shiny)
library(leaflet)

# Make data with several positions
data2_red=data.frame(LONG=aux_non_economy$lng[aux_non_economy$response==1], LAT=aux_non_economy$lat[aux_non_economy$response==1], PLACE=paste("Non-economy Device_id",aux_non_economy$device_id[aux_non_economy$response==1]))
data2_green=data.frame(LONG=aux_non_economy$lng[aux_non_economy$response==0], LAT=aux_non_economy$lat[aux_non_economy$response==0], PLACE=paste("Economy Device_id",aux_non_economy$device_id[aux_non_economy$response==0]))

# Initialize the leaflet map:
leaflet() %>% 
  setView(lng=23, lat=42, zoom=8 ) %>%
  # Initialize the leaflet map:
  leaflet() %>% 
  setView(lng=23, lat=42, zoom=8 ) %>%
  
  # Add two tiles
  addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group="background 2") %>%
  
  # Add 2 marker groups
  addCircleMarkers(data=data2_red, lng=~LONG , lat=~LAT, radius=5 , color="black",  fillColor="red", stroke = TRUE, fillOpacity = 0.8, group="Red") %>%
  addCircleMarkers(data=data2_green, lng=~LONG , lat=~LAT, radius=5 , color="black",  fillColor="green", stroke = TRUE, fillOpacity = 0.8, group="Green") %>%
  
  # Add the control widget
  addLayersControl(overlayGroups = c("Non-economy","Economy") , baseGroups = c("background 2"), options = layersControlOptions(collapsed = FALSE))

#Danger----
#rm(aux_danger)
#aux_danger=device_aggr[grep("vehicle_speed|engine_rpm|detailed_brake_pedal|detailed_handbrake|detailed_high_beams|detailed_low_beams|detailed_rear_fog_lights|detailed_front_fog_lights|doors_hood",names(device_aggr))]

aux_danger$device_id=device_aggr$device_id
aux_danger$response=0
library(ISLR)
library(MASS)

aux_danger$Sums=rowSums(aux_danger[,1:8])
summary(aux_danger$Sums)
aux_danger$response=ifelse(aux_danger$Sums>=1275,1,0)
names(aux_danger)
attach(aux_danger)

#Logistic regression application
#aux_danger=device_aggr[grep("vehicle_speed|engine_rpm|detailed_brake_pedal|detailed_handbrake|detailed_high_beams|detailed_low_beams|detailed_rear_fog_lights|detailed_front_fog_lights|doors_hood",names(device_aggr))]
eq3=glm(response~vehicle_speed+engine_rpm++detailed_running_lights+detailed_high_beams+detailed_low_beams+detailed_rear_fog_lights+detailed_front_fog_lights+doors_hood,data=aux_danger,family=binomial)
summary(eq3)

# forecasting in-sample probabilities using eq1
eq3Pr=predict.glm(eq3, newdata=aux_danger, type = "response")
aux_danger$predicted=eq3Pr
summary(eq3Pr)
aux_danger$response=ifelse(aux_danger$predicted>0.538,1,0)
sum(aux_danger$response) # 96 device_id behave "dangerously"
aux_danger$lat=device_aggr$latitude
aux_danger$lng=device_aggr$longitude

#Plotting results for danger-
library(shiny)
library(leaflet)

# Make data with several positions
data3_red=data.frame(LONG=aux_danger$lng[aux_danger$response==1], LAT=aux_danger$lat[aux_danger$response==1], PLACE=paste("Dangerous Device_id",aux_danger$device_id[aux_danger$response==1]))
data3_green=data.frame(LONG=aux_danger$lng[aux_danger$response==0], LAT=aux_danger$lat[aux_danger$response==0], PLACE=paste("Safe Device_id",aux_danger$device_id[aux_danger$response==0]))

# Initialize the leaflet map:
leaflet() %>% 
  setView(lng=23, lat=42, zoom=8 ) %>%
  # Initialize the leaflet map:
  leaflet() %>% 
  setView(lng=23, lat=42, zoom=8 ) %>%
  
  # Add two tiles
  addProviderTiles("Esri.WorldImagery", group="background 1") %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group="background 2") %>%
  
  # Add 2 marker groups
  addCircleMarkers(data=data3_red, lng=~LONG , lat=~LAT, radius=5 , color="black",  fillColor="red", stroke = TRUE, fillOpacity = 0.8, group="Red") %>%
  addCircleMarkers(data=data3_green, lng=~LONG , lat=~LAT, radius=5 , color="black",  fillColor="green", stroke = TRUE, fillOpacity = 0.8, group="Green") %>%
  
  # Add the control widget
  addLayersControl(overlayGroups = c("Dangerous","Safe") , baseGroups = c("background 2"), options = layersControlOptions(collapsed = FALSE))


#Plotting data------------
#Plot 1 - leaflet map with devices' location
#install.packages("leaflet")
library(leaflet)
device_aggr %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())
leaflet() %>%
  derivePoints(lng = device_aggr$longitude, lat = device_aggr$latitude, missingLng = missing(lng),
               missingLat = missing(lat), funcName = "f")%>%
  addTiles() %>%
  addCircleMarkers(weight = 0.1, color = "red") %>%
  addRectangles(lat1 = (min(device_aggr$latitude)-0.05), lng1 = (max(device_aggr$longitude)-0.05),
                lat2 = (min(device_aggr$latitude)+0.05), lng2 = (min(device_aggr$longitude)+0.05),
                fillColor = "transparent")


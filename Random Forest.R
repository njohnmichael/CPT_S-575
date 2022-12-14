#  title: "CPT_S 575 Project"
#author: "Najarian & Shatanawi"
# Method: Random Forest
#date: "2022-12-13"
#output: pdf_document
##Load data and libraries
library(readxl)

columbia_river_flow_data_2015_2022<-read.csv("C:/Users/sshat/Desktop/Data_science_project/columbia_river_flow_data_2015-2022.csv", sep = ",", header = TRUE)
#View(columbia_river_flow_data_2015_2022)
library(ggplot2)
library(tidyr)
library(dplyr)


#Fill Missing Vlaues

#install.packages("imputeTS")
library(imputeTS)


#Rivers Inflow
columbia_river_flow_data_2015_2022$Wells_Total_Discharge <- c(na_ma(columbia_river_flow_data_2015_2022$Wells_Total_Discharge, k = 1))
columbia_river_flow_data_2015_2022$Chelan_Flow <- c(na_ma(columbia_river_flow_data_2015_2022$Chelan_Flow, k = 1))
columbia_river_flow_data_2015_2022$Entiat_Flow <- c(na_ma(columbia_river_flow_data_2015_2022$Entiat_Flow, k = 1))
columbia_river_flow_data_2015_2022$Wenatchee_Flow <- c(na_ma(columbia_river_flow_data_2015_2022$Wenatchee_Flow, k = 1))

#Inflow
columbia_river_flow_data_2015_2022$Rocky_Reach_Inflow <- c(na_ma(columbia_river_flow_data_2015_2022$Rocky_Reach_Inflow, k = 1))
columbia_river_flow_data_2015_2022$Rock_Island_Inflow <- c(na_ma(columbia_river_flow_data_2015_2022$Rock_Island_Inflow, k = 1))

#Discharge
columbia_river_flow_data_2015_2022$Rocky_Reach_Total_Discharge <- c(na_ma(columbia_river_flow_data_2015_2022$Rocky_Reach_Total_Discharge, k = 1))
columbia_river_flow_data_2015_2022$Rock_Island_Total_Discharge <- c(na_ma(columbia_river_flow_data_2015_2022$Rock_Island_Total_Discharge, k = 1))

#Headwaters
columbia_river_flow_data_2015_2022$Rocky_Reach_Headwater_Elevation <- c(na_ma(columbia_river_flow_data_2015_2022$Rocky_Reach_Headwater_Elevation, k = 1))
columbia_river_flow_data_2015_2022$Rock_Island_Headwater_Elevation <- c(na_ma(columbia_river_flow_data_2015_2022$Rock_Island_Headwater_Elevation, k = 1))

#Time
datetime_utc <- columbia_river_flow_data_2015_2022$datetime_utc
datetime_stamp <- columbia_river_flow_data_2015_2022$datetime_stamp

#Construct clean Matrix
#Clean_CR_FlowData <- data.frame(datetime_utc, datetime_stamp, Rocky_Reach_Inflow, Rock_Island_Inflow, Rocky_Reach_Total_Discharge, Rock_Island_Total_Discharge, #Rocky_Reach_Headwater_Elevation, Rock_Island_Headwater_Elevation)


#Convert from 5 min data to hrouly data 

columbia_river_flow_data_2015_2022$datetime_stamp=as.POSIXct(strptime(columbia_river_flow_data_2015_2022$datetime_stamp, 
                                                                      format = "%m/%d/%Y %H:%M"))

hr.means <- aggregate(columbia_river_flow_data_2015_2022[c("Wells_Total_Discharge","Chelan_Flow","Entiat_Flow","Rocky_Reach_Inflow","Rocky_Reach_Total_Discharge","Wenatchee_Flow","Rock_Island_Inflow","Rock_Island_Total_Discharge","Rock_Island_Headwater_Elevation","Rocky_Reach_Headwater_Elevation")], format(columbia_river_flow_data_2015_2022["datetime_stamp"],"%m/%d/%Y %H"),
                      mean, na.rm = TRUE) 



#Splitting 


columbia_river_new<-hr.means  %>%
  separate(datetime_stamp, into =c("date","time"), sep = " ")


columbia_river_new_2<-columbia_river_new  %>%
  separate(date, into =c("month","day","year"), sep = "/")

columbia_river_new_3 <-subset(columbia_river_new_2,year!=2015)

columbia_river_new_4 <-subset(columbia_river_new_3,year!=2022)

str(columbia_river_new_4)


#UPSTREAM ONLY


#columbia_river_new_5 <-subset(columbia_river_new_4,year!=2022)
# Remove using subset
#df2 <- subset(df, select = -c(id, name, chapters))
Upstream_data_withdischarge <-subset(columbia_river_new_4, select =-c(Wenatchee_Flow,Rock_Island_Total_Discharge,Rock_Island_Headwater_Elevation,Rock_Island_Inflow))
#Upstream_data_with_OUT_discharge <-subset(columbia_river_new_4, select =-c(Wenatchee_Flow,Rock_Island_Total_Discharge,Rock_Island_Headwater_Elevation,Rock_Island_Inflow, Rocky_Reach_Total_Discharge))

write.csv(Upstream_data_with_discharge, "C:\\Users\\sshat\\Desktop\\Data_science_project\\Upstream_with_discharge.csv",row.names=FALSE)



# split the data into training and testing



Training_data <-subset(Upstream_data_withdischarge,year!=2021)
Training_data <-subset(Training_data,year!=2020)
Testing_data <-subset(Upstream_data_withdischarge,year==2020)


# Building a regression code using Random Forest
#install the required library

#install.packages("randomForest")
# Load the library
library(randomForest)

### Import libraries
library(randomForest)
library(ggplot2)


#build the model

set.seed(71)
RockyReach_WaterDischarge.rf <- randomForest(Rocky_Reach_Total_Discharge ~ .,data=Training_data, mtry=4, importance=TRUE, na.action = na.omit)
print(RockyReach_WaterDischarge.rf)

which.min(RockyReach_WaterDischarge.rf$mse)


sqrt(RockyReach_WaterDischarge.rf$mse[which.min(RockyReach_WaterDischarge.rf$mse)])

plot(RockyReach_WaterDischarge.rf)

#produce variable importance plot
varImpPlot(RockyReach_WaterDischarge.rf) 


#Tuning the OOL

#RockyReach_WaterElevation.rf
mtry <- tuneRF(Training_data[-9],Training_data$Rocky_Reach_Total_Discharge, ntreeTry=500,mytryStart=5,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)


# build the model adain with the best paramaters
set.seed(71)
RockyReach_WaterDischarge_best2.rf <- randomForest(Rocky_Reach_Total_Discharge ~ .,data=Training_data, mtry=2, importance=TRUE, na.action = na.omit)
print(RockyReach_WaterDischarge_best2.rf)

# Test it over testing data

WaterDischargePred_best2 <- predict(RockyReach_WaterDischarge_best2.rf, Testing_data)

print (mean((WaterDischargePred_best2-Training_data$Rocky_Reach_Total_Discharge)^2))


#plotting 

columbia_river_new_4_plot<- unite(columbia_river_new_4,month,day,year,time, col = "timeseries", sep = " ")
str(columbia_river_new_4_plot)

ggplot(columbia_river_new_4_plot, aes(x = timeseries,y = Rock_Island_Total_Discharge))+
  geom_point(size = .01, color = "blue", shape = 1) +
  ggtitle("Rock Island Dam- Hourly Discharge from 2016-2020")+
  xlab("Time")+
  ylab("Water Discharge (Kcfs)")
#+ylim(700,708)

plot_2020<- unite(Testing_data,month,day,year,time, col = "timeseries", sep = " ")

ggplot(plot_2020, aes(x = timeseries,y = Rocky_Reach_Total_Discharge))+
  
  
  geom_point(size = .01, color = "blue", shape = 1) +
  ggtitle("Rocky Reach Dam- Hourly water discharge for 2020")+
  xlab("Time")+
  ylab("Water Discharge (kcfs)")
#+ylim(700,708)


#actualandprediction = data.frame(actual=Testing_data$Rocky_Reach_Total_Discharge, prediction=WaterDischargePred_best2)
#ggplot (actualandprediction,aes(x=actual,y=prediction)+
#          geom_point()
#)

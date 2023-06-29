#Lindsey Bell
#6/28/2023
#Calculating flux footprint for US-CMW

library(lubridate)
library(dplyr)

CMW <- read.csv("C:/Users/Asus PC/Documents/R_Projects/Footprints/data/AMF_US-CMW_BASE_HH_2-5.csv", na.strings = "-9999", header = TRUE, sep = ",", skip = 2)
can_h <- 14

CMW$TIMESTAMP_START <- ymd_hm(as.character(CMW$TIMESTAMP_START))
CMW$day <- day(CMW$TIMESTAMP_START)
CMW$mm <- month(CMW$TIMESTAMP_START)
CMW$yyyy <- year(CMW$TIMESTAMP_START)
CMW$MM <- minute(CMW$TIMESTAMP_START)
CMW$HH_UTC <- hour(CMW$TIMESTAMP_START)
CMW$zm <- 14
CMW$d <- (2/3)*can_h
CMW$z0 <- 0.1*can_h
CMW$u_mean <- mean(CMW$WS_1_1_1, na.rm = TRUE)
CMW$L <- (-(((CMW$USTAR)^3)*(CMW$TA_1_1_1 + 273)) / (0.4*9.8*((CMW$H) / (1.25*1004))))

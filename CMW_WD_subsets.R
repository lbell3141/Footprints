#7/12/2023
#Subsetting CMW data by wind direction to compare fluxes 

#loading data and libraries
library(lubridate)
library(dplyr)
library(ggplot2)
dat_file <- read.csv("C:/Users/lindseybell/OneDrive - University of Arizona/Documents/Footprints/data/AMF_US-CMW_BASE_HH_2-5.csv", na.strings = "-9999", header = TRUE, sep = ",", skip = 2)

meas_h <- 14
d <- (2/3) * meas_h
dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START))
mean_WS <- mean(dat_file$WS_1_1_1, na.rm = TRUE)

dat_voi = dat_file %>%
  mutate(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    zm = meas_h,
    d = d, 
    u_mean = mean(WS_1_1_1, na.rm = TRUE),
    wind_sp = WS_1_1_1,
    L = (-((USTAR^3) * (TA_1_1_1 + 273)) / (0.4 * 9.8 * (H / (1.25 * 1004)))),
    H = H,
    temp_atmos = TA_1_1_1,
    sigma_v = sqrt((u_mean*((-1.3*L + 0.1)^2))/100000),
    u_star = USTAR,
    wind_dir = WD_1_1_1,
    test = zm/L,
    #adding associated fluxes
    gpp = GPP_PI,
    nee = NEE_PI,
    reco = RECO_PI,
    le = LE,
    ppfd = PPFD_IN_PI_F,
    precip = P
  ) %>%
  filter(test >= -15.5)%>%
  filter(u_star > 0.1)%>%
  filter(ppfd != 0)%>%
  select(yyyy, mm, doy, day, HH_UTC, MM, wind_sp, L, u_star, wind_dir, temp_atmos, H, gpp, nee, reco, le, ppfd, precip)%>%
  filter(if_any(everything(), ~ . != "NA"))%>%
  # filter(yyyy == 2017)%>%
  #filtering for 3 sequential sunny days with moderate temps, WS with 0.5 of the mean, and no precip events w/i 5 days
  filter(HH_UTC %in% 8:17)%>%
  filter(lag(precip) == 0, lead(precip) == 0)%>%
  filter(precip == 0)%>%
  filter(ppfd %in% 600:1800)%>%  
  filter(temp_atmos >= 15 & temp_atmos <= 35)%>%
  filter(wind_dir %in% c(270:350, 90:170))%>%
  filter(wind_sp >= 0.5 & wind_sp <= 2.5) 

%>%
  #filter(yyyy == 2005)

#%>%
%>%
 # filter(wind_sp <= (mean_WS + 1.5) & wind_sp >= (mean_WS - 1.5))%>%
 filter(wind_sp %in% 0.5:2.5) #%>%
  # filter(lag(ppfd)>500, lead(ppfd)>500)%>%
  
  filter(wind_dir %in% c(270:350, 90:170))



var_cols = c("wind_sp", "L", "u_star", "wind_dir", "temp_atmos", "H","gpp", "nee", "reco", "le", "ppfd", "precip")
daily_means_voi = dat_voi%>%
  group_by(day)%>%
  summarize(across(all_of(var_cols), mean, na.rm = TRUE)) #%>%
#distinct()

dat_voi_A <- dat_voi%>%
  filter(wind_dir %in% 270:350)
plot(dat_voi_A$doy, dat_voi_A$gpp, col = "red")

dat_voi_B <- dat_voi%>%
  filter(wind_dir %in% 90:170)
plot(dat_voi_B$doy, dat_voi_B$gpp, col = "blue")

plot(dat_voi_A$doy, dat_voi_A$gpp, col = "red")
points(dat_voi_B$doy, dat_voi_B$gpp, col = "blue")
legend("topright", legend = c("Southeastern WD", "Northwestern WD"), col = c("red", "blue"), pch = 16)

plot(dat_voi$wind_dir, dat_voi$gpp)
#--------------
#frequency histograms for filter variables
dat_voi = dat_file %>%
  mutate(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    zm = meas_h,
    d = d, 
    u_mean = mean(WS_1_1_1, na.rm = TRUE),
    wind_sp = WS_1_1_1,
    L = (-((USTAR^3) * (TA_1_1_1 + 273)) / (0.4 * 9.8 * (H / (1.25 * 1004)))),
    H = H,
    temp_atmos = TA_1_1_1,
    sigma_v = sqrt((u_mean*((-1.3*L + 0.1)^2))/100000),
    u_star = USTAR,
    wind_dir = WD_1_1_1,
    test = zm/L,
    #adding associated fluxes
    gpp = GPP_PI,
    nee = NEE_PI,
    reco = RECO_PI,
    le = LE,
    ppfd = PPFD_IN_PI_F,
    precip = P
  ) %>%
  filter(test >= -15.5)%>%
  filter(u_star > 0.1)%>%
  select(yyyy, mm, doy, day, HH_UTC, MM, wind_sp, L, u_star, wind_dir, temp_atmos, H, gpp, nee, reco, le, ppfd, precip)%>%
  filter(if_any(everything(), ~ . != "NA"))%>%
  filter(HH_UTC %in% 8:17)
         

par(mfrow = c(1, 1))
hist(dat_voi$temp_atmos) #20:30
hist(dat_voi$ppfd) #1000:1500
hist(dat_voi$wind_sp) #0.5:2
hist(dat_voi$wind_dir) #150, 250, 350
hist(dat_voi$precip)
#------------------
dat_voi_280 = dat_voi%>%
  filter(doy ==280)%>%
  filter(yyyy ==2017)
plot(dat_voi_280$HH_UTC, dat_voi_280$gpp)

dat_voi = dat_voi%>%
  filter(yyyy == 2017)
plot(dat_voi$HH_UTC, dat_voi$gpp)

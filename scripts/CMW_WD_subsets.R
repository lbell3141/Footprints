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

dat_voi <- dat_file %>%
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
  filter(across(everything(), ~ . != "NA"))%>%
  filter(yyyy == 2017)%>%
  #filtering for 3 sequential sunny days with moderate temps, WS with 0.5 of the mean, and no precip events w/i 5 days
  filter(HH_UTC %in% 7:19)%>%
  filter(ppfd > 600)%>%
  filter(lag(ppfd)>600, lead(ppfd)>600)%>%
  filter(temp_atmos %in% 14:35)%>%
  filter(wind_sp <= (mean(wind_sp)+1) & wind_sp >= (mean(wind_sp)-1))%>%
  filter(lag(precip) == 0, lead(precip) == 0)

var_cols = c("wind_sp", "L", "u_star", "wind_dir", "temp_atmos", "H","gpp", "nee", "reco", "le", "ppfd", "precip")
daily_means_voi = dat_voi%>%
  mutate(d_ten = ceiling(doy / 10)) %>%
  group_by(d_ten)%>%
  summarize(across(all_of(var_cols), mean, na.rm = TRUE)) #%>%
#distinct()

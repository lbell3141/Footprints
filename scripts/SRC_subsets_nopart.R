#7/17/2023
#Subsetting SRC by wind direction to compare magnitude of fluxes 

#--------------------------------------------------------------------------------
#loading data and libraries; defining variables
library(lubridate)
library(dplyr)
library(ggplot2)
library(zoo)
dat_file <- read.csv("C:/Users/lindseybell/OneDrive - University of Arizona/Documents/Footprints/data/AMF_US-SRC_FLUXNET_SUBSET_HH_2008-2014_3-5.csv", na.strings = "-9999", header = TRUE, sep = ",", skip = 0)
meas_h <- 3.75
d <- (2/3) * meas_h
dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START)) 

#--------------------------------------------------------------------------------

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
    u_mean = mean(WS_F, na.rm = TRUE),
    wind_sp = WS_F,
    L = (-((USTAR^3) * (TA_F + 273)) / (0.4 * 9.8 * (H_F_MDS / (1.25 * 1004)))),
    H = H_F_MDS,
    temp_atmos = TA_F,
    sigma_v = sqrt((u_mean*((-1.3*L + 0.1)^2))/100000),
    u_star = USTAR,
    wind_dir = WD,
    test = zm/L,
    #adding associated fluxes
    gpp = GPP_PI,
    nee = NEE_PI,
    reco = RECO_PI,
    le = LE,
    ppfd = PPFD_IN_PI_F,
    precip = P,
    rel_h = RH_1_1_1,
    swc = SWC_PI_1_1_A
  ) %>%
  filter(test >= -15.5)%>%
  filter(u_star > 0.1)%>%
  select(yyyy, mm, doy, day, HH_UTC, MM, wind_sp, L, u_star, wind_dir, temp_atmos, H, gpp, nee, reco, le, ppfd, precip, rel_h, swc)%>%
  filter(if_any(everything(), ~ . != "NA"))%>%
  filter(HH_UTC %in% 8:17)


par(mfrow = c(1, 1))
hist(dat_voi$temp_atmos) #20:30
hist(dat_voi$ppfd) #1000:1500
hist(dat_voi$wind_sp) #0.5:2
hist(dat_voi$wind_dir) #150, 250, 350
hist(dat_voi$precip) #0
hist(dat_voi$swc)

#---------------------------------------------------------------------------------
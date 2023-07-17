#generating SRC footprint for online FFP calculator 

library(lubridate)
library(dplyr)

dat_file <- read.csv("C:/Users/lindseybell/OneDrive - University of Arizona/Documents/Footprints/data/AMF_US-SRC_FLUXNET_SUBSET_HH_2008-2014_3-5.csv", na.strings = "-9999", header = TRUE, sep = ",", skip = 0)

meas_h <- 3.75
d <- (2/3) * meas_h
#bound_h <- 1000

dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START))

#daytime
dat_ffp <- dat_file %>%
  mutate(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    zm = meas_h,
    d = d, 
    #h = bound_h,
    z0 = "-999",
    u_mean = mean(WS_F, na.rm = TRUE),
    L = (-((USTAR^3) * (TA_F + 273)) / (0.4 * 9.8 * (H_F_MDS / (1.25 * 1004)))),
    sigma_v = sqrt((u_mean*((-1.3*L + 0.1)^2))/100000),
    u_star = USTAR,
    wind_dir = WD,
    test = zm/L
  ) %>%
  filter(test >= -15.5)%>%
  filter(u_star > 0.1)%>%
  select(yyyy, mm, day, HH_UTC, MM, zm, d, z0, u_mean, L, sigma_v, u_star, wind_dir)%>%
  filter(across(everything(), ~ . != "NA"))%>%
  filter(HH_UTC %in% c(6:19))%>%
  filter(yyyy == 2012)

dat_ffp$L = as.numeric(dat_ffp$L)
dat_ffp$sigma_v = as.numeric(dat_ffp$sigma_v)
dat_ffp$u_star = as.numeric(dat_ffp$u_star)
dat_ffp$wind_dir = as.numeric(dat_ffp$wind_dir)

write.csv(dat_ffp, "SRC_dt.csv", row.names = FALSE)

#Nighttime
dat_ffp <- dat_file %>%
  mutate(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    zm = meas_h,
    d = d, 
    #h = bound_h,
    z0 = "-999",
    u_mean = mean(WS_F, na.rm = TRUE),
    L = (-((USTAR^3) * (TA_F + 273)) / (0.4 * 9.8 * (H_F_MDS / (1.25 * 1004)))),
    sigma_v = sqrt((u_mean*((-1.3*L + 0.1)^2))/100000),
    u_star = USTAR,
    wind_dir = WD,
    test = zm/L
  ) %>%
  filter(test >= -15.5)%>%
  filter(u_star > 0.1)%>%
  select(yyyy, mm, day, HH_UTC, MM, zm, d, z0, u_mean, L, sigma_v, u_star, wind_dir)%>%
  filter(across(everything(), ~ . != "NA"))%>%
  filter(HH_UTC %in% c(0:5, 20:23))%>%
  filter(yyyy == 2012)

dat_ffp$L = as.numeric(dat_ffp$L)
dat_ffp$sigma_v = as.numeric(dat_ffp$sigma_v)
dat_ffp$u_star = as.numeric(dat_ffp$u_star)
dat_ffp$wind_dir = as.numeric(dat_ffp$wind_dir)

write.csv(dat_ffp, "SRC_nt.csv", row.names = FALSE)

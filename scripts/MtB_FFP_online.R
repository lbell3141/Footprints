#footprint for MtB

library(lubridate)
library(dplyr)

dat_file <- read.csv("C:/Users/lindseybell/OneDrive - University of Arizona/Documents/Footprints/data/AMF_US-MtB_BASE_HH_4-5.csv",  na.strings = "-9999", header = TRUE, sep = ",", skip = 2)

meas_h <- 29.8
d <- (2/3) * meas_h
bound_h <- 1000



#formatting for online template
dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START))

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
    u_mean = mean(WS, na.rm = TRUE),
    L = (-((USTAR^3) * (TA_1_1_1 + 273)) / (0.4 * 9.8 * (H / (1.25 * 1004)))),
    sigma_v = sqrt((u_mean*((-1.3*L + 0.1)^2))/100000),
    u_star = USTAR,
    wind_dir = WD,
    test = zm/L
  ) %>%
  filter(test >= -15.5)%>%
  filter(u_star > 0.1)%>%
  select(yyyy, mm, day, HH_UTC, MM, zm, d, z0, u_mean, L, sigma_v, u_star, wind_dir)%>%
  filter(across(everything(), ~ . != "NA"))%>%
  #filter(test >= -15.5)%>%
  filter(HH_UTC %in% c(0:6, 20:24))%>%
  #filter(mm >= 8, mm <= 10)%>%
  # filter(day == 5)%>%
  filter(yyyy == 2017)


dat_ffp$L = as.numeric(dat_ffp$L)
dat_ffp$sigma_v = as.numeric(dat_ffp$sigma_v)
dat_ffp$u_star = as.numeric(dat_ffp$u_star)
dat_ffp$wind_dir = as.numeric(dat_ffp$wind_dir)

write.csv(dat_ffp, "MtB.csv", row.names = FALSE)
#----------------------------------------------------
#daytime 
dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START))

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
    u_mean = mean(WS, na.rm = TRUE),
    L = (-((USTAR^3) * (TA_1_1_1 + 273)) / (0.4 * 9.8 * (H / (1.25 * 1004)))),
    sigma_v = sqrt((u_mean*((-1.3*L + 0.1)^2))/100000),
    u_star = USTAR,
    wind_dir = WD,
    test = zm/L
  ) %>%
  filter(test >= -15.5)%>%
  filter(u_star > 0.1)%>%
  select(yyyy, mm, day, HH_UTC, MM, zm, d, z0, u_mean, L, sigma_v, u_star, wind_dir)%>%
  filter(across(everything(), ~ . != "NA"))%>%
  #filter(test >= -15.5)%>%
  filter(HH_UTC %in% 6:19)%>%
  #filter(mm >= 8, mm <= 10)%>%
  # filter(day == 5)%>%
  filter(yyyy == 2017)


dat_ffp$L = as.numeric(dat_ffp$L)
dat_ffp$sigma_v = as.numeric(dat_ffp$sigma_v)
dat_ffp$u_star = as.numeric(dat_ffp$u_star)
dat_ffp$wind_dir = as.numeric(dat_ffp$wind_dir)

write.csv(dat_ffp, "MtB_DT.csv", row.names = FALSE)

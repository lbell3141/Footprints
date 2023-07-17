#DT/NT FFPs for LS1 and LS2 for Kljun online FFP calculator template 
#generating SRC footprint for online FFP calculator 

library(lubridate)
library(dplyr)

#LS1
dat_file <- read.csv("C:/Users/lindseybell/OneDrive - University of Arizona/Documents/Footprints/data/AMF_US-LS1_BASE_HH_1-5.csv", na.strings = c("-9999", "-9999.00000", "-9999.000"), header = TRUE, sep = ",", skip = 2)

meas_h <- 2.8
d <- (2/3) * meas_h
#bound_h <- 1000

dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START))

#daytime
dat_ffp <- dat_file %>%
  filter(WS_1_1_1 > 0 )%>%
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
    u_mean = mean(WS_1_1_1, na.rm = TRUE),
    L = (-((USTAR^3) * (TA_1_1_1 + 273)) / (0.4 * 9.8 * (H / (1.25 * 1004)))),
    sigma_v = sqrt((u_mean*((-1.3*L + 0.1)^2))/100000),
    u_star = USTAR,
    wind_dir = WD_1_1_1,
    test = zm/L
  ) %>%
  filter(test >= -15.5)%>%
  filter(u_star > 0.1)%>%
  select(yyyy, mm, day, HH_UTC, MM, zm, d, z0, u_mean, L, sigma_v, u_star, wind_dir)%>%
  filter(across(everything(), ~ . != "NA"))%>%
  filter(HH_UTC %in% c(6:19))%>%
  filter(yyyy == 2005)

dat_ffp$L = as.numeric(dat_ffp$L)
dat_ffp$sigma_v = as.numeric(dat_ffp$sigma_v)
dat_ffp$u_star = as.numeric(dat_ffp$u_star)
dat_ffp$wind_dir = as.numeric(dat_ffp$wind_dir)

write.csv(dat_ffp, "LS1_dt.csv", row.names = FALSE)

#nighttime
dat_ffp <- dat_file %>%
  filter(WS_1_1_1 > 0 )%>%
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
    u_mean = mean(WS_1_1_1, na.rm = TRUE),
    L = (-((USTAR^3) * (TA_1_1_1 + 273)) / (0.4 * 9.8 * (H / (1.25 * 1004)))),
    sigma_v = sqrt((u_mean*((-1.3*L + 0.1)^2))/100000),
    u_star = USTAR,
    wind_dir = WD_1_1_1,
    test = zm/L
  ) %>%
  filter(test >= -15.5)%>%
  filter(u_star > 0.1)%>%
  select(yyyy, mm, day, HH_UTC, MM, zm, d, z0, u_mean, L, sigma_v, u_star, wind_dir)%>%
  filter(across(everything(), ~ . != "NA"))%>%
  filter(HH_UTC %in% c(0:5, 20:23))%>%
  filter(yyyy == 2005)

dat_ffp$L = as.numeric(dat_ffp$L)
dat_ffp$sigma_v = as.numeric(dat_ffp$sigma_v)
dat_ffp$u_star = as.numeric(dat_ffp$u_star)
dat_ffp$wind_dir = as.numeric(dat_ffp$wind_dir)

write.csv(dat_ffp, "LS1_nt.csv", row.names = FALSE)
#-----------------------------------------------------------------------------------
#LS2

dat_file <- read.csv("C:/Users/lindseybell/OneDrive - University of Arizona/Documents/Footprints/data/AMF_US-LS2_BASE_HH_1-5.csv", na.strings = c("-9999", "-9999.00000", "-9999.000"), header = TRUE, sep = ",", skip = 2)

meas_h <- 6.4
d <- (2/3) * meas_h
#bound_h <- 1000

dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START))

#daytime
dat_ffp <- dat_file %>%
  filter(WS_1_1_1 > 0 )%>%
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
    u_mean = mean(WS_1_1_1, na.rm = TRUE),
    L = (-((USTAR^3) * (TA_1_1_1 + 273)) / (0.4 * 9.8 * (H / (1.25 * 1004)))),
    sigma_v = sqrt((u_mean*((-1.3*L + 0.1)^2))/100000),
    u_star = USTAR,
    wind_dir = WD_1_1_1,
    test = zm/L
  ) %>%
  filter(test >= -15.5)%>%
  filter(u_star > 0.1)%>%
  select(yyyy, mm, day, HH_UTC, MM, zm, d, z0, u_mean, L, sigma_v, u_star, wind_dir)%>%
  filter(across(everything(), ~ . != "NA"))%>%
  filter(HH_UTC %in% c(6:19))%>%
  filter(yyyy == 2005)

dat_ffp$L = as.numeric(dat_ffp$L)
dat_ffp$sigma_v = as.numeric(dat_ffp$sigma_v)
dat_ffp$u_star = as.numeric(dat_ffp$u_star)
dat_ffp$wind_dir = as.numeric(dat_ffp$wind_dir)

write.csv(dat_ffp, "LS2_dt.csv", row.names = FALSE)

#nighttime
dat_ffp <- dat_file %>%
  filter(WS_1_1_1 > 0 )%>%
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
    u_mean = mean(WS_1_1_1, na.rm = TRUE),
    L = (-((USTAR^3) * (TA_1_1_1 + 273)) / (0.4 * 9.8 * (H / (1.25 * 1004)))),
    sigma_v = sqrt((u_mean*((-1.3*L + 0.1)^2))/100000),
    u_star = USTAR,
    wind_dir = WD_1_1_1,
    test = zm/L
  ) %>%
  filter(test >= -15.5)%>%
  filter(u_star > 0.1)%>%
  select(yyyy, mm, day, HH_UTC, MM, zm, d, z0, u_mean, L, sigma_v, u_star, wind_dir)%>%
  filter(across(everything(), ~ . != "NA"))%>%
  filter(HH_UTC %in% c(0:5, 20:23))%>%
  filter(yyyy == 2005)

dat_ffp$L = as.numeric(dat_ffp$L)
dat_ffp$sigma_v = as.numeric(dat_ffp$sigma_v)
dat_ffp$u_star = as.numeric(dat_ffp$u_star)
dat_ffp$wind_dir = as.numeric(dat_ffp$wind_dir)

write.csv(dat_ffp, "LS2_nt.csv", row.names = FALSE)
#Lindsey Bell
#6/28/2023
#Calculating flux footprint for US-CMW

library(lubridate)
library(dplyr)

#dat_file <- read.csv("C:/Users/Asus PC/Documents/R_Projects/Footprints/data/AMF_US-CMW_BASE_HH_2-5.csv", na.strings = "-9999", header = TRUE, sep = ",", skip = 2)
dat_file <- read.csv("data/AMF_US-CMW_BASE_HH_2-5.csv",  na.strings = "-9999", header = TRUE, sep = ",", skip = 2)

meas_h <- 14
d <- (2/3) * meas_h
bound_h <- 1000

dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START))

dat_ffp <- dat_file %>%
  mutate(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    zm = (meas_h - d),
    d = d, 
    h = bound_h,
    z0 = 0.1 * meas_h,
    u_mean = mean(WS_1_1_1, na.rm = TRUE),
    L = (-((USTAR^3) * (TA_1_1_1 + 273)) / (0.4 * 9.8 * (H / (1.25 * 1004)))),
    sigma_v = sqrt((u_mean*((-1.3*L + 0.1)^2))/100000),
    u_star = USTAR,
    wind_dir = WD_1_1_1
  ) %>%
  select(yyyy, mm, day, HH_UTC, MM, zm, d, h, z0, u_mean, L, sigma_v, u_star, wind_dir)%>%
  filter(across(everything(), ~ . != "NA"))
  
dat_ffp$L = as.numeric(dat_ffp$L)
dat_ffp$sigma_v = as.numeric(dat_ffp$sigma_v)
dat_ffp$u_star = as.numeric(dat_ffp$u_star)
dat_ffp$wind_dir = as.numeric(dat_ffp$wind_dir)

#write.csv(dat_ffp, "CMW_ffp_test.csv", row.names = FALSE)

calc_footprint_FFP_climatology(zm = dat_ffp$zm, 
                               z0 = dat_ffp$z0, 
                               umean = dat_ffp$u_mean,
                               h = dat_ffp$h, 
                               ol = dat_ffp$L,
                               sigmav = dat_ffp$sigma_v,
                               ustar = dat_ffp$u_star,
                               wind_dir = dat_ffp$wind_dir)
#---------------------------------------------------------------------------
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
  #filter(test >= -15.5)%>%
  filter(HH_UTC %in% c(0:6, 20:24))%>%
  #filter(mm >= 8, mm <= 10)%>%
  # filter(day == 5)%>%
  filter(yyyy == 2017)

dat_ffp$L = as.numeric(dat_ffp$L)
dat_ffp$sigma_v = as.numeric(dat_ffp$sigma_v)
dat_ffp$u_star = as.numeric(dat_ffp$u_star)
dat_ffp$wind_dir = as.numeric(dat_ffp$wind_dir)

write.csv(dat_ffp, "CMW_ffp_4.csv", row.names = FALSE)
#----------------------------------------------------------------------------------------------
#daytime online template
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
  #filter(test >= -15.5)%>%
  filter(HH_UTC %in% 6:19)%>%
  #filter(mm >= 8, mm <= 10)%>%
  # filter(day == 5)%>%
  filter(yyyy == 2017)

dat_ffp$L = as.numeric(dat_ffp$L)
dat_ffp$sigma_v = as.numeric(dat_ffp$sigma_v)
dat_ffp$u_star = as.numeric(dat_ffp$u_star)
dat_ffp$wind_dir = as.numeric(dat_ffp$wind_dir)

write.csv(dat_ffp, "CMW_dt.csv", row.names = FALSE)
 
#==============================================================================
#footprints for winter, spring, and late summer 
#winter

dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START))

dat_ffp <- dat_file %>%
  mutate(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
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
  filter(test >= -15.5) %>%
  filter(u_star > 0.2) %>%
  filter(doy %in% c(0:100, 325:366))%>%
  select(yyyy, mm, day, HH_UTC, MM, zm, d, z0, u_mean, L, sigma_v, u_star, wind_dir) %>%
  filter(across(everything(), ~ . != "NA")) %>%
  filter(HH_UTC %in% 8:17)
 
dat_ffp$L = as.numeric(dat_ffp$L)
dat_ffp$sigma_v = as.numeric(dat_ffp$sigma_v)
dat_ffp$u_star = as.numeric(dat_ffp$u_star)
dat_ffp$wind_dir = as.numeric(dat_ffp$wind_dir)

write.csv(dat_ffp, "CMW_dt_win.csv", row.names = FALSE)

#for spring
dat_ffp <- dat_file %>%
  mutate(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
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
  filter(test >= -15.5) %>%
  filter(u_star > 0.2) %>%
  filter(doy >= 140 & doy<= 200)%>%
  select(yyyy, mm, day, HH_UTC, MM, zm, d, z0, u_mean, L, sigma_v, u_star, wind_dir) %>%
  filter(across(everything(), ~ . != "NA")) %>%
  filter(HH_UTC %in% 8:17)

dat_ffp$L = as.numeric(dat_ffp$L)
dat_ffp$sigma_v = as.numeric(dat_ffp$sigma_v)
dat_ffp$u_star = as.numeric(dat_ffp$u_star)
dat_ffp$wind_dir = as.numeric(dat_ffp$wind_dir)

write.csv(dat_ffp, "CMW_dt_spr.csv", row.names = FALSE)

#for late summer
dat_ffp <- dat_file %>%
  mutate(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
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
  filter(test >= -15.5) %>%
  filter(u_star > 0.2) %>%
  filter(doy %in% doy >= 250 & doy<= 300)%>%
  select(yyyy, mm, day, HH_UTC, MM, zm, d, z0, u_mean, L, sigma_v, u_star, wind_dir) %>%
  filter(across(everything(), ~ . != "NA")) %>%
  filter(HH_UTC %in% 8:17)

dat_ffp$L = as.numeric(dat_ffp$L)
dat_ffp$sigma_v = as.numeric(dat_ffp$sigma_v)
dat_ffp$u_star = as.numeric(dat_ffp$u_star)
dat_ffp$wind_dir = as.numeric(dat_ffp$wind_dir)

write.csv(dat_ffp, "CMW_dt_sum.csv", row.names = FALSE)

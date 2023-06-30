#Lindsey Bell
#6/28/2023
#Calculating flux footprint for US-CMW

library(lubridate)
library(dplyr)

dat_file <- read.csv("C:/Users/Asus PC/Documents/R_Projects/Footprints/data/AMF_US-CMW_BASE_HH_2-5.csv", na.strings = "-9999", header = TRUE, sep = ",", skip = 2)

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

#write.csv(dat_ffp, "CMW_ffp.csv", row.names = FALSE)

calc_footprint_FFP_climatology(zm = dat_ffp$zm, 
                               z0 = dat_ffp$z0, 
                               umean = dat_ffp$u_mean,
                               h = dat_ffp$h, 
                               ol = dat_ffp$L,
                               sigmav = dat_ffp$sigma_v,
                               ustar = dat_ffp$u_star,
                               wind_dir = dat_ffp$wind_dir)



[1] "zm must be above roughness sublayer"
[1] "zm/L must be equal or larger than -15.5"
library(lubridate)
library(dplyr)
library(jpeg)
library(fields)
library(zoom)
library(EBImage)
library(plot3D)

dat_file <- read.csv("C:/Users/Asus PC/Documents/R_Projects/Footprints/data/AMF_US-SRM_BASE_HH_25-5.csv", na.strings = "-9999", header = TRUE, sep = ",", skip = 2)

meas_h <- 7.82
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
    z0 = NaN,
    u_mean = mean(WS_1_1_1, na.rm = TRUE),
    L = (-((USTAR^3) * (TA_1_1_1 + 273)) / (0.4 * 9.8 * (H / (1.25 * 1004)))),
    sigma_v = sqrt((u_mean*((-1.3*L + 0.1)^2))/100000),
    u_star = USTAR,
    wind_dir = WD_1_1_1,
    test = zm/L
  ) %>%
  select(yyyy, mm, day, HH_UTC, MM, zm, d, h, z0, u_mean, L, sigma_v, u_star, wind_dir, test)%>%
  filter(across(everything(), ~ . != "NA"))%>%
  filter(test >= -15.5)%>%
  filter(HH_UTC > 6, HH_UTC < 20)%>%
 # filter(mm >= 8, mm <= 10)%>%
 # filter(day == 5)%>%
  filter(yyyy == 2008)

dat_ffp$L = as.numeric(dat_ffp$L)
dat_ffp$sigma_v = as.numeric(dat_ffp$sigma_v)
dat_ffp$u_star = as.numeric(dat_ffp$u_star)
dat_ffp$wind_dir = as.numeric(dat_ffp$wind_dir)

#write.csv(dat_ffp, "CMW_ffp.csv", row.names = FALSE)

ffp = calc_footprint_FFP_climatology(zm = dat_ffp$zm, 
                                     z0 = dat_ffp$z0, 
                                     umean = dat_ffp$u_mean,
                                     h = dat_ffp$h, 
                                     ol = dat_ffp$L,
                                     sigmav = dat_ffp$sigma_v,
                                     ustar = dat_ffp$u_star,
                                     wind_dir = dat_ffp$wind_dir)


image.plot(ffp$x_2d[1,], ffp$y_2d[,1], ffp$fclim_2d) 
for (i in 1:8) lines(ffp$xr[[i]], ffp$yr[[i]], type="l", col="red") 

surf3D(ffp$x_2d, ffp$y_2d,ffp$fclim_2d) 


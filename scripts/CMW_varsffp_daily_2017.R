#7/11/2023
#Plotting time series for variables in footprint calculations and key fluxes
#US-CMW

#loading data and libraries
library(lubridate)
library(dplyr)
library(ggplot2)
dat_file <- read.csv("C:/Users/lindseybell/OneDrive - University of Arizona/Documents/Footprints/data/AMF_US-CMW_BASE_HH_2-5.csv", na.strings = "-9999", header = TRUE, sep = ",", skip = 2)

#making ffp data frame for full day
meas_h <- 14
d <- (2/3) * meas_h

dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START))

dat_ffp <- dat_file %>%
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
    sigma_v = sqrt((u_mean*((-1.3*L + 0.1)^2))/100000),
    u_star = USTAR,
    wind_dir = WD_1_1_1,
    test = zm/L
  ) %>%
  filter(test >= -15.5)%>%
  filter(u_star > 0.1)%>%
  select(yyyy, mm, doy, day, HH_UTC, MM, zm, d, wind_sp, L, sigma_v, u_star, wind_dir)%>%
  filter(across(everything(), ~ . != "NA"))%>%
  #filter(HH_UTC %in% c(0:6, 20:24))%>%
  filter(yyyy == 2017)

dat_ffp$L = as.numeric(dat_ffp$L)
dat_ffp$sigma_v = as.numeric(dat_ffp$sigma_v)
dat_ffp$u_star = as.numeric(dat_ffp$u_star)
dat_ffp$wind_dir = as.numeric(dat_ffp$wind_dir)

ffp_cols = c("wind_sp", "L", "u_star", "wind_dir")
daily_means_ffp = dat_ffp%>%
  group_by(mm, doy)%>%
  summarize(across(all_of(ffp_cols), mean, na.rm = TRUE))%>%
  distinct()

par(mfrow = c(2,2))
ffp_vars = c("wind_sp", "L", "u_star", "wind_dir")
#ffp_vars = c(daily_means$wind_sp, daily_means$L, daily_means$u_star, daily_means$wind_dir)

plots = lapply(ffp_vars, function(vars){
  plot(daily_means$doy, daily_means[[vars]],
       main = vars,
       xlab = "day",
       ylab = "var",
       type = 'l',
       #lwd = 1
  )
  
}
  )

#-------------------------------------------------
#daily averages for associated fluxes 


#making ffp data frame for full day

dat_flux <- dat_file %>%
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
    sigma_v = sqrt((u_mean*((-1.3*L + 0.1)^2))/100000),
    u_star = USTAR,
    wind_dir = WD_1_1_1,
    test = zm/L,
    #adding associated fluxes
    gpp = GPP_PI,
    nee = NEE_PI,
    reco = RECO_PI,
    le = LE,
    ppfd = PPFD_IN_PI_F
  ) %>%
  filter(test >= -15.5)%>%
  filter(u_star > 0.1)%>%
  select(yyyy, mm, doy, day, HH_UTC, MM, gpp, nee, reco, le, ppfd)%>%
  filter(across(everything(), ~ . != "NA"))%>%
  #filter(HH_UTC %in% c(0:6, 20:24))%>%
  filter(yyyy == 2017)

flux_cols = c("gpp", "nee", "reco", "le", "ppfd")
daily_means_flux = dat_flux%>%
  group_by(mm, doy)%>%
  summarize(across(all_of(flux_cols), mean, na.rm = TRUE))%>%
  distinct()

par(mfrow = c(2,3))
plots = lapply(flux_cols, function(vars){
  plot(daily_means$doy, daily_means[[vars]],
       main = vars,
       xlab = "day",
       ylab = "var",
       type = 'l',
       #lwd = 1
  )
  
}
)

#-------------------------------------------------
#10 day averages for flux and ffp variables

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
    ppfd = PPFD_IN_PI_F
  ) %>%
  filter(test >= -15.5)%>%
  filter(u_star > 0.1)%>%
  select(yyyy, mm, doy, day, HH_UTC, MM, wind_sp, L, u_star, wind_dir, temp_atmos, H, gpp, nee, reco, le, ppfd)%>%
  filter(across(everything(), ~ . != "NA"))%>%
  #filter(HH_UTC %in% c(0:6, 20:24))%>%
  filter(yyyy == 2017)

var_cols = c("wind_sp", "L", "u_star", "wind_dir", "temp_atmos", "H","gpp", "nee", "reco", "le", "ppfd")
daily_means_voi = dat_voi%>%
  mutate(d_ten = ceiling(doy / 10)) %>%
  group_by(d_ten)%>%
  summarize(across(all_of(var_cols), mean, na.rm = TRUE)) #%>%
  #distinct()

par(mfrow = c(4,3))
plots = lapply(var_cols, function(vars){
  plot(daily_means_voi$d_ten, daily_means_voi[[vars]],
       main = vars,
       xlab = "day",
       ylab = "var",
       type = 'l',
       #lwd = 1
  )
  })

#----------------------------------------------
#monthly averages
var_cols = c("wind_sp", "L", "u_star", "wind_dir", "temp_atmos", "H","gpp", "nee", "reco", "le", "ppfd")
daily_means_voi = dat_voi%>%
  group_by(mm)%>%
  summarize(across(all_of(var_cols), mean, na.rm = TRUE)) #%>%
#distinct()

par(mfrow = c(4,3))
plots = lapply(var_cols, function(vars){
  plot(daily_means_voi$mm, daily_means_voi[[vars]],
       main = vars,
       xlab = "month",
       ylab = "var",
       type = 'l',
       #lwd = 1
  )
})

#--------------------------------------------------------------
#seasonal daily averages for each season

var_cols = c("wind_sp", "L", "u_star", "wind_dir", "temp_atmos", "H","gpp", "nee", "reco", "le", "ppfd")
daily_means_voi = dat_voi%>%
  group_by(doy)%>%
  summarize(across(all_of(var_cols), mean, na.rm = TRUE)) #%>%
#distinct()

#winter = nov-feb = 305-59
#spring = mar-jun = 60-181
#summer = jul-oct = 182-304

win_szn <- daily_means_voi %>%
  filter(doy <= 59 | doy >= 305)
win_szn = bind_rows(filter(win_szn, doy >= 305), filter(win_szn, doy <= 59))%>%
  mutate(row_num = seq(1, n()))
par(mfrow = c(3,4))
plots = lapply(var_cols, function(vars){
  plot(win_szn$row_num, win_szn[[vars]],
       main = vars,
       xlab = "day",
       ylab = "var",
       type = 'l',
       #lwd = 1
  )
})

spr_szn <- daily_means_voi %>%
  filter(doy >= 60 & doy <= 181)
par(mfrow = c(3,4))
plots = lapply(var_cols, function(vars){
  plot(spr_szn$doy, spr_szn[[vars]],
       main = vars,
       xlab = "day",
       ylab = "var",
       type = 'l',
       #lwd = 1
  )
})

sum_szn <- daily_means_voi %>%
  filter(doy >= 182 & doy <= 304)
par(mfrow = c(3,4))
plots = lapply(var_cols, function(vars){
  plot(sum_szn$doy, sum_szn[[vars]],
       main = vars,
       xlab = "day",
       ylab = "var",
       type = 'l',
       #lwd = 1
  )
})

#-------------------------------------------------------------
#seasonal by 5 day averages 

var_cols = c("wind_sp", "L", "u_star", "wind_dir", "temp_atmos", "H","gpp", "nee", "reco", "le", "ppfd")
daily_means_voi = dat_voi%>%
  mutate(d_five = ceiling(doy / 5)) %>%
  group_by(d_five)%>%
  summarize(across(all_of(var_cols), mean, na.rm = TRUE)) #%>%
#distinct()

par(mfrow = c(4,3))
plots = lapply(var_cols, function(vars){
  plot(daily_means_voi$d_five, daily_means_voi[[vars]],
       main = vars,
       xlab = "day",
       ylab = "var",
       type = 'l',
       #lwd = 1,
       abline(v = c(12, 36, 60), col = "blue")  
  )
})















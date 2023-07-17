#LS1 WD comparison 

library(lubridate)
library(dplyr)

#LS1
dat_file <- read.csv("C:/Users/lindseybell/OneDrive - University of Arizona/Documents/Footprints/data/AMF_US-LS1_BASE_HH_1-5.csv", na.strings = c("-9999", "-9999.00000", "-9999.000"), header = TRUE, sep = ",", skip = 2)

meas_h <- 2.8
d <- (2/3) * meas_h
#bound_h <- 1000

dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START))
#-------------------------------------------------------------------------------
#histograms

dat_voi = dat_file %>%
  filter(WS_1_1_1 > 0 )%>%
  mutate(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    #flux variables and params
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
    #adding key drivers and fluxes
    gpp = GPP_PI_F,
    nee = NEE_PI_F,
    reco = RECO_PI_F,
    le = LE,
    ppfd = PPFD_IN,
    precip = P,
    rel_h = RH_1_1_1,
    swc = SWC_PI_1_1_A
  ) %>%
  #filter data to work with ffp code/online calculator 
  filter(test >= -15.5)%>%
  filter(u_star > 0.1)%>%
  select(yyyy, mm, doy, day, HH_UTC, MM, wind_sp, L, u_star, wind_dir, temp_atmos, H, gpp, nee, reco, le, ppfd, precip, rel_h, swc)%>%
  filter(if_any(everything(), ~ . != "NA"))%>%
  #filter for high frequency values during the day
  filter(HH_UTC %in% 8:17)%>%
  filter(lag(precip) == 0, lead(precip) == 0)%>%
  filter(precip == 0)

par(mfrow = c(3,2))
hist(dat_voi$temp_atmos) #20:30
hist(dat_voi$ppfd) #1000:1500
hist(dat_voi$wind_sp) #0.5:2
hist(dat_voi$wind_dir) #150, 250, 350
hist(dat_voi$precip) #0
hist(dat_voi$swc)


#-------------------------------------------------------------------------------
dat_voi = dat_file %>%
  filter(WS_1_1_1 > 0 )%>%
  mutate(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    #flux variables and params
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
    #adding key drivers and fluxes
    gpp = GPP_PI_F,
    nee = NEE_PI_F,
    reco = RECO_PI_F,
    le = LE,
    ppfd = PPFD_IN,
    precip = P,
    rel_h = RH_1_1_1,
    swc = SWC_PI_1_1_A
  ) %>%
  #filter data to work with ffp code/online calculator 
  filter(test >= -15.5)%>%
  filter(u_star > 0.1)%>%
  select(yyyy, mm, doy, day, HH_UTC, MM, wind_sp, L, u_star, wind_dir, temp_atmos, H, gpp, nee, reco, le, ppfd, precip, rel_h, swc)%>%
  filter(if_any(everything(), ~ . != "NA"))%>%
  #filter for high frequency values during the day
  filter(HH_UTC >= 8 & HH_UTC <= 17)%>%
  filter(lag(precip) == 0, lead(precip) == 0)%>%
  filter(precip == 0)%>%
  filter(ppfd > 600 & ppfd < 1800)%>%  
  filter(temp_atmos >= 15 & temp_atmos <= 35)%>%
  filter(wind_sp >= 0.5 & wind_sp <= 2.5) 

#subset main data into frames with opposite WD (here, NW ad SE directions)
dat_voi_A <- dat_voi%>%
  filter(wind_dir > 270 & wind_dir < 350)
dat_voi_B <- dat_voi%>%
  filter(wind_dir > 90 & wind_dir < 170)

par(mfrow = c(2,2))
plot(dat_voi_A$doy, dat_voi_A$gpp, col = "blue")
points(dat_voi_B$doy, dat_voi_B$gpp, col = "red")
legend("topright", legend = c("Northwestern WD", "Southeastern WD"), col = c("blue", "red"), pch = 16, cex = 0.7)

plot(dat_voi_A$doy, dat_voi_A$reco, col = "blue")
points(dat_voi_B$doy, dat_voi_B$reco, col = "red")
legend("topright", legend = c("Northwestern WD", "Southeastern WD"), col = c("blue", "red"), pch = 16, cex = 0.7)

plot(dat_voi_A$doy, dat_voi_A$le, col = "blue")
points(dat_voi_B$doy, dat_voi_B$le, col = "red")
legend("topright", legend = c("Northwestern WD", "Southeastern WD"), col = c("blue", "red"), pch = 16, cex = 0.7)

plot(dat_voi_A$doy, dat_voi_A$nee, col = "blue")
points(dat_voi_B$doy, dat_voi_B$nee, col = "red")
legend("topright", legend = c("Northwestern WD", "Southeastern WD"), col = c("blue", "red"), pch = 16, cex = 0.7)

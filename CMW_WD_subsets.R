#7/12/2023
#Subsetting CMW data by wind direction to compare fluxes 

#--------------------------------------------------------------------------------
#loading data and libraries; defining variables
library(lubridate)
library(dplyr)
library(ggplot2)
library(zoo)
dat_file <- read.csv("C:/Users/lindseybell/OneDrive - University of Arizona/Documents/Footprints/data/AMF_US-CMW_BASE_HH_2-5.csv", na.strings = "-9999", header = TRUE, sep = ",", skip = 2)
meas_h <- 14
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
  filter(if_any(everything(), ~ . != "NA"))%>%
  filter(HH_UTC %in% 8:17)


par(mfrow = c(1, 1))
hist(dat_voi$temp_atmos) #20:30
hist(dat_voi$ppfd) #1000:1500
hist(dat_voi$wind_sp) #0.5:2
hist(dat_voi$wind_dir) #150, 250, 350
hist(dat_voi$precip) #0

#---------------------------------------------------------------------------------
#make full data frame

dat_voi = dat_file %>%
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
    gpp = GPP_PI,
    nee = NEE_PI,
    reco = RECO_PI,
    le = LE,
    ppfd = PPFD_IN_PI_F,
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
  filter(precip == 0)%>%
  filter(ppfd %in% 600:1800)%>%  
  filter(temp_atmos >= 15 & temp_atmos <= 35)%>%
  filter(wind_sp >= 0.5 & wind_sp <= 2.5) 

#subset main data into frames with opposite WD (here, NW ad SE directions)
dat_voi_A <- dat_voi%>%
  filter(wind_dir %in% 270:350)
dat_voi_B <- dat_voi%>%
  filter(wind_dir %in% 90:170)

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

#--------------------------------------------------------------------------------
#plotting drivers and gpp 
drv_var <- c("wind_sp", "ppfd", "temp_atmos", "rel_h", "swc")

par(mfrow = c( 2, 3))
plots = lapply(drv_var, function(vars){
  plot(dat_voi_A[[vars]], dat_voi_A$gpp,
       main = vars,
       xlab = "var",
       ylab = "gpp",
       #lwd = 1
  )
cor(dat_voi_A[[vars]], dat_voi_A$gpp, method = "pearson")
abline(lm(dat_voi_A$gpp ~dat_voi_A[[vars]]))
})

par(mfrow = c( 2, 3))
plots = lapply(drv_var, function(vars){
  plot(dat_voi_B[[vars]], dat_voi_B$gpp,
       main = vars,
       xlab = "var",
       ylab = "gpp",
       #lwd = 1
  )
  cor(dat_voi_A[[vars]], dat_voi_A$gpp, method = "pearson")
  abline(lm(dat_voi_A$gpp ~dat_voi_A[[vars]]))
  })

#overlaying graphs for dat_voi_A and dat_voi_B

par(mfrow = c(2, 3))
for (vars in drv_var) {
  plot(dat_voi_A[[vars]], dat_voi_A$gpp,
       main = vars,
       xlab = "var",
       ylab = "gpp"
  )
  points(dat_voi_A[[vars]], dat_voi_A$gpp, col = "blue")
  abline(lm(dat_voi_A$gpp ~ dat_voi_A[[vars]]), col = "blue")
  
  points(dat_voi_B[[vars]], dat_voi_B$gpp, col = "red")  
  abline(lm(dat_voi_B$gpp ~ dat_voi_B[[vars]]), col = "red")
  
  legend("topright", legend = c("Northwestern WD", "Southeastern WD"), col = c("blue", "red"), pch = 1, cex = 0.7)
 }

#--------------------------------------------------------------------------------
#subsetting data to only winter/early spring
dat_voi_A_win = dat_voi_A %>%
  filter(doy %in% c(0:100, 310:366))%>%
  filter(temp_atmos > 18)
dat_voi_B_win = dat_voi_B %>% 
  filter(doy %in% c(0:100, 310:366))%>%
  filter(temp_atmos > 18)

dat_B_selected <- dat_voi_B %>% sample_n(80)


par(mfrow = c(2, 3))
for (vars in drv_var) {
  plot(dat_voi_A_win[[vars]], dat_voi_A_win$gpp,
       main = vars,
       xlab = "var",
       ylab = "gpp"
  )
  points(dat_voi_A_win[[vars]], dat_voi_A_win$gpp, col = "blue")
  abline(lm(dat_voi_A_win$gpp ~ dat_voi_A_win[[vars]]), col = "blue")
  
  points(dat_B_selected[[vars]], dat_B_selected$gpp, col = "red")  
  abline(lm(dat_B_selected$gpp ~ dat_B_selected[[vars]]), col = "red")
  
  legend("topright", legend = c("Northwestern WD", "Southeastern WD"), col = c("blue", "red"), pch = 1, cex = 0.7)
}

#--------------------------------------------------------------------------------
#moving average 
par(mfrow = c(1,1))
dat_A_arr = dat_voi_A %>% arrange(doy)
dat_B_arr = dat_voi_B %>% arrange(doy)

dat_A_arr$movavg_A = rollmean(dat_A_arr$gpp, k = 20, fill = NA)
dat_B_arr$movavg_B = rollmean(dat_B_arr$gpp, k = 20, fill = NA)

ggplot() +
  geom_line(data = dat_A_arr, aes(x = doy, y = movavg_A, color = "Northwestern WD")) +
  geom_line(data = dat_B_arr, aes(x = doy, y = movavg_B, color = "Southeastern WD")) +
  labs(title = "Moving Avg", x = "DOY", y = "GPP", color = "Data Source") +
  scale_color_manual(values = c("Northwestern WD" = "blue", "Southeastern WD" = "red"))+
  theme_minimal()



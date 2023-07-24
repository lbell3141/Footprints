#LS2 WD comparison 

#loading data and libraries; defining variables
library(lubridate)
library(dplyr)
library(ggplot2)
library(zoo)
library(plantecophys)
library(gridExtra)

#LS2
dat_file <- read.csv("C:/Users/lindseybell/OneDrive - University of Arizona/Documents/Footprints/data/AMF_US-LS2_BASE_HH_1-5.csv", na.strings = c("-9999", "-9999.00000", "-9999.000"), header = TRUE, sep = ",", skip = 2)

meas_h <- 6.4
d <- (2/3) * meas_h
#bound_h <- 1000

dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START))
#--------------------------------------------------------------------------------
#use library plantecophys to calc VPD

dat_file$VPD = RHtoVPD(dat_file$RH_1_1_1, dat_file$TA_1_1_1, dat_file$PA)

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
    swc = SWC_PI_1_1_A,
    VPD = VPD
  ) %>%
  #filter data to work with ffp code/online calculator 
  filter(test >= -15.5)%>%
  filter(u_star > 0.1)%>%
  filter(le > -9999)%>%
  filter(swc > -9999)%>%
  filter(rel_h > -9999)%>%
  filter(ppfd > -9999)%>%
  select(yyyy, mm, doy, day, HH_UTC, MM, wind_sp, L, u_star, wind_dir, temp_atmos, H, gpp, nee, reco, le, ppfd, precip, rel_h, swc, VPD)%>%
  filter(if_any(everything(), ~ . != "NA"))%>%
  #filter for high frequency values during the day
  filter(HH_UTC %in% 8:17)%>%
  filter(lag(precip) == 0, lead(precip) == 0)%>%
  filter(precip == 0)%>%
  filter(swc >= 0)

par(mfrow = c(3,2))
hist(dat_voi$temp_atmos) #25:30
hist(dat_voi$ppfd) #1000:1500
hist(dat_voi$wind_sp) #0.5:2
hist(dat_voi$wind_dir) #175, 325
hist(dat_voi$precip) #0
hist(dat_voi$swc) #2.5:7.5


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
    swc = SWC_PI_1_1_A,
    VPD = VPD
  ) %>%
  #filter data to work with ffp code/online calculator 
  filter(test >= -15.5)%>%
  filter(u_star > 0.1)%>%
  filter(le > -9999)%>%
  filter(swc > -9999)%>%
  filter(rel_h > -9999)%>%
  filter(ppfd > -9999)%>%
  select(yyyy, mm, doy, day, HH_UTC, MM, wind_sp, L, u_star, wind_dir, temp_atmos, H, gpp, nee, reco, le, ppfd, precip, rel_h, swc, VPD)%>%
  filter(if_any(everything(), ~ . != "NA"))%>%
  #filter for high frequency values during the day
  filter(HH_UTC >= 8 & HH_UTC <= 18)%>%
  filter(lag(precip) == 0, lead(precip) == 0)%>%
  filter(precip == 0)%>%
  filter(ppfd > 1000 & ppfd < 1500)%>%  
  filter(temp_atmos >= 20 & temp_atmos <= 30)%>%
  filter(wind_sp >= 0.5 & wind_sp <= 2) 

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

#plotting drivers and gpp 
#overlaying graphs for dat_voi_A and dat_voi_B
drv_var <- c("wind_sp", "ppfd", "temp_atmos", "rel_h", "swc", "VPD")

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
#moving average GPP
par(mfrow = c(1,1))
dat_A_arr = dat_voi_A %>% arrange(doy)
dat_B_arr = dat_voi_B %>% arrange(doy)


col_var = c("gpp", "nee", "reco","wind_sp", "ppfd", "temp_atmos", "rel_h", "swc", "VPD")

dat_A_arr = dat_voi_A %>% 
  arrange(doy)%>%
  group_by(doy)%>%
  summarize(across(all_of(col_var), mean, na.rm = TRUE, .names = "mn_{.col}"))

dat_B_arr = dat_voi_B %>% 
  arrange(doy)%>%
  group_by(doy)%>%
  summarize(across(all_of(col_var), mean, na.rm = TRUE, .names = "mn_{.col}"))

dat_A_arr$movavg_A = rollmean(dat_A_arr$mn_gpp, k = 10, fill = NA)
dat_B_arr$movavg_B = rollmean(dat_B_arr$mn_gpp, k = 10, fill = NA)

ggplot() +
  geom_line(data = dat_A_arr, aes(x = doy, y = movavg_A, color = "Northwestern WD")) +
  geom_line(data = dat_B_arr, aes(x = doy, y = movavg_B, color = "Southeastern WD")) +
  labs(title = "Moving Avg", x = "DOY", y = "GPP", color = "Data") +
  scale_color_manual(values = c("Northwestern WD" = "blue", "Southeastern WD" = "red"))+
  theme_minimal()

#MA NEE
par(mfrow = c(1,1))
dat_A_arr = dat_voi_A %>% arrange(doy)
dat_B_arr = dat_voi_B %>% arrange(doy)


col_var = c("gpp", "nee", "reco","wind_sp", "ppfd", "temp_atmos", "rel_h", "swc", "VPD")

dat_A_arr = dat_voi_A %>% 
  arrange(doy)%>%
  group_by(doy)%>%
  summarize(across(all_of(col_var), mean, na.rm = TRUE, .names = "mn_{.col}"))

dat_B_arr = dat_voi_B %>% 
  arrange(doy)%>%
  group_by(doy)%>%
  summarize(across(all_of(col_var), mean, na.rm = TRUE, .names = "mn_{.col}"))

dat_A_arr$movavg_A = rollmean(dat_A_arr$mn_nee, k = 20, fill = NA)
dat_B_arr$movavg_B = rollmean(dat_B_arr$mn_nee, k = 20, fill = NA)

ggplot() +
  geom_line(data = dat_A_arr, aes(x = doy, y = movavg_A, color = "Northwestern WD")) +
  geom_line(data = dat_B_arr, aes(x = doy, y = movavg_B, color = "Southeastern WD")) +
  labs(title = "Moving Avg", x = "DOY", y = "NEE", color = "Data") +
  scale_color_manual(values = c("Northwestern WD" = "blue", "Southeastern WD" = "red"))+
  theme_minimal()

dat_A_arr$movavg_A = rollmean(dat_A_arr$mn_reco, k = 20, fill = NA)
dat_B_arr$movavg_B = rollmean(dat_B_arr$mn_reco, k = 20, fill = NA)

ggplot() +
  geom_line(data = dat_A_arr, aes(x = doy, y = movavg_A, color = "Northwestern WD")) +
  geom_line(data = dat_B_arr, aes(x = doy, y = movavg_B, color = "Southeastern WD")) +
  labs(title = "Moving Avg", x = "DOY", y = "RECO", color = "Data") +
  scale_color_manual(values = c("Northwestern WD" = "blue", "Southeastern WD" = "red"))+
  theme_minimal()

vsoi = c("mn_wind_sp", "mn_ppfd", "mn_temp_atmos", "mn_rel_h", "mn_swc", "mn_VPD")
dat_voi_A = dat_A_arr
dat_voi_B = dat_B_arr
par(mfrow = c(2, 3))
for (vars in vsoi) {
  plot(dat_voi_A[[vars]], dat_voi_A$mn_gpp,
       main = vars,
       xlab = "var",
       ylab = "gpp"
  )
  points(dat_voi_A[[vars]], dat_voi_A$mn_gpp, col = "blue")
  abline(lm(dat_voi_A$mn_gpp ~ dat_voi_A[[vars]]), col = "blue")
  
  points(dat_voi_B[[vars]], dat_voi_B$mn_gpp, col = "red")  
  abline(lm(dat_voi_B$mn_gpp ~ dat_voi_B[[vars]]), col = "red")
  
  legend("topright", legend = c("Northwestern WD", "Southeastern WD"), col = c("blue", "red"), pch = 1, cex = 0.7)
}

#===============================================================================
#splitting into 8 directions
deg_int <- seq(0, 360, by = 45)
split_dat <- split(dat_voi, cut(dat_voi$wind_dir, deg_int, include.lowest = TRUE, labels = FALSE))
dat_frames <- c("dat_A", "dat_B", "dat_C", "dat_D", "dat_E", "dat_F", "dat_G", "dat_H")

for (i in seq_along(dat_frames)) {
  assign(dat_frames[i], split_dat[[i]])
}
dat_frames <- lapply(c("dat_A", "dat_B", "dat_C", "dat_D", "dat_E", "dat_F", "dat_G", "dat_H"), function(x) get(x))
c_var <- c("gpp", "reco", "nee")
r_var <- c("temp_atmos", "rel_h", "VPD", "swc", "ppfd", "wind_sp")
plots <- list()

for (var in c_var){
  p <- ggplot() +
    labs(x = "DOY", y = var, color = "Data Frame")
  
  for (i in 1:8){
    df <- dat_frames[[i]]
    df_name <- i
    df$df_name <- factor(df_name)
    
    p <- p + geom_point(data = df, aes_string(x = "doy", y = var, color = "df_name"))
  }
  plots[[var]] <- p
}

grid.arrange(grobs = plots, ncol = 3)

#for r_var plots
for (var in r_var){
  p <- ggplot() +
    labs(x = var, y = "GPP" , color = "Data Frame")
  
  for (i in 1:8){
    df <- dat_frames[[i]]
    df_name <- i
    df$df_name <- factor(df_name)
    
    p <- p + geom_point(data = df, aes_string(x = var, y = "gpp", color = "df_name"))
  }
  plots[[var]] <- p
}

grid.arrange(grobs = plots, ncol = 3)


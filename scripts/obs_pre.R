# "observed/predicted" graph using NW/SE wind directions from CMW and GPP from LS1

library(lubridate)
library(dplyr)
library(plantecophys)
library(ggplot2)
library(zoo)


#Loading/formatting CMW data====================================================
dat_file <- read.csv("data/AMF_US-CMW_BASE_HH_2-5.csv", na.strings = "-9999", header = TRUE, sep = ",", skip = 2)
dat_file$TIMESTAMP_START <- ymd_hm(as.character(dat_file$TIMESTAMP_START))

meas_h <- 14
d <- (2/3) * meas_h
#use library plantecophys to calc VPD

dat_file$VPD = RHtoVPD(dat_file$RH_1_1_1, dat_file$TA_1_1_1, dat_file$PA)

#create data frame with desired variables and filters for daytime, precip, and u*.
#Test variable is to fit parameters of Kljun FFP code
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
    WS = WS_1_1_1,
    L = (-((USTAR^3) * (TA_1_1_1 + 273)) / (0.4 * 9.8 * (H / (1.25 * 1004)))),
    H = H,
    TA = TA_1_1_1,
    sigma_v = sqrt((u_mean*((-1.3*L + 0.1)^2))/100000),
    u_star = USTAR,
    wind_dir = WD_1_1_1,
    test = zm/L,
    #adding associated fluxes
    GPP = GPP_PI,
    NEE = NEE_PI,
    RECO = RECO_PI,
    le = LE,
    PPFD = PPFD_IN_PI_F,
    precip = P,
    RH = RH_1_1_1,
    swc = SWC_PI_1_1_A,
    VPD = VPD
  ) %>%
  filter(test >= -15.5)%>%
  filter(u_star > 0.25)%>%
  dplyr::select(yyyy, mm, doy, day, HH_UTC, MM, WS, L, u_star, wind_dir, TA, H, GPP, NEE, RECO, le, PPFD, precip, RH, swc, VPD)%>%
  filter(if_any(everything(), ~ . != "NA"))%>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)%>%
  filter(lag(precip) == 0, lead(precip) == 0)%>%
  filter(precip == 0)

dat_voi_A <- dat_voi%>%
  filter(wind_dir >= 270 & wind_dir <= 350)
dat_voi_B <- dat_voi%>%
  filter(wind_dir >= 90 & wind_dir <= 170)

#Loading/formatting LS1 data===============================================
gr_data <- read.csv("data/AMF_US-LS1_BASE_HH_1-5.csv", na.strings = "-9999", header = TRUE, sep = ",", skip = 2)
gr_data$TIMESTAMP_START <- ymd_hm(as.character(gr_data$TIMESTAMP_START))

gr_data$VPD = RHtoVPD(gr_data$RH_1_1_1, gr_data$TA_1_1_1, gr_data$PA)

gr_voi = gr_data %>%
  mutate(
    yyyy = year(TIMESTAMP_START),
    mm = month(TIMESTAMP_START),
    doy = yday(TIMESTAMP_START),
    day = day(TIMESTAMP_START),
    HH_UTC = hour(TIMESTAMP_START),
    MM = minute(TIMESTAMP_START),
    u_mean = mean(WS_1_1_1, na.rm = TRUE),
    WS = WS_1_1_1,
    L = (-((USTAR^3) * (TA_1_1_1 + 273)) / (0.4 * 9.8 * (H / (1.25 * 1004)))),
    H = H,
    TA = TA_1_1_1,
    sigma_v = sqrt((u_mean*((-1.3*L + 0.1)^2))/100000),
    u_star = USTAR,
    wind_dir = WD_1_1_1,
    zm = 3,
    test = zm/L,
    #adding associated fluxes
    GPP = GPP_PI_F,
    NEE = NEE_PI_F,
    RECO = RECO_PI_F,
    le = LE,
    PPFD = PPFD_IN,
    precip = P,
    RH = RH_1_1_1,
    swc = SWC_PI_1_1_A,
    VPD = VPD
  ) %>%
  filter(test >= -15.5)%>%
  filter(u_star > 0.25)%>%
  dplyr::select(yyyy, mm, doy, day, HH_UTC, MM, WS, L, u_star, wind_dir, TA, H, GPP, NEE, RECO, le, PPFD, precip, RH, swc, VPD)%>%
  filter(if_any(everything(), ~ . != "NA"))%>%
  filter(HH_UTC >= 8 & HH_UTC <= 17)%>%
  filter(lag(precip) == 0, lead(precip) == 0)%>%
  filter(precip == 0)

#plotting=======================================================================
dat_A_arr = dat_voi_A %>% 
  arrange(doy)%>%
  group_by(doy)%>%
  summarise(across(everything(), mean, na.rm = TRUE))%>%
  filter(GPP != -9999)
dat_B_arr = dat_voi_B %>% 
  arrange(doy)%>%
  group_by(doy)%>%
  summarise(across(everything(), mean, na.rm = TRUE))%>%
  filter(GPP != -9999)
dat_gr_arr = gr_voi %>% 
  arrange(doy)%>%
  group_by(doy)%>%
  summarise(across(everything(), mean, na.rm = TRUE))%>%
  filter(GPP != -9999)

obs_pre = data.frame(
  A = dat_A_arr$GPP,
  B = dat_B_arr$GPP,
  gr = dat_gr_arr$GPP
  )

ggplot(obs_pre, aes(x = A, y = gr, color = "A")) +
  geom_point(size = 3, alpha = 0.15) +
  scale_color_manual(values = c(A = "blue")) +
  labs(x = "GPP A", y = "GPP gr", title = "Comparison of GPP") +
  geom_smooth(aes(x = A, y = gr), method = "lm", se = FALSE, linetype = "solid", color = "blue") +
  geom_point(aes(x = B, y = gr, color = "B"), size = 3, alpha = 0.15, color = "red") +
  geom_smooth(aes(x = B, y = gr), method = "lm", se = FALSE, linetype = "solid", color = "red") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray", size = 1.3) +
  labs(x = "US-CMW GPP", y = "US-LS1 GPP", title = "Comparison of GPP of CMW Wind Directions with Grassland") +
  theme_minimal()


dat_A_arr$movavg_A = rollmean(dat_A_arr$GPP, k = 500, fill = NA)
dat_B_arr$movavg_B = rollmean(dat_B_arr$GPP, k = 500, fill = NA)
dat_gr_arr$movavg_gr = rollmean(dat_gr_arr$GPP, k = 500, fill = NA)

#Standardize====================================================================
A_zs <- scale(obs_pre$A)
B_zs <- scale(obs_pre$B)
gr_zs <- scale(obs_pre$gr)

z_score_df <- data.frame(A_zs, B_zs, gr_zs)


ggplot(z_score_df, aes(x = A_zs, y = gr_zs, color = "A_zs")) +
  geom_point(size = 3, alpha = 0.15) +
  scale_color_manual(values = c(A_zs = "blue")) +
  labs(x = "GPP A", y = "GPP gr", title = "Comparison of GPP") +
  geom_smooth(aes(x = A_zs, y = gr_zs), method = "lm", se = FALSE, linetype = "solid", color = "blue") +
  geom_point(aes(x = B_zs, y = gr_zs, color = "B_zs"), size = 3, alpha = 0.15, color = "red") +
  geom_smooth(aes(x = B_zs, y = gr_zs), method = "lm", se = FALSE, linetype = "solid", color = "red") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray", size = 1.3) +
  labs(x = "US-CMW GPP", y = "US-LS1 GPP", title = "Comparison of GPP of CMW Wind Directions with Grassland") +
  theme_minimal()

#log transform=================================================================
obs_pre$log_A <- log(obs_pre$A + 1e-10)
obs_pre$log_B <- log(obs_pre$B + 1e-10)
obs_pre$log_gr <- log(obs_pre$gr + 1e-10)

obs_pre_filtered <- na.omit(obs_pre)
obs_pre_filtered <- obs_pre_filtered[-41, ]

log_A_zs <- scale(obs_pre_filtered$log_A)
log_B_zs <- scale(obs_pre_filtered$log_B)
log_gr_zs <- scale(obs_pre_filtered$log_gr)
log_z_score_df <- data.frame(log_A_zs, log_B_zs, log_gr_zs)

#===================================
  # Fit log-log regression model
model <- lm(log_gr_zs ~ log_A_zs, data = log_z_score_df)

# Extract coefficients
slope <- coef(model)[2]
intercept <- coef(model)[1]

# Predict log(GPPls) using the log-log regression equation
log_GPPls_pred <- predict(model, newdata = log_z_score_df)

# Plot the log-transformed data with log-log regression line
ggplot(log_z_score_df, aes(x = log_A_zs, y = log_gr_zs, color = "log_A_zs")) +
  geom_point(size = 3, alpha = 0.15) +
  scale_color_manual(values = c(log_A_zs = "blue")) +
  labs(x = "Log(GPP A)", y = "Log(GPP gr)", title = "Log-Log Regression of GPP") +
  geom_smooth(aes(x = log_A_zs, y = log_GPPls_pred), se = FALSE, linetype = "solid", color = "blue") +
  labs(x = "Log(US-CMW GPP)", y = "Log(US-LS1 GPP)", title = "Comparison of Log-transformed GPP of CMW Wind Directions with Grassland") +
  theme_minimal()



#---------
ggplot(log_z_score_df, aes(x = log_A_zs, y = log_gr_zs, color = "log_A_zs")) +
  geom_point(size = 3, alpha = 0.15) +
  scale_color_manual(values = c(log_A_zs = "blue")) +
  labs(x = "Log(GPP A)", y = "Log(GPP gr)", title = "Comparison of Log-transformed GPP") +
  #geom_smooth(aes(x = log_A_zs, y = log_gr_zs), method = "glm", se = FALSE, linetype = "solid", color = "blue") +
  geom_point(aes(x = log_B_zs, y = log_gr_zs, color = "log_B_zs"), size = 3, alpha = 0.15, color = "red") +
  #geom_smooth(aes(x = log_B_zs, y = log_gr_zs), method = "glm", se = FALSE, linetype = "solid", color = "red") +
  #geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray", size = 1.3) +
  labs(x = "Log(US-CMW GPP)", y = "Log(US-LS1 GPP)", title = "Comparison of Log-transformed GPP of CMW Wind Directions with Grassland") +
  theme_minimal()


ggplot(log_z_score_df, aes(x = log_A_zs, y = log_gr_zs))+
  geom_point(size = 3, alpha = 0.15)+
  geom_smooth(aes(z = log_A_zs, y = log_gr_zs), method = "lm", formula = y~log(x))

fit <- lm(log_z_score_df$log_gr_zs ~ log(log_z_score_df$log_A_zs))
x=seq(from=1,to=n,length.out=1000)
y=predict(fit,newdata=list(x=seq(from=1,to=n,length.out=1000)),
            +           interval="confidence")
matlines(x,y,lwd=2)

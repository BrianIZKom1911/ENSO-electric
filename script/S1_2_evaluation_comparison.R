# Programs to run:
# 1. TRF and CTRF models (which generate results in the Tables), not involving bootstrap
# 2. Regress elec on ENSO index (Nino 3.4 anom) for comparison
# 3. Compute cntf_elec by cntf_temp and estimated CTRF

rm(list=ls())

library(here)
library(readr)
library(dplyr)
library(AER)
library(broom)

fff33 <- function(data, colname, a=NULL, b=NULL){
  a <- ifelse(is.null(a), min(data[[colname]]), a)
  b <- ifelse(is.null(b), max(data[[colname]]), b)
  data %>%
    mutate(ts = (.data[[colname]]-a)/(b-a), 
    		ts2 = ts^2, ts3 = ts^3, 
    		sin1 = sin(2*pi*ts), cos1 = cos(2*pi*ts), 
    		sin2 = sin(4*pi*ts), cos2 = cos(4*pi*ts), 
    		sin3 = sin(6*pi*ts), cos3 = cos(6*pi*ts)) -> data
  return(data)
}

source(file.path("script", "00_functions.R"))
md <- here()
savedir <- file.path(md, "output")
if (!dir.exists(savedir)) {
  dir.create(savedir, recursive = TRUE)
}
# Merge ----------------------------------------------------
dt_mm <- read_csv(file.path(md, "data_clean", "cntf_t.csv"))
# Get FF terms and integrate hourly TRF wrt the density
a <- min(dt_mm$temperature); b <- max(dt_mm$temperature)
dt_mm %>%
  fff33(., "temperature") %>%
  mutate(f_s = f_r/(b-a)) %>%
  group_by(yyyymm) %>%
  reframe(across(c(ts, ts2, ts3, sin1, cos1, sin2, cos2, sin3, cos3), ~ sum(.x*f_s), .names="x{col}")) %>%
  group_by(yyyymm) -> dt_mon

# Elec data
dt_elec <- read_csv(file.path(md, "data_clean", "WA_elec_pc_rp_1990_2023.csv"))
colstodrop <- c("resid_count", "comm_count", "County")
dt_elec <- dt_elec[, !(names(dt_elec) %in% colstodrop)]
dt_elec$yyyymm <- paste0(dt_elec$Year, "-", sprintf("%02d", dt_elec$Month))
# Change unit to kwh per person
dt_elec$resid_pc <- dt_elec$resid_pc*1000
dt_elec$comm_pc <- dt_elec$comm_pc*1000
dt_run <- merge(dt_elec, dt_mon, by=c("yyyymm"), all.x=TRUE)

# Simple regression: elec vs ENSO -------------------------------------

read_csv(file.path(md, "data_clean", "nino3.4_anom_1960_2024 - Copy.csv")) %>%
  mutate(anom_pos = ifelse(anom>0, anom, 0),
         anom_neg = anom - anom_pos) -> dt_Nino
# Get the frequency of ENSO events by month
dt_Nino %>%
  group_by(Month) %>%
  summarise(fr_neg4 = sum(i_nina, na.rm=TRUE),
            fr_pos4 = sum(i_nino, na.rm=TRUE),
            fr_nina = sum(la_nina, na.rm=TRUE),
            fr_nino = sum(el_nino, na.rm=TRUE)) -> tab_freq
write.csv(tab_freq, file=file.path(md, "output", "tab_ENSOfreq.csv"), row.names=FALSE)

# Table 1, part 2. Electricity #
dt_run %>%
  mutate(num_month =  seq(1, 408),
         nn_m = (num_month-1)/407,
         nn_y = (Year - 1990)/33) -> dt_run
tmp <- merge(dt_run, dt_Nino)
lm_y_s <- lm(log(resid_pc) ~ anom_pos + anom_neg +factor(Month)+nn_m, data = tmp)
lm_y_en <- lm(log(resid_pc) ~ el_nino + la_nina +factor(Month)+nn_m, data = tmp)
save_lm_output(file.path(md, "output", "lm_elec_anom.txt"), lm_y_s, lm_y_en)

# TRF and CTRF models --------------------------------------

## Residential ----------
dt_run %>%
  mutate(resrp_std = scale(resid_rp)[,1], comrp_std = scale(comm_rp)[,1]) -> dt_run
save(dt_run, file = file.path("S1_dt_run.RDS"))
# TRF Table #
trf22 <- lm(log(resid_pc) ~ xts+xts2+xsin1+xcos1+xsin2+xcos2
		        +factor(Month)+nn_m, data=dt_run) # (2, 2) BIC preferred
trf22_p <- lm(log(resid_pc) ~ xts+xts2+xsin1+xcos1+xsin2+xcos2
            +factor(Month)+nn_m+resrp_std, data=dt_run)
save_lm_output(file.path(md, "output", "trf22_m_p.txt"), trf22, trf22_p)
# CTRF Table #
ctrf_resid <- lm(log(resid_pc) ~ xts+xts2+xsin1+xcos1+xsin2+xcos2
                 +resrp_std+I(resrp_std*xts)+I(resrp_std*xsin1)+I(resrp_std*xcos1)
                 +nn_m+I(nn_m*xts)+I(nn_m*xsin1)+I(nn_m*xcos1)
                 +factor(Month), data=dt_run)
ctrf_p <- lm(log(resid_pc) ~ xts+xts2+xsin1+xcos1+xsin2+xcos2
             +resrp_std
             +nn_m+I(nn_m*xts)+I(nn_m*xsin1)+I(nn_m*xcos1)
             +factor(Month), data=dt_run) # BIC preferred
save_lm_output(file.path(md, "output", "ctrf22_m_p.txt"), ctrf_resid, ctrf_p)

## Commercial ----------
# What is a good way to detrend comm_sales or comm_pc? 
# a) Removing SMA is simple in detrending but will make the cntf series "irreversible"
# b) linear time trend interacted with I(>=2002-11)
# Breaks: 2001-07 through 2003-01; 2020-03 through 2020-09

# Get past moving average (rolling mean)
avg1990 <- mean(dt_run[dt_run$Year==1990, "comm_sales"])
dt_run$ma_comm <- lag(rollmean(dt_run$comm_sales, k=12, align="right", fill=NA), n=1)
dt_run[dt_run$Year==1990, "ma_comm"] <- avg1990
dt_run$comm_dt <- dt_run$comm_sales - dt_run$ma_comm
# Breakpoint indicators
dt_run$i200211 <- as.numeric(dt_run$Year>=2003)
dt_run[(dt_run$Year==2002)&(dt_run$Month>=11), "i200211"] <- 1

trf21 <- lm(comm_dt ~ xts+xts2+xsin1+xcos1
            +factor(Month), data=dt_run) # (2, 1) BIC preferred
# with time and interaction, (2, 2) is BIC preferred
trf22_i <- lm(log(comm_pc) ~ xts+xts2+xsin1+xcos1+xsin2+xcos2
              +nn_m+i200211+I(nn_m*i200211)+factor(Month), data=dt_run)
trf22_i_p <- lm(log(comm_pc) ~ xts+xts2+xsin1+xcos1+xsin2+xcos2
              +nn_m+i200211+I(nn_m*i200211)+comrp_std+factor(Month), data=dt_run)
save_lm_output(file.path(md, "output", "trf_comm.txt"), trf21, trf22_i, trf22_i_p)

# CTRF Table #
# underlying asmp: just makes comm_pc stationary, not affecting cross-TRFs
ctrf11_comm <- lm(log(comm_pc) ~ xts+xsin1+xcos1
                +comrp_std+I(comrp_std*xts)+I(comrp_std*xsin1)+I(comrp_std*xcos1)
                +nn_m+i200211+I(nn_m*i200211)+I(nn_m*xts)+I(nn_m*xsin1)+I(nn_m*xcos1)
                +factor(Month), data = dt_run) # (1, 1) BIC preferred
save_lm_output(file.path(md, "output", "ctrf_comm.txt"), ctrf11_comm)

# Cntf elec -------------------------------------------
# We have already gotten cntf_temp from 1_cntf_temps.py. Now just plug it in 
#the estimated CTRF models.
# Get FF terms and integrals
dt_mm %>%
  fff33(., "cntf_temp") %>%
  mutate(f_s = f_r/(b-a)) %>%
  group_by(yyyymm) %>%
  reframe(across(c(ts, ts2, ts3, sin1, cos1, sin2, cos2, sin3, cos3), ~ sum(.x*f_s), .names="x{col}")) %>%
  group_by(yyyymm) -> dt_mon
cntf_run <- merge(dt_elec, dt_mon, by=c("yyyymm"), all.x=TRUE)

# By how much does temperature change? #
dt_mm %>% 
  mutate(diff_T_h = temperature - cntf_temp) %>%
  group_by(yyyymm) %>%
  summarise(diff_T = mean(diff_T_h)) %>%
  mutate(date01 = as.Date(paste(yyyymm, "01", sep = "-"))) -> df_compar_T

## Residential ----------
GetCntf_y <- function(data_a, data_b, y, regressor){
  X0 <- as.matrix(data_a[, regressor])
  Xf0 <- as.matrix(data_b[, regressor])
  model <- lm(y ~ X0, na.action = "na.exclude")
  beta <- coef(model)
  u <- resid(model)
  ones <- matrix(1, nrow=length(y), ncol=1)
  Xf <- cbind(ones, Xf0)
  # hypothetical elec
  cntf_y <- (Xf %*% beta + u)[, 1]
  return(cntf_y)
}

dt_run %>%
  select(Year, Month, xts, xts2, xsin1, xcos1, xsin2, xcos2, nn_m, resrp_std, comrp_std) %>%
  mutate(x_t = xts*nn_m, xsin1_t = xsin1*nn_m, xcos1_t = xcos1*nn_m,
         x_cp=xts*comrp_std, xsin1_cp=xsin1*comrp_std, xcos1_cp=xcos1*comrp_std) %>%
  GetMonthcol() -> X_act
cntf_run %>%
  mutate(num_month =  seq(1, 408), nn_m = (num_month-1)/407, nn_y = (Year-1990)/33,
         resrp_std = scale(resid_rp)[,1], comrp_std = scale(comm_rp)[,1]) %>%
  select(Year, Month, xts, xts2, xsin1, xcos1, xsin2, xcos2, nn_m, resrp_std, comrp_std) %>%
  mutate(x_t = xts*nn_m, xsin1_t = xsin1*nn_m, xcos1_t = xcos1*nn_m,
         x_cp=xts*comrp_std, xsin1_cp=xsin1*comrp_std, xcos1_cp=xcos1*comrp_std) %>%
  GetMonthcol() -> X_cnf
df_compar <- cntf_run[, c("Year", "Month")]

# ctrf(2, 2) + time(1,1) + resrp_std #
y <- log(dt_run$resid_pc)
rgrss22 <- c("xts", "xts2", "xsin1", "xcos1", "xsin2", "xcos2", "resrp_std",
             "nn_m", "x_t", "xsin1_t", "xcos1_t",
             "m02", "m03", "m04", "m05", "m06", "m07", "m08", "m09", "m10", "m11", "m12")
# Hypothetical resident elec
cntf_run$cntf_resid <- GetCntf_y(X_act, X_cnf, y, rgrss22)
df_compar$diff_resid <- cntf_run$resid_pc - exp(cntf_run$cntf_resid)
df_compar$pers_resid <- (df_compar$diff_resid/exp(cntf_run$cntf_resid))*100

## Commercial ----------
X_act$i200211 <- dt_run$i200211
X_act$t_i <- X_act$nn_m * X_act$i200211
X_cnf$i200211 <- dt_run$i200211
X_cnf$t_i <- X_cnf$nn_m * X_cnf$i200211

# ctrf(1, 1) + time(1,1)+i200211 + price(1, 1) #
y <- log(dt_run$comm_pc)
rgrss11 <- c("xts", "xsin1", "xcos1",
             "comrp_std", "x_cp", "xsin1_cp", "xcos1_cp",
             "nn_m", "i200211", "t_i", "x_t", "xsin1_t", "xcos1_t",
             "m02", "m03", "m04", "m05", "m06", "m07", "m08", "m09", "m10", "m11", "m12")
# Hypothetical comm elec
cntf_run$cntf_comm <- GetCntf_y(X_act, X_cnf, y, rgrss11)
df_compar$diff_comm <- cntf_run$comm_pc - exp(cntf_run$cntf_comm)
df_compar$pers_comm <- (df_compar$diff_comm/exp(cntf_run$cntf_comm))*100
df_compar$kwh_resid <- exp(cntf_run$cntf_resid)
df_compar$kwh_comm <- exp(cntf_run$cntf_comm)
save(a, b, df_compar_T, df_compar, file = "S1_dataforcompare.RData")

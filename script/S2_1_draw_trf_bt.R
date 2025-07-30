rm(list = ls())

library(here)
library(readr)
library(dplyr)
library(broom)
library(ggplot2)
library(urca)

md <- here()
source(file.path("script", "00_packages_functions.R"))
load(here("script", "S1_dt_run.RDS"))
# Breakpoint indicator
dt_run$i200211 <- as.numeric(dt_run$Year>=2003)
dt_run[(dt_run$Year==2002)&(dt_run$Month>=11), "i200211"] <- 1

# Description -----------------------------------------------
dt_mm <- read_csv(here("data_clean", "cntf_t.csv"))
dt_mm$datetime00 <- as.POSIXct(dt_mm$datetime00, tz="UTC")

# Raw temperature: hourly
ggplot(dt_mm, aes(x=datetime00))+
  geom_line(aes(y=temperature), color="turquoise4", linewidth=0.25)+
  scale_y_continuous(name="degree Celsius", breaks=seq(-10, 40, by=10))+
  scale_x_datetime(breaks=seq(as.POSIXct("1990-01-01"), as.POSIXct("2022-01-01"), by ="8 years"), 
                   labels=as.character(seq(1990, 2022, by=8)))+
  xlab("")+
  theme(legend.position = "bottom")
ggsave(file=here("output", "fig_WA_hrtemp.png"), width=9.5, height=0.618*6.5)

## Series: temp and elec ------------
col_elec <- c("yyyymm", "Year", "Month", "resid_pc", "comm_pc", "resid_rp", "comm_rp")
dt_mm %>%
  group_by(yyyymm) %>%
  summarise(tavgm = mean(temperature)) %>%
  merge(., dt_run[, col_elec]) -> df_plot1
df_plot1$date01 <- as.Date(paste0(df_plot1$yyyymm, "-01"))

# Two series, resid #
scalefactor <- max(df_plot1$resid_pc) / max(df_plot1$tavgm)
fig_twoseries <- ggplot(df_plot1, aes(x=date01))+
  geom_line(aes(y=tavgm, color="temperature"))+
  geom_line(aes(y=resid_pc/scalefactor, color="electricity usage"))+
  scale_y_continuous(name="degree Celsius", sec.axis=sec_axis(~.*scalefactor, name="kWh/person"))+
  scale_x_date(breaks=seq(as.Date("1990-01-01"), as.Date("2022-01-01"), by ="8 years"), 
               labels=as.character(seq(1990, 2022, by=8)))+
  scale_color_manual(name="", values=c("orangered", "turquoise3"))+
  labs(x="")
fig_twoseries +
  theme(axis.title.y = element_text(color = "turquoise3"),
        axis.title.y.right = element_text(color = "orangered"),
        legend.position = "bottom")
ggsave(file=file.path(md, "output", "fig_WA_series_pc.png"), width=8, height=0.618*6.5)

# Two series, comm #
scalefactor <- max(df_plot1$comm_pc) / max(df_plot1$tavgm)
fig_twoseries <- ggplot(df_plot1, aes(x=date01))+
  geom_line(aes(y=tavgm, color="temperature"))+
  geom_line(aes(y=comm_pc/scalefactor, color="electricity usage"))+
  scale_y_continuous(name="degree Celsius", sec.axis=sec_axis(~.*scalefactor, name="kWh/person"))+
  scale_x_date(breaks=seq(as.Date("1990-01-01"), as.Date("2022-01-01"), by ="8 years"), 
               labels=as.character(seq(1990, 2022, by=8)))+
  scale_color_manual(name="", values=c("orangered", "turquoise3"))+
  labs(x="")
fig_twoseries +
  theme(axis.title.y = element_text(color = "turquoise3"),
        axis.title.y.right = element_text(color = "orangered"),
        legend.position = "bottom")
ggsave(file=file.path(md, "output", "fig_WA_series_comm_pc.png"), width=8, height=0.618*6.5)

# Figure. Retail price real #
scalefactor <- max(df_plot1$tavgm) / max(df_plot1$comm_rp) #real price
ggplot(df_plot1, aes(x=date01))+
  geom_line(aes(y=tavgm/scalefactor), col="turquoise3")+
  geom_line(aes(y=resid_rp, color="residential"))+
  geom_line(aes(y=comm_rp, color="commercial"))+
  scale_y_continuous(name="cents/kWh", sec.axis=sec_axis(~.*scalefactor, name="degree Celsius"))+
  scale_x_date(breaks=seq(as.Date("1990-01-01"), as.Date("2022-01-01"), by ="8 years"), 
               labels=as.character(seq(1990, 2022, by=8)))+
  scale_color_manual(name="", values=c("violet", "tomato"))+
  labs(x="")+
  theme(axis.title.y.right = element_text(color = "turquoise3"), 
        legend.position = "bottom")
ggsave(file=file.path(md, "output", "fig_WA_rprices_r3.png"), width=8, height=0.618*6.5)

# Scatter plots -------------------------------------------------
# The series to plot has linear trend removed, as how resid_pc is treated in the TRF model
lm_resid_tb <- lm(log(resid_pc) ~ nn_m, data=dt_run)
df_plot1$resid_rsdl <- resid(lm_resid_tb) + coef(lm_resid_tb)[1]
lm_comm_tb <- lm(log(comm_pc) ~ nn_m*i200211+comrp_std, data=dt_run)
df_plot1$comm_rsdl <- resid(lm_comm_tb) + coef(lm_comm_tb)[1]

######################
# Stationarity tests #
######################
pp_resid <- ur.pp(df_plot1$resid_rsdl, type="Z-tau", model="constant", lags="short")
pp_comm <- ur.pp(df_plot1$comm_rsdl, type="Z-tau", model="constant", lags="short")
sink(file.path(md, "output", "pp_y_mtrd.txt"))
summary(pp_resid)
summary(pp_comm)
sink(file=NULL)

# a) log usage
#plotbymonth <- ggplot()+
#  geom_point(df_plot1, mapping = aes(x=tavgm, y=log(resid_pc), color=factor(Month)))+
#  labs(x="Temperature (C)", y="log usage", color="month")
# b) log usage residuals # USE THIS.
plot_rsdl_res <- ggplot()+
  geom_point(df_plot1, mapping = aes(x=tavgm, y=resid_rsdl, color=factor(Month)))+
  labs(x="Temperature (C)", y="log usage residuals", color="month")

plot_rsdl_com <- ggplot()+
  geom_point(df_plot1, mapping = aes(x=tavgm, y=comm_rsdl, color=factor(Month)))+
  labs(x="Temperature (C)", y="log usage residuals", color="month")

## Bootstrapped CI ------------
dt_run %>%
  mutate(
    t_i = nn_m*i200211, x_t = xts*nn_m, xsin1_t = xsin1*nn_m, xcos1_t = xcos1*nn_m,
    x_rp=xts*resrp_std, xsin1_rp=xsin1*resrp_std, xcos1_rp=xcos1*resrp_std,
    x_cp=xts*comrp_std, xsin1_cp=xsin1*comrp_std, xcos1_cp=xcos1*comrp_std
  ) %>%
  GetMonthcol() -> dt_run

####################
#  Bootstrap - CI  #
####################
round <- 1000
y_term <- "resid_pc"
x_term <- c("xts", "xts2", "xsin1", "xcos1", "xsin2", "xcos2", "nn_m")
y <- log(dt_run[[y_term]])
k <- length(x_term)
# No month dummies
#x_term <- c(x_term, "m02","m03","m04","m05","m06","m07","m08","m09","m10","m11","m12")
X11 <- as.matrix(dt_run[, x_term])
ones <- matrix(1, nrow=length(y), ncol=1)
X <- cbind(ones, X11)
var0 <- c("(Intercept)", paste0("X11", x_term[1:6]))
rgr <- lm(y ~ X11, na.action = "na.exclude")
u <- resid(rgr) # unchanged, original residuals
tib_coef <- broom::tidy(rgr)
beta.mx <- as.matrix(tib_coef[, 2]) # unchanged, original coefficients

list_bt_beta <- vector("list", round)
for (rd in 1:round){
  list_bt_beta[rd] <- tib_coef[which(tib_coef$term %in% var0), 2]
  u_ <- sample(u, replace = TRUE)
  y_ <- X %*% beta.mx + u_
  rgr_ <- lm(y_ ~ X11)
  tib_coef <- broom::tidy(rgr_)
  #print(rd)
}

# Get bt_CIs
a <- min(dt_mm$temperature); b <- max(dt_mm$temperature)
mx22 <- create_sim_mx(0, 22, tmin=a, tmax=b, order=c(2, 2), intercept=TRUE)
df_trf_bt <- fill_table(list_bt_beta, 0, 22, sim_mx=mx22, round=1000)
# Figure. Plot + fitted curve w/ CI #
plot_rsdl_res +
  #scale_x_continuous(limits=c(0, 25), breaks=seq(0, 25, 5))+
  geom_line(data=df_trf_bt, aes(x=temperature, y=fv_median))+
  geom_ribbon(data=df_trf_bt, aes(x=temperature, ymin=CI_lb, ymax=CI_ub), fill="grey", alpha=0.5)
ggsave(file=file.path(md, "output", "fig_WA_plotTRF_bt.png"), width=7, height=0.618*6)
#ggsave(file=file.path(md, "output", "new_adjusted", "plot_TRF_bt.png"), width=7, height=0.618*6)

## Commercial ------------

####################
#  Bootstrap - CI  #
####################
round <- 1000
y_term <- "comm_pc"
x_term <- c("xts", "xts2", "xsin1", "xcos1", "nn_m", "i200211", "t_i", "comrp_std")
y <- log(dt_run[[y_term]])
k <- length(x_term)
X11 <- as.matrix(dt_run[, x_term])
ones <- matrix(1, nrow=length(y), ncol=1)
X <- cbind(ones, X11)
var0 <- c("(Intercept)", paste0("X11", x_term[1:4]))

rgr <- lm(y ~ X11, na.action = "na.exclude")
u <- resid(rgr) # unchanged, original residuals
tib_coef <- broom::tidy(rgr)
beta.mx <- as.matrix(tib_coef[, 2]) # unchanged, original coefficients

list_bt_beta <- vector("list", round)
for (rd in 1:round){
  list_bt_beta[rd] <- tib_coef[which(tib_coef$term %in% var0), 2]
  u_ <- sample(u, replace = TRUE)
  y_ <- X %*% beta.mx + u_
  rgr_ <- lm(y_ ~ X11)
  tib_coef <- broom::tidy(rgr_)
  print(rd)
}

mx21 <- create_sim_mx(0, 25, tmin=a, tmax=b, order=c(2, 1), intercept=TRUE)
df_trf_bt <- fill_table(list_bt_beta, 0, 25, sim_mx=mx21, round=1000)
# Figure. Plot + fitted curve w/ CI #
plot_rsdl_com +
  scale_x_continuous(limits=c(0, 25), breaks=seq(0, 25, 5))+
  geom_line(data=df_trf_bt, aes(x=temperature, y=fv_median))+
  geom_ribbon(data=df_trf_bt, aes(x=temperature, ymin=CI_lb, ymax=CI_ub), fill="grey", alpha=0.5)
ggsave(file=file.path(md, "output", "new_adjusted", "plot_TRF_bt_com.png"), width=7, height=0.618*6)

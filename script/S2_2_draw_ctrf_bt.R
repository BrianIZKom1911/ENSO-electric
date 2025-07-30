rm(list = ls())

library(here)
library(dplyr)
library(broom)
library(ggplot2)

md <- here()
source(file.path(md, "00_functions.R"))
load(file.path(md, "S1_dt_run.RDS"))
load(file.path(md, "S1_dataforcompare.RData"))
# Breakpoint indicator
dt_run$i200211 <- as.numeric(dt_run$Year>=2003)
dt_run[(dt_run$Year==2002)&(dt_run$Month>=11), "i200211"] <- 1
# More variables
dt_run %>%
  mutate(
    t_i = nn_m*i200211, x_t = xts*nn_m, xsin1_t = xsin1*nn_m, xcos1_t = xcos1*nn_m,
    x_cp=xts*comrp_std, xsin1_cp=xsin1*comrp_std, xcos1_cp=xcos1*comrp_std
  ) %>%
  GetMonthcol() -> dt_run

# Residential -----------------------------------------
x_term <- c("xts", "xts2", "xsin1", "xcos1", "xsin2", "xcos2", 
            "resrp_std", "nn_m", "x_t", "xsin1_t", "xcos1_t")
# Add month dummies
x_termm <- c(x_term, "m02","m03","m04","m05","m06","m07","m08","m09","m10","m11","m12")
vars0 <- c("(Intercept)", paste0("Xa", x_term[1:6]))
vars1 <- paste0("Xa", x_term[8:11])
vars2 <- c("Xaresrp_std")

lists_bt_beta <- bootstrap_ctrf(dt_run, "resid_pc", x_termm, 1000)
mx22 <- create_sim_mx(0, 22, tmin=a, tmax=b, order=c(2, 2), intercept=TRUE)
mx11 <- create_sim_mx(0, 22, tmin=a, tmax=b, order=c(1, 1), intercept=TRUE)
# base TRF
df_ctrf_bt0 <- fill_table(lists_bt_beta[[1]], 22, 0, sim_mx=mx22, round=1000)
# time cross-TRF
df_ctrf_bt1 <- fill_table(lists_bt_beta[[2]], 22, 0, sim_mx=mx11, round=1000)

## Plot ----------
ggplot()+
  geom_line(data=df_ctrf_bt0, aes(x=temperature, y=fv_median))+
  geom_ribbon(data=df_ctrf_bt0, aes(x=temperature, ymin=CI_lb, ymax=CI_ub), fill="grey", alpha=0.5)+
  labs(x="Temperature (C)", y="base usage (log)")+
  scale_x_continuous(breaks = seq(0, 20, 5))
ggsave(file=file.path(md, "output", "cTRF_res_0.png"), width=5, height=0.618*5)

ggplot()+
  geom_line(data=df_ctrf_bt1, aes(x=temperature, y=fv_median))+
  geom_ribbon(data=df_ctrf_bt1, aes(x=temperature, ymin=CI_lb, ymax=CI_ub), fill="grey", alpha=0.5)+
  labs(x="Temperature (C)", y="time partial effect")+
  scale_x_continuous(breaks = seq(0, 20, 5))
ggsave(file=file.path(md, "output", "cTRF_res_1.png"), width=5, height=0.618*5)

# Commercial ----------------------------------------
x_term <- c("xts", "xsin1", "xcos1", 
            "comrp_std", "x_cp", "xsin1_cp", "xcos1_cp",
            "nn_m", "i200211", "t_i", "x_t", "xsin1_t", "xcos1_t")
# Add month dummies
x_termm <- c(x_term, "m02","m03","m04","m05","m06","m07","m08","m09","m10","m11","m12")
vars0 <- c("(Intercept)", paste0("Xa", x_term[1:3]))
vars1 <- paste0("Xa", c("nn_m", "t_i", "x_t", "xsin1_t", "xcos1_t"))
vars2 <- paste0("Xa", x_term[4:7])

lists_bt_beta <- bootstrap_ctrf(dt_run, "comm_pc", x_termm, 1000)
mx11 <- create_sim_mx(0, 22, tmin=a, tmax=b, order=c(1, 1), intercept=TRUE)
mx11p <- cbind(mx11[, 1], mx11)

# base TRF
df_ctrf_bt0 <- fill_table(lists_bt_beta[[1]], 22, 0, sim_mx=mx11, round=1000)
# time cross-TRF after 2002/11
df_ctrf_bt1 <- fill_table(lists_bt_beta[[2]], 22, 0, sim_mx=mx11p, round=1000)
# price cross-TRF
df_ctrf_bt2 <- fill_table(lists_bt_beta[[3]], 22, 0, sim_mx=mx11, round=1000)

## Plot ----------
ggplot()+
  geom_line(data=df_ctrf_bt0, aes(x=temperature, y=fv_median))+
  geom_ribbon(data=df_ctrf_bt0, aes(x=temperature, ymin=CI_lb, ymax=CI_ub), fill="grey", alpha=0.5)+
  labs(x="Temperature (C)", y="base usage (log)")+
  scale_x_continuous(breaks = seq(0, 20, 5))
ggsave(file=file.path(md, "output", "cTRF_com_0.png"), width=5, height=0.618*5)

ggplot()+
  geom_line(data=df_ctrf_bt1, aes(x=temperature, y=fv_median))+
  geom_ribbon(data=df_ctrf_bt1, aes(x=temperature, ymin=CI_lb, ymax=CI_ub), fill="grey", alpha=0.5)+
  labs(x="Temperature (C)", y="time partial effect")+
  scale_x_continuous(breaks = seq(0, 20, 5))
ggsave(file=file.path(md, "output", "cTRF_com_1.png"), width=5, height=0.618*5)

ggplot()+
  geom_line(data=df_ctrf_bt2, aes(x=temperature, y=fv_median))+
  geom_ribbon(data=df_ctrf_bt2, aes(x=temperature, ymin=CI_lb, ymax=CI_ub), fill="grey", alpha=0.5)+
  labs(x="Temperature (C)", y="price partial effect")+
  scale_x_continuous(breaks = seq(0, 20, 5))
ggsave(file=file.path(md, "output", "cTRF_com_2.png"), width=5, height=0.618*5)

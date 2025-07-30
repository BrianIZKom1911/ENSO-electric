# Tip: 1 kWh can power a 100-watt light bulb for 10 hours.
rm(list = ls())

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

md <- here()
if (file.exists(file.path(md, "output", "cntf_series"))){
  print("Expected folder exists") 
} else {dir.create(file.path(md, "output", "cntf_series"))}
load(file.path(md, "S1_dataforcompare.RData"))
df_compar %>% 
  mutate(date01 = as.Date(paste(Year, Month, "01", sep="-"))) -> df_compar

GetEventdate <- function(df, col) {
  df %>%
    mutate(en_flag = get(col),
           en_change = en_flag - lag(en_flag, default = 0),  #detect changes
           start_date = as.Date(ifelse(en_change == 1, date01, NA)),
           end_date = as.Date(ifelse(en_change == -1, lag(date01), NA)) 
    ) %>%
    # Fill end_date upward to propagate detected end dates
    fill(end_date, .direction = "up") %>%
    # Check ongoing events at the end of df
    mutate(end_date = as.Date(ifelse(is.na(end_date) & en_flag == 1, max(date01), end_date))) %>%
    # Fill start_date downward to propagate detected start dates
    fill(start_date, .direction="down") %>%
    filter(en_flag == 1) %>%
    select(en_flag, start_date, end_date) %>%
    drop_na() %>%
    unique() -> col_date
  return(col_date)
}

# 1. Description -----------------------------------------------
read_csv(file.path(md, "data_clean", "nino3.4_anom_1960_2024 - Copy.csv")) %>%
  mutate(date01 = as.Date(paste(Year, Month, "01", sep="-"))) -> dt_Nino

## ENSO occurrence ----------
dt_Nino %>%
  GetEventdate("el_nino") %>%
  mutate(event_type = ifelse(en_flag==1, "El_Nino", NA)) %>%
  select(-en_flag) -> nino_date
dt_Nino %>%
  GetEventdate("la_nina") %>%
  mutate(event_type = ifelse(en_flag==1, "La_Nina", NA)) %>%
  select(-en_flag) -> nina_date
df_eventdate <- bind_rows(nino_date, nina_date)
saveRDS(df_eventdate, "S3_eventdate.RDS")
rm(nino_date, nina_date)

# Figure. Series and bgd #
ggplot() +
  # Background layer with colorful bars for El Nino and La Nina
  geom_rect(data=df_eventdate,
            aes(xmin=start_date, xmax=end_date, ymin=-Inf, ymax=Inf, fill=event_type),
            color=NA, alpha=0.8) +
  scale_fill_manual(values=c("El_Nino"="lemonchiffon", "La_Nina"="lightcyan"), guide="none")+
  # Constant lines at +/-4 degrees
  geom_hline(yintercept=0.4, color="darkorange", linetype="dashed")+
  geom_hline(yintercept=-0.4, color="darkorange", linetype="dashed")+
  # Temperature series
  geom_line(data=dt_Nino, aes(x=date01, y=anom), color="dodgerblue2")+
  labs(title=NULL, x=NULL, y="Temperature (C)")+
  theme_minimal()+
  theme(legend.position = "none")
ggsave(file=file.path(md, "output", "SSTa_1960.png"), bg="white", width=8, height=0.618*7)

# 2. Cntfc series with background -------------------------------------

# Figure. By how much does temperature change? #
ggplot() +
  # Background layer
  geom_rect(data=df_eventdate[df_eventdate$start_date >= as.Date("1990-01-01"), ],
            aes(xmin=start_date, xmax=end_date, ymin=-Inf, ymax=Inf, fill=event_type),
            color=NA, alpha=0.8) +
  scale_fill_manual(values=c("El_Nino"="lemonchiffon", "La_Nina"="lightcyan"), guide="none")+
  # Temperature difference
  geom_bar(data=df_compar_T, aes(x=date01, y=diff_T), 
           color="turquoise3", stat='identity', position='identity', width=0.05)+
  scale_x_date(breaks=seq(as.Date("1990-01-01"), as.Date("2022-01-01"), by="8 years"), 
               labels=as.character(seq(1990, 2022, by=8)))+
  ylim(-0.7, 1.5)+
  labs(title=NULL, x=NULL, y="Temperature differences (C)")+
  theme_minimal()+
  theme(legend.position = "none")
ggsave(file=file.path(md, "output", "cntf_series", "diff_T_1990.png"), 
       bg="white", width=8, height=0.618*7)

# Figures. Monthly differences of elec #
ggplot() +
  # Background layer
  geom_rect(data=df_eventdate[df_eventdate$start_date >= as.Date("1990-01-01"), ],
            aes(xmin=start_date, xmax=end_date, ymin=-Inf, ymax=Inf, fill=event_type),
            color=NA, alpha=0.8) +
  scale_fill_manual(values=c("El_Nino"="lemonchiffon", "La_Nina"="lightcyan"), guide="none")+
  # Temperature difference
  geom_bar(data=df_compar, aes(x=date01, y=pers_resid), 
           color="orangered", stat='identity', position='identity', width=0.05)+
  scale_x_date(breaks=seq(as.Date("1990-01-01"), as.Date("2022-01-01"), by="8 years"), 
               labels=as.character(seq(1990, 2022, by=8)))+
  scale_y_continuous(breaks=seq(-4.8, 2.4, by=2.4))+
  labs(title=NULL, x=NULL, y="percentage")+
  theme_minimal()+
  theme(legend.position = "none")
ggsave(file=file.path(md, "output", "cntf_series", "pers_resid.png"), 
       bg="white", width=8, height=0.618*7)

ggplot() +
  # Background layer
  geom_rect(data=df_eventdate[df_eventdate$start_date >= as.Date("1990-01-01"), ],
            aes(xmin=start_date, xmax=end_date, ymin=-Inf, ymax=Inf, fill=event_type),
            color=NA, alpha=0.8) +
  scale_fill_manual(values=c("El_Nino"="lemonchiffon", "La_Nina"="lightcyan"), guide="none")+
  # Temperature difference
  geom_bar(data=df_compar, aes(x=date01, y=pers_comm), 
           color="violet", stat='identity', position='identity', width=0.05)+
  scale_x_date(breaks=seq(as.Date("1990-01-01"), as.Date("2022-01-01"), by="8 years"), 
               labels=as.character(seq(1990, 2022, by=8)))+
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  labs(title=NULL, x=NULL, y="percentage")+
  theme_minimal()+
  theme(legend.position = "none")
ggsave(file=file.path(md, "output", "cntf_series", "pers_comm.png"),
       bg="white", width=8, height=0.618*7)

# 3. Aggregation ---------------------------------------------------

# Annual gain and loss
df_compar %>%
  mutate(pos_1res = as.numeric(diff_resid > 0), neg_1res = 1-pos_1res,
         pos_1com = as.numeric(diff_comm > 0), neg_1com = 1-pos_1com) %>%
  group_by(Year) %>%
  summarise(base_resid = sum(kwh_resid), base_comm = sum(kwh_comm),
            yrd_p_res = sum(diff_resid*pos_1res), 
            yrd_n_res = sum(diff_resid*neg_1res),
            yrd_p_com = sum(diff_comm*pos_1com), 
            yrd_n_com = sum(diff_comm*neg_1com)) -> df_yrdiff
df_yrdiff %>%
  mutate(pers_p_resid = (yrd_p_res/base_resid)*100, 
         pers_n_resid = (yrd_n_res/base_resid)*100,
         pers_p_comm = (yrd_p_com/base_comm)*100, 
         pers_n_comm = (yrd_n_com/base_comm)*100) -> df_yrdiff
# Figure. Annual differences #
df_yrdiff %>%
  select(Year, pers_p_resid, pers_n_resid) %>%
  pivot_longer(!Year, names_to = "pos", values_to = "diff") %>%
  mutate(pos = as.numeric(pos=="pers_p_resid")) %>% 
  ggplot(aes(x=Year, y=diff, fill=pos))+
  geom_bar(stat='identity', position='identity', width=0.1)+
  ylim(-1, 1)+
  labs(x=NULL, y="percentage")+
  theme(legend.position = 'none')
ggsave(file=file.path(md, "output", "cntf_series", "annual_pers_resid.png"), 
       width=7, height=0.618*7)

df_yrdiff %>%
  select(Year, pers_p_comm, pers_n_comm) %>%
  pivot_longer(!Year, names_to = "pos", values_to = "diff") %>%
  mutate(pos = as.numeric(pos=="pers_p_comm")) %>% 
  ggplot(aes(x=Year, y=diff, fill=pos))+
  geom_bar(stat='identity', position='identity', width=0.1)+
  ylim(-0.5, 0.5)+
  labs(x=NULL, y="percentage")+
  theme(legend.position = 'none')
ggsave(file=file.path(md, "output", "cntf_series", "annual_pers_comm.png"), 
       width=7, height=0.618*7)

# New: Aggregate by individual event
# i) Assign each row to an event based on date range
df_compar %>%
  rowwise() %>%
  mutate(event_id=list(which(date01 >= df_eventdate$start_date & date01 <= df_eventdate$end_date))) %>%
  unnest(event_id) %>%
  mutate(start_date = df_eventdate$start_date[event_id],
         end_date = df_eventdate$end_date[event_id],
         event_type = df_eventdate$event_type[event_id]) %>%
  ungroup() -> df_compar_events
# ii) Aggregate losses by each event (start_date and end_date)
df_compar_events %>%
  mutate(pos_1res = as.numeric(diff_resid > 0), neg_1res = 1-pos_1res,
         pos_1com = as.numeric(diff_comm > 0), neg_1com = 1-pos_1com) %>%
  group_by(start_date, end_date, event_type) %>%
  summarise(base_resid = sum(kwh_resid), base_comm = sum(kwh_comm),
            envd_p_res = sum(diff_resid*pos_1res), 
            envd_n_res = sum(diff_resid*neg_1res),
            envd_p_com = sum(diff_comm*pos_1com), 
            envd_n_com = sum(diff_comm*neg_1com), .groups="drop") -> df_envdiff
df_envdiff %>%
  mutate(pers_p_resid = (envd_p_res/base_resid)*100, 
         pers_n_resid = (envd_n_res/base_resid)*100,
         pers_p_comm = (envd_p_com/base_comm)*100, 
         pers_n_comm = (envd_n_com/base_comm)*100) -> df_envdiff
write.csv(df_envdiff, file.path(md, "output", "cntf_series", "tab_diffbyevent.csv"), row.names=FALSE)

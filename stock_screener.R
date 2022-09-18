library(httr)
library(jsonlite)
library(tidyverse)
library(quantmod)
library(tidyquant)
library(tidymodels)
library(randomForest)
library(glmnet)

sp_500_1 <- read_csv("stock_prices_new.csv")

rm(list = ls())

nas <- tq_exchange("NASDAQ")
nyse <- tq_exchange("NYSE")

nas <- nas %>%
  filter(market.cap >= 1000000000)

nyse <- nyse%>%
  filter(market.cap >= 1000000000)

full_index <- bind_rows(nas,nyse)

start_date <- 
end_date <- Sys.Date()-wday(Sys.Date()+1)

ful_ind_price <-tq_get(full_index$symbol,get="stock.prices",
                       from = start_date, end =end_date)


count_stockyear <- ful_ind_price %>%
  group_by(symbol)%>%
  count(symbol)%>%
  filter(n >=365)%>%
  arrange(n)

sp_500_1 <-ful_ind_price%>%
  filter(symbol %in% count_stockyear$symbol)

sp_500_1 <- sp_500_1 %>%
  filter(date >="2018-01-01")


# date_filter <- ful_ind_price %>%
#   group_by(symbol)%>%
#   slice(1)%>%
#   filter(date == start_date)


# avg_vol <- ful_ind_price %>%
#   filter(symbol %in% date_filter$symbol)%>%
#   group_by(symbol)%>%
#   summarise(mean_vol = mean(volume), n=n())%>%
#   filter(mean_vol >= 500000)%>%
#   select(symbol)

avg_vol <- ful_ind_price %>%
  group_by(symbol)%>%
  summarise(mean_vol = mean(volume), n=n())%>%
  filter(mean_vol >= 500000)%>%
  select(symbol)

sp_500_1 <- sp_500_1%>%
  filter(symbol %in% avg_vol$symbol)

write_csv(ful_ind_price,"stock_prices_new.csv")
remove(ful_ind_price)

sp_500_1$day <- wday(ymd(sp_500_1$date), label = TRUE, abbr = FALSE)

sp_500_1 %>%
  filter(day == "Friday")

sp_500_1 <- sp_500_1 %>%
  filter(date >="2016-06-01")


sp_500_1 <- sp_500_1 %>%
  group_by(symbol)

sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  mutate(return_lag1 = (close/lag(close)-1)*100)

sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  mutate(return_daily = ((close-open)/open)*100)

sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  mutate(close_shift_5 = lead(close, n=5))

sp_500_1 <- sp_500_1%>%
  mutate(target_5 = (close_shift_5-lead(open))/(lead(open))*100)

sp_500_1 <- sp_500_1 %>%
  mutate(target_direction_5 = if_else(target_5>0,1,0))

sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  mutate(close_shift_10 = lead(close, n=10))

sp_500_1 <- sp_500_1%>%
  mutate(target_10 = (close_shift_10-lead(open))/(lead(open))*100)

sp_500_1 <- sp_500_1 %>%
  mutate(target_direction_10 = if_else(target_10>0,1,0))

sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  mutate(close_shift_15 = lead(close, n=15))

sp_500_1 <- sp_500_1%>%
  mutate(target_15 = (close_shift_15-lead(open))/(lead(open))*100)

sp_500_1 <- sp_500_1 %>%
  mutate(target_direction_15 = if_else(target_15>0,1,0))

sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  mutate(close_shift_20 = lead(close, n=20))

sp_500_1 <- sp_500_1%>%
  mutate(target_20 = (close_shift_20-lead(open))/(lead(open))*100)

sp_500_1 <- sp_500_1 %>%
  mutate(target_direction_20 = if_else(target_20>0,1,0))

# sp_500_1 <- sp_500_1 %>%
#   filter(!symbol %in% c("AEVA","AMCR","BTRS","DKNG","LAZR","LPRO","AMLX",
#                         "DOUG","TKLF","TPG","VIGL","ZGN","HCP","IOT","NU"))

# sp_500_1$date <- as.POSIXct(sp_500_1$date)

sp_500_1 <- sp_500_1%>%
  group_by(symbol) %>%
  tq_mutate(select     = close, 
            mutate_fun = MACD, 
            nFast      = 5, 
            nSlow      = 15, 
            nSig       = 9, 
            maType     = "EMA")%>%
  mutate(diff_short = macd - signal)


sp_500_1$diff_short_lag1 <- lag(sp_500_1$diff_short)
sp_500_1$diff_short_lag2 <- lag(sp_500_1$diff_short, n=2)
sp_500_1$diff_short_lag3 <- lag(sp_500_1$diff_short, n=3)
sp_500_1$diff_short_lag4 <- lag(sp_500_1$diff_short, n=4)
sp_500_1$diff_short_lag5 <- lag(sp_500_1$diff_short, n=5)
sp_500_1$diff_short_lag6 <- lag(sp_500_1$diff_short, n=6)
sp_500_1$diff_short_lag7 <- lag(sp_500_1$diff_short, n=7)
sp_500_1$diff_short_lag8 <- lag(sp_500_1$diff_short, n=8)
sp_500_1$diff_short_lag9 <- lag(sp_500_1$diff_short, n=9)
sp_500_1$diff_short_lag10 <- lag(sp_500_1$diff_short, n=10)
sp_500_1$diff_short_lag11 <- lag(sp_500_1$diff_short, n=11)
sp_500_1$diff_short_lag12 <- lag(sp_500_1$diff_short, n=12)
sp_500_1$diff_short_lag13 <- lag(sp_500_1$diff_short, n=13)
sp_500_1$diff_short_lag14 <- lag(sp_500_1$diff_short, n=14)
sp_500_1$diff_short_lag15 <- lag(sp_500_1$diff_short, n=15)
sp_500_1$diff_short_lag16 <- lag(sp_500_1$diff_short, n=16)
sp_500_1$diff_short_lag17 <- lag(sp_500_1$diff_short, n=17)
sp_500_1$diff_short_lag18 <- lag(sp_500_1$diff_short, n=18)
sp_500_1$diff_short_lag19<- lag(sp_500_1$diff_short, n=19)
sp_500_1$diff_short_lag20<- lag(sp_500_1$diff_short, n=20)

sp_500_1 <- sp_500_1%>%
  group_by(symbol) %>%
  tq_mutate(select     = close, 
            mutate_fun = MACD, 
            nFast      = 12, 
            nSlow      = 26, 
            nSig       = 9, 
            maType     = "EMA")%>%
  mutate(diff_long = macd - signal)


sp_500_1$diff_long_lag1 <- lag(sp_500_1$diff_long)
sp_500_1$diff_long_lag2 <- lag(sp_500_1$diff_long, n=2)
sp_500_1$diff_long_lag3 <- lag(sp_500_1$diff_long, n=3)
sp_500_1$diff_long_lag4 <- lag(sp_500_1$diff_long, n=4)
sp_500_1$diff_long_lag5 <- lag(sp_500_1$diff_long, n=5)
sp_500_1$diff_long_lag6 <- lag(sp_500_1$diff_long, n=6)
sp_500_1$diff_long_lag7 <- lag(sp_500_1$diff_long, n=7)
sp_500_1$diff_long_lag8 <- lag(sp_500_1$diff_long, n=8)
sp_500_1$diff_long_lag9 <- lag(sp_500_1$diff_long, n=9)
sp_500_1$diff_long_lag10 <- lag(sp_500_1$diff_long, n=10)
sp_500_1$diff_long_lag11 <- lag(sp_500_1$diff_long, n=11)
sp_500_1$diff_long_lag12 <- lag(sp_500_1$diff_long, n=12)
sp_500_1$diff_long_lag13 <- lag(sp_500_1$diff_long, n=13)
sp_500_1$diff_long_lag14 <- lag(sp_500_1$diff_long, n=14)
sp_500_1$diff_long_lag15 <- lag(sp_500_1$diff_long, n=15)
sp_500_1$diff_long_lag16 <- lag(sp_500_1$diff_long, n=16)
sp_500_1$diff_long_lag17 <- lag(sp_500_1$diff_long, n=17)
sp_500_1$diff_long_lag18 <- lag(sp_500_1$diff_long, n=18)
sp_500_1$diff_long_lag19 <- lag(sp_500_1$diff_long, n=19)
sp_500_1$diff_long_lag20 <- lag(sp_500_1$diff_long, n=20)

sp_500_1<- sp_500_1 %>%
  tq_mutate(select = close,
            mutate_fun = HMA,
            n = 9,
            col_rename = "hma_9")

sp_500_1 <- sp_500_1 %>%
  tq_mutate(select = close,
            mutate_fun = HMA,
            n = 25,
            col_rename = "hma_25")

sp_500_1 <- sp_500_1 %>%
  tq_mutate(select = close,
            mutate_fun = HMA,
            n = 49,
            col_rename = "hma_49")

sp_500_1 <- sp_500_1 %>%
  tq_mutate(select = close,
            mutate_fun = HMA,
            n = 81,
            col_rename = "hma_81")

sp_500_1 <- sp_500_1 %>%
  tq_mutate(select = close,
            mutate_fun = HMA,
            n = 200,
            col_rename = "hma_200")

sp_500_1 <- sp_500_1 %>%
  tq_mutate(select = close,
            mutate_fun = SMA,
            n = 200,
            col_rename = "sma_200")



sp_500_1 <- sp_500_1 %>%
  tq_mutate(select = close,
            mutate_fun = SMA,
            n = 100,
            col_rename = "sma_100")

sp_500_1 <- sp_500_1 %>%
  tq_mutate(select = close,
            mutate_fun = SMA,
            n = 50,
            col_rename = "sma_50")

sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  tq_mutate(select = close,
            mutate_fun = RSI,
            n=14,
            col_rename = "rsi_14"
  )

sp_500_1$rsi_14_lag1 <- lag(sp_500_1$rsi_14)
sp_500_1$rsi_14_lag2 <- lag(sp_500_1$rsi_14, n=2)
sp_500_1$rsi_14_lag3 <- lag(sp_500_1$rsi_14, n=3)
sp_500_1$rsi_14_lag4 <- lag(sp_500_1$rsi_14, n=4)
sp_500_1$rsi_14_lag5 <- lag(sp_500_1$rsi_14, n=5)
sp_500_1$rsi_14_lag6 <- lag(sp_500_1$rsi_14, n=6)
sp_500_1$rsi_14_lag7 <- lag(sp_500_1$rsi_14, n=7)
sp_500_1$rsi_14_lag8 <- lag(sp_500_1$rsi_14, n=8)
sp_500_1$rsi_14_lag9 <- lag(sp_500_1$rsi_14, n=9)
sp_500_1$rsi_14_lag10 <- lag(sp_500_1$rsi_14, n=10)
sp_500_1$rsi_14_lag11 <- lag(sp_500_1$rsi_14, n=11)
sp_500_1$rsi_14_lag12 <- lag(sp_500_1$rsi_14, n=12)
sp_500_1$rsi_14_lag13 <- lag(sp_500_1$rsi_14, n=13)
sp_500_1$rsi_14_lag14 <- lag(sp_500_1$rsi_14, n=14)
sp_500_1$rsi_14_lag15 <- lag(sp_500_1$rsi_14, n=15)
sp_500_1$rsi_14_lag16 <- lag(sp_500_1$rsi_14, n=16)
sp_500_1$rsi_14_lag17 <- lag(sp_500_1$rsi_14, n=17)
sp_500_1$rsi_14_lag18 <- lag(sp_500_1$rsi_14, n=18)
sp_500_1$rsi_14_lag19 <- lag(sp_500_1$rsi_14, n=19)
sp_500_1$rsi_14_lag20 <- lag(sp_500_1$rsi_14, n=20)


sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  tq_mutate(select = c("high","low"),
            mutate_fun = SAR,
            col_rename = "sar")



sp_500_1$sar_ratio <- sp_500_1$close/sp_500_1$sar

sp_500_1$sar_ratio_lag1 <- lag(sp_500_1$sar_ratio)
sp_500_1$sar_ratio_lag2 <- lag(sp_500_1$sar_ratio, n=2)
sp_500_1$sar_ratio_lag3 <- lag(sp_500_1$sar_ratio, n=3)
sp_500_1$sar_ratio_lag4 <- lag(sp_500_1$sar_ratio, n=4)
sp_500_1$sar_ratio_lag5 <- lag(sp_500_1$sar_ratio, n=5)
sp_500_1$sar_ratio_lag6 <- lag(sp_500_1$sar_ratio, n=6)
sp_500_1$sar_ratio_lag7 <- lag(sp_500_1$sar_ratio, n=7)
sp_500_1$sar_ratio_lag8 <- lag(sp_500_1$sar_ratio, n=8)
sp_500_1$sar_ratio_lag9 <- lag(sp_500_1$sar_ratio, n=9)
sp_500_1$sar_ratio_lag10 <- lag(sp_500_1$sar_ratio, n=10)
sp_500_1$sar_ratio_lag11 <- lag(sp_500_1$sar_ratio, n=11)
sp_500_1$sar_ratio_lag12 <- lag(sp_500_1$sar_ratio, n=12)
sp_500_1$sar_ratio_lag13 <- lag(sp_500_1$sar_ratio, n=13)
sp_500_1$sar_ratio_lag14 <- lag(sp_500_1$sar_ratio, n=14)
sp_500_1$sar_ratio_lag15 <- lag(sp_500_1$sar_ratio, n=15)
sp_500_1$sar_ratio_lag16 <- lag(sp_500_1$sar_ratio, n=16)
sp_500_1$sar_ratio_lag17 <- lag(sp_500_1$sar_ratio, n=17)
sp_500_1$sar_ratio_lag18 <- lag(sp_500_1$sar_ratio, n=18)
sp_500_1$sar_ratio_lag19 <- lag(sp_500_1$sar_ratio, n=19)
sp_500_1$sar_ratio_lag20 <- lag(sp_500_1$sar_ratio, n=20)



sp_500_1 <- sp_500_1 %>%
  filter(!symbol %in% c("AEVA","AMCR","BTRS","DKNG","LAZR","LPRO","MAXR"))

sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  tq_mutate(select = c("high","low","close"),
            mutate_fun = stoch)

sp_500_1$stoch_diff <- sp_500_1$fastD/sp_500_1$stoch

sp_500_1$stoch_diff_lag1 <-lag(sp_500_1$stoch_diff)
sp_500_1$stoch_diff_lag2 <-lag(sp_500_1$stoch_diff,n=2)
sp_500_1$stoch_diff_lag3 <-lag(sp_500_1$stoch_diff,n=3)
sp_500_1$stoch_diff_lag4<-lag(sp_500_1$stoch_diff,n=4)
sp_500_1$stoch_diff_lag5<-lag(sp_500_1$stoch_diff,n=5)
sp_500_1$stoch_diff_lag6 <-lag(sp_500_1$stoch_diff, n=6)
sp_500_1$stoch_diff_lag7 <-lag(sp_500_1$stoch_diff,n=7)
sp_500_1$stoch_diff_lag8 <-lag(sp_500_1$stoch_diff,n=8)
sp_500_1$stoch_diff_lag9<-lag(sp_500_1$stoch_diff,n=9)
sp_500_1$stoch_diff_lag10<-lag(sp_500_1$stoch_diff,n=10)
sp_500_1$stoch_diff_lag11 <-lag(sp_500_1$stoch_diff,n=11)
sp_500_1$stoch_diff_lag12 <-lag(sp_500_1$stoch_diff,n=12)
sp_500_1$stoch_diff_lag13 <-lag(sp_500_1$stoch_diff,n=13)
sp_500_1$stoch_diff_lag14<-lag(sp_500_1$stoch_diff,n=14)
sp_500_1$stoch_diff_lag15<-lag(sp_500_1$stoch_diff,n=15)
sp_500_1$stoch_diff_lag16 <-lag(sp_500_1$stoch_diff, n=16)
sp_500_1$stoch_diff_lag17 <-lag(sp_500_1$stoch_diff,n=17)
sp_500_1$stoch_diff_lag18 <-lag(sp_500_1$stoch_diff,n=18)
sp_500_1$stoch_diff_lag19 <-lag(sp_500_1$stoch_diff,n=19)
sp_500_1$stoch_diff_lag20 <-lag(sp_500_1$stoch_diff,n=20)


sp_500_1$fastD_lag1 <- lag(sp_500_1$fastD)
sp_500_1$fastD_lag2 <- lag(sp_500_1$fastD, n=2)
sp_500_1$fastD_lag3 <- lag(sp_500_1$fastD, n=3)
sp_500_1$fastD_lag4 <- lag(sp_500_1$fastD, n=4)
sp_500_1$fastD_lag5 <- lag(sp_500_1$fastD, n=5)
sp_500_1$fastD_lag6 <- lag(sp_500_1$fastD, n=6)
sp_500_1$fastD_lag7 <- lag(sp_500_1$fastD, n=7)
sp_500_1$fastD_lag8 <- lag(sp_500_1$fastD, n=8)
sp_500_1$fastD_lag9 <- lag(sp_500_1$fastD, n=9)
sp_500_1$fastD_lag10 <- lag(sp_500_1$fastD, n=10)
sp_500_1$fastD_lag11 <- lag(sp_500_1$fastD, n=11)
sp_500_1$fastD_lag12 <- lag(sp_500_1$fastD, n=12)
sp_500_1$fastD_lag13 <- lag(sp_500_1$fastD, n=13)
sp_500_1$fastD_lag14 <- lag(sp_500_1$fastD, n=14)
sp_500_1$fastD_lag15 <- lag(sp_500_1$fastD, n=15)
sp_500_1$fastD_lag16 <- lag(sp_500_1$fastD, n=16)
sp_500_1$fastD_lag17 <- lag(sp_500_1$fastD, n=17)
sp_500_1$fastD_lag18 <- lag(sp_500_1$fastD, n=18)
sp_500_1$fastD_lag19 <- lag(sp_500_1$fastD, n=19)
sp_500_1$fastD_lag20 <- lag(sp_500_1$fastD, n=20)

sp_500_1$stoch_lag1 <- lag(sp_500_1$stoch)
sp_500_1$stoch_lag2 <- lag(sp_500_1$stoch, n=2)
sp_500_1$stoch_lag3 <- lag(sp_500_1$stoch, n=3)
sp_500_1$stoch_lag4 <- lag(sp_500_1$stoch, n=4)
sp_500_1$stoch_lag5 <- lag(sp_500_1$stoch, n=5)
sp_500_1$stoch_lag6 <- lag(sp_500_1$stoch, n=6)
sp_500_1$stoch_lag7 <- lag(sp_500_1$stoch, n=7)
sp_500_1$stoch_lag8 <- lag(sp_500_1$stoch, n=8)
sp_500_1$stoch_lag9 <- lag(sp_500_1$stoch, n=9)
sp_500_1$stoch_lag10 <- lag(sp_500_1$stoch, n=10)
sp_500_1$stoch_lag11 <- lag(sp_500_1$stoch, n=11)
sp_500_1$stoch_lag12 <- lag(sp_500_1$stoch, n=12)
sp_500_1$stoch_lag13 <- lag(sp_500_1$stoch, n=13)
sp_500_1$stoch_lag14 <- lag(sp_500_1$stoch, n=14)
sp_500_1$stoch_lag15 <- lag(sp_500_1$stoch, n=15)
sp_500_1$stoch_lag16 <- lag(sp_500_1$stoch,n=16)
sp_500_1$stoch_lag17 <- lag(sp_500_1$stoch, n=17)
sp_500_1$stoch_lag18 <- lag(sp_500_1$stoch, n=18)
sp_500_1$stoch_lag19 <- lag(sp_500_1$stoch, n=19)
sp_500_1$stoch_lag20<- lag(sp_500_1$stoch, n=20)

sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  tq_mutate(select = c("high","low","close"),
            mutate_fun = ADX,
            maType = "EMA")

sp_500_1$di_ratio <- sp_500_1$DIp/sp_500_1$DIn
sp_500_1$adx_ratio<- sp_500_1$DIp/sp_500_1$ADX

sp_500_1$di_ratio_lag1 <- lag(sp_500_1$di_ratio)
sp_500_1$di_ratio_lag2<- lag(sp_500_1$di_ratio, n=2)
sp_500_1$di_ratio_lag3<- lag(sp_500_1$di_ratio, n=3)
sp_500_1$di_ratio_lag4<- lag(sp_500_1$di_ratio, n=4)
sp_500_1$di_ratio_lag5<- lag(sp_500_1$di_ratio, n=5)
sp_500_1$di_ratio_lag6 <- lag(sp_500_1$di_ratio, n=6)
sp_500_1$di_ratio_lag7<- lag(sp_500_1$di_ratio, n=7)
sp_500_1$di_ratio_lag8<- lag(sp_500_1$di_ratio, n=8)
sp_500_1$di_ratio_lag9<- lag(sp_500_1$di_ratio, n=9)
sp_500_1$di_ratio_lag10<- lag(sp_500_1$di_ratio, n=10)
sp_500_1$di_ratio_lag11<- lag(sp_500_1$di_ratio, n=11)
sp_500_1$di_ratio_lag12<- lag(sp_500_1$di_ratio, n=12)
sp_500_1$di_ratio_lag13<- lag(sp_500_1$di_ratio, n=13)
sp_500_1$di_ratio_lag14<- lag(sp_500_1$di_ratio, n=14)
sp_500_1$di_ratio_lag15<- lag(sp_500_1$di_ratio, n=15)
sp_500_1$di_ratio_lag16 <- lag(sp_500_1$di_ratio, n=16)
sp_500_1$di_ratio_lag17<- lag(sp_500_1$di_ratio, n=17)
sp_500_1$di_ratio_lag18<- lag(sp_500_1$di_ratio, n=18)
sp_500_1$di_ratio_lag19<- lag(sp_500_1$di_ratio, n=19)
sp_500_1$di_ratio_lag20<- lag(sp_500_1$di_ratio, n=20)

sp_500_1$adx_ratio_lag1 <- lag(sp_500_1$adx_ratio)
sp_500_1$adx_ratio_lag2<- lag(sp_500_1$adx_ratio, n=2)
sp_500_1$adx_ratio_lag3<- lag(sp_500_1$adx_ratio, n=3)
sp_500_1$adx_ratio_lag4<- lag(sp_500_1$adx_ratio, n=4)
sp_500_1$adx_ratio_lag5<- lag(sp_500_1$adx_ratio, n=5)
sp_500_1$adx_ratio_lag6 <- lag(sp_500_1$adx_ratio, n=6)
sp_500_1$adx_ratio_lag7<- lag(sp_500_1$adx_ratio, n=7)
sp_500_1$adx_ratio_lag8<- lag(sp_500_1$adx_ratio, n=8)
sp_500_1$adx_ratio_lag9<- lag(sp_500_1$adx_ratio, n=9)
sp_500_1$adx_ratio_lag10<- lag(sp_500_1$adx_ratio, n=10)
sp_500_1$adx_ratio_lag11 <- lag(sp_500_1$adx_ratio, n=11)
sp_500_1$adx_ratio_lag12<- lag(sp_500_1$adx_ratio, n=12)
sp_500_1$adx_ratio_lag13<- lag(sp_500_1$adx_ratio, n=13)
sp_500_1$adx_ratio_lag14<- lag(sp_500_1$adx_ratio, n=14)
sp_500_1$adx_ratio_lag15<- lag(sp_500_1$adx_ratio, n=15)
sp_500_1$adx_ratio_lag16 <- lag(sp_500_1$adx_ratio, n=16)
sp_500_1$adx_ratio_lag17<- lag(sp_500_1$adx_ratio, n=17)
sp_500_1$adx_ratio_lag18<- lag(sp_500_1$adx_ratio, n=18)
sp_500_1$adx_ratio_lag19<- lag(sp_500_1$adx_ratio, n=19)
sp_500_1$adx_ratio_lag20<- lag(sp_500_1$adx_ratio, n=20)

sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  tq_mutate(select = c("high","low","close"),
            mutate_fun = BBands)

sp_500_1$lowerBB_candle <- sp_500_1$close/sp_500_1$dn
sp_500_1$upperBB_candle <- sp_500_1$close/sp_500_1$up
sp_500_1$bbands_size <- sp_500_1$up/sp_500_1$dn

sp_500_1$lowerBB_candle_lag1 <- lag(sp_500_1$lowerBB_candle)
sp_500_1$lowerBB_candle_lag2 <- lag(sp_500_1$lowerBB_candle, n=2)
sp_500_1$lowerBB_candle_lag3 <- lag(sp_500_1$lowerBB_candle, n=3)
sp_500_1$lowerBB_candle_lag4 <- lag(sp_500_1$lowerBB_candle, n=4)
sp_500_1$lowerBB_candle_lag5 <- lag(sp_500_1$lowerBB_candle, n=5)
sp_500_1$lowerBB_candle_lag6 <- lag(sp_500_1$lowerBB_candle, n=6)
sp_500_1$lowerBB_candle_lag7 <- lag(sp_500_1$lowerBB_candle, n=7)
sp_500_1$lowerBB_candle_lag8 <- lag(sp_500_1$lowerBB_candle, n=8)
sp_500_1$lowerBB_candle_lag9 <- lag(sp_500_1$lowerBB_candle, n=9)
sp_500_1$lowerBB_candle_lag10 <- lag(sp_500_1$lowerBB_candle, n=10)
sp_500_1$lowerBB_candle_lag11 <- lag(sp_500_1$lowerBB_candle, n=11)
sp_500_1$lowerBB_candle_lag12 <- lag(sp_500_1$lowerBB_candle, n=12)
sp_500_1$lowerBB_candle_lag13 <- lag(sp_500_1$lowerBB_candle, n=13)
sp_500_1$lowerBB_candle_lag14 <- lag(sp_500_1$lowerBB_candle, n=14)
sp_500_1$lowerBB_candle_lag15 <- lag(sp_500_1$lowerBB_candle, n=15)
sp_500_1$lowerBB_candle_lag16 <- lag(sp_500_1$lowerBB_candle, n=16)
sp_500_1$lowerBB_candle_lag17 <- lag(sp_500_1$lowerBB_candle, n=17)
sp_500_1$lowerBB_candle_lag18 <- lag(sp_500_1$lowerBB_candle, n=18)
sp_500_1$lowerBB_candle_lag19 <- lag(sp_500_1$lowerBB_candle, n=19)
sp_500_1$lowerBB_candle_lag20 <- lag(sp_500_1$lowerBB_candle, n=20)

sp_500_1$upperBB_candle_lag1 <- lag(sp_500_1$upperBB_candle)
sp_500_1$upperBB_candle_lag2 <- lag(sp_500_1$upperBB_candle, n=2)
sp_500_1$upperBB_candle_lag3 <- lag(sp_500_1$upperBB_candle, n=3)
sp_500_1$upperBB_candle_lag4 <- lag(sp_500_1$upperBB_candle, n=4)
sp_500_1$upperBB_candle_lag5 <- lag(sp_500_1$upperBB_candle, n=5)
sp_500_1$upperBB_candle_lag6 <- lag(sp_500_1$upperBB_candle, n=6)
sp_500_1$upperBB_candle_lag7 <- lag(sp_500_1$upperBB_candle, n=7)
sp_500_1$upperBB_candle_lag8 <- lag(sp_500_1$upperBB_candle, n=8)
sp_500_1$upperBB_candle_lag9 <- lag(sp_500_1$upperBB_candle, n=9)
sp_500_1$upperBB_candle_lag10 <- lag(sp_500_1$upperBB_candle, n=10)
sp_500_1$upperBB_candle_lag11 <- lag(sp_500_1$upperBB_candle, n=11)
sp_500_1$upperBB_candle_lag12 <- lag(sp_500_1$upperBB_candle, n=12)
sp_500_1$upperBB_candle_lag13 <- lag(sp_500_1$upperBB_candle, n=13)
sp_500_1$upperBB_candle_lag14 <- lag(sp_500_1$upperBB_candle, n=14)
sp_500_1$upperBB_candle_lag15 <- lag(sp_500_1$upperBB_candle, n=15)
sp_500_1$upperBB_candle_lag16 <- lag(sp_500_1$upperBB_candle, n=16)
sp_500_1$upperBB_candle_lag17 <- lag(sp_500_1$upperBB_candle, n=17)
sp_500_1$upperBB_candle_lag18 <- lag(sp_500_1$upperBB_candle, n=18)
sp_500_1$upperBB_candle_lag19 <- lag(sp_500_1$upperBB_candle, n=19)
sp_500_1$upperBB_candle_lag20 <- lag(sp_500_1$upperBB_candle, n=20)

sp_500_1$bbands_size_lag1  <- lag(sp_500_1$bbands_size)
sp_500_1$bbands_size_lag2 <- lag(sp_500_1$bbands_size, n=2)
sp_500_1$bbands_size_lag3 <- lag(sp_500_1$bbands_size, n=3)
sp_500_1$bbands_size_lag4 <- lag(sp_500_1$bbands_size, n=4)
sp_500_1$bbands_size_lag5 <- lag(sp_500_1$bbands_size, n=5)
sp_500_1$bbands_size_lag6  <- lag(sp_500_1$bbands_size, n=6)
sp_500_1$bbands_size_lag7 <- lag(sp_500_1$bbands_size, n=7)
sp_500_1$bbands_size_lag8 <- lag(sp_500_1$bbands_size, n=8)
sp_500_1$bbands_size_lag9 <- lag(sp_500_1$bbands_size, n=9)
sp_500_1$bbands_size_lag10 <- lag(sp_500_1$bbands_size, n=10)
sp_500_1$bbands_size_lag11  <- lag(sp_500_1$bbands_size, n=11)
sp_500_1$bbands_size_lag12 <- lag(sp_500_1$bbands_size, n=12)
sp_500_1$bbands_size_lag13 <- lag(sp_500_1$bbands_size, n=13)
sp_500_1$bbands_size_lag14 <- lag(sp_500_1$bbands_size, n=14)
sp_500_1$bbands_size_lag15 <- lag(sp_500_1$bbands_size, n=15)
sp_500_1$bbands_size_lag16  <- lag(sp_500_1$bbands_size, n=16)
sp_500_1$bbands_size_lag17 <- lag(sp_500_1$bbands_size, n=17)
sp_500_1$bbands_size_lag18 <- lag(sp_500_1$bbands_size, n=18)
sp_500_1$bbands_size_lag19 <- lag(sp_500_1$bbands_size, n=19)
sp_500_1$bbands_size_lag20 <- lag(sp_500_1$bbands_size, n=20)


sp_500_1$hma_9_candle <- sp_500_1$close/sp_500_1$hma_9
sp_500_1$hma_25_candle <- sp_500_1$close/sp_500_1$hma_25
sp_500_1$hma_49_candle <- sp_500_1$close/sp_500_1$hma_49
sp_500_1$hma_81_candle <- sp_500_1$close/sp_500_1$hma_81
sp_500_1$hma_200_candle <- sp_500_1$close/sp_500_1$hma_200
sp_500_1$sma_50_candle <- sp_500_1$close/sp_500_1$sma_50
sp_500_1$sma_100_candle <- sp_500_1$close/sp_500_1$sma_100
sp_500_1$sma_200_candle <- sp_500_1$close/sp_500_1$sma_200

sp_500_1$hma_9_candle_lag1 <- lag(sp_500_1$hma_9_candle)
sp_500_1$hma_25_candle_lag1 <- lag(sp_500_1$hma_25_candle)
sp_500_1$hma_49_candle_lag1 <- lag(sp_500_1$hma_49_candle)
sp_500_1$hma_81_candle_lag1 <- lag(sp_500_1$hma_81_candle)
sp_500_1$hma_200_candle_lag1 <- lag(sp_500_1$hma_200_candle)
sp_500_1$sma_50_candle_lag1 <- lag(sp_500_1$sma_50_candle)
sp_500_1$sma_100_candle_lag1 <- lag(sp_500_1$sma_100_candle)
sp_500_1$sma_200_candle_lag1 <- lag(sp_500_1$sma_200_candle)

sp_500_1$hma_9_candle_lag2 <- lag(sp_500_1$hma_9_candle,n=2)
sp_500_1$hma_25_candle_lag2 <- lag(sp_500_1$hma_25_candle,n=2)
sp_500_1$hma_49_candle_lag2 <- lag(sp_500_1$hma_49_candle,n=2)
sp_500_1$hma_81_candle_lag2 <- lag(sp_500_1$hma_81_candle,n=2)
sp_500_1$hma_200_candle_lag2 <- lag(sp_500_1$hma_200_candle,n=2)
sp_500_1$sma_50_candle_lag2<- lag(sp_500_1$sma_50_candle,n=2)
sp_500_1$sma_100_candle_lag2 <- lag(sp_500_1$sma_100_candle,n=2)
sp_500_1$sma_200_candle_lag2 <- lag(sp_500_1$sma_200_candle,n=2)

sp_500_1$hma_9_candle_lag3 <- lag(sp_500_1$hma_9_candle,n=3)
sp_500_1$hma_25_candle_lag3 <- lag(sp_500_1$hma_25_candle,n=3)
sp_500_1$hma_49_candle_lag3 <- lag(sp_500_1$hma_49_candle,n=3)
sp_500_1$hma_81_candle_lag3 <- lag(sp_500_1$hma_81_candle,n=3)
sp_500_1$hma_200_candle_lag3 <- lag(sp_500_1$hma_200_candle,n=3)
sp_500_1$sma_50_candle_lag3<- lag(sp_500_1$sma_50_candle,n=3)
sp_500_1$sma_100_candle_lag3 <- lag(sp_500_1$sma_100_candle,n=3)
sp_500_1$sma_200_candle_lag3 <- lag(sp_500_1$sma_200_candle,n=3)

sp_500_1$hma_9_candle_lag4 <- lag(sp_500_1$hma_9_candle,n=4)
sp_500_1$hma_25_candle_lag4 <- lag(sp_500_1$hma_25_candle,n=4)
sp_500_1$hma_49_candle_lag4 <- lag(sp_500_1$hma_49_candle,n=4)
sp_500_1$hma_81_candle_lag4 <- lag(sp_500_1$hma_81_candle,n=4)
sp_500_1$hma_200_candle_lag4 <- lag(sp_500_1$hma_200_candle,n=4)
sp_500_1$sma_50_candle_lag4<- lag(sp_500_1$sma_50_candle,n=4)
sp_500_1$sma_100_candle_lag4 <- lag(sp_500_1$sma_100_candle,n=4)
sp_500_1$sma_200_candle_lag4 <- lag(sp_500_1$sma_200_candle,n=4)

sp_500_1$hma_9_candle_lag5 <- lag(sp_500_1$hma_9_candle,n=5)
sp_500_1$hma_25_candle_lag5 <- lag(sp_500_1$hma_25_candle,n=5)
sp_500_1$hma_49_candle_lag5 <- lag(sp_500_1$hma_49_candle,n=5)
sp_500_1$hma_81_candle_lag5 <- lag(sp_500_1$hma_81_candle,n=5)
sp_500_1$hma_200_candle_lag5 <- lag(sp_500_1$hma_200_candle,n=5)
sp_500_1$sma_50_candle_lag5<- lag(sp_500_1$sma_50_candle,n=5)
sp_500_1$sma_100_candle_lag5 <- lag(sp_500_1$sma_100_candle,n=5)
sp_500_1$sma_200_candle_lag5 <- lag(sp_500_1$sma_200_candle,n=5)

sp_500_1$hma_9_candle_lag6 <- lag(sp_500_1$hma_9_candle,n=6)
sp_500_1$hma_25_candle_lag6 <- lag(sp_500_1$hma_25_candle,n=6)
sp_500_1$hma_49_candle_lag6 <- lag(sp_500_1$hma_49_candle,n=6)
sp_500_1$hma_81_candle_lag6 <- lag(sp_500_1$hma_81_candle,n=6)
sp_500_1$hma_200_candle_lag6 <- lag(sp_500_1$hma_200_candle,n=6)
sp_500_1$sma_50_candle_lag6<- lag(sp_500_1$sma_50_candle,n=6)
sp_500_1$sma_100_candle_lag6 <- lag(sp_500_1$sma_100_candle,n=6)
sp_500_1$sma_200_candle_lag6 <- lag(sp_500_1$sma_200_candle,n=6)

sp_500_1$hma_9_candle_lag7 <- lag(sp_500_1$hma_9_candle,n=7)
sp_500_1$hma_25_candle_lag7 <- lag(sp_500_1$hma_25_candle,n=7)
sp_500_1$hma_49_candle_lag7 <- lag(sp_500_1$hma_49_candle,n=7)
sp_500_1$hma_81_candle_lag7 <- lag(sp_500_1$hma_81_candle,n=7)
sp_500_1$hma_200_candle_lag7 <- lag(sp_500_1$hma_200_candle,n=7)
sp_500_1$sma_50_candle_lag7<- lag(sp_500_1$sma_50_candle,n=7)
sp_500_1$sma_100_candle_lag7 <- lag(sp_500_1$sma_100_candle,n=7)
sp_500_1$sma_200_candle_lag7 <- lag(sp_500_1$sma_200_candle,n=7)

sp_500_1$hma_9_candle_lag8 <- lag(sp_500_1$hma_9_candle,n=8)
sp_500_1$hma_25_candle_lag8 <- lag(sp_500_1$hma_25_candle,n=8)
sp_500_1$hma_49_candle_lag8 <- lag(sp_500_1$hma_49_candle,n=8)
sp_500_1$hma_81_candle_lag8 <- lag(sp_500_1$hma_81_candle,n=8)
sp_500_1$hma_200_candle_lag8 <- lag(sp_500_1$hma_200_candle,n=8)
sp_500_1$sma_50_candle_lag8<- lag(sp_500_1$sma_50_candle,n=8)
sp_500_1$sma_100_candle_lag8 <- lag(sp_500_1$sma_100_candle,n=8)
sp_500_1$sma_200_candle_lag8 <- lag(sp_500_1$sma_200_candle,n=8)

sp_500_1$hma_9_candle_lag9 <- lag(sp_500_1$hma_9_candle,n=9)
sp_500_1$hma_25_candle_lag9 <- lag(sp_500_1$hma_25_candle,n=9)
sp_500_1$hma_49_candle_lag9 <- lag(sp_500_1$hma_49_candle,n=9)
sp_500_1$hma_81_candle_lag9 <- lag(sp_500_1$hma_81_candle,n=9)
sp_500_1$hma_200_candle_lag9 <- lag(sp_500_1$hma_200_candle,n=9)
sp_500_1$sma_50_candle_lag9<- lag(sp_500_1$sma_50_candle,n=9)
sp_500_1$sma_100_candle_lag9 <- lag(sp_500_1$sma_100_candle,n=9)
sp_500_1$sma_200_candle_lag9 <- lag(sp_500_1$sma_200_candle,n=9)

sp_500_1$hma_9_candle_lag10 <- lag(sp_500_1$hma_9_candle,n=10)
sp_500_1$hma_25_candle_lag10 <- lag(sp_500_1$hma_25_candle,n=10)
sp_500_1$hma_49_candle_lag10 <- lag(sp_500_1$hma_49_candle,n=10)
sp_500_1$hma_81_candle_lag10 <- lag(sp_500_1$hma_81_candle,n=10)
sp_500_1$hma_200_candle_lag10 <- lag(sp_500_1$hma_200_candle,n=10)
sp_500_1$sma_50_candle_lag10 <- lag(sp_500_1$sma_50_candle,n=10)
sp_500_1$sma_100_candle_lag10 <- lag(sp_500_1$sma_100_candle,n=10)
sp_500_1$sma_200_candle_lag10 <- lag(sp_500_1$sma_200_candle,n=10)

sp_500_1$hma_9_candle_lag11 <- lag(sp_500_1$hma_9_candle,n=11)
sp_500_1$hma_25_candle_lag11 <- lag(sp_500_1$hma_25_candle,n=11)
sp_500_1$hma_49_candle_lag11 <- lag(sp_500_1$hma_49_candle,n=11)
sp_500_1$hma_81_candle_lag11 <- lag(sp_500_1$hma_81_candle,n=11)
sp_500_1$hma_200_candle_lag11 <- lag(sp_500_1$hma_200_candle,n=11)
sp_500_1$sma_50_candle_lag11<- lag(sp_500_1$sma_50_candle,n=11)
sp_500_1$sma_100_candle_lag11 <- lag(sp_500_1$sma_100_candle,n=11)
sp_500_1$sma_200_candle_lag11 <- lag(sp_500_1$sma_200_candle,n=11)

sp_500_1$hma_9_candle_lag12 <- lag(sp_500_1$hma_9_candle,n=12)
sp_500_1$hma_25_candle_lag12 <- lag(sp_500_1$hma_25_candle,n=12)
sp_500_1$hma_49_candle_lag12 <- lag(sp_500_1$hma_49_candle,n=12)
sp_500_1$hma_81_candle_lag12 <- lag(sp_500_1$hma_81_candle,n=12)
sp_500_1$hma_200_candle_lag12 <- lag(sp_500_1$hma_200_candle,n=12)
sp_500_1$sma_50_candle_lag12<- lag(sp_500_1$sma_50_candle,n=12)
sp_500_1$sma_100_candle_lag12 <- lag(sp_500_1$sma_100_candle,n=12)
sp_500_1$sma_200_candle_lag12<- lag(sp_500_1$sma_200_candle,n=12)

sp_500_1$hma_9_candle_lag13 <- lag(sp_500_1$hma_9_candle,n=13)
sp_500_1$hma_25_candle_lag13 <- lag(sp_500_1$hma_25_candle,n=13)
sp_500_1$hma_49_candle_lag13 <- lag(sp_500_1$hma_49_candle,n=13)
sp_500_1$hma_81_candle_lag13 <- lag(sp_500_1$hma_81_candle,n=13)
sp_500_1$hma_200_candle_lag13 <- lag(sp_500_1$hma_200_candle,n=13)
sp_500_1$sma_50_candle_lag13<- lag(sp_500_1$sma_50_candle,n=13)
sp_500_1$sma_100_candle_lag13 <- lag(sp_500_1$sma_100_candle,n=13)
sp_500_1$sma_200_candle_lag13<- lag(sp_500_1$sma_200_candle,n=13)

sp_500_1$hma_9_candle_lag14 <- lag(sp_500_1$hma_9_candle,n=14)
sp_500_1$hma_25_candle_lag14 <- lag(sp_500_1$hma_25_candle,n=14)
sp_500_1$hma_49_candle_lag14 <- lag(sp_500_1$hma_49_candle,n=14)
sp_500_1$hma_81_candle_lag14 <- lag(sp_500_1$hma_81_candle,n=14)
sp_500_1$hma_200_candle_lag14 <- lag(sp_500_1$hma_200_candle,n=14)
sp_500_1$sma_50_candle_lag14<- lag(sp_500_1$sma_50_candle,n=14)
sp_500_1$sma_100_candle_lag14 <- lag(sp_500_1$sma_100_candle,n=14)
sp_500_1$sma_200_candle_lag14<- lag(sp_500_1$sma_200_candle,n=14)

sp_500_1$hma_9_candle_lag15 <- lag(sp_500_1$hma_9_candle,n=15)
sp_500_1$hma_25_candle_lag15 <- lag(sp_500_1$hma_25_candle,n=15)
sp_500_1$hma_49_candle_lag15 <- lag(sp_500_1$hma_49_candle,n=15)
sp_500_1$hma_81_candle_lag15 <- lag(sp_500_1$hma_81_candle,n=15)
sp_500_1$hma_200_candle_lag15 <- lag(sp_500_1$hma_200_candle,n=15)
sp_500_1$sma_50_candle_lag15<- lag(sp_500_1$sma_50_candle,n=15)
sp_500_1$sma_100_candle_lag15 <- lag(sp_500_1$sma_100_candle,n=15)
sp_500_1$sma_200_candle_lag15<- lag(sp_500_1$sma_200_candle,n=15)

sp_500_1$hma_9_candle_lag16 <- lag(sp_500_1$hma_9_candle,n=16)
sp_500_1$hma_25_candle_lag16 <- lag(sp_500_1$hma_25_candle,n=16)
sp_500_1$hma_49_candle_lag16 <- lag(sp_500_1$hma_49_candle,n=16)
sp_500_1$hma_81_candle_lag16 <- lag(sp_500_1$hma_81_candle,n=16)
sp_500_1$hma_200_candle_lag16 <- lag(sp_500_1$hma_200_candle,n=16)
sp_500_1$sma_50_candle_lag16<- lag(sp_500_1$sma_50_candle,n=16)
sp_500_1$sma_100_candle_lag16 <- lag(sp_500_1$sma_100_candle,n=16)
sp_500_1$sma_200_candle_lag16<- lag(sp_500_1$sma_200_candle,n=16)

sp_500_1$hma_9_candle_lag17 <- lag(sp_500_1$hma_9_candle,n=17)
sp_500_1$hma_25_candle_lag17 <- lag(sp_500_1$hma_25_candle,n=17)
sp_500_1$hma_49_candle_lag17 <- lag(sp_500_1$hma_49_candle,n=17)
sp_500_1$hma_81_candle_lag17 <- lag(sp_500_1$hma_81_candle,n=17)
sp_500_1$hma_200_candle_lag17 <- lag(sp_500_1$hma_200_candle,n=17)
sp_500_1$sma_50_candle_lag17<- lag(sp_500_1$sma_50_candle,n=17)
sp_500_1$sma_100_candle_lag17 <- lag(sp_500_1$sma_100_candle,n=17)
sp_500_1$sma_200_candle_lag17<- lag(sp_500_1$sma_200_candle,n=17)

sp_500_1$hma_9_candle_lag18 <- lag(sp_500_1$hma_9_candle,n=18)
sp_500_1$hma_25_candle_lag18 <- lag(sp_500_1$hma_25_candle,n=18)
sp_500_1$hma_49_candle_lag18 <- lag(sp_500_1$hma_49_candle,n=18)
sp_500_1$hma_81_candle_lag18 <- lag(sp_500_1$hma_81_candle,n=18)
sp_500_1$hma_200_candle_lag18 <- lag(sp_500_1$hma_200_candle,n=18)
sp_500_1$sma_50_candle_lag18<- lag(sp_500_1$sma_50_candle,n=18)
sp_500_1$sma_100_candle_lag18 <- lag(sp_500_1$sma_100_candle,n=18)
sp_500_1$sma_200_candle_lag18<- lag(sp_500_1$sma_200_candle,n=18)

sp_500_1$hma_9_candle_lag19 <- lag(sp_500_1$hma_9_candle,n=19)
sp_500_1$hma_25_candle_lag19 <- lag(sp_500_1$hma_25_candle,n=19)
sp_500_1$hma_49_candle_lag19 <- lag(sp_500_1$hma_49_candle,n=19)
sp_500_1$hma_81_candle_lag19 <- lag(sp_500_1$hma_81_candle,n=19)
sp_500_1$hma_200_candle_lag19 <- lag(sp_500_1$hma_200_candle,n=19)
sp_500_1$sma_50_candle_lag19<- lag(sp_500_1$sma_50_candle,n=19)
sp_500_1$sma_100_candle_lag19 <- lag(sp_500_1$sma_100_candle,n=19)
sp_500_1$sma_200_candle_lag19<- lag(sp_500_1$sma_200_candle,n=19)

sp_500_1$hma_9_candle_lag20 <- lag(sp_500_1$hma_9_candle,n=20)
sp_500_1$hma_25_candle_lag20 <- lag(sp_500_1$hma_25_candle,n=20)
sp_500_1$hma_49_candle_lag20 <- lag(sp_500_1$hma_49_candle,n=20)
sp_500_1$hma_81_candle_lag20 <- lag(sp_500_1$hma_81_candle,n=20)
sp_500_1$hma_200_candle_lag20 <- lag(sp_500_1$hma_200_candle,n=20)
sp_500_1$sma_50_candle_lag20<- lag(sp_500_1$sma_50_candle,n=20)
sp_500_1$sma_100_candle_lag20 <- lag(sp_500_1$sma_100_candle,n=20)
sp_500_1$sma_200_candle_lag20<- lag(sp_500_1$sma_200_candle,n=20)

sp_500_1$hma_9_lag_1 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 1)
sp_500_1$hma_9_lag_2 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 2)
sp_500_1$hma_9_lag_3 <- sp_500_1$hma_9 /lag(sp_500_1$hma_9, n = 3)
sp_500_1$hma_9_lag_4 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 4)
sp_500_1$hma_9_lag_5 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 5)
sp_500_1$hma_9_lag_6 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 6)
sp_500_1$hma_9_lag_7 <- sp_500_1$hma_9 /lag(sp_500_1$hma_9, n = 7)
sp_500_1$hma_9_lag_8 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 8)
sp_500_1$hma_9_lag_9 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 9)
sp_500_1$hma_9_lag_10 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 10)
sp_500_1$hma_9_lag_11<- sp_500_1$hma_9 /lag(sp_500_1$hma_9, n = 11)
sp_500_1$hma_9_lag_12 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 12)
sp_500_1$hma_9_lag_13 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 13)
sp_500_1$hma_9_lag_14 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 14)
sp_500_1$hma_9_lag_15 <- sp_500_1$hma_9 /lag(sp_500_1$hma_9, n = 15)
sp_500_1$hma_9_lag_16 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 16)
sp_500_1$hma_9_lag_17 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 17)
sp_500_1$hma_9_lag_18 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 18)
sp_500_1$hma_9_lag_19 <- sp_500_1$hma_9 /lag(sp_500_1$hma_9, n = 19)
sp_500_1$hma_9_lag_20 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 20)



sp_500_1$hma_9co25 <- sp_500_1$hma_9/sp_500_1$hma_25
sp_500_1$hma_9co49 <- sp_500_1$hma_9/sp_500_1$hma_49
sp_500_1$hma_9co81 <- sp_500_1$hma_9/sp_500_1$hma_81
sp_500_1$hma_25co49<- sp_500_1$hma_25/sp_500_1$hma_49
sp_500_1$hma_25co81 <- sp_500_1$hma_25/sp_500_1$hma_81
sp_500_1$hma_49co81 <- sp_500_1$hma_49/sp_500_1$hma_81
sp_500_1$hma_9co200<- sp_500_1$hma_9/sp_500_1$hma_200
sp_500_1$hma_25co200<- sp_500_1$hma_25/sp_500_1$hma_200
sp_500_1$hma_49co200<- sp_500_1$hma_49/sp_500_1$hma_200
sp_500_1$hma_81co200<- sp_500_1$hma_81/sp_500_1$hma_200
sp_500_1$hma_9co_sma50<- sp_500_1$hma_9/sp_500_1$sma_50
sp_500_1$hma_25co_sma50<- sp_500_1$hma_25/sp_500_1$sma_50
sp_500_1$hma_49co_sma50<- sp_500_1$hma_49/sp_500_1$sma_50
sp_500_1$hma_81co_sma50<- sp_500_1$hma_81/sp_500_1$sma_50
sp_500_1$hma_9co_sma100<- sp_500_1$hma_9/sp_500_1$sma_100
sp_500_1$hma_25co_sma100<- sp_500_1$hma_25/sp_500_1$sma_100
sp_500_1$hma_49co_sma100<- sp_500_1$hma_49/sp_500_1$sma_100
sp_500_1$hma_81co_sma100<- sp_500_1$hma_81/sp_500_1$sma_100
sp_500_1$hma_9co_sma200<- sp_500_1$hma_9/sp_500_1$sma_200
sp_500_1$hma_25co_sma200<- sp_500_1$hma_25/sp_500_1$sma_200
sp_500_1$hma_49co_sma200<- sp_500_1$hma_49/sp_500_1$sma_200
sp_500_1$hma_81co_sma200<- sp_500_1$hma_81/sp_500_1$sma_200

sp_500_1$hma_9co200_lag1<- lag(sp_500_1$hma_9co200)
sp_500_1$hma_25co200_lag1<- lag(sp_500_1$hma_25co200)
sp_500_1$hma_49co200_lag1<- lag(sp_500_1$hma_49co200)
sp_500_1$hma_81co200_lag1<- lag(sp_500_1$hma_81co200)
sp_500_1$hma_9co_sma50_lag1<- lag(sp_500_1$hma_9co_sma50)
sp_500_1$hma_25co_sma50_lag1<- lag(sp_500_1$hma_25co_sma50)
sp_500_1$hma_49co_sma50_lag1<- lag(sp_500_1$hma_49co_sma50)
sp_500_1$hma_81co_sma50_lag1<- lag(sp_500_1$hma_81co_sma50)
sp_500_1$hma_9co_sma100_lag1<- lag(sp_500_1$hma_9co_sma100)
sp_500_1$hma_25co_sma100_lag1<- lag(sp_500_1$hma_25co_sma100)
sp_500_1$hma_49co_sma100_lag1<- lag(sp_500_1$hma_49co_sma100)
sp_500_1$hma_81co_sma100_lag1<- lag(sp_500_1$hma_81co_sma100)
sp_500_1$hma_9co_sma200_lag1<- lag(sp_500_1$hma_9co_sma200)
sp_500_1$hma_25co_sma200_lag1<- lag(sp_500_1$hma_25co_sma200)
sp_500_1$hma_49co_sma200_lag1<- lag(sp_500_1$hma_49co_sma200)
sp_500_1$hma_81co_sma200_lag1<- lag(sp_500_1$hma_81co_sma200)

sp_500_1$hma_9co200_lag2<- lag(sp_500_1$hma_9co200,n=2)
sp_500_1$hma_25co200_lag2<- lag(sp_500_1$hma_25co200,n=2)
sp_500_1$hma_49co200_lag2<- lag(sp_500_1$hma_49co200,n=2)
sp_500_1$hma_81co200_lag2<- lag(sp_500_1$hma_81co200,n=2)
sp_500_1$hma_9co_sma50_lag2<- lag(sp_500_1$hma_9co_sma50,n=2)
sp_500_1$hma_25co_sma50_lag2<- lag(sp_500_1$hma_25co_sma50,n=2)
sp_500_1$hma_49co_sma50_lag2<- lag(sp_500_1$hma_49co_sma50,n=2)
sp_500_1$hma_81co_sma50_lag2<- lag(sp_500_1$hma_81co_sma50,n=2)
sp_500_1$hma_9co_sma100_lag2<- lag(sp_500_1$hma_9co_sma100,n=2)
sp_500_1$hma_25co_sma100_lag2<- lag(sp_500_1$hma_25co_sma100,n=2)
sp_500_1$hma_49co_sma100_lag2<- lag(sp_500_1$hma_49co_sma100,n=2)
sp_500_1$hma_81co_sma100_lag2<- lag(sp_500_1$hma_81co_sma100,n=2)
sp_500_1$hma_9co_sma200_lag2<- lag(sp_500_1$hma_9co_sma200,n=2)
sp_500_1$hma_25co_sma200_lag2<- lag(sp_500_1$hma_25co_sma200,n=2)
sp_500_1$hma_49co_sma200_lag2<- lag(sp_500_1$hma_49co_sma200,n=2)
sp_500_1$hma_81co_sma200_lag2<- lag(sp_500_1$hma_81co_sma200,n=2)

sp_500_1$hma_9co200_lag3<- lag(sp_500_1$hma_9co200,n=3)
sp_500_1$hma_25co200_lag3<- lag(sp_500_1$hma_25co200,n=3)
sp_500_1$hma_49co200_lag3<- lag(sp_500_1$hma_49co200,n=3)
sp_500_1$hma_81co200_lag3<- lag(sp_500_1$hma_81co200,n=3)
sp_500_1$hma_9co_sma50_lag3<- lag(sp_500_1$hma_9co_sma50,n=3)
sp_500_1$hma_25co_sma50_lag3<- lag(sp_500_1$hma_25co_sma50,n=3)
sp_500_1$hma_49co_sma50_lag3<- lag(sp_500_1$hma_49co_sma50,n=3)
sp_500_1$hma_81co_sma50_lag3<- lag(sp_500_1$hma_81co_sma50,n=3)
sp_500_1$hma_9co_sma100_lag3<- lag(sp_500_1$hma_9co_sma100,n=3)
sp_500_1$hma_25co_sma100_lag3<- lag(sp_500_1$hma_25co_sma100,n=3)
sp_500_1$hma_49co_sma100_lag3<- lag(sp_500_1$hma_49co_sma100,n=3)
sp_500_1$hma_81co_sma100_lag3<- lag(sp_500_1$hma_81co_sma100,n=3)
sp_500_1$hma_9co_sma200_lag3<- lag(sp_500_1$hma_9co_sma200,n=3)
sp_500_1$hma_25co_sma200_lag3<- lag(sp_500_1$hma_25co_sma200,n=3)
sp_500_1$hma_49co_sma200_lag3<- lag(sp_500_1$hma_49co_sma200,n=3)
sp_500_1$hma_81co_sma200_lag3<- lag(sp_500_1$hma_81co_sma200,n=3)

sp_500_1$hma_9co200_lag4<- lag(sp_500_1$hma_9co200,n=4)
sp_500_1$hma_25co200_lag4<- lag(sp_500_1$hma_25co200,n=4)
sp_500_1$hma_49co200_lag4<- lag(sp_500_1$hma_49co200,n=4)
sp_500_1$hma_81co200_lag4<- lag(sp_500_1$hma_81co200,n=4)
sp_500_1$hma_9co_sma50_lag4<- lag(sp_500_1$hma_9co_sma50,n=4)
sp_500_1$hma_25co_sma50_lag4<- lag(sp_500_1$hma_25co_sma50,n=4)
sp_500_1$hma_49co_sma50_lag4<- lag(sp_500_1$hma_49co_sma50,n=4)
sp_500_1$hma_81co_sma50_lag4<- lag(sp_500_1$hma_81co_sma50,n=4)
sp_500_1$hma_9co_sma100_lag4<- lag(sp_500_1$hma_9co_sma100,n=4)
sp_500_1$hma_25co_sma100_lag4<- lag(sp_500_1$hma_25co_sma100,n=4)
sp_500_1$hma_49co_sma100_lag4<- lag(sp_500_1$hma_49co_sma100,n=4)
sp_500_1$hma_81co_sma100_lag4<- lag(sp_500_1$hma_81co_sma100,n=4)
sp_500_1$hma_9co_sma200_lag4<- lag(sp_500_1$hma_9co_sma200,n=4)
sp_500_1$hma_25co_sma200_lag4<- lag(sp_500_1$hma_25co_sma200,n=4)
sp_500_1$hma_49co_sma200_lag4<- lag(sp_500_1$hma_49co_sma200,n=4)
sp_500_1$hma_81co_sma200_lag4<- lag(sp_500_1$hma_81co_sma200,n=4)

sp_500_1$hma_9co200_lag5<- lag(sp_500_1$hma_9co200,n=5)
sp_500_1$hma_25co200_lag5<- lag(sp_500_1$hma_25co200,n=5)
sp_500_1$hma_49co200_lag5<- lag(sp_500_1$hma_49co200,n=5)
sp_500_1$hma_81co200_lag5<- lag(sp_500_1$hma_81co200,n=5)
sp_500_1$hma_9co_sma50_lag5<- lag(sp_500_1$hma_9co_sma50,n=5)
sp_500_1$hma_25co_sma50_lag5<- lag(sp_500_1$hma_25co_sma50,n=5)
sp_500_1$hma_49co_sma50_lag5<- lag(sp_500_1$hma_49co_sma50,n=5)
sp_500_1$hma_81co_sma50_lag5<- lag(sp_500_1$hma_81co_sma50,n=5)
sp_500_1$hma_9co_sma100_lag5<- lag(sp_500_1$hma_9co_sma100,n=5)
sp_500_1$hma_25co_sma100_lag5<- lag(sp_500_1$hma_25co_sma100,n=5)
sp_500_1$hma_49co_sma100_lag5<- lag(sp_500_1$hma_49co_sma100,n=5)
sp_500_1$hma_81co_sma100_lag5<- lag(sp_500_1$hma_81co_sma100,n=5)
sp_500_1$hma_9co_sma200_lag5<- lag(sp_500_1$hma_9co_sma200,n=5)
sp_500_1$hma_25co_sma200_lag5<- lag(sp_500_1$hma_25co_sma200,n=5)
sp_500_1$hma_49co_sma200_lag5<- lag(sp_500_1$hma_49co_sma200,n=5)
sp_500_1$hma_81co_sma200_lag5<- lag(sp_500_1$hma_81co_sma200,n=5)

sp_500_1$hma_9co200_lag6<- lag(sp_500_1$hma_9co200,n=6)
sp_500_1$hma_25co200_lag6<- lag(sp_500_1$hma_25co200,n=6)
sp_500_1$hma_49co200_lag6<- lag(sp_500_1$hma_49co200,n=6)
sp_500_1$hma_81co200_lag6<- lag(sp_500_1$hma_81co200,n=6)
sp_500_1$hma_9co_sma50_lag6<- lag(sp_500_1$hma_9co_sma50,n=6)
sp_500_1$hma_25co_sma50_lag6<- lag(sp_500_1$hma_25co_sma50,n=6)
sp_500_1$hma_49co_sma50_lag6<- lag(sp_500_1$hma_49co_sma50,n=6)
sp_500_1$hma_81co_sma50_lag6<- lag(sp_500_1$hma_81co_sma50,n=6)
sp_500_1$hma_9co_sma100_lag6<- lag(sp_500_1$hma_9co_sma100,n=6)
sp_500_1$hma_25co_sma100_lag6<- lag(sp_500_1$hma_25co_sma100,n=6)
sp_500_1$hma_49co_sma100_lag6<- lag(sp_500_1$hma_49co_sma100,n=6)
sp_500_1$hma_81co_sma100_lag6<- lag(sp_500_1$hma_81co_sma100,n=6)
sp_500_1$hma_9co_sma200_lag6<- lag(sp_500_1$hma_9co_sma200,n=6)
sp_500_1$hma_25co_sma200_lag6<- lag(sp_500_1$hma_25co_sma200,n=6)
sp_500_1$hma_49co_sma200_lag6<- lag(sp_500_1$hma_49co_sma200,n=6)
sp_500_1$hma_81co_sma200_lag6<- lag(sp_500_1$hma_81co_sma200,n=6)

sp_500_1$hma_9co200_lag7<- lag(sp_500_1$hma_9co200,n=7)
sp_500_1$hma_25co200_lag7<- lag(sp_500_1$hma_25co200,n=7)
sp_500_1$hma_49co200_lag7<- lag(sp_500_1$hma_49co200,n=7)
sp_500_1$hma_81co200_lag7<- lag(sp_500_1$hma_81co200,n=7)
sp_500_1$hma_9co_sma50_lag7<- lag(sp_500_1$hma_9co_sma50,n=7)
sp_500_1$hma_25co_sma50_lag7<- lag(sp_500_1$hma_25co_sma50,n=7)
sp_500_1$hma_49co_sma50_lag7<- lag(sp_500_1$hma_49co_sma50,n=7)
sp_500_1$hma_81co_sma50_lag7<- lag(sp_500_1$hma_81co_sma50,n=7)
sp_500_1$hma_9co_sma100_lag7<- lag(sp_500_1$hma_9co_sma100,n=7)
sp_500_1$hma_25co_sma100_lag7<- lag(sp_500_1$hma_25co_sma100,n=7)
sp_500_1$hma_49co_sma100_lag7<- lag(sp_500_1$hma_49co_sma100,n=7)
sp_500_1$hma_81co_sma100_lag7<- lag(sp_500_1$hma_81co_sma100,n=7)
sp_500_1$hma_9co_sma200_lag7<- lag(sp_500_1$hma_9co_sma200,n=7)
sp_500_1$hma_25co_sma200_lag7<- lag(sp_500_1$hma_25co_sma200,n=7)
sp_500_1$hma_49co_sma200_lag7<- lag(sp_500_1$hma_49co_sma200,n=7)
sp_500_1$hma_81co_sma200_lag7<- lag(sp_500_1$hma_81co_sma200,n=7)

sp_500_1$hma_9co200_lag8<- lag(sp_500_1$hma_9co200,n=8)
sp_500_1$hma_25co200_lag8<- lag(sp_500_1$hma_25co200,n=8)
sp_500_1$hma_49co200_lag8<- lag(sp_500_1$hma_49co200,n=8)
sp_500_1$hma_81co200_lag8<- lag(sp_500_1$hma_81co200,n=8)
sp_500_1$hma_9co_sma50_lag8<- lag(sp_500_1$hma_9co_sma50,n=8)
sp_500_1$hma_25co_sma50_lag8<- lag(sp_500_1$hma_25co_sma50,n=8)
sp_500_1$hma_49co_sma50_lag8<- lag(sp_500_1$hma_49co_sma50,n=8)
sp_500_1$hma_81co_sma50_lag8<- lag(sp_500_1$hma_81co_sma50,n=8)
sp_500_1$hma_9co_sma100_lag8<- lag(sp_500_1$hma_9co_sma100,n=8)
sp_500_1$hma_25co_sma100_lag8<- lag(sp_500_1$hma_25co_sma100,n=8)
sp_500_1$hma_49co_sma100_lag8<- lag(sp_500_1$hma_49co_sma100,n=8)
sp_500_1$hma_81co_sma100_lag8<- lag(sp_500_1$hma_81co_sma100,n=8)
sp_500_1$hma_9co_sma200_lag8<- lag(sp_500_1$hma_9co_sma200,n=8)
sp_500_1$hma_25co_sma200_lag8<- lag(sp_500_1$hma_25co_sma200,n=8)
sp_500_1$hma_49co_sma200_lag8<- lag(sp_500_1$hma_49co_sma200,n=8)
sp_500_1$hma_81co_sma200_lag8<- lag(sp_500_1$hma_81co_sma200,n=8)

sp_500_1$hma_9co200_lag9<- lag(sp_500_1$hma_9co200,n=9)
sp_500_1$hma_25co200_lag9<- lag(sp_500_1$hma_25co200,n=9)
sp_500_1$hma_49co200_lag9<- lag(sp_500_1$hma_49co200,n=9)
sp_500_1$hma_81co200_lag9<- lag(sp_500_1$hma_81co200,n=9)
sp_500_1$hma_9co_sma50_lag9<- lag(sp_500_1$hma_9co_sma50,n=9)
sp_500_1$hma_25co_sma50_lag9<- lag(sp_500_1$hma_25co_sma50,n=9)
sp_500_1$hma_49co_sma50_lag9<- lag(sp_500_1$hma_49co_sma50,n=9)
sp_500_1$hma_81co_sma50_lag9<- lag(sp_500_1$hma_81co_sma50,n=9)
sp_500_1$hma_9co_sma100_lag9<- lag(sp_500_1$hma_9co_sma100,n=9)
sp_500_1$hma_25co_sma100_lag9<- lag(sp_500_1$hma_25co_sma100,n=9)
sp_500_1$hma_49co_sma100_lag9<- lag(sp_500_1$hma_49co_sma100,n=9)
sp_500_1$hma_81co_sma100_lag9<- lag(sp_500_1$hma_81co_sma100,n=9)
sp_500_1$hma_9co_sma200_lag9<- lag(sp_500_1$hma_9co_sma200,n=9)
sp_500_1$hma_25co_sma200_lag9<- lag(sp_500_1$hma_25co_sma200,n=9)
sp_500_1$hma_49co_sma200_lag9<- lag(sp_500_1$hma_49co_sma200,n=9)
sp_500_1$hma_81co_sma200_lag9<- lag(sp_500_1$hma_81co_sma200,n=9)

sp_500_1$hma_9co200_lag10<- lag(sp_500_1$hma_9co200,n=10)
sp_500_1$hma_25co200_lag10<- lag(sp_500_1$hma_25co200,n=10)
sp_500_1$hma_49co200_lag10<- lag(sp_500_1$hma_49co200,n=10)
sp_500_1$hma_81co200_lag10<- lag(sp_500_1$hma_81co200,n=10)
sp_500_1$hma_9co_sma50_lag10<- lag(sp_500_1$hma_9co_sma50,n=10)
sp_500_1$hma_25co_sma50_lag10<- lag(sp_500_1$hma_25co_sma50,n=10)
sp_500_1$hma_49co_sma50_lag10<- lag(sp_500_1$hma_49co_sma50,n=10)
sp_500_1$hma_81co_sma50_lag10<- lag(sp_500_1$hma_81co_sma50,n=10)
sp_500_1$hma_9co_sma100_lag10<- lag(sp_500_1$hma_9co_sma100,n=10)
sp_500_1$hma_25co_sma100_lag10<- lag(sp_500_1$hma_25co_sma100,n=10)
sp_500_1$hma_49co_sma100_lag10<- lag(sp_500_1$hma_49co_sma100,n=10)
sp_500_1$hma_81co_sma100_lag10<- lag(sp_500_1$hma_81co_sma100,n=10)
sp_500_1$hma_9co_sma200_lag10<- lag(sp_500_1$hma_9co_sma200,n=10)
sp_500_1$hma_25co_sma200_lag10<- lag(sp_500_1$hma_25co_sma200,n=10)
sp_500_1$hma_49co_sma200_lag10<- lag(sp_500_1$hma_49co_sma200,n=10)
sp_500_1$hma_81co_sma200_lag10<- lag(sp_500_1$hma_81co_sma200,n=10)

sp_500_1$hma_9co200_lag11<- lag(sp_500_1$hma_9co200,n=11)
sp_500_1$hma_25co200_lag11<- lag(sp_500_1$hma_25co200,n=11)
sp_500_1$hma_49co200_lag11<- lag(sp_500_1$hma_49co200,n=11)
sp_500_1$hma_81co200_lag11<- lag(sp_500_1$hma_81co200,n=11)
sp_500_1$hma_9co_sma50_lag11<- lag(sp_500_1$hma_9co_sma50,n=11)
sp_500_1$hma_25co_sma50_lag11<- lag(sp_500_1$hma_25co_sma50,n=11)
sp_500_1$hma_49co_sma50_lag11<- lag(sp_500_1$hma_49co_sma50,n=11)
sp_500_1$hma_81co_sma50_lag11<- lag(sp_500_1$hma_81co_sma50,n=11)
sp_500_1$hma_9co_sma100_lag11<- lag(sp_500_1$hma_9co_sma100,n=11)
sp_500_1$hma_25co_sma100_lag11<- lag(sp_500_1$hma_25co_sma100,n=11)
sp_500_1$hma_49co_sma100_lag11<- lag(sp_500_1$hma_49co_sma100,n=11)
sp_500_1$hma_81co_sma100_lag11<- lag(sp_500_1$hma_81co_sma100,n=11)
sp_500_1$hma_9co_sma200_lag11<- lag(sp_500_1$hma_9co_sma200,n=11)
sp_500_1$hma_25co_sma200_lag11<- lag(sp_500_1$hma_25co_sma200,n=11)
sp_500_1$hma_49co_sma200_lag11<- lag(sp_500_1$hma_49co_sma200,n=11)
sp_500_1$hma_81co_sma200_lag11<- lag(sp_500_1$hma_81co_sma200,n=11)

sp_500_1$hma_9co200_lag12<- lag(sp_500_1$hma_9co200,n=12)
sp_500_1$hma_25co200_lag12<- lag(sp_500_1$hma_25co200,n=12)
sp_500_1$hma_49co200_lag12<- lag(sp_500_1$hma_49co200,n=12)
sp_500_1$hma_81co200_lag12<- lag(sp_500_1$hma_81co200,n=12)
sp_500_1$hma_9co_sma50_lag12<- lag(sp_500_1$hma_9co_sma50,n=12)
sp_500_1$hma_25co_sma50_lag12<- lag(sp_500_1$hma_25co_sma50,n=12)
sp_500_1$hma_49co_sma50_lag12<- lag(sp_500_1$hma_49co_sma50,n=12)
sp_500_1$hma_81co_sma50_lag12<- lag(sp_500_1$hma_81co_sma50,n=12)
sp_500_1$hma_9co_sma100_lag12<- lag(sp_500_1$hma_9co_sma100,n=12)
sp_500_1$hma_25co_sma100_lag12<- lag(sp_500_1$hma_25co_sma100,n=12)
sp_500_1$hma_49co_sma100_lag12<- lag(sp_500_1$hma_49co_sma100,n=12)
sp_500_1$hma_81co_sma100_lag12<- lag(sp_500_1$hma_81co_sma100,n=12)
sp_500_1$hma_9co_sma200_lag12<- lag(sp_500_1$hma_9co_sma200,n=12)
sp_500_1$hma_25co_sma200_lag12<- lag(sp_500_1$hma_25co_sma200,n=12)
sp_500_1$hma_49co_sma200_lag12<- lag(sp_500_1$hma_49co_sma200,n=12)
sp_500_1$hma_81co_sma200_lag12<- lag(sp_500_1$hma_81co_sma200,n=12)

sp_500_1$hma_9co200_lag13<- lag(sp_500_1$hma_9co200,n=13)
sp_500_1$hma_25co200_lag13<- lag(sp_500_1$hma_25co200,n=13)
sp_500_1$hma_49co200_lag13<- lag(sp_500_1$hma_49co200,n=13)
sp_500_1$hma_81co200_lag13<- lag(sp_500_1$hma_81co200,n=13)
sp_500_1$hma_9co_sma50_lag13<- lag(sp_500_1$hma_9co_sma50,n=13)
sp_500_1$hma_25co_sma50_lag13<- lag(sp_500_1$hma_25co_sma50,n=13)
sp_500_1$hma_49co_sma50_lag13<- lag(sp_500_1$hma_49co_sma50,n=13)
sp_500_1$hma_81co_sma50_lag13<- lag(sp_500_1$hma_81co_sma50,n=13)
sp_500_1$hma_9co_sma100_lag13<- lag(sp_500_1$hma_9co_sma100,n=13)
sp_500_1$hma_25co_sma100_lag13<- lag(sp_500_1$hma_25co_sma100,n=13)
sp_500_1$hma_49co_sma100_lag13<- lag(sp_500_1$hma_49co_sma100,n=13)
sp_500_1$hma_81co_sma100_lag13<- lag(sp_500_1$hma_81co_sma100,n=13)
sp_500_1$hma_9co_sma200_lag13<- lag(sp_500_1$hma_9co_sma200,n=13)
sp_500_1$hma_25co_sma200_lag13<- lag(sp_500_1$hma_25co_sma200,n=13)
sp_500_1$hma_49co_sma200_lag13<- lag(sp_500_1$hma_49co_sma200,n=13)
sp_500_1$hma_81co_sma200_lag13<- lag(sp_500_1$hma_81co_sma200,n=13)

sp_500_1$hma_9co200_lag14<- lag(sp_500_1$hma_9co200,n=14)
sp_500_1$hma_25co200_lag14<- lag(sp_500_1$hma_25co200,n=14)
sp_500_1$hma_49co200_lag14<- lag(sp_500_1$hma_49co200,n=14)
sp_500_1$hma_81co200_lag14<- lag(sp_500_1$hma_81co200,n=14)
sp_500_1$hma_9co_sma50_lag14<- lag(sp_500_1$hma_9co_sma50,n=14)
sp_500_1$hma_25co_sma50_lag14<- lag(sp_500_1$hma_25co_sma50,n=14)
sp_500_1$hma_49co_sma50_lag14<- lag(sp_500_1$hma_49co_sma50,n=14)
sp_500_1$hma_81co_sma50_lag14<- lag(sp_500_1$hma_81co_sma50,n=14)
sp_500_1$hma_9co_sma100_lag14<- lag(sp_500_1$hma_9co_sma100,n=14)
sp_500_1$hma_25co_sma100_lag14<- lag(sp_500_1$hma_25co_sma100,n=14)
sp_500_1$hma_49co_sma100_lag14<- lag(sp_500_1$hma_49co_sma100,n=14)
sp_500_1$hma_81co_sma100_lag14<- lag(sp_500_1$hma_81co_sma100,n=14)
sp_500_1$hma_9co_sma200_lag14<- lag(sp_500_1$hma_9co_sma200,n=14)
sp_500_1$hma_25co_sma200_lag14<- lag(sp_500_1$hma_25co_sma200,n=14)
sp_500_1$hma_49co_sma200_lag14<- lag(sp_500_1$hma_49co_sma200,n=14)
sp_500_1$hma_81co_sma200_lag14<- lag(sp_500_1$hma_81co_sma200,n=14)

sp_500_1$hma_9co200_lag15<- lag(sp_500_1$hma_9co200,n=15)
sp_500_1$hma_25co200_lag15<- lag(sp_500_1$hma_25co200,n=15)
sp_500_1$hma_49co200_lag15<- lag(sp_500_1$hma_49co200,n=15)
sp_500_1$hma_81co200_lag15<- lag(sp_500_1$hma_81co200,n=15)
sp_500_1$hma_9co_sma50_lag15<- lag(sp_500_1$hma_9co_sma50,n=15)
sp_500_1$hma_25co_sma50_lag15<- lag(sp_500_1$hma_25co_sma50,n=15)
sp_500_1$hma_49co_sma50_lag15<- lag(sp_500_1$hma_49co_sma50,n=15)
sp_500_1$hma_81co_sma50_lag15<- lag(sp_500_1$hma_81co_sma50,n=15)
sp_500_1$hma_9co_sma100_lag15<- lag(sp_500_1$hma_9co_sma100,n=15)
sp_500_1$hma_25co_sma100_lag15<- lag(sp_500_1$hma_25co_sma100,n=15)
sp_500_1$hma_49co_sma100_lag15<- lag(sp_500_1$hma_49co_sma100,n=15)
sp_500_1$hma_81co_sma100_lag15<- lag(sp_500_1$hma_81co_sma100,n=15)
sp_500_1$hma_9co_sma200_lag15<- lag(sp_500_1$hma_9co_sma200,n=15)
sp_500_1$hma_25co_sma200_lag15<- lag(sp_500_1$hma_25co_sma200,n=15)
sp_500_1$hma_49co_sma200_lag15<- lag(sp_500_1$hma_49co_sma200,n=15)
sp_500_1$hma_81co_sma200_lag15<- lag(sp_500_1$hma_81co_sma200,n=15)

sp_500_1$hma_9co200_lag16<- lag(sp_500_1$hma_9co200,n=16)
sp_500_1$hma_25co200_lag16<- lag(sp_500_1$hma_25co200,n=16)
sp_500_1$hma_49co200_lag16<- lag(sp_500_1$hma_49co200,n=16)
sp_500_1$hma_81co200_lag16<- lag(sp_500_1$hma_81co200,n=16)
sp_500_1$hma_9co_sma50_lag16<- lag(sp_500_1$hma_9co_sma50,n=16)
sp_500_1$hma_25co_sma50_lag16<- lag(sp_500_1$hma_25co_sma50,n=16)
sp_500_1$hma_49co_sma50_lag16<- lag(sp_500_1$hma_49co_sma50,n=16)
sp_500_1$hma_81co_sma50_lag16<- lag(sp_500_1$hma_81co_sma50,n=16)
sp_500_1$hma_9co_sma100_lag16<- lag(sp_500_1$hma_9co_sma100,n=16)
sp_500_1$hma_25co_sma100_lag16<- lag(sp_500_1$hma_25co_sma100,n=16)
sp_500_1$hma_49co_sma100_lag16<- lag(sp_500_1$hma_49co_sma100,n=16)
sp_500_1$hma_81co_sma100_lag16<- lag(sp_500_1$hma_81co_sma100,n=16)
sp_500_1$hma_9co_sma200_lag16<- lag(sp_500_1$hma_9co_sma200,n=16)
sp_500_1$hma_25co_sma200_lag16<- lag(sp_500_1$hma_25co_sma200,n=16)
sp_500_1$hma_49co_sma200_lag16<- lag(sp_500_1$hma_49co_sma200,n=16)
sp_500_1$hma_81co_sma200_lag16<- lag(sp_500_1$hma_81co_sma200,n=16)

sp_500_1$hma_9co200_lag17<- lag(sp_500_1$hma_9co200,n=17)
sp_500_1$hma_25co200_lag17<- lag(sp_500_1$hma_25co200,n=17)
sp_500_1$hma_49co200_lag17<- lag(sp_500_1$hma_49co200,n=17)
sp_500_1$hma_81co200_lag17<- lag(sp_500_1$hma_81co200,n=17)
sp_500_1$hma_9co_sma50_lag17<- lag(sp_500_1$hma_9co_sma50,n=17)
sp_500_1$hma_25co_sma50_lag17<- lag(sp_500_1$hma_25co_sma50,n=17)
sp_500_1$hma_49co_sma50_lag17<- lag(sp_500_1$hma_49co_sma50,n=17)
sp_500_1$hma_81co_sma50_lag17<- lag(sp_500_1$hma_81co_sma50,n=17)
sp_500_1$hma_9co_sma100_lag17<- lag(sp_500_1$hma_9co_sma100,n=17)
sp_500_1$hma_25co_sma100_lag17<- lag(sp_500_1$hma_25co_sma100,n=17)
sp_500_1$hma_49co_sma100_lag17<- lag(sp_500_1$hma_49co_sma100,n=17)
sp_500_1$hma_81co_sma100_lag17<- lag(sp_500_1$hma_81co_sma100,n=17)
sp_500_1$hma_9co_sma200_lag17<- lag(sp_500_1$hma_9co_sma200,n=17)
sp_500_1$hma_25co_sma200_lag17<- lag(sp_500_1$hma_25co_sma200,n=17)
sp_500_1$hma_49co_sma200_lag17<- lag(sp_500_1$hma_49co_sma200,n=17)
sp_500_1$hma_81co_sma200_lag17<- lag(sp_500_1$hma_81co_sma200,n=17)

sp_500_1$hma_9co200_lag18<- lag(sp_500_1$hma_9co200,n=18)
sp_500_1$hma_25co200_lag18<- lag(sp_500_1$hma_25co200,n=18)
sp_500_1$hma_49co200_lag18<- lag(sp_500_1$hma_49co200,n=18)
sp_500_1$hma_81co200_lag18<- lag(sp_500_1$hma_81co200,n=18)
sp_500_1$hma_9co_sma50_lag18<- lag(sp_500_1$hma_9co_sma50,n=18)
sp_500_1$hma_25co_sma50_lag18<- lag(sp_500_1$hma_25co_sma50,n=18)
sp_500_1$hma_49co_sma50_lag18<- lag(sp_500_1$hma_49co_sma50,n=18)
sp_500_1$hma_81co_sma50_lag18<- lag(sp_500_1$hma_81co_sma50,n=18)
sp_500_1$hma_9co_sma100_lag18<- lag(sp_500_1$hma_9co_sma100,n=18)
sp_500_1$hma_25co_sma100_lag18<- lag(sp_500_1$hma_25co_sma100,n=18)
sp_500_1$hma_49co_sma100_lag18<- lag(sp_500_1$hma_49co_sma100,n=18)
sp_500_1$hma_81co_sma100_lag18<- lag(sp_500_1$hma_81co_sma100,n=18)
sp_500_1$hma_9co_sma200_lag18<- lag(sp_500_1$hma_9co_sma200,n=18)
sp_500_1$hma_25co_sma200_lag18<- lag(sp_500_1$hma_25co_sma200,n=18)
sp_500_1$hma_49co_sma200_lag18<- lag(sp_500_1$hma_49co_sma200,n=18)
sp_500_1$hma_81co_sma200_lag18<- lag(sp_500_1$hma_81co_sma200,n=18)

sp_500_1$hma_9co200_lag19<- lag(sp_500_1$hma_9co200,n=19)
sp_500_1$hma_25co200_lag19<- lag(sp_500_1$hma_25co200,n=19)
sp_500_1$hma_49co200_lag19<- lag(sp_500_1$hma_49co200,n=19)
sp_500_1$hma_81co200_lag19<- lag(sp_500_1$hma_81co200,n=19)
sp_500_1$hma_9co_sma50_lag19<- lag(sp_500_1$hma_9co_sma50,n=19)
sp_500_1$hma_25co_sma50_lag19<- lag(sp_500_1$hma_25co_sma50,n=19)
sp_500_1$hma_49co_sma50_lag19<- lag(sp_500_1$hma_49co_sma50,n=19)
sp_500_1$hma_81co_sma50_lag19<- lag(sp_500_1$hma_81co_sma50,n=19)
sp_500_1$hma_9co_sma100_lag19<- lag(sp_500_1$hma_9co_sma100,n=19)
sp_500_1$hma_25co_sma100_lag19<- lag(sp_500_1$hma_25co_sma100,n=19)
sp_500_1$hma_49co_sma100_lag19<- lag(sp_500_1$hma_49co_sma100,n=19)
sp_500_1$hma_81co_sma100_lag19<- lag(sp_500_1$hma_81co_sma100,n=19)
sp_500_1$hma_9co_sma200_lag19<- lag(sp_500_1$hma_9co_sma200,n=19)
sp_500_1$hma_25co_sma200_lag19<- lag(sp_500_1$hma_25co_sma200,n=19)
sp_500_1$hma_49co_sma200_lag19<- lag(sp_500_1$hma_49co_sma200,n=19)
sp_500_1$hma_81co_sma200_lag19<- lag(sp_500_1$hma_81co_sma200,n=19)

sp_500_1$hma_9co200_lag20<- lag(sp_500_1$hma_9co200,n=20)
sp_500_1$hma_25co200_lag20<- lag(sp_500_1$hma_25co200,n=20)
sp_500_1$hma_49co200_lag20<- lag(sp_500_1$hma_49co200,n=20)
sp_500_1$hma_81co200_lag20<- lag(sp_500_1$hma_81co200,n=20)
sp_500_1$hma_9co_sma50_lag20<- lag(sp_500_1$hma_9co_sma50,n=20)
sp_500_1$hma_25co_sma50_lag20<- lag(sp_500_1$hma_25co_sma50,n=20)
sp_500_1$hma_49co_sma50_lag20<- lag(sp_500_1$hma_49co_sma50,n=20)
sp_500_1$hma_81co_sma50_lag20<- lag(sp_500_1$hma_81co_sma50,n=20)
sp_500_1$hma_9co_sma100_lag20<- lag(sp_500_1$hma_9co_sma100,n=20)
sp_500_1$hma_25co_sma100_lag20<- lag(sp_500_1$hma_25co_sma100,n=20)
sp_500_1$hma_49co_sma100_lag20<- lag(sp_500_1$hma_49co_sma100,n=20)
sp_500_1$hma_81co_sma100_lag20<- lag(sp_500_1$hma_81co_sma100,n=20)
sp_500_1$hma_9co_sma200_lag20<- lag(sp_500_1$hma_9co_sma200,n=20)
sp_500_1$hma_25co_sma200_lag20<- lag(sp_500_1$hma_25co_sma200,n=20)
sp_500_1$hma_49co_sma200_lag20<- lag(sp_500_1$hma_49co_sma200,n=20)
sp_500_1$hma_81co_sma200_lag20<- lag(sp_500_1$hma_81co_sma200,n=20)


sp_500_1$hma_9co25_lag1 <- lag(sp_500_1$hma_9co25)
sp_500_1$hma_9co25_lag2 <- lag(sp_500_1$hma_9co25,n=2)
sp_500_1$hma_9co25_lag3 <- lag(sp_500_1$hma_9co25,n=3)
sp_500_1$hma_9co25_lag4 <- lag(sp_500_1$hma_9co25,n=4)
sp_500_1$hma_9co25_lag5 <- lag(sp_500_1$hma_9co25,n=5)
sp_500_1$hma_9co25_lag6 <- lag(sp_500_1$hma_9co25, n=6)
sp_500_1$hma_9co25_lag7 <- lag(sp_500_1$hma_9co25,n=7)
sp_500_1$hma_9co25_lag8 <- lag(sp_500_1$hma_9co25,n=8)
sp_500_1$hma_9co25_lag9 <- lag(sp_500_1$hma_9co25,n=9)
sp_500_1$hma_9co25_lag10 <- lag(sp_500_1$hma_9co25,n=10)
sp_500_1$hma_9co25_lag11 <- lag(sp_500_1$hma_9co25, n=11)
sp_500_1$hma_9co25_lag12 <- lag(sp_500_1$hma_9co25,n=12)
sp_500_1$hma_9co25_lag13 <- lag(sp_500_1$hma_9co25,n=13)
sp_500_1$hma_9co25_lag14 <- lag(sp_500_1$hma_9co25,n=14)
sp_500_1$hma_9co25_lag15 <- lag(sp_500_1$hma_9co25,n=15)
sp_500_1$hma_9co25_lag16 <- lag(sp_500_1$hma_9co25, n=16)
sp_500_1$hma_9co25_lag17 <- lag(sp_500_1$hma_9co25,n=17)
sp_500_1$hma_9co25_lag18 <- lag(sp_500_1$hma_9co25,n=18)
sp_500_1$hma_9co25_lag19 <- lag(sp_500_1$hma_9co25,n=19)
sp_500_1$hma_9co25_lag20 <- lag(sp_500_1$hma_9co25,n=20)


sp_500_1$hma_9co49_lag1 <- lag(sp_500_1$hma_9co49)
sp_500_1$hma_9co49_lag2 <- lag(sp_500_1$hma_9co49,n=2)
sp_500_1$hma_9co49_lag3 <- lag(sp_500_1$hma_9co49,n=3)
sp_500_1$hma_9co49_lag4 <- lag(sp_500_1$hma_9co49,n=4)
sp_500_1$hma_9co49_lag5 <- lag(sp_500_1$hma_9co49,n=5)
sp_500_1$hma_9co49_lag6 <- lag(sp_500_1$hma_9co49, n=6)
sp_500_1$hma_9co49_lag7 <- lag(sp_500_1$hma_9co49,n=7)
sp_500_1$hma_9co49_lag8 <- lag(sp_500_1$hma_9co49,n=8)
sp_500_1$hma_9co49_lag9 <- lag(sp_500_1$hma_9co49,n=9)
sp_500_1$hma_9co49_lag10 <- lag(sp_500_1$hma_9co49,n=10)
sp_500_1$hma_9co49_lag11 <- lag(sp_500_1$hma_9co49,n=11)
sp_500_1$hma_9co49_lag12 <- lag(sp_500_1$hma_9co49,n=12)
sp_500_1$hma_9co49_lag13 <- lag(sp_500_1$hma_9co49,n=13)
sp_500_1$hma_9co49_lag14 <- lag(sp_500_1$hma_9co49,n=14)
sp_500_1$hma_9co49_lag15 <- lag(sp_500_1$hma_9co49,n=15)
sp_500_1$hma_9co49_lag16 <- lag(sp_500_1$hma_9co49, n=16)
sp_500_1$hma_9co49_lag17 <- lag(sp_500_1$hma_9co49,n=17)
sp_500_1$hma_9co49_lag18 <- lag(sp_500_1$hma_9co49,n=18)
sp_500_1$hma_9co49_lag19 <- lag(sp_500_1$hma_9co49,n=19)
sp_500_1$hma_9co49_lag20 <- lag(sp_500_1$hma_9co49,n=20)


sp_500_1$hma_9co81_lag1 <- lag(sp_500_1$hma_9co81)
sp_500_1$hma_9co81_lag2 <- lag(sp_500_1$hma_9co81,n=2)
sp_500_1$hma_9co81_lag3 <- lag(sp_500_1$hma_9co81,n=3)
sp_500_1$hma_9co81_lag4 <- lag(sp_500_1$hma_9co81,n=4)
sp_500_1$hma_9co81_lag5 <- lag(sp_500_1$hma_9co81,n=5)
sp_500_1$hma_9co81_lag6 <- lag(sp_500_1$hma_9co81, n=6)
sp_500_1$hma_9co81_lag7 <- lag(sp_500_1$hma_9co81,n=7)
sp_500_1$hma_9co81_lag8 <- lag(sp_500_1$hma_9co81,n=8)
sp_500_1$hma_9co81_lag9 <- lag(sp_500_1$hma_9co81,n=9)
sp_500_1$hma_9co81_lag10 <- lag(sp_500_1$hma_9co81,n=10)
sp_500_1$hma_9co81_lag11 <- lag(sp_500_1$hma_9co81, n=11)
sp_500_1$hma_9co81_lag12 <- lag(sp_500_1$hma_9co81,n=12)
sp_500_1$hma_9co81_lag13 <- lag(sp_500_1$hma_9co81,n=13)
sp_500_1$hma_9co81_lag14 <- lag(sp_500_1$hma_9co81,n=14)
sp_500_1$hma_9co81_lag15 <- lag(sp_500_1$hma_9co81,n=15)
sp_500_1$hma_9co81_lag16 <- lag(sp_500_1$hma_9co81, n=16)
sp_500_1$hma_9co81_lag17 <- lag(sp_500_1$hma_9co81,n=17)
sp_500_1$hma_9co81_lag18 <- lag(sp_500_1$hma_9co81,n=18)
sp_500_1$hma_9co81_lag19 <- lag(sp_500_1$hma_9co81,n=19)
sp_500_1$hma_9co81_lag20 <- lag(sp_500_1$hma_9co81,n=20)

sp_500_1$hma_25co49_lag1 <- lag(sp_500_1$hma_25co49)
sp_500_1$hma_25co49_lag2 <- lag(sp_500_1$hma_25co49,n=2)
sp_500_1$hma_25co49_lag3 <- lag(sp_500_1$hma_25co49,n=3)
sp_500_1$hma_25co49_lag4 <- lag(sp_500_1$hma_25co49,n=4)
sp_500_1$hma_25co49_lag5 <- lag(sp_500_1$hma_25co49,n=5)
sp_500_1$hma_25co49_lag6 <- lag(sp_500_1$hma_25co49,n=6)
sp_500_1$hma_25co49_lag7 <- lag(sp_500_1$hma_25co49,n=7)
sp_500_1$hma_25co49_lag8 <- lag(sp_500_1$hma_25co49,n=8)
sp_500_1$hma_25co49_lag9 <- lag(sp_500_1$hma_25co49,n=9)
sp_500_1$hma_25co49_lag10 <- lag(sp_500_1$hma_25co49,n=10)
sp_500_1$hma_25co49_lag11<- lag(sp_500_1$hma_25co49, n=11)
sp_500_1$hma_25co49_lag12 <- lag(sp_500_1$hma_25co49,n=12)
sp_500_1$hma_25co49_lag13 <- lag(sp_500_1$hma_25co49,n=13)
sp_500_1$hma_25co49_lag14 <- lag(sp_500_1$hma_25co49,n=14)
sp_500_1$hma_25co49_lag15 <- lag(sp_500_1$hma_25co49,n=15)
sp_500_1$hma_25co49_lag16 <- lag(sp_500_1$hma_25co49, n=16)
sp_500_1$hma_25co49_lag17 <- lag(sp_500_1$hma_25co49,n=17)
sp_500_1$hma_25co49_lag18 <- lag(sp_500_1$hma_25co49,n=18)
sp_500_1$hma_25co49_lag19 <- lag(sp_500_1$hma_25co49,n=19)
sp_500_1$hma_25co49_lag20 <- lag(sp_500_1$hma_25co49,n=20)

sp_500_1$hma_25co81_lag1 <- lag(sp_500_1$hma_25co81)
sp_500_1$hma_25co81_lag2 <- lag(sp_500_1$hma_25co81,n=2)
sp_500_1$hma_25co81_lag3 <- lag(sp_500_1$hma_25co81,n=3)
sp_500_1$hma_25co81_lag4 <- lag(sp_500_1$hma_25co81,n=4)
sp_500_1$hma_25co81_lag5 <- lag(sp_500_1$hma_25co81,n=5)
sp_500_1$hma_25co81_lag6 <- lag(sp_500_1$hma_25co81, n=6)
sp_500_1$hma_25co81_lag7 <- lag(sp_500_1$hma_25co81,n=7)
sp_500_1$hma_25co81_lag8 <- lag(sp_500_1$hma_25co81,n=8)
sp_500_1$hma_25co81_lag9 <- lag(sp_500_1$hma_25co81,n=9)
sp_500_1$hma_25co81_lag10 <- lag(sp_500_1$hma_25co81,n=10)
sp_500_1$hma_25co81_lag11 <- lag(sp_500_1$hma_25co81, n=11)
sp_500_1$hma_25co81_lag12 <- lag(sp_500_1$hma_25co81,n=12)
sp_500_1$hma_25co81_lag13 <- lag(sp_500_1$hma_25co81,n=13)
sp_500_1$hma_25co81_lag14 <- lag(sp_500_1$hma_25co81,n=14)
sp_500_1$hma_25co81_lag15 <- lag(sp_500_1$hma_25co81,n=15)
sp_500_1$hma_25co81_lag16 <- lag(sp_500_1$hma_25co81, n=16)
sp_500_1$hma_25co81_lag17 <- lag(sp_500_1$hma_25co81,n=17)
sp_500_1$hma_25co81_lag18 <- lag(sp_500_1$hma_25co81,n=18)
sp_500_1$hma_25co81_lag19 <- lag(sp_500_1$hma_25co81,n=19)
sp_500_1$hma_25co81_lag20 <- lag(sp_500_1$hma_25co81,n=20)

sp_500_1$hma_49co81_lag1 <- lag(sp_500_1$hma_49co81)
sp_500_1$hma_49co81_lag2 <- lag(sp_500_1$hma_49co81,n=2)
sp_500_1$hma_49co81_lag3 <- lag(sp_500_1$hma_49co81,n=3)
sp_500_1$hma_49co81_lag4 <- lag(sp_500_1$hma_49co81,n=4)
sp_500_1$hma_49co81_lag5 <- lag(sp_500_1$hma_49co81,n=5)
sp_500_1$hma_49co81_lag6 <- lag(sp_500_1$hma_49co81, n=6)
sp_500_1$hma_49co81_lag7 <- lag(sp_500_1$hma_49co81,n=7)
sp_500_1$hma_49co81_lag8 <- lag(sp_500_1$hma_49co81,n=8)
sp_500_1$hma_49co81_lag9 <- lag(sp_500_1$hma_49co81,n=9)
sp_500_1$hma_49co81_lag10 <- lag(sp_500_1$hma_49co81,n=10)
sp_500_1$hma_49co81_lag11 <- lag(sp_500_1$hma_49co81, n=11)
sp_500_1$hma_49co81_lag12 <- lag(sp_500_1$hma_49co81,n=12)
sp_500_1$hma_49co81_lag13 <- lag(sp_500_1$hma_49co81,n=13)
sp_500_1$hma_49co81_lag14 <- lag(sp_500_1$hma_49co81,n=14)
sp_500_1$hma_49co81_lag15 <- lag(sp_500_1$hma_49co81,n=15)
sp_500_1$hma_49co81_lag16 <- lag(sp_500_1$hma_49co81, n=16)
sp_500_1$hma_49co81_lag17 <- lag(sp_500_1$hma_49co81,n=17)
sp_500_1$hma_49co81_lag18 <- lag(sp_500_1$hma_49co81,n=18)
sp_500_1$hma_49co81_lag19 <- lag(sp_500_1$hma_49co81,n=19)
sp_500_1$hma_49co81_lag20 <- lag(sp_500_1$hma_49co81,n=20)


write.csv(sp_500_1,"stockmodel_techind_data20.csv", row.names = FALSE)

sp_500_1 <- read_csv("stockmodel_techind_data20.csv") 
sp_500_1 <- read_csv("stockmodel_techind_data19.csv") 


##split sp_500_1 frame into smaller chunks by filtering on date from ML

stock_ml <- sp_500_1 %>%
  filter(date >="2021-10-01")

friday <- Sys.Date()-wday(Sys.Date()+1)

date_filter <- sp_500_1 %>%
  filter(date == friday)

date_filter2 <- sp_500_1 %>%
  filter(date == "2021-12-03")


### graph indicators


prv_friday_pos <- date_filter2 %>%
  select(symbol,close,close_shift_5, target_5, target_direction_5,sar_ratio,hma_9co25,hma_9co49,hma_25co49, hma_9_lag_1, hma_9_lag_2,hma_9_lag_3,fastD,stoch,
         rsi_14, DIp,ADX,adx_ratio,di_ratio)

prv_friday_pos$target_direction_5<- if_else(prv_friday_pos$target_direction_5 == 1, "buy","sell")

prv_friday_pos$target_direction_5 <- factor(prv_friday_pos$target_direction_5, levels=c("buy","sell"))

prv_friday_pos %>%
  ungroup(symbol)%>%
  select(sar_ratio,target_direction_5)%>%
  pivot_longer(cols = sar_ratio)%>%
  ggplot(aes(x=name, y=value, fill= target_direction_5))+
  geom_violin()

  prv_friday_pos %>%
  ungroup(symbol)%>%
  select(rsi_14,target_direction_5)%>%
  pivot_longer(cols = rsi_14)%>%
  ggplot(aes(x=name, y=value, fill= target_direction_5))+
  geom_violin()+
    scale_y_continuous(breaks=seq(0,90,5))
  
  prv_friday_pos %>%
    ungroup(symbol)%>%
    select(hma_9co25,hma_9co49,hma_25co49, hma_9_lag_1,hma_9_lag_3,target_direction_5)%>%
    pivot_longer(cols = 1:5)%>%
    ggplot(aes(x=name, y=value, fill =target_direction_5))+
    geom_violin()+
    scale_y_continuous(breaks=seq(0,2,.10))
  
  prv_friday_pos %>%
    ungroup(symbol)%>%
    select(adx_ratio,di_ratio,target_direction_5)%>%
    filter(di_ratio <=3)%>%
    pivot_longer(cols = 1:2)%>%
    ggplot(aes(x=name, y=value, fill = target_direction_5))+
    geom_violin()+
    scale_y_continuous(breaks=seq(0,4,.25))
  
  prv_friday_pos %>%
    ungroup(symbol)%>%
    select(fastD,stoch,target_direction_5)%>%
    pivot_longer(cols = 1:2)%>%
    ggplot(aes(x=name, y=value, fill= target_direction_5))+
    geom_violin()
  
  
### Filter symbols for option picks  
  

stock_buy<-date_filter %>%
  filter(rsi_14 <= 50 | rsi_14_lag1 <= 50 | rsi_14_lag2 <= 50,
         hma_9_lag_1>=1, hma_9_lag_3>=1, 
         sar_ratio >=1, diff>=0)%>%
  select(symbol)%>%
  arrange(symbol)
  select(symbol,close,sar_ratio,hma_9co25,hma_9co49,hma_25co49, hma_9_lag_1, hma_9_lag_2,hma_9_lag_3,fastD,stoch,
         rsi_14, DIp,ADX,adx_ratio,di_ratio)%>%
  arrange(desc(adx_ratio), desc(ADX))

stock_buy<-date_filter %>%
  filter(rsi_14 <= 50 | rsi_14_lag1 <= 50 | rsi_14_lag2 <= 50,
         hma_9_lag_1>=1, hma_9_lag_3>=1, 
         sar_ratio >=1, diff>=0)%>%
  select(symbol,close,hma_9co25,hma_9co49,hma_25co49)

stock_buy2<-date_filter %>%
  filter(rsi_14 <= 50 | rsi_14_lag1 <= 50 ,fastD<=.70 |stoch <=.70,
         hma_9_lag_1>=1, hma_9_lag_3>=1, 
         sar_ratio >=1, sar_ratio_lag2 >=1, diff>=0, diff_lag2>=0)%>%
  select(symbol,close,hma_9co25,hma_9co49,hma_25co49)

stock_buy3<-date_filter %>%
  filter(rsi_14<=60&
           sar_ratio >=1& diff>=0&
         hma_9_lag_1>=1& hma_9_lag_3>=1, di_ratio>=1, adx_ratio>=1 )%>%
  select(symbol,close,sar_ratio,hma_9co25,hma_9co49,hma_25co49, hma_9_lag_1, hma_9_lag_2,hma_9_lag_3,fastD,stoch,
         rsi_14, DIp,ADX,adx_ratio,di_ratio)%>%
  arrange(desc(hma_25co49))

##(rsi_14 <= 50 | rsi_14_lag1 <= 50 | rsi_14_lag2 <= 50)

watch_stocks <- date_filter%>%
  filter(rsi_14 <= 30)

sp500 <- tq_index("SP500")%>%
  select(symbol, company)
second_option_buy <- stock_buy2%>%
  filter(symbol %in% sp500$symbol)%>%
  arrange(desc(hma_25co49))

stock_short<- date_filter %>%
  filter( fastD>=.55 | stoch >=.55, sar_ratio<1,
          hma_9_lag_1<1, hma_9_lag_3<1,
          hma_9co25 <1, diff<0)%>%
  select(symbol,close,sar_ratio,hma_9co25,hma_9co49,hma_25co49, hma_9_lag_1, hma_9_lag_2,hma_9_lag_3,fastD,stoch,
         rsi_14, DIp,DIn,ADX,adx_ratio,di_ratio)%>%
  arrange(adx_ratio,di_ratio)


long_term <- date_filter %>%
  filter(close >= sma_200)%>%
  mutate(sma_200_close = close/sma_200)%>%
  ungroup(symbol)%>%
  slice_max(sma_200_close, n=100)%>%
  arrange(desc(sma_200_close))

##find options that have weekly options

stock_calls = list()
stock_puts = list()
dat= data.frame()
for (symbol in stock_buy$symbol) {
  # ... make some data
  tryCatch({
  dat <- getOptionChain(Symbols = symbol)
  dat$symbol <- symbol  # maybe you want to keep track of which iteration produced it?
  row_call <- rownames_to_column(dat$calls, var= "stock_date")
  stock_calls[[symbol]] <- row_call # add it to your list
},
  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}



stock_call <- bind_rows(stock_calls, .id = "column_label")


stock_call$symbol_char <-nchar(stock_call$column_label)+1
stock_call$end_symbol_char <- nchar(stock_call$column_label)+6

stock_call$start_date1 <- ymd(str_sub(stock_call$stock_date,
                                      start = stock_call$symbol_char,
                                      end =stock_call$end_symbol_char))

stock_call <- stock_call %>%
  relocate(start_date1, .after = column_label)
stock_call <- stock_call%>%
  group_by(column_label)%>%
  slice(1)%>%
  select(1:2)


stock_call$this_week_option <- if_else((stock_call$start_date1-as.Date(friday))<=7,1,0)

stock_call <- stock_call %>%
  filter(start_date1 <= Sys.Date()+5)

stock_call1 <- date_filter %>%
  filter(symbol %in% stock_call$column_label, di_ratio>=1)%>%
  select(symbol,close,sar_ratio,hma_9co25,hma_9co49,hma_25co49, hma_9_lag_1, hma_9_lag_2,hma_9_lag_3,fastD,stoch,
         rsi_14, DIp,ADX,adx_ratio,di_ratio)%>%
  arrange(desc(adx_ratio),di_ratio)
  

### ML Stock techinical indictators 

stock_ml <- sp_500_1 %>%
  filter(date >="2021-06-01")

##target 10 day
stock_ml <- stock_ml %>% ##sp_500_1 goes here other wise
  ungroup(symbol)%>%
  select(-c(symbol, open:target_10,close_shift_15:signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar))%>%
  na.omit()

### target 5 day 
stock_ml <- stock_ml %>% ##sp_500_1 goes here other wise
  ungroup(symbol)%>%
  select(-c(symbol, open:target_5,close_shift_10:signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar))%>%
  na.omit()
  
stock_ml$target_direction_5<- if_else(stock_ml$target_direction_5 == 1, "buy","sell")

stock_ml$target_direction_5 <- factor(stock_ml$target_direction_5, levels=c("buy","sell"))

stock_ml$target_direction_10<- if_else(stock_ml$target_direction_10 == 1, "buy","sell")

stock_ml$target_direction_10 <- factor(stock_ml$target_direction_10, levels=c("buy","sell"))

###randomly split the data
set.seed(42)
stock_split <- initial_split(stock_ml, prop = 3/4, strata = "target_direction_5")
rows_instockprice_train <- training(stock_split)
rows_instockprice_test <- testing(stock_split)


### or split data by date
rows_instockprice_train <- stock_ml%>%
  filter(date <= "2021-12-20")

rows_instockprice_test <-   stock_ml%>%
  filter(date > "2021-12-20")

rows_instockprice_train <-rows_instockprice_train%>%
  select(-date)

rows_instockprice_test <-rows_instockprice_test%>%
  select(-date)

stock_recipe <- recipe(target_direction_5~., data = rows_instockprice_train)

stock_recipe <- recipe(target_direction_10~., data = rows_instockprice_train)

library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
cl <- makeCluster(all_cores)
registerDoParallel(cl)

cores <- parallel::detectCores()
cores

cv <- vfold_cv(rows_instockprice_train, v = 10, repeats = 5)

lr_mod <- 
  logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet",num.threads = cores)%>%
  set_mode("classification")

lr_workflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(stock_recipe)

lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 15))

lr_res <- 
  lr_workflow %>% 
  tune_grid(cv,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

lr_plot <- 
  lr_res %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

lr_plot 

top_models <-
  lr_res %>% 
  show_best("roc_auc", n = 15) %>% 
  arrange(penalty) 

top_models

lr_best <- 
  lr_res %>% 
  select_best(metric = "roc_auc")

lr_best

lr_auc <- 
  lr_res %>% 
  collect_predictions(parameters = lr_best) %>% 
  roc_curve(target_direction_5, .pred_buy) %>% 
  mutate(model = "Logistic Regression")

autoplot(lr_auc)

last_lr_workflow <- finalize_workflow(
  lr_workflow,
  lr_best
)

last_lr_train <- fit(last_lr_workflow, rows_instockprice_train)##, rows_instockprice_train )

test_pred_lr <- predict(last_lr_train, rows_instockprice_test, type = "prob") %>%
  bind_cols(predict(last_lr_train, rows_instockprice_test)) %>%
  bind_cols(select(rows_instockprice_test, target_direction_5)) %>%
  glimpse()

test_pred_lr %>%
  sens(truth = target_direction_5,estimate = .pred_class)

test_pred_lr%>%
  spec(truth = target_direction_5,estimate = .pred_class)

test_pred_lr%>%
  roc_auc(truth = target_direction_5,estimate = .pred_buy)

test_pred_lr%>%
conf_mat(truth = target_direction_5,estimate = .pred_class)

test_pred_lr %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_5), 
               alpha = 0.5)+
  ggtitle("LR Stock 5 day pick ")+
  xlab("Probability of Yes")

test_pred_lr

##fit(target_direction_5~., data = rows_instockprice_train)

tidy(fitted_logistic_model)

tidy(fitted_logistic_model, exponentiate = TRUE) %>%
  filter(p.value < 0.05)

pred_proba <- predict(fitted_logistic_model,
                      new_data = rows_instockprice_test,
                      type = "prob")
pred_class <- predict(fitted_logistic_model,
                      new_data = rows_instockprice_test,
                      type = "class")

stock_results <- rows_instockprice_test %>%
  select(target_direction_5) %>%
  bind_cols(pred_class, pred_proba)

conf_mat(stock_results, truth = target_direction_5,
         estimate = .pred_class)

sens(stock_results, truth = target_direction_5,
     estimate = .pred_class)

spec(stock_results, truth = target_direction_5,
     estimate = .pred_class)

stock_results%>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_5), 
               alpha = 0.5)+
  ggtitle("Logistic Reg Probability of Yes")+
  xlab("Probability of Yes")

rf_auc <- 
  stock_results%>%
  roc_curve(target_direction_5, .pred_buy) %>% 
  mutate(model = "LR") 

auc_rf <-stock_results %>%
  roc_auc(truth = target_direction_5,estimate = .pred_buy)

stock_recipe <- recipe(target_direction_5~., data = rows_instockprice_train)
  ##step_dummy(all_nominal_predictors())%>%
  ##step_impute_bag(all_predictors())%>%
  ##step_range(all_numeric(), min = 0, max = 1)

cores <- parallel::detectCores()
cores

doParallel::registerDoParallel()


rf_grid <- grid_regular(
  mtry = c(5, 30),
  min_n = c(2, 8),
  levels = 5
)

rf_grid <- expand.grid(
  mtry = c(40,45,50),
  min_n = c(2,4,8)
)


rf_mod1 <- 
  rand_forest(mtry = 15, min_n = 2,trees = 2500) %>% 
  set_engine("ranger",importance = "impurity") %>% 
  set_mode("classification")%>%
  fit(target_direction_5~., data = rows_instockprice_train)

saveRDS(rf_mod1, "stock_pick_model_5day.rds")
rf_mod <- readRDS(stock_pick_model.rds)


saveRDS(train_fit_rf, "stock_pick_model_10day_rf.rds")
rf_mod <- readRDS(stock_pick_model.rds)

rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(),trees = 500) %>% 
  set_engine("ranger",importance = "impurity",num.threads = cores ) %>% 
  set_mode("classification")
##%>%
  fit(target_direction_5~., data = rows_instockprice_train)

rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(stock_recipe)

cv <- vfold_cv(rows_instockprice_train, v = 2)

set.seed(345)
rf_res <- 
  rf_workflow %>% 
  tune_grid(cv,
            grid = rf_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

rf_res %>% 
  collect_metrics()%>%
  arrange(desc(mean))

rf_res %>%
  collect_predictions()


autoplot(rf_res)


rf_res %>% 
  show_best(metric = "roc_auc")

rf_best <- 
  rf_res %>% 
  select_best(metric = "roc_auc")
rf_best

last_rf_workflow <- finalize_workflow(
  rf_workflow,
  rf_best
)
library(vip)


train_fit_rf <- fit(last_rf_workflow,rows_instockprice_train)

train_fit_rf%>%
  pull_workflow_fit() %>%
  vip(geom = "point",num_features = 50)



test_pred_rf <- predict(train_fit_rf, rows_instockprice_test, type = "prob") %>%
  bind_cols(predict(train_fit_rf, rows_instockprice_test)) %>%
  bind_cols(select(rows_instockprice_test, target_direction_5)) %>%
  glimpse()

test_pred_rf <- predict(rf_mod, rows_instockprice_test, type = "prob") %>%
  bind_cols(predict(rf_mod, rows_instockprice_test)) %>%
  bind_cols(select(rows_instockprice_test, target_direction_5)) %>%
  glimpse()


test_pred_rf %>%
  sens(truth = target_direction_5,estimate = .pred_class)

test_pred_rf%>%
  spec(truth = target_direction_5,estimate = .pred_class)

test_pred_rf %>%
  roc_auc(truth = target_direction_5,estimate = .pred_buy)

test_pred_rf %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_5), 
               alpha = 0.5)+
  ggtitle("Random Forest ")+
  xlab("Probability of Yes")

conf_mat(test_pred_rf, truth = target_direction_5,
         estimate = as_factor(higher_threshold))

test_pred_rf$higher_threshold <- if_else(test_pred_rf$.pred_buy>=0.60,1,0)

test_pred_rf$higher_threshold<- if_else(test_pred_rf$higher_threshold == 1, "buy","sell")

test_pred_rf$higher_threshold <- factor(test_pred_rf$higher_threshold, levels=c("buy","sell"))

test_pred_rf%>%
  spec(truth = target_direction_10,estimate = as.factor(higher_threshold))

test_pred_rf%>%
  sens(truth = target_direction_10,estimate = as.factor(higher_threshold))
test_pred_rf %>%
  conf_mat( truth = target_direction_10, estimate =higher_threshold)


svm_mod <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

svm_workflow <- 
  workflow() %>% 
  add_model(rsvm_mod) %>% 
  add_recipe(stock_recipe)



cv <- vfold_cv(rows_instockprice_train, v = 3, strata = target_direction_10)


set.seed(345)
svm_res <- 
  svm_workflow %>% 
  tune_grid(cv,
            grid = 20,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

svm_res %>% 
  collect_metrics()%>%
  arrange(desc(mean))

svm_res %>%
  collect_predictions()


autoplot(svm_res)


svm_res %>% 
  show_best(metric = "roc_auc")

svm_best <- 
  svm_res %>% 
  select_best(metric = "roc_auc")
svm_best

last_svm_workflow <- finalize_workflow(
  svm_workflow,
  svm_best
)



train_fit_svm <- fit(last_svm_workflow,rows_instockprice_train)


test_pred_svm <- predict(train_fit_svm, rows_instockprice_test, type = "prob") %>%
  bind_cols(predict(train_fit_svm, rows_instockprice_test)) %>%
  bind_cols(select(rows_instockprice_test, target_direction_5)) %>%
  glimpse()

test_pred_svm %>%
  sens(truth = target_direction_5,estimate = .pred_class)

test_pred_svm%>%
  spec(truth = target_direction_5,estimate = .pred_class)

test_pred_svm %>%
  roc_auc(truth = target_direction_5,estimate = .pred_buy)

test_pred_svm %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_5), 
               alpha = 0.5)+
  ggtitle("Random Forest Probability of Yes - Day One")+
  xlab("Probability of Yes")




conf_mat(test_pred_rf, truth = target_direction_5,
         estimate = as_factor(higher_threshold))

test_pred_rf$higher_threshold <- if_else(test_pred_rf$.pred_buy>=0.54,1,0)

test_pred_rf$higher_threshold<- if_else(test_pred_rf$higher_threshold == 1, "buy","sell")

test_pred_rf$higher_threshold <- factor(test_pred_rf$higher_threshold, levels=c("buy","sell"))

test_pred_rf%>%
  spec(truth = target_direction_5,estimate = as.factor(higher_threshold))

test_pred_rf%>%
  sens(truth = target_direction_5,estimate = as.factor(higher_threshold))
test_pred_rf %>%
conf_mat( truth = target_direction_5, estimate =higher_threshold)

tech_indic <- sp_500_1 %>%
  filter(date > "2021-3-25")

test <- sp_500_1 %>%
  na.omit()

full_pred <- sp_500_1 %>%
  select(-c(symbol,date, open:target_5,close_shift_10:signal,hma_9:hma_200,DIp:ADX))%>%
  na.omit()

sp_500_1 <- sp_500_1%>%
  mutate(price_diff_5 = (close_shift_5-lead(open)))

sp_500_1 <- sp_500_1 %>%
  relocate(price_diff_5, .before = target_5)

full_set_test <- sp_500_1 %>%
  select(-c(open:return_daily,close_shift_10:signal,hma_9:hma_200,DIp:ADX))%>%
  na.omit()

full_pred_test <- predict(rf_mod, full_pred, type = "prob") %>%
  bind_cols(predict(rf_mod, full_pred)) %>%
  bind_cols(select(full_pred, target_direction_5)) %>%
  glimpse()

full_pred_test$target_direction_5<- if_else(full_pred_test$target_direction_5 == 1, "buy","sell")

full_pred_test$target_direction_5 <- factor(full_pred_test$target_direction_5, levels=c("buy","sell"))


full_pred_test%>%
  sens(truth = target_direction_5,estimate = .pred_class)

full_pred_test%>%
  spec(truth = target_direction_5,estimate = .pred_class)

full_pred_test %>%
  roc_auc(truth = target_direction_5,estimate = .pred_buy)

full_pred$tech_rules <- if_else(full_pred$rsi_14 <= 50 | full_pred$rsi_14_lag1 <= 50 | full_pred$rsi_14_lag2 <= 50 &&
                                full_pred$hma_9_lag_1>=1&& full_pred$hma_9_lag_3>=1&&
                                full_pred$sar_ratio >=1&& full_pred$diff>=0,1,0)

full_pred_test$tech_rules <- full_pred$tech_rules

full_pred_test$tech_rules<- if_else(full_pred_test$tech_rules == 1, "buy","sell")

full_pred_test$tech_rules <- factor(full_pred_test$tech_rules, levels=c("buy","sell"))

full_pred_test%>%
  sens(truth = target_direction_5,estimate = tech_rules)

full_pred_test%>%
  spec(truth = target_direction_5,estimate = tech_rules)

full_pred_test %>%
  roc_auc(truth = target_direction_5,estimate = tech_rules)

full_set <- bind_cols(full_pred_test,full_set_test)%>%
  filter(date=="2020-12-31")

full_set_filtered <- full_set %>%
  slice_max(.pred_buy, n =20)


full_set_thanksgiving <- bind_cols(full_pred_test,full_set_test)%>%
  filter(date=="2021-11-5",close_shift_5 >=15 & close_shift_5<100)

full_set_filtered <- full_set_thanksgiving %>%
  slice_max(.pred_buy, n =20)

cumsum(full_set_filtered$target_5)

full_set_thanksgiving <- bind_cols(full_pred_test,full_set_test)%>%
  filter(date=="2021-12-30", close_shift_5 >=15 & close_shift_5<100)

full_set_filtered <- full_set_thanksgiving %>%
  slice_max(.pred_buy, n =20)

sum(full_set_filtered$price_diff_5)

cumsum(full_set_filtered$target_5)



rf_grid1 <- expand.grid(
  mtry = c(20,25,30,35,40)
)




rf_mod1 <- 
  rand_forest(mtry = tune(), trees= 2000) %>% 
  set_engine("randomForest",num.threads = cores) %>% 
  set_mode("classification")

rf_workflow1 <- 
  workflow() %>% 
  add_model(rf_mod1) %>% 
  add_recipe(stock_recipe)



cv <- vfold_cv(rows_instockprice_train, v = 2)

set.seed(345)
rf_res1 <- 
  rf_workflow1 %>% 
  tune_grid(cv,
            grid = rf_grid1)


set.seed(345)
rf_res <- 
  rf_workflow %>% 
  tune_grid(cv,
            grid = rf_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

autoplot(rf_res1)

rf_best1 <- 
  rf_res1 %>% 
  select_best(metric = "roc_auc")
rf_best1

last_rf_workflow1 <- finalize_workflow(
  rf_workflow1,
  rf_best1
)

train_fit_rf1 <- fit(last_rf_workflow1,rows_instockprice_train)


test_pred_rf1 <- predict(train_fit_rf1, rows_instockprice_test, type = "prob") %>%
  bind_cols(predict(train_fit_rf1, rows_instockprice_test)) %>%
  bind_cols(select(rows_instockprice_test, target_direction_5)) %>%
  glimpse()

test_pred_rf1 %>%
  sens(truth = target_direction_5,estimate = .pred_class)

test_pred_rf1%>%
  spec(truth = target_direction_5,estimate = .pred_class)

test_pred_rf1 %>%
  roc_auc(truth = target_direction_5,estimate = .pred_buy)

test_pred_rf1 %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_5), 
               alpha = 0.5)+
  ggtitle("Random Forest Probability of Yes - Day One")+
  xlab("Probability of Yes")

buy_higher_perc <- test_pred_rf1 %>%
  filter(.pred_buy >=0.60)

buy_higher_perc %>%
  sens(truth = target_direction_5,estimate = .pred_class)

buy_higher_perc%>%
  spec(truth = target_direction_5,estimate = .pred_class)

buy_higher_perc %>%
  roc_auc(truth = target_direction_5,estimate = .pred_buy)

test_filter <- sp_500_1 %>%
  filter(date == Sys.Date()-2)

look <- sp_500_1 %>%
  filter(symbol =="IVT")

watch<-test_filter %>%
  filter(rsi_14 <= 50 & diff >=0 & hma_9_lag_1 >= hma_9_lag_2 & hma_9_lag_2 >= hma_9_lag_3 & hma_9co25>=1)

watch_1<-test_filter %>%
  filter(rsi_14 <= 50 & diff >=0 &hma_9co25 >=1 &sar_ratio>=1 & volume>=500000)

watch_2<-test_filter %>%
  filter(rsi_14 <= 50  &
           hma_9_lag_1>=1 & hma_9_lag_3>=1 & 
           sar_ratio >=1 & diff>=0)%>%
  select(symbol)

watch_3<-test_filter %>%
  filter(rsi_14_lag1 <= 50  &
           hma_9_lag_1>=1 & hma_9_lag_3>=1 & 
           sar_ratio >=1 & diff>=0)%>%
  select(symbol)

watch_4<-test_filter %>%
  filter(rsi_14_lag2 <= 50  &
           hma_9_lag_1>=1 & hma_9_lag_3>=1 & 
           sar_ratio >=1 & diff>=0)%>%
  select(symbol)

watch_symbol <- rbind(watch_2, watch_3, watch_4)

watch_test<-test_filter %>%
  filter(rsi_14 <= 50 | rsi_14_lag1 <= 50 | rsi_14_lag2 <= 50, fastD<=.35 | stoch <=.35,
           hma_9_lag_1>=1, hma_9_lag_3>=1, 
           sar_ratio >=1, diff>=0)%>%
  select(symbol)

#### Use this for call option


sum(long_term$close)

test_filter$long_term <- test_filter$close/test_filter$sma_200

test <- test_filter %>%
  arrange(desc(long_term))

mrna <- sp_500_1 %>%
  filter(symbol =="MRNA")

nas <- tq_exchange("NASDAQ")
nyse <- tq_exchange("NYSE")

nas <- nas %>%
  filter(market.cap >=1000000000)%>%
select(symbol)

nyse <- nyse%>%
  filter(market.cap >=1000000000)%>%
  select(symbol)

full_index <- bind_rows(nas,nyse)

ful_ind_price <-tq_get(full_index$symbol,get="stock.prices",
       from = start_date)


  
##sp_500_1 <- sp_500_1 %>%
##  select(-c("fastK",                "fastD" ,               "stoch" ,              
##             "SMI",                  "signal..1",            "fastK..1" ,           
##            "fastD..1",             "stoch..1"  ,           "fastK..2" ,           
##            "fastD..2" ,            "stoch..2" ,            "fastK..3" ,           
##            "fastD..3",             "stoch..3" ))
  


##rules for indicators

sp_500_1$diff_signal <- if_else(sp_500_1$diff >=0,1,0)
sp_500_1$hma_short_signal <- if_else(sp_500_1$hma_9co25 >=1,1,0)
sp_500_1$hma_medium_signal <- if_else(sp_500_1$hma_9co81 >=1,1,0)
sp_500_1$hma_long_signal <- if_else(sp_500_1$hma_25co81 >=1,1,0)
sp_500_1$sar_signal <- if_else(sp_500_1$sar_ratio >=1,1,0)
sp_500_1$rsi_signal <- if_else(sp_500_1$rsi_14<=50,1,0)

##rules for lag indicators
sp_500_1$diff_sginal_lag<- if_else(sp_500_1$diff>=0 & sp_500_1$diff_lag1 <0,
                                  "yes","no")

sp_500_1$hma_9co25_sginal_lag<- if_else(sp_500_1$hma_9co25 >=0 & sp_500_1$hma_9co25_lag1 <0,
                                  "yes","no")

sp_500_1$sar_sginal_lag<- if_else(sp_500_1$sar_ratio >=0 & sp_500_1$sar_ratio_lag1 <0,
                                        "yes","no")




signal_group <- c("diff_signal","hma_short_signal",
                  "hma_long_signal","sar_signal",
                  "rsi_signal","hma_medium_signal")

sp_500_1$signal_total <- rowSums(sp_500_1[,signal_group],na.rm=TRUE)

friday_signals <- sp_500_1%>%
  filter(date == Sys.Date()-4)

watch_signals <- friday_signals %>%
  filter(rsi_14 <=50 & hma_short_signal ==1 &
           hma_medium_signal ==0 & hma_long_signal == 0 & 
           volume >=500000 & fastK <=0.35 )%>%
  arrange(hma_9co25)
  ##select(symbol)

buy_signal <- friday_signals %>%
  filter(signal_total >=4 & close <200)

buy_signal <- friday_signals %>%
  filter(signal_total >=5 & close <175)

buy_signal <- friday_signals %>%
  filter(signal_total >=5 & volume >=500000 & close >175)

macd_signal <- friday_signals %>%
  filter(diff_signal == 1)


macd_signal <- friday_signals %>%
  filter(diff_signal== 1)


AAPL = getOptionChain(Symbols = vec_sym)
head(AAPL$calls)

AAPL$calls
AAPL$calls
glimpse(AAPL$calls)


vec_sym <- as.vector(t(watch_signals$symbol))

test_split <- rownames_to_column(AAPL$calls, var= "stock_date")

  ymd(str_sub(test_split$stock_dat,start = 4, end =9 ))
  
nas <- nas %>%
    filter(market.cap >=1000000000)%>%
    select(symbol)

for (symbol in watch_4$symbol) {
  name <- getOptionChain(Symbols = symbol)
}

##loop through symbols that are picked 
## used to find option date
stock_calls = list()
stock_puts = list()
for (symbol in watch_4$symbol) {
  # ... make some data
  dat <- getOptionChain(Symbols = symbol)
  dat$symbol <- symbol  # maybe you want to keep track of which iteration produced it?
  row_put <- rownames_to_column(dat$calls, var= "stock_date")
  row_call <-rownames_to_column(dat$puts, var= "stock_date")
  stock_calls[[symbol]] <- row_call # add it to your list
  stock_puts[[symbol]] <- row_put
}
stock_call <- bind_rows(stock_calls, .id = "column_label")


stock_call$symbol_char <-nchar(stock_call$column_label)+1
stock_call$end_symbol_char <- nchar(stock_call$column_label)+6

stock_call$start_date1 <- ymd(str_sub(stock_call$stock_date,
              start = stock_call$symbol_char,
              end =stock_call$end_symbol_char))

stock_call <- stock_call %>%
  relocate(start_date1, .after = column_label)%>%
  group_by(column_label)%>%
  slice(1)%>%
  select(1:2)%>%
  mutate(this_week_option =if_else((start_date1-friday)<=7,1,0) )%>%
  filter(this_week_option ==1)


friday <- Sys.Date()-wday(Sys.Date()+1) ## shows last Friday

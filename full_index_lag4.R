library(httr)
library(jsonlite)
library(tidyverse)
library(quantmod)
library(tidyquant)
library(tidymodels)
library(randomForest)
library(glmnet)
library(doParallel)

rm(list = ls())


start_date <- "2019-01-01"

nas <- tq_exchange("NASDAQ")
nyse <- tq_exchange("NYSE")

##@300mil theres 1776 symbols
##1.7 mil rows 2018-1-1

nas <- nas %>%
  filter(market.cap >= 1000000000)

nyse <- nyse%>%
  filter(market.cap >= 1000000000)

full_index <- bind_rows(nas,nyse)

stock_calls = list()
stock_puts = list()
dat= data.frame()
for (symbol in full_index$symbol) {
  # ... make some data
  tryCatch({
    dat <- getOptionChain(Symbols = symbol, Exp = "2022-04-01")
    dat$symbol <- symbol  # maybe you want to keep track of which iteration produced it?
    row_call <- rownames_to_column(dat$calls, var= "stock_date")
    stock_calls[[symbol]] <- row_call # add it to your list
  },
  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}



stock_call <- bind_rows(stock_calls, .id = "column_label")

n_distinct(stock_call$column_label)

ful_ind_price <-tq_get(full_index$symbol,get="stock.prices",
                       from = start_date)

avg_vol <- ful_ind_price%>%
  filter(volume >= 500000)%>%
  group_by(symbol)%>%
  count()

stock_build <- avg_vol%>%
  filter(n >=200)

date_filter <- ful_ind_price %>%
  group_by(symbol)%>%
  slice(1)%>%
  filter(date == "2019-01-02")

avg_vol <- sp_500 %>%
  #filter(symbol %in% date_filter$symbol)%>%
  group_by(symbol)%>%
  summarise(mean_vol = mean(volume), n=n())%>%
  filter(mean_vol >= 500000)
#%>%
 # select(symbol)

sp_500_1 <- ful_ind_price%>%
  filter(symbol %in% stock_build$symbol)

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
# #sp_500_1$diff_short_lag5 <- lag(sp_500_1$diff_short, n=5)
# sp_500_1$diff_short_lag6 <- lag(sp_500_1$diff_short, n=6)
# #sp_500_1$diff_short_lag7 <- lag(sp_500_1$diff_short, n=7)
# sp_500_1$diff_short_lag8 <- lag(sp_500_1$diff_short, n=8)
# #sp_500_1$diff_short_lag9 <- lag(sp_500_1$diff_short, n=9)
# sp_500_1$diff_short_lag10 <- lag(sp_500_1$diff_short, n=10)
# #sp_500_1$diff_short_lag11 <- lag(sp_500_1$diff_short, n=11)
# sp_500_1$diff_short_lag12 <- lag(sp_500_1$diff_short, n=12)
# #sp_500_1$diff_short_lag13 <- lag(sp_500_1$diff_short, n=13)
# sp_500_1$diff_short_lag14 <- lag(sp_500_1$diff_short, n=14)
# #sp_500_1$diff_short_lag15 <- lag(sp_500_1$diff_short, n=15)
# sp_500_1$diff_short_lag16 <- lag(sp_500_1$diff_short, n=16)
# #sp_500_1$diff_short_lag17 <- lag(sp_500_1$diff_short, n=17)
# sp_500_1$diff_short_lag18 <- lag(sp_500_1$diff_short, n=18)
# #sp_500_1$diff_short_lag19<- lag(sp_500_1$diff_short, n=19)
# sp_500_1$diff_short_lag20<- lag(sp_500_1$diff_short, n=20)

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
# #sp_500_1$diff_long_lag5 <- lag(sp_500_1$diff_long, n=5)
# sp_500_1$diff_long_lag6 <- lag(sp_500_1$diff_long, n=6)
# #sp_500_1$diff_long_lag7 <- lag(sp_500_1$diff_long, n=7)
# sp_500_1$diff_long_lag8 <- lag(sp_500_1$diff_long, n=8)
# #sp_500_1$diff_long_lag9 <- lag(sp_500_1$diff_long, n=9)
# sp_500_1$diff_long_lag10 <- lag(sp_500_1$diff_long, n=10)
# #sp_500_1$diff_long_lag11 <- lag(sp_500_1$diff_long, n=11)
# sp_500_1$diff_long_lag12 <- lag(sp_500_1$diff_long, n=12)
# #sp_500_1$diff_long_lag13 <- lag(sp_500_1$diff_long, n=13)
# sp_500_1$diff_long_lag14 <- lag(sp_500_1$diff_long, n=14)
# #sp_500_1$diff_long_lag15 <- lag(sp_500_1$diff_long, n=15)
# sp_500_1$diff_long_lag16 <- lag(sp_500_1$diff_long, n=16)
# #sp_500_1$diff_long_lag17 <- lag(sp_500_1$diff_long, n=17)
# sp_500_1$diff_long_lag18 <- lag(sp_500_1$diff_long, n=18)
# #sp_500_1$diff_long_lag19 <- lag(sp_500_1$diff_long, n=19)
# sp_500_1$diff_long_lag20 <- lag(sp_500_1$diff_long, n=20)

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
# #sp_500_1$rsi_14_lag5 <- lag(sp_500_1$rsi_14, n=5)
# sp_500_1$rsi_14_lag6 <- lag(sp_500_1$rsi_14, n=6)
# #sp_500_1$rsi_14_lag7 <- lag(sp_500_1$rsi_14, n=7)
# sp_500_1$rsi_14_lag8 <- lag(sp_500_1$rsi_14, n=8)
# #sp_500_1$rsi_14_lag9 <- lag(sp_500_1$rsi_14, n=9)
# sp_500_1$rsi_14_lag10 <- lag(sp_500_1$rsi_14, n=10)
# #sp_500_1$rsi_14_lag11 <- lag(sp_500_1$rsi_14, n=11)
# sp_500_1$rsi_14_lag12 <- lag(sp_500_1$rsi_14, n=12)
# #sp_500_1$rsi_14_lag13 <- lag(sp_500_1$rsi_14, n=13)
# sp_500_1$rsi_14_lag14 <- lag(sp_500_1$rsi_14, n=14)
# #sp_500_1$rsi_14_lag15 <- lag(sp_500_1$rsi_14, n=15)
# sp_500_1$rsi_14_lag16 <- lag(sp_500_1$rsi_14, n=16)
# #sp_500_1$rsi_14_lag17 <- lag(sp_500_1$rsi_14, n=17)
# sp_500_1$rsi_14_lag18 <- lag(sp_500_1$rsi_14, n=18)
# #sp_500_1$rsi_14_lag19 <- lag(sp_500_1$rsi_14, n=19)
# sp_500_1$rsi_14_lag20 <- lag(sp_500_1$rsi_14, n=20)


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
# #sp_500_1$sar_ratio_lag5 <- lag(sp_500_1$sar_ratio, n=5)
# sp_500_1$sar_ratio_lag6 <- lag(sp_500_1$sar_ratio, n=6)
# #sp_500_1$sar_ratio_lag7 <- lag(sp_500_1$sar_ratio, n=7)
# sp_500_1$sar_ratio_lag8 <- lag(sp_500_1$sar_ratio, n=8)
# #sp_500_1$sar_ratio_lag9 <- lag(sp_500_1$sar_ratio, n=9)
# sp_500_1$sar_ratio_lag10 <- lag(sp_500_1$sar_ratio, n=10)
# #sp_500_1$sar_ratio_lag11 <- lag(sp_500_1$sar_ratio, n=11)
# sp_500_1$sar_ratio_lag12 <- lag(sp_500_1$sar_ratio, n=12)
# #sp_500_1$sar_ratio_lag13 <- lag(sp_500_1$sar_ratio, n=13)
# sp_500_1$sar_ratio_lag14 <- lag(sp_500_1$sar_ratio, n=14)
# #sp_500_1$sar_ratio_lag15 <- lag(sp_500_1$sar_ratio, n=15)
# sp_500_1$sar_ratio_lag16 <- lag(sp_500_1$sar_ratio, n=16)
# #sp_500_1$sar_ratio_lag17 <- lag(sp_500_1$sar_ratio, n=17)
# sp_500_1$sar_ratio_lag18 <- lag(sp_500_1$sar_ratio, n=18)
# #sp_500_1$sar_ratio_lag19 <- lag(sp_500_1$sar_ratio, n=19)
# sp_500_1$sar_ratio_lag20 <- lag(sp_500_1$sar_ratio, n=20)



# sp_500_1 <- sp_500_1 %>%
#   filter(!symbol %in% c("AEVA","AMCR","BTRS","DKNG","LAZR","LPRO","MAXR"))

sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  tq_mutate(select = c("high","low","close"),
            mutate_fun = stoch)

sp_500_1$stoch_diff <- sp_500_1$fastD/sp_500_1$stoch

sp_500_1$stoch_diff_lag1 <-lag(sp_500_1$stoch_diff)
sp_500_1$stoch_diff_lag2 <-lag(sp_500_1$stoch_diff,n=2)
sp_500_1$stoch_diff_lag3 <-lag(sp_500_1$stoch_diff,n=3)
sp_500_1$stoch_diff_lag4<-lag(sp_500_1$stoch_diff,n=4)
# #sp_500_1$stoch_diff_lag5<-lag(sp_500_1$stoch_diff,n=5)
# sp_500_1$stoch_diff_lag6 <-lag(sp_500_1$stoch_diff, n=6)
# #sp_500_1$stoch_diff_lag7 <-lag(sp_500_1$stoch_diff,n=7)
# sp_500_1$stoch_diff_lag8 <-lag(sp_500_1$stoch_diff,n=8)
# #sp_500_1$stoch_diff_lag9<-lag(sp_500_1$stoch_diff,n=9)
# sp_500_1$stoch_diff_lag10<-lag(sp_500_1$stoch_diff,n=10)
# #sp_500_1$stoch_diff_lag11 <-lag(sp_500_1$stoch_diff,n=11)
# sp_500_1$stoch_diff_lag12 <-lag(sp_500_1$stoch_diff,n=12)
# #sp_500_1$stoch_diff_lag13 <-lag(sp_500_1$stoch_diff,n=13)
# sp_500_1$stoch_diff_lag14<-lag(sp_500_1$stoch_diff,n=14)
# #sp_500_1$stoch_diff_lag15<-lag(sp_500_1$stoch_diff,n=15)
# sp_500_1$stoch_diff_lag16 <-lag(sp_500_1$stoch_diff, n=16)
# #sp_500_1$stoch_diff_lag17 <-lag(sp_500_1$stoch_diff,n=17)
# sp_500_1$stoch_diff_lag18 <-lag(sp_500_1$stoch_diff,n=18)
# #sp_500_1$stoch_diff_lag19 <-lag(sp_500_1$stoch_diff,n=19)
# sp_500_1$stoch_diff_lag20 <-lag(sp_500_1$stoch_diff,n=20)
# 

sp_500_1$fastD_lag1 <- lag(sp_500_1$fastD)
sp_500_1$fastD_lag2 <- lag(sp_500_1$fastD, n=2)
sp_500_1$fastD_lag3 <- lag(sp_500_1$fastD, n=3)
sp_500_1$fastD_lag4 <- lag(sp_500_1$fastD, n=4)
# #sp_500_1$fastD_lag5 <- lag(sp_500_1$fastD, n=5)
# sp_500_1$fastD_lag6 <- lag(sp_500_1$fastD, n=6)
# #sp_500_1$fastD_lag7 <- lag(sp_500_1$fastD, n=7)
# sp_500_1$fastD_lag8 <- lag(sp_500_1$fastD, n=8)
# #sp_500_1$fastD_lag9 <- lag(sp_500_1$fastD, n=9)
# sp_500_1$fastD_lag10 <- lag(sp_500_1$fastD, n=10)
# #sp_500_1$fastD_lag11 <- lag(sp_500_1$fastD, n=11)
# sp_500_1$fastD_lag12 <- lag(sp_500_1$fastD, n=12)
# #sp_500_1$fastD_lag13 <- lag(sp_500_1$fastD, n=13)
# sp_500_1$fastD_lag14 <- lag(sp_500_1$fastD, n=14)
# #sp_500_1$fastD_lag15 <- lag(sp_500_1$fastD, n=15)
# sp_500_1$fastD_lag16 <- lag(sp_500_1$fastD, n=16)
# #sp_500_1$fastD_lag17 <- lag(sp_500_1$fastD, n=17)
# sp_500_1$fastD_lag18 <- lag(sp_500_1$fastD, n=18)
# #sp_500_1$fastD_lag19 <- lag(sp_500_1$fastD, n=19)
# sp_500_1$fastD_lag20 <- lag(sp_500_1$fastD, n=20)

sp_500_1$stoch_lag1 <- lag(sp_500_1$stoch)
sp_500_1$stoch_lag2 <- lag(sp_500_1$stoch, n=2)
sp_500_1$stoch_lag3 <- lag(sp_500_1$stoch, n=3)
sp_500_1$stoch_lag4 <- lag(sp_500_1$stoch, n=4)
# #sp_500_1$stoch_lag5 <- lag(sp_500_1$stoch, n=5)
# sp_500_1$stoch_lag6 <- lag(sp_500_1$stoch, n=6)
# #sp_500_1$stoch_lag7 <- lag(sp_500_1$stoch, n=7)
# sp_500_1$stoch_lag8 <- lag(sp_500_1$stoch, n=8)
# #sp_500_1$stoch_lag9 <- lag(sp_500_1$stoch, n=9)
# sp_500_1$stoch_lag10 <- lag(sp_500_1$stoch, n=10)
# #sp_500_1$stoch_lag11 <- lag(sp_500_1$stoch, n=11)
# sp_500_1$stoch_lag12 <- lag(sp_500_1$stoch, n=12)
# #sp_500_1$stoch_lag13 <- lag(sp_500_1$stoch, n=13)
# sp_500_1$stoch_lag14 <- lag(sp_500_1$stoch, n=14)
# #sp_500_1$stoch_lag15 <- lag(sp_500_1$stoch, n=15)
# sp_500_1$stoch_lag16 <- lag(sp_500_1$stoch,n=16)
# #sp_500_1$stoch_lag17 <- lag(sp_500_1$stoch, n=17)
# sp_500_1$stoch_lag18 <- lag(sp_500_1$stoch, n=18)
# #sp_500_1$stoch_lag19 <- lag(sp_500_1$stoch, n=19)
# sp_500_1$stoch_lag20<- lag(sp_500_1$stoch, n=20)

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
# #sp_500_1$di_ratio_lag5<- lag(sp_500_1$di_ratio, n=5)
# sp_500_1$di_ratio_lag6 <- lag(sp_500_1$di_ratio, n=6)
# #sp_500_1$di_ratio_lag7<- lag(sp_500_1$di_ratio, n=7)
# sp_500_1$di_ratio_lag8<- lag(sp_500_1$di_ratio, n=8)
# #sp_500_1$di_ratio_lag9<- lag(sp_500_1$di_ratio, n=9)
# sp_500_1$di_ratio_lag10<- lag(sp_500_1$di_ratio, n=10)
# #sp_500_1$di_ratio_lag11<- lag(sp_500_1$di_ratio, n=11)
# sp_500_1$di_ratio_lag12<- lag(sp_500_1$di_ratio, n=12)
# #sp_500_1$di_ratio_lag13<- lag(sp_500_1$di_ratio, n=13)
# sp_500_1$di_ratio_lag14<- lag(sp_500_1$di_ratio, n=14)
# #sp_500_1$di_ratio_lag15<- lag(sp_500_1$di_ratio, n=15)
# sp_500_1$di_ratio_lag16 <- lag(sp_500_1$di_ratio, n=16)
# #sp_500_1$di_ratio_lag17<- lag(sp_500_1$di_ratio, n=17)
# sp_500_1$di_ratio_lag18<- lag(sp_500_1$di_ratio, n=18)
# #sp_500_1$di_ratio_lag19<- lag(sp_500_1$di_ratio, n=19)
# sp_500_1$di_ratio_lag20<- lag(sp_500_1$di_ratio, n=20)

sp_500_1$adx_ratio_lag1 <- lag(sp_500_1$adx_ratio)
sp_500_1$adx_ratio_lag2<- lag(sp_500_1$adx_ratio, n=2)
sp_500_1$adx_ratio_lag3<- lag(sp_500_1$adx_ratio, n=3)
sp_500_1$adx_ratio_lag4<- lag(sp_500_1$adx_ratio, n=4)
# #sp_500_1$adx_ratio_lag5<- lag(sp_500_1$adx_ratio, n=5)
# sp_500_1$adx_ratio_lag6 <- lag(sp_500_1$adx_ratio, n=6)
# #sp_500_1$adx_ratio_lag7<- lag(sp_500_1$adx_ratio, n=7)
# sp_500_1$adx_ratio_lag8<- lag(sp_500_1$adx_ratio, n=8)
# #sp_500_1$adx_ratio_lag9<- lag(sp_500_1$adx_ratio, n=9)
# sp_500_1$adx_ratio_lag10<- lag(sp_500_1$adx_ratio, n=10)
# #sp_500_1$adx_ratio_lag11 <- lag(sp_500_1$adx_ratio, n=11)
# sp_500_1$adx_ratio_lag12<- lag(sp_500_1$adx_ratio, n=12)
# #sp_500_1$adx_ratio_lag13<- lag(sp_500_1$adx_ratio, n=13)
# sp_500_1$adx_ratio_lag14<- lag(sp_500_1$adx_ratio, n=14)
# #sp_500_1$adx_ratio_lag15<- lag(sp_500_1$adx_ratio, n=15)
# sp_500_1$adx_ratio_lag16 <- lag(sp_500_1$adx_ratio, n=16)
# #sp_500_1$adx_ratio_lag17<- lag(sp_500_1$adx_ratio, n=17)
# sp_500_1$adx_ratio_lag18<- lag(sp_500_1$adx_ratio, n=18)
# #sp_500_1$adx_ratio_lag19<- lag(sp_500_1$adx_ratio, n=19)
# sp_500_1$adx_ratio_lag20<- lag(sp_500_1$adx_ratio, n=20)

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
# #sp_500_1$lowerBB_candle_lag5 <- lag(sp_500_1$lowerBB_candle, n=5)
# sp_500_1$lowerBB_candle_lag6 <- lag(sp_500_1$lowerBB_candle, n=6)
# #sp_500_1$lowerBB_candle_lag7 <- lag(sp_500_1$lowerBB_candle, n=7)
# sp_500_1$lowerBB_candle_lag8 <- lag(sp_500_1$lowerBB_candle, n=8)
# #sp_500_1$lowerBB_candle_lag9 <- lag(sp_500_1$lowerBB_candle, n=9)
# sp_500_1$lowerBB_candle_lag10 <- lag(sp_500_1$lowerBB_candle, n=10)
# #sp_500_1$lowerBB_candle_lag11 <- lag(sp_500_1$lowerBB_candle, n=11)
# sp_500_1$lowerBB_candle_lag12 <- lag(sp_500_1$lowerBB_candle, n=12)
# #sp_500_1$lowerBB_candle_lag13 <- lag(sp_500_1$lowerBB_candle, n=13)
# sp_500_1$lowerBB_candle_lag14 <- lag(sp_500_1$lowerBB_candle, n=14)
# #sp_500_1$lowerBB_candle_lag15 <- lag(sp_500_1$lowerBB_candle, n=15)
# sp_500_1$lowerBB_candle_lag16 <- lag(sp_500_1$lowerBB_candle, n=16)
# #sp_500_1$lowerBB_candle_lag17 <- lag(sp_500_1$lowerBB_candle, n=17)
# sp_500_1$lowerBB_candle_lag18 <- lag(sp_500_1$lowerBB_candle, n=18)
# #sp_500_1$lowerBB_candle_lag19 <- lag(sp_500_1$lowerBB_candle, n=19)
# sp_500_1$lowerBB_candle_lag20 <- lag(sp_500_1$lowerBB_candle, n=20)

sp_500_1$upperBB_candle_lag1 <- lag(sp_500_1$upperBB_candle)
sp_500_1$upperBB_candle_lag2 <- lag(sp_500_1$upperBB_candle, n=2)
sp_500_1$upperBB_candle_lag3 <- lag(sp_500_1$upperBB_candle, n=3)
sp_500_1$upperBB_candle_lag4 <- lag(sp_500_1$upperBB_candle, n=4)
# #sp_500_1$upperBB_candle_lag5 <- lag(sp_500_1$upperBB_candle, n=5)
# sp_500_1$upperBB_candle_lag6 <- lag(sp_500_1$upperBB_candle, n=6)
# #sp_500_1$upperBB_candle_lag7 <- lag(sp_500_1$upperBB_candle, n=7)
# sp_500_1$upperBB_candle_lag8 <- lag(sp_500_1$upperBB_candle, n=8)
# #sp_500_1$upperBB_candle_lag9 <- lag(sp_500_1$upperBB_candle, n=9)
# sp_500_1$upperBB_candle_lag10 <- lag(sp_500_1$upperBB_candle, n=10)
# #sp_500_1$upperBB_candle_lag11 <- lag(sp_500_1$upperBB_candle, n=11)
# sp_500_1$upperBB_candle_lag12 <- lag(sp_500_1$upperBB_candle, n=12)
# #sp_500_1$upperBB_candle_lag13 <- lag(sp_500_1$upperBB_candle, n=13)
# sp_500_1$upperBB_candle_lag14 <- lag(sp_500_1$upperBB_candle, n=14)
# #sp_500_1$upperBB_candle_lag15 <- lag(sp_500_1$upperBB_candle, n=15)
# sp_500_1$upperBB_candle_lag16 <- lag(sp_500_1$upperBB_candle, n=16)
# #sp_500_1$upperBB_candle_lag17 <- lag(sp_500_1$upperBB_candle, n=17)
# sp_500_1$upperBB_candle_lag18 <- lag(sp_500_1$upperBB_candle, n=18)
# #sp_500_1$upperBB_candle_lag19 <- lag(sp_500_1$upperBB_candle, n=19)
# sp_500_1$upperBB_candle_lag20 <- lag(sp_500_1$upperBB_candle, n=20)

sp_500_1$bbands_size_lag1  <- lag(sp_500_1$bbands_size)
sp_500_1$bbands_size_lag2 <- lag(sp_500_1$bbands_size, n=2)
sp_500_1$bbands_size_lag3 <- lag(sp_500_1$bbands_size, n=3)
sp_500_1$bbands_size_lag4 <- lag(sp_500_1$bbands_size, n=4)
# #sp_500_1$bbands_size_lag5 <- lag(sp_500_1$bbands_size, n=5)
# sp_500_1$bbands_size_lag6  <- lag(sp_500_1$bbands_size, n=6)
# #sp_500_1$bbands_size_lag7 <- lag(sp_500_1$bbands_size, n=7)
# sp_500_1$bbands_size_lag8 <- lag(sp_500_1$bbands_size, n=8)
# #sp_500_1$bbands_size_lag9 <- lag(sp_500_1$bbands_size, n=9)
# sp_500_1$bbands_size_lag10 <- lag(sp_500_1$bbands_size, n=10)
# #sp_500_1$bbands_size_lag11  <- lag(sp_500_1$bbands_size, n=11)
# sp_500_1$bbands_size_lag12 <- lag(sp_500_1$bbands_size, n=12)
# #sp_500_1$bbands_size_lag13 <- lag(sp_500_1$bbands_size, n=13)
# sp_500_1$bbands_size_lag14 <- lag(sp_500_1$bbands_size, n=14)
# #sp_500_1$bbands_size_lag15 <- lag(sp_500_1$bbands_size, n=15)
# sp_500_1$bbands_size_lag16  <- lag(sp_500_1$bbands_size, n=16)
# #sp_500_1$bbands_size_lag17 <- lag(sp_500_1$bbands_size, n=17)
# sp_500_1$bbands_size_lag18 <- lag(sp_500_1$bbands_size, n=18)
# #sp_500_1$bbands_size_lag19 <- lag(sp_500_1$bbands_size, n=19)
# sp_500_1$bbands_size_lag20 <- lag(sp_500_1$bbands_size, n=20)


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
# 
# # sp_500_1$hma_9_candle_lag5 <- lag(sp_500_1$hma_9_candle,n=5)
# # sp_500_1$hma_25_candle_lag5 <- lag(sp_500_1$hma_25_candle,n=5)
# # sp_500_1$hma_49_candle_lag5 <- lag(sp_500_1$hma_49_candle,n=5)
# # sp_500_1$hma_81_candle_lag5 <- lag(sp_500_1$hma_81_candle,n=5)
# # sp_500_1$hma_200_candle_lag5 <- lag(sp_500_1$hma_200_candle,n=5)
# # sp_500_1$sma_50_candle_lag5<- lag(sp_500_1$sma_50_candle,n=5)
# # sp_500_1$sma_100_candle_lag5 <- lag(sp_500_1$sma_100_candle,n=5)
# # sp_500_1$sma_200_candle_lag5 <- lag(sp_500_1$sma_200_candle,n=5)
# 
# sp_500_1$hma_9_candle_lag6 <- lag(sp_500_1$hma_9_candle,n=6)
# sp_500_1$hma_25_candle_lag6 <- lag(sp_500_1$hma_25_candle,n=6)
# sp_500_1$hma_49_candle_lag6 <- lag(sp_500_1$hma_49_candle,n=6)
# sp_500_1$hma_81_candle_lag6 <- lag(sp_500_1$hma_81_candle,n=6)
# sp_500_1$hma_200_candle_lag6 <- lag(sp_500_1$hma_200_candle,n=6)
# sp_500_1$sma_50_candle_lag6<- lag(sp_500_1$sma_50_candle,n=6)
# sp_500_1$sma_100_candle_lag6 <- lag(sp_500_1$sma_100_candle,n=6)
# sp_500_1$sma_200_candle_lag6 <- lag(sp_500_1$sma_200_candle,n=6)
# 
# # sp_500_1$hma_9_candle_lag7 <- lag(sp_500_1$hma_9_candle,n=7)
# # sp_500_1$hma_25_candle_lag7 <- lag(sp_500_1$hma_25_candle,n=7)
# # sp_500_1$hma_49_candle_lag7 <- lag(sp_500_1$hma_49_candle,n=7)
# # sp_500_1$hma_81_candle_lag7 <- lag(sp_500_1$hma_81_candle,n=7)
# # sp_500_1$hma_200_candle_lag7 <- lag(sp_500_1$hma_200_candle,n=7)
# # sp_500_1$sma_50_candle_lag7<- lag(sp_500_1$sma_50_candle,n=7)
# # sp_500_1$sma_100_candle_lag7 <- lag(sp_500_1$sma_100_candle,n=7)
# # sp_500_1$sma_200_candle_lag7 <- lag(sp_500_1$sma_200_candle,n=7)
# 
# sp_500_1$hma_9_candle_lag8 <- lag(sp_500_1$hma_9_candle,n=8)
# sp_500_1$hma_25_candle_lag8 <- lag(sp_500_1$hma_25_candle,n=8)
# sp_500_1$hma_49_candle_lag8 <- lag(sp_500_1$hma_49_candle,n=8)
# sp_500_1$hma_81_candle_lag8 <- lag(sp_500_1$hma_81_candle,n=8)
# sp_500_1$hma_200_candle_lag8 <- lag(sp_500_1$hma_200_candle,n=8)
# sp_500_1$sma_50_candle_lag8<- lag(sp_500_1$sma_50_candle,n=8)
# sp_500_1$sma_100_candle_lag8 <- lag(sp_500_1$sma_100_candle,n=8)
# sp_500_1$sma_200_candle_lag8 <- lag(sp_500_1$sma_200_candle,n=8)
# 
# # sp_500_1$hma_9_candle_lag9 <- lag(sp_500_1$hma_9_candle,n=9)
# # sp_500_1$hma_25_candle_lag9 <- lag(sp_500_1$hma_25_candle,n=9)
# # sp_500_1$hma_49_candle_lag9 <- lag(sp_500_1$hma_49_candle,n=9)
# # sp_500_1$hma_81_candle_lag9 <- lag(sp_500_1$hma_81_candle,n=9)
# # sp_500_1$hma_200_candle_lag9 <- lag(sp_500_1$hma_200_candle,n=9)
# # sp_500_1$sma_50_candle_lag9<- lag(sp_500_1$sma_50_candle,n=9)
# # sp_500_1$sma_100_candle_lag9 <- lag(sp_500_1$sma_100_candle,n=9)
# # sp_500_1$sma_200_candle_lag9 <- lag(sp_500_1$sma_200_candle,n=9)
# 
# sp_500_1$hma_9_candle_lag10 <- lag(sp_500_1$hma_9_candle,n=10)
# sp_500_1$hma_25_candle_lag10 <- lag(sp_500_1$hma_25_candle,n=10)
# sp_500_1$hma_49_candle_lag10 <- lag(sp_500_1$hma_49_candle,n=10)
# sp_500_1$hma_81_candle_lag10 <- lag(sp_500_1$hma_81_candle,n=10)
# sp_500_1$hma_200_candle_lag10 <- lag(sp_500_1$hma_200_candle,n=10)
# sp_500_1$sma_50_candle_lag10 <- lag(sp_500_1$sma_50_candle,n=10)
# sp_500_1$sma_100_candle_lag10 <- lag(sp_500_1$sma_100_candle,n=10)
# sp_500_1$sma_200_candle_lag10 <- lag(sp_500_1$sma_200_candle,n=10)
# 
# # sp_500_1$hma_9_candle_lag11 <- lag(sp_500_1$hma_9_candle,n=11)
# # sp_500_1$hma_25_candle_lag11 <- lag(sp_500_1$hma_25_candle,n=11)
# # sp_500_1$hma_49_candle_lag11 <- lag(sp_500_1$hma_49_candle,n=11)
# # sp_500_1$hma_81_candle_lag11 <- lag(sp_500_1$hma_81_candle,n=11)
# # sp_500_1$hma_200_candle_lag11 <- lag(sp_500_1$hma_200_candle,n=11)
# # sp_500_1$sma_50_candle_lag11<- lag(sp_500_1$sma_50_candle,n=11)
# # sp_500_1$sma_100_candle_lag11 <- lag(sp_500_1$sma_100_candle,n=11)
# # sp_500_1$sma_200_candle_lag11 <- lag(sp_500_1$sma_200_candle,n=11)
# 
# sp_500_1$hma_9_candle_lag12 <- lag(sp_500_1$hma_9_candle,n=12)
# sp_500_1$hma_25_candle_lag12 <- lag(sp_500_1$hma_25_candle,n=12)
# sp_500_1$hma_49_candle_lag12 <- lag(sp_500_1$hma_49_candle,n=12)
# sp_500_1$hma_81_candle_lag12 <- lag(sp_500_1$hma_81_candle,n=12)
# sp_500_1$hma_200_candle_lag12 <- lag(sp_500_1$hma_200_candle,n=12)
# sp_500_1$sma_50_candle_lag12<- lag(sp_500_1$sma_50_candle,n=12)
# sp_500_1$sma_100_candle_lag12 <- lag(sp_500_1$sma_100_candle,n=12)
# sp_500_1$sma_200_candle_lag12<- lag(sp_500_1$sma_200_candle,n=12)
# 
# # sp_500_1$hma_9_candle_lag13 <- lag(sp_500_1$hma_9_candle,n=13)
# # sp_500_1$hma_25_candle_lag13 <- lag(sp_500_1$hma_25_candle,n=13)
# # sp_500_1$hma_49_candle_lag13 <- lag(sp_500_1$hma_49_candle,n=13)
# # sp_500_1$hma_81_candle_lag13 <- lag(sp_500_1$hma_81_candle,n=13)
# # sp_500_1$hma_200_candle_lag13 <- lag(sp_500_1$hma_200_candle,n=13)
# # sp_500_1$sma_50_candle_lag13<- lag(sp_500_1$sma_50_candle,n=13)
# # sp_500_1$sma_100_candle_lag13 <- lag(sp_500_1$sma_100_candle,n=13)
# # sp_500_1$sma_200_candle_lag13<- lag(sp_500_1$sma_200_candle,n=13)
# 
# sp_500_1$hma_9_candle_lag14 <- lag(sp_500_1$hma_9_candle,n=14)
# sp_500_1$hma_25_candle_lag14 <- lag(sp_500_1$hma_25_candle,n=14)
# sp_500_1$hma_49_candle_lag14 <- lag(sp_500_1$hma_49_candle,n=14)
# sp_500_1$hma_81_candle_lag14 <- lag(sp_500_1$hma_81_candle,n=14)
# sp_500_1$hma_200_candle_lag14 <- lag(sp_500_1$hma_200_candle,n=14)
# sp_500_1$sma_50_candle_lag14<- lag(sp_500_1$sma_50_candle,n=14)
# sp_500_1$sma_100_candle_lag14 <- lag(sp_500_1$sma_100_candle,n=14)
# sp_500_1$sma_200_candle_lag14<- lag(sp_500_1$sma_200_candle,n=14)
# 
# # sp_500_1$hma_9_candle_lag15 <- lag(sp_500_1$hma_9_candle,n=15)
# # sp_500_1$hma_25_candle_lag15 <- lag(sp_500_1$hma_25_candle,n=15)
# # sp_500_1$hma_49_candle_lag15 <- lag(sp_500_1$hma_49_candle,n=15)
# # sp_500_1$hma_81_candle_lag15 <- lag(sp_500_1$hma_81_candle,n=15)
# # sp_500_1$hma_200_candle_lag15 <- lag(sp_500_1$hma_200_candle,n=15)
# # sp_500_1$sma_50_candle_lag15<- lag(sp_500_1$sma_50_candle,n=15)
# # sp_500_1$sma_100_candle_lag15 <- lag(sp_500_1$sma_100_candle,n=15)
# # sp_500_1$sma_200_candle_lag15<- lag(sp_500_1$sma_200_candle,n=15)
# 
# sp_500_1$hma_9_candle_lag16 <- lag(sp_500_1$hma_9_candle,n=16)
# sp_500_1$hma_25_candle_lag16 <- lag(sp_500_1$hma_25_candle,n=16)
# sp_500_1$hma_49_candle_lag16 <- lag(sp_500_1$hma_49_candle,n=16)
# sp_500_1$hma_81_candle_lag16 <- lag(sp_500_1$hma_81_candle,n=16)
# sp_500_1$hma_200_candle_lag16 <- lag(sp_500_1$hma_200_candle,n=16)
# sp_500_1$sma_50_candle_lag16<- lag(sp_500_1$sma_50_candle,n=16)
# sp_500_1$sma_100_candle_lag16 <- lag(sp_500_1$sma_100_candle,n=16)
# sp_500_1$sma_200_candle_lag16<- lag(sp_500_1$sma_200_candle,n=16)
# 
# # sp_500_1$hma_9_candle_lag17 <- lag(sp_500_1$hma_9_candle,n=17)
# # sp_500_1$hma_25_candle_lag17 <- lag(sp_500_1$hma_25_candle,n=17)
# # sp_500_1$hma_49_candle_lag17 <- lag(sp_500_1$hma_49_candle,n=17)
# # sp_500_1$hma_81_candle_lag17 <- lag(sp_500_1$hma_81_candle,n=17)
# # sp_500_1$hma_200_candle_lag17 <- lag(sp_500_1$hma_200_candle,n=17)
# # sp_500_1$sma_50_candle_lag17<- lag(sp_500_1$sma_50_candle,n=17)
# # sp_500_1$sma_100_candle_lag17 <- lag(sp_500_1$sma_100_candle,n=17)
# # sp_500_1$sma_200_candle_lag17<- lag(sp_500_1$sma_200_candle,n=17)
# 
# sp_500_1$hma_9_candle_lag18 <- lag(sp_500_1$hma_9_candle,n=18)
# sp_500_1$hma_25_candle_lag18 <- lag(sp_500_1$hma_25_candle,n=18)
# sp_500_1$hma_49_candle_lag18 <- lag(sp_500_1$hma_49_candle,n=18)
# sp_500_1$hma_81_candle_lag18 <- lag(sp_500_1$hma_81_candle,n=18)
# sp_500_1$hma_200_candle_lag18 <- lag(sp_500_1$hma_200_candle,n=18)
# sp_500_1$sma_50_candle_lag18<- lag(sp_500_1$sma_50_candle,n=18)
# sp_500_1$sma_100_candle_lag18 <- lag(sp_500_1$sma_100_candle,n=18)
# sp_500_1$sma_200_candle_lag18<- lag(sp_500_1$sma_200_candle,n=18)
# 
# # sp_500_1$hma_9_candle_lag19 <- lag(sp_500_1$hma_9_candle,n=19)
# # sp_500_1$hma_25_candle_lag19 <- lag(sp_500_1$hma_25_candle,n=19)
# # sp_500_1$hma_49_candle_lag19 <- lag(sp_500_1$hma_49_candle,n=19)
# # sp_500_1$hma_81_candle_lag19 <- lag(sp_500_1$hma_81_candle,n=19)
# # sp_500_1$hma_200_candle_lag19 <- lag(sp_500_1$hma_200_candle,n=19)
# # sp_500_1$sma_50_candle_lag19<- lag(sp_500_1$sma_50_candle,n=19)
# # sp_500_1$sma_100_candle_lag19 <- lag(sp_500_1$sma_100_candle,n=19)
# # sp_500_1$sma_200_candle_lag19<- lag(sp_500_1$sma_200_candle,n=19)
# 
# sp_500_1$hma_9_candle_lag20 <- lag(sp_500_1$hma_9_candle,n=20)
# sp_500_1$hma_25_candle_lag20 <- lag(sp_500_1$hma_25_candle,n=20)
# sp_500_1$hma_49_candle_lag20 <- lag(sp_500_1$hma_49_candle,n=20)
# sp_500_1$hma_81_candle_lag20 <- lag(sp_500_1$hma_81_candle,n=20)
# sp_500_1$hma_200_candle_lag20 <- lag(sp_500_1$hma_200_candle,n=20)
# sp_500_1$sma_50_candle_lag20<- lag(sp_500_1$sma_50_candle,n=20)
# sp_500_1$sma_100_candle_lag20 <- lag(sp_500_1$sma_100_candle,n=20)
# sp_500_1$sma_200_candle_lag20<- lag(sp_500_1$sma_200_candle,n=20)

sp_500_1$hma_9_lag_1 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 1)
sp_500_1$hma_9_lag_2 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 2)
sp_500_1$hma_9_lag_3 <- sp_500_1$hma_9 /lag(sp_500_1$hma_9, n = 3)
sp_500_1$hma_9_lag_4 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 4)
# #sp_500_1$hma_9_lag_5 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 5)
# sp_500_1$hma_9_lag_6 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 6)
# #sp_500_1$hma_9_lag_7 <- sp_500_1$hma_9 /lag(sp_500_1$hma_9, n = 7)
# sp_500_1$hma_9_lag_8 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 8)
# #sp_500_1$hma_9_lag_9 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 9)
# sp_500_1$hma_9_lag_10 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 10)
# #sp_500_1$hma_9_lag_11<- sp_500_1$hma_9 /lag(sp_500_1$hma_9, n = 11)
# sp_500_1$hma_9_lag_12 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 12)
# #sp_500_1$hma_9_lag_13 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 13)
# sp_500_1$hma_9_lag_14 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 14)
# #sp_500_1$hma_9_lag_15 <- sp_500_1$hma_9 /lag(sp_500_1$hma_9, n = 15)
# sp_500_1$hma_9_lag_16 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 16)
# #sp_500_1$hma_9_lag_17 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 17)
# sp_500_1$hma_9_lag_18 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 18)
# #sp_500_1$hma_9_lag_19 <- sp_500_1$hma_9 /lag(sp_500_1$hma_9, n = 19)
# sp_500_1$hma_9_lag_20 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 20)
# 


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

# sp_500_1$hma_9co200_lag5<- lag(sp_500_1$hma_9co200,n=5)
# sp_500_1$hma_25co200_lag5<- lag(sp_500_1$hma_25co200,n=5)
# sp_500_1$hma_49co200_lag5<- lag(sp_500_1$hma_49co200,n=5)
# sp_500_1$hma_81co200_lag5<- lag(sp_500_1$hma_81co200,n=5)
# sp_500_1$hma_9co_sma50_lag5<- lag(sp_500_1$hma_9co_sma50,n=5)
# sp_500_1$hma_25co_sma50_lag5<- lag(sp_500_1$hma_25co_sma50,n=5)
# sp_500_1$hma_49co_sma50_lag5<- lag(sp_500_1$hma_49co_sma50,n=5)
# sp_500_1$hma_81co_sma50_lag5<- lag(sp_500_1$hma_81co_sma50,n=5)
# sp_500_1$hma_9co_sma100_lag5<- lag(sp_500_1$hma_9co_sma100,n=5)
# sp_500_1$hma_25co_sma100_lag5<- lag(sp_500_1$hma_25co_sma100,n=5)
# sp_500_1$hma_49co_sma100_lag5<- lag(sp_500_1$hma_49co_sma100,n=5)
# sp_500_1$hma_81co_sma100_lag5<- lag(sp_500_1$hma_81co_sma100,n=5)
# sp_500_1$hma_9co_sma200_lag5<- lag(sp_500_1$hma_9co_sma200,n=5)
# sp_500_1$hma_25co_sma200_lag5<- lag(sp_500_1$hma_25co_sma200,n=5)
# sp_500_1$hma_49co_sma200_lag5<- lag(sp_500_1$hma_49co_sma200,n=5)
# sp_500_1$hma_81co_sma200_lag5<- lag(sp_500_1$hma_81co_sma200,n=5)
# 
# sp_500_1$hma_9co200_lag6<- lag(sp_500_1$hma_9co200,n=6)
# sp_500_1$hma_25co200_lag6<- lag(sp_500_1$hma_25co200,n=6)
# sp_500_1$hma_49co200_lag6<- lag(sp_500_1$hma_49co200,n=6)
# sp_500_1$hma_81co200_lag6<- lag(sp_500_1$hma_81co200,n=6)
# sp_500_1$hma_9co_sma50_lag6<- lag(sp_500_1$hma_9co_sma50,n=6)
# sp_500_1$hma_25co_sma50_lag6<- lag(sp_500_1$hma_25co_sma50,n=6)
# sp_500_1$hma_49co_sma50_lag6<- lag(sp_500_1$hma_49co_sma50,n=6)
# sp_500_1$hma_81co_sma50_lag6<- lag(sp_500_1$hma_81co_sma50,n=6)
# sp_500_1$hma_9co_sma100_lag6<- lag(sp_500_1$hma_9co_sma100,n=6)
# sp_500_1$hma_25co_sma100_lag6<- lag(sp_500_1$hma_25co_sma100,n=6)
# sp_500_1$hma_49co_sma100_lag6<- lag(sp_500_1$hma_49co_sma100,n=6)
# sp_500_1$hma_81co_sma100_lag6<- lag(sp_500_1$hma_81co_sma100,n=6)
# sp_500_1$hma_9co_sma200_lag6<- lag(sp_500_1$hma_9co_sma200,n=6)
# sp_500_1$hma_25co_sma200_lag6<- lag(sp_500_1$hma_25co_sma200,n=6)
# sp_500_1$hma_49co_sma200_lag6<- lag(sp_500_1$hma_49co_sma200,n=6)
# sp_500_1$hma_81co_sma200_lag6<- lag(sp_500_1$hma_81co_sma200,n=6)
# 
# # sp_500_1$hma_9co200_lag7<- lag(sp_500_1$hma_9co200,n=7)
# # sp_500_1$hma_25co200_lag7<- lag(sp_500_1$hma_25co200,n=7)
# # sp_500_1$hma_49co200_lag7<- lag(sp_500_1$hma_49co200,n=7)
# # sp_500_1$hma_81co200_lag7<- lag(sp_500_1$hma_81co200,n=7)
# # sp_500_1$hma_9co_sma50_lag7<- lag(sp_500_1$hma_9co_sma50,n=7)
# # sp_500_1$hma_25co_sma50_lag7<- lag(sp_500_1$hma_25co_sma50,n=7)
# # sp_500_1$hma_49co_sma50_lag7<- lag(sp_500_1$hma_49co_sma50,n=7)
# # sp_500_1$hma_81co_sma50_lag7<- lag(sp_500_1$hma_81co_sma50,n=7)
# # sp_500_1$hma_9co_sma100_lag7<- lag(sp_500_1$hma_9co_sma100,n=7)
# # sp_500_1$hma_25co_sma100_lag7<- lag(sp_500_1$hma_25co_sma100,n=7)
# # sp_500_1$hma_49co_sma100_lag7<- lag(sp_500_1$hma_49co_sma100,n=7)
# # sp_500_1$hma_81co_sma100_lag7<- lag(sp_500_1$hma_81co_sma100,n=7)
# # sp_500_1$hma_9co_sma200_lag7<- lag(sp_500_1$hma_9co_sma200,n=7)
# # sp_500_1$hma_25co_sma200_lag7<- lag(sp_500_1$hma_25co_sma200,n=7)
# # sp_500_1$hma_49co_sma200_lag7<- lag(sp_500_1$hma_49co_sma200,n=7)
# # sp_500_1$hma_81co_sma200_lag7<- lag(sp_500_1$hma_81co_sma200,n=7)
# 
# sp_500_1$hma_9co200_lag8<- lag(sp_500_1$hma_9co200,n=8)
# sp_500_1$hma_25co200_lag8<- lag(sp_500_1$hma_25co200,n=8)
# sp_500_1$hma_49co200_lag8<- lag(sp_500_1$hma_49co200,n=8)
# sp_500_1$hma_81co200_lag8<- lag(sp_500_1$hma_81co200,n=8)
# sp_500_1$hma_9co_sma50_lag8<- lag(sp_500_1$hma_9co_sma50,n=8)
# sp_500_1$hma_25co_sma50_lag8<- lag(sp_500_1$hma_25co_sma50,n=8)
# sp_500_1$hma_49co_sma50_lag8<- lag(sp_500_1$hma_49co_sma50,n=8)
# sp_500_1$hma_81co_sma50_lag8<- lag(sp_500_1$hma_81co_sma50,n=8)
# sp_500_1$hma_9co_sma100_lag8<- lag(sp_500_1$hma_9co_sma100,n=8)
# sp_500_1$hma_25co_sma100_lag8<- lag(sp_500_1$hma_25co_sma100,n=8)
# sp_500_1$hma_49co_sma100_lag8<- lag(sp_500_1$hma_49co_sma100,n=8)
# sp_500_1$hma_81co_sma100_lag8<- lag(sp_500_1$hma_81co_sma100,n=8)
# sp_500_1$hma_9co_sma200_lag8<- lag(sp_500_1$hma_9co_sma200,n=8)
# sp_500_1$hma_25co_sma200_lag8<- lag(sp_500_1$hma_25co_sma200,n=8)
# sp_500_1$hma_49co_sma200_lag8<- lag(sp_500_1$hma_49co_sma200,n=8)
# sp_500_1$hma_81co_sma200_lag8<- lag(sp_500_1$hma_81co_sma200,n=8)
# 
# # sp_500_1$hma_9co200_lag9<- lag(sp_500_1$hma_9co200,n=9)
# # sp_500_1$hma_25co200_lag9<- lag(sp_500_1$hma_25co200,n=9)
# # sp_500_1$hma_49co200_lag9<- lag(sp_500_1$hma_49co200,n=9)
# # sp_500_1$hma_81co200_lag9<- lag(sp_500_1$hma_81co200,n=9)
# # sp_500_1$hma_9co_sma50_lag9<- lag(sp_500_1$hma_9co_sma50,n=9)
# # sp_500_1$hma_25co_sma50_lag9<- lag(sp_500_1$hma_25co_sma50,n=9)
# # sp_500_1$hma_49co_sma50_lag9<- lag(sp_500_1$hma_49co_sma50,n=9)
# # sp_500_1$hma_81co_sma50_lag9<- lag(sp_500_1$hma_81co_sma50,n=9)
# # sp_500_1$hma_9co_sma100_lag9<- lag(sp_500_1$hma_9co_sma100,n=9)
# # sp_500_1$hma_25co_sma100_lag9<- lag(sp_500_1$hma_25co_sma100,n=9)
# # sp_500_1$hma_49co_sma100_lag9<- lag(sp_500_1$hma_49co_sma100,n=9)
# # sp_500_1$hma_81co_sma100_lag9<- lag(sp_500_1$hma_81co_sma100,n=9)
# # sp_500_1$hma_9co_sma200_lag9<- lag(sp_500_1$hma_9co_sma200,n=9)
# # sp_500_1$hma_25co_sma200_lag9<- lag(sp_500_1$hma_25co_sma200,n=9)
# # sp_500_1$hma_49co_sma200_lag9<- lag(sp_500_1$hma_49co_sma200,n=9)
# # sp_500_1$hma_81co_sma200_lag9<- lag(sp_500_1$hma_81co_sma200,n=9)
# 
# sp_500_1$hma_9co200_lag10<- lag(sp_500_1$hma_9co200,n=10)
# sp_500_1$hma_25co200_lag10<- lag(sp_500_1$hma_25co200,n=10)
# sp_500_1$hma_49co200_lag10<- lag(sp_500_1$hma_49co200,n=10)
# sp_500_1$hma_81co200_lag10<- lag(sp_500_1$hma_81co200,n=10)
# sp_500_1$hma_9co_sma50_lag10<- lag(sp_500_1$hma_9co_sma50,n=10)
# sp_500_1$hma_25co_sma50_lag10<- lag(sp_500_1$hma_25co_sma50,n=10)
# sp_500_1$hma_49co_sma50_lag10<- lag(sp_500_1$hma_49co_sma50,n=10)
# sp_500_1$hma_81co_sma50_lag10<- lag(sp_500_1$hma_81co_sma50,n=10)
# sp_500_1$hma_9co_sma100_lag10<- lag(sp_500_1$hma_9co_sma100,n=10)
# sp_500_1$hma_25co_sma100_lag10<- lag(sp_500_1$hma_25co_sma100,n=10)
# sp_500_1$hma_49co_sma100_lag10<- lag(sp_500_1$hma_49co_sma100,n=10)
# sp_500_1$hma_81co_sma100_lag10<- lag(sp_500_1$hma_81co_sma100,n=10)
# sp_500_1$hma_9co_sma200_lag10<- lag(sp_500_1$hma_9co_sma200,n=10)
# sp_500_1$hma_25co_sma200_lag10<- lag(sp_500_1$hma_25co_sma200,n=10)
# sp_500_1$hma_49co_sma200_lag10<- lag(sp_500_1$hma_49co_sma200,n=10)
# sp_500_1$hma_81co_sma200_lag10<- lag(sp_500_1$hma_81co_sma200,n=10)
# 
# # sp_500_1$hma_9co200_lag11<- lag(sp_500_1$hma_9co200,n=11)
# # sp_500_1$hma_25co200_lag11<- lag(sp_500_1$hma_25co200,n=11)
# # sp_500_1$hma_49co200_lag11<- lag(sp_500_1$hma_49co200,n=11)
# # sp_500_1$hma_81co200_lag11<- lag(sp_500_1$hma_81co200,n=11)
# # sp_500_1$hma_9co_sma50_lag11<- lag(sp_500_1$hma_9co_sma50,n=11)
# # sp_500_1$hma_25co_sma50_lag11<- lag(sp_500_1$hma_25co_sma50,n=11)
# # sp_500_1$hma_49co_sma50_lag11<- lag(sp_500_1$hma_49co_sma50,n=11)
# # sp_500_1$hma_81co_sma50_lag11<- lag(sp_500_1$hma_81co_sma50,n=11)
# # sp_500_1$hma_9co_sma100_lag11<- lag(sp_500_1$hma_9co_sma100,n=11)
# # sp_500_1$hma_25co_sma100_lag11<- lag(sp_500_1$hma_25co_sma100,n=11)
# # sp_500_1$hma_49co_sma100_lag11<- lag(sp_500_1$hma_49co_sma100,n=11)
# # sp_500_1$hma_81co_sma100_lag11<- lag(sp_500_1$hma_81co_sma100,n=11)
# # sp_500_1$hma_9co_sma200_lag11<- lag(sp_500_1$hma_9co_sma200,n=11)
# # sp_500_1$hma_25co_sma200_lag11<- lag(sp_500_1$hma_25co_sma200,n=11)
# # sp_500_1$hma_49co_sma200_lag11<- lag(sp_500_1$hma_49co_sma200,n=11)
# # sp_500_1$hma_81co_sma200_lag11<- lag(sp_500_1$hma_81co_sma200,n=11)
# 
# sp_500_1$hma_9co200_lag12<- lag(sp_500_1$hma_9co200,n=12)
# sp_500_1$hma_25co200_lag12<- lag(sp_500_1$hma_25co200,n=12)
# sp_500_1$hma_49co200_lag12<- lag(sp_500_1$hma_49co200,n=12)
# sp_500_1$hma_81co200_lag12<- lag(sp_500_1$hma_81co200,n=12)
# sp_500_1$hma_9co_sma50_lag12<- lag(sp_500_1$hma_9co_sma50,n=12)
# sp_500_1$hma_25co_sma50_lag12<- lag(sp_500_1$hma_25co_sma50,n=12)
# sp_500_1$hma_49co_sma50_lag12<- lag(sp_500_1$hma_49co_sma50,n=12)
# sp_500_1$hma_81co_sma50_lag12<- lag(sp_500_1$hma_81co_sma50,n=12)
# sp_500_1$hma_9co_sma100_lag12<- lag(sp_500_1$hma_9co_sma100,n=12)
# sp_500_1$hma_25co_sma100_lag12<- lag(sp_500_1$hma_25co_sma100,n=12)
# sp_500_1$hma_49co_sma100_lag12<- lag(sp_500_1$hma_49co_sma100,n=12)
# sp_500_1$hma_81co_sma100_lag12<- lag(sp_500_1$hma_81co_sma100,n=12)
# sp_500_1$hma_9co_sma200_lag12<- lag(sp_500_1$hma_9co_sma200,n=12)
# sp_500_1$hma_25co_sma200_lag12<- lag(sp_500_1$hma_25co_sma200,n=12)
# sp_500_1$hma_49co_sma200_lag12<- lag(sp_500_1$hma_49co_sma200,n=12)
# sp_500_1$hma_81co_sma200_lag12<- lag(sp_500_1$hma_81co_sma200,n=12)
# 
# # sp_500_1$hma_9co200_lag13<- lag(sp_500_1$hma_9co200,n=13)
# # sp_500_1$hma_25co200_lag13<- lag(sp_500_1$hma_25co200,n=13)
# # sp_500_1$hma_49co200_lag13<- lag(sp_500_1$hma_49co200,n=13)
# # sp_500_1$hma_81co200_lag13<- lag(sp_500_1$hma_81co200,n=13)
# # sp_500_1$hma_9co_sma50_lag13<- lag(sp_500_1$hma_9co_sma50,n=13)
# # sp_500_1$hma_25co_sma50_lag13<- lag(sp_500_1$hma_25co_sma50,n=13)
# # sp_500_1$hma_49co_sma50_lag13<- lag(sp_500_1$hma_49co_sma50,n=13)
# # sp_500_1$hma_81co_sma50_lag13<- lag(sp_500_1$hma_81co_sma50,n=13)
# # sp_500_1$hma_9co_sma100_lag13<- lag(sp_500_1$hma_9co_sma100,n=13)
# # sp_500_1$hma_25co_sma100_lag13<- lag(sp_500_1$hma_25co_sma100,n=13)
# # sp_500_1$hma_49co_sma100_lag13<- lag(sp_500_1$hma_49co_sma100,n=13)
# # sp_500_1$hma_81co_sma100_lag13<- lag(sp_500_1$hma_81co_sma100,n=13)
# # sp_500_1$hma_9co_sma200_lag13<- lag(sp_500_1$hma_9co_sma200,n=13)
# # sp_500_1$hma_25co_sma200_lag13<- lag(sp_500_1$hma_25co_sma200,n=13)
# # sp_500_1$hma_49co_sma200_lag13<- lag(sp_500_1$hma_49co_sma200,n=13)
# # sp_500_1$hma_81co_sma200_lag13<- lag(sp_500_1$hma_81co_sma200,n=13)
# 
# sp_500_1$hma_9co200_lag14<- lag(sp_500_1$hma_9co200,n=14)
# sp_500_1$hma_25co200_lag14<- lag(sp_500_1$hma_25co200,n=14)
# sp_500_1$hma_49co200_lag14<- lag(sp_500_1$hma_49co200,n=14)
# sp_500_1$hma_81co200_lag14<- lag(sp_500_1$hma_81co200,n=14)
# sp_500_1$hma_9co_sma50_lag14<- lag(sp_500_1$hma_9co_sma50,n=14)
# sp_500_1$hma_25co_sma50_lag14<- lag(sp_500_1$hma_25co_sma50,n=14)
# sp_500_1$hma_49co_sma50_lag14<- lag(sp_500_1$hma_49co_sma50,n=14)
# sp_500_1$hma_81co_sma50_lag14<- lag(sp_500_1$hma_81co_sma50,n=14)
# sp_500_1$hma_9co_sma100_lag14<- lag(sp_500_1$hma_9co_sma100,n=14)
# sp_500_1$hma_25co_sma100_lag14<- lag(sp_500_1$hma_25co_sma100,n=14)
# sp_500_1$hma_49co_sma100_lag14<- lag(sp_500_1$hma_49co_sma100,n=14)
# sp_500_1$hma_81co_sma100_lag14<- lag(sp_500_1$hma_81co_sma100,n=14)
# sp_500_1$hma_9co_sma200_lag14<- lag(sp_500_1$hma_9co_sma200,n=14)
# sp_500_1$hma_25co_sma200_lag14<- lag(sp_500_1$hma_25co_sma200,n=14)
# sp_500_1$hma_49co_sma200_lag14<- lag(sp_500_1$hma_49co_sma200,n=14)
# sp_500_1$hma_81co_sma200_lag14<- lag(sp_500_1$hma_81co_sma200,n=14)
# 
# # sp_500_1$hma_9co200_lag15<- lag(sp_500_1$hma_9co200,n=15)
# # sp_500_1$hma_25co200_lag15<- lag(sp_500_1$hma_25co200,n=15)
# # sp_500_1$hma_49co200_lag15<- lag(sp_500_1$hma_49co200,n=15)
# # sp_500_1$hma_81co200_lag15<- lag(sp_500_1$hma_81co200,n=15)
# # sp_500_1$hma_9co_sma50_lag15<- lag(sp_500_1$hma_9co_sma50,n=15)
# # sp_500_1$hma_25co_sma50_lag15<- lag(sp_500_1$hma_25co_sma50,n=15)
# # sp_500_1$hma_49co_sma50_lag15<- lag(sp_500_1$hma_49co_sma50,n=15)
# # sp_500_1$hma_81co_sma50_lag15<- lag(sp_500_1$hma_81co_sma50,n=15)
# # sp_500_1$hma_9co_sma100_lag15<- lag(sp_500_1$hma_9co_sma100,n=15)
# # sp_500_1$hma_25co_sma100_lag15<- lag(sp_500_1$hma_25co_sma100,n=15)
# # sp_500_1$hma_49co_sma100_lag15<- lag(sp_500_1$hma_49co_sma100,n=15)
# # sp_500_1$hma_81co_sma100_lag15<- lag(sp_500_1$hma_81co_sma100,n=15)
# # sp_500_1$hma_9co_sma200_lag15<- lag(sp_500_1$hma_9co_sma200,n=15)
# # sp_500_1$hma_25co_sma200_lag15<- lag(sp_500_1$hma_25co_sma200,n=15)
# # sp_500_1$hma_49co_sma200_lag15<- lag(sp_500_1$hma_49co_sma200,n=15)
# # sp_500_1$hma_81co_sma200_lag15<- lag(sp_500_1$hma_81co_sma200,n=15)
# 
# sp_500_1$hma_9co200_lag16<- lag(sp_500_1$hma_9co200,n=16)
# sp_500_1$hma_25co200_lag16<- lag(sp_500_1$hma_25co200,n=16)
# sp_500_1$hma_49co200_lag16<- lag(sp_500_1$hma_49co200,n=16)
# sp_500_1$hma_81co200_lag16<- lag(sp_500_1$hma_81co200,n=16)
# sp_500_1$hma_9co_sma50_lag16<- lag(sp_500_1$hma_9co_sma50,n=16)
# sp_500_1$hma_25co_sma50_lag16<- lag(sp_500_1$hma_25co_sma50,n=16)
# sp_500_1$hma_49co_sma50_lag16<- lag(sp_500_1$hma_49co_sma50,n=16)
# sp_500_1$hma_81co_sma50_lag16<- lag(sp_500_1$hma_81co_sma50,n=16)
# sp_500_1$hma_9co_sma100_lag16<- lag(sp_500_1$hma_9co_sma100,n=16)
# sp_500_1$hma_25co_sma100_lag16<- lag(sp_500_1$hma_25co_sma100,n=16)
# sp_500_1$hma_49co_sma100_lag16<- lag(sp_500_1$hma_49co_sma100,n=16)
# sp_500_1$hma_81co_sma100_lag16<- lag(sp_500_1$hma_81co_sma100,n=16)
# sp_500_1$hma_9co_sma200_lag16<- lag(sp_500_1$hma_9co_sma200,n=16)
# sp_500_1$hma_25co_sma200_lag16<- lag(sp_500_1$hma_25co_sma200,n=16)
# sp_500_1$hma_49co_sma200_lag16<- lag(sp_500_1$hma_49co_sma200,n=16)
# sp_500_1$hma_81co_sma200_lag16<- lag(sp_500_1$hma_81co_sma200,n=16)
# 
# # sp_500_1$hma_9co200_lag17<- lag(sp_500_1$hma_9co200,n=17)
# # sp_500_1$hma_25co200_lag17<- lag(sp_500_1$hma_25co200,n=17)
# # sp_500_1$hma_49co200_lag17<- lag(sp_500_1$hma_49co200,n=17)
# # sp_500_1$hma_81co200_lag17<- lag(sp_500_1$hma_81co200,n=17)
# # sp_500_1$hma_9co_sma50_lag17<- lag(sp_500_1$hma_9co_sma50,n=17)
# # sp_500_1$hma_25co_sma50_lag17<- lag(sp_500_1$hma_25co_sma50,n=17)
# # sp_500_1$hma_49co_sma50_lag17<- lag(sp_500_1$hma_49co_sma50,n=17)
# # sp_500_1$hma_81co_sma50_lag17<- lag(sp_500_1$hma_81co_sma50,n=17)
# # sp_500_1$hma_9co_sma100_lag17<- lag(sp_500_1$hma_9co_sma100,n=17)
# # sp_500_1$hma_25co_sma100_lag17<- lag(sp_500_1$hma_25co_sma100,n=17)
# # sp_500_1$hma_49co_sma100_lag17<- lag(sp_500_1$hma_49co_sma100,n=17)
# # sp_500_1$hma_81co_sma100_lag17<- lag(sp_500_1$hma_81co_sma100,n=17)
# # sp_500_1$hma_9co_sma200_lag17<- lag(sp_500_1$hma_9co_sma200,n=17)
# # sp_500_1$hma_25co_sma200_lag17<- lag(sp_500_1$hma_25co_sma200,n=17)
# # sp_500_1$hma_49co_sma200_lag17<- lag(sp_500_1$hma_49co_sma200,n=17)
# # sp_500_1$hma_81co_sma200_lag17<- lag(sp_500_1$hma_81co_sma200,n=17)
# 
# sp_500_1$hma_9co200_lag18<- lag(sp_500_1$hma_9co200,n=18)
# sp_500_1$hma_25co200_lag18<- lag(sp_500_1$hma_25co200,n=18)
# sp_500_1$hma_49co200_lag18<- lag(sp_500_1$hma_49co200,n=18)
# sp_500_1$hma_81co200_lag18<- lag(sp_500_1$hma_81co200,n=18)
# sp_500_1$hma_9co_sma50_lag18<- lag(sp_500_1$hma_9co_sma50,n=18)
# sp_500_1$hma_25co_sma50_lag18<- lag(sp_500_1$hma_25co_sma50,n=18)
# sp_500_1$hma_49co_sma50_lag18<- lag(sp_500_1$hma_49co_sma50,n=18)
# sp_500_1$hma_81co_sma50_lag18<- lag(sp_500_1$hma_81co_sma50,n=18)
# sp_500_1$hma_9co_sma100_lag18<- lag(sp_500_1$hma_9co_sma100,n=18)
# sp_500_1$hma_25co_sma100_lag18<- lag(sp_500_1$hma_25co_sma100,n=18)
# sp_500_1$hma_49co_sma100_lag18<- lag(sp_500_1$hma_49co_sma100,n=18)
# sp_500_1$hma_81co_sma100_lag18<- lag(sp_500_1$hma_81co_sma100,n=18)
# sp_500_1$hma_9co_sma200_lag18<- lag(sp_500_1$hma_9co_sma200,n=18)
# sp_500_1$hma_25co_sma200_lag18<- lag(sp_500_1$hma_25co_sma200,n=18)
# sp_500_1$hma_49co_sma200_lag18<- lag(sp_500_1$hma_49co_sma200,n=18)
# sp_500_1$hma_81co_sma200_lag18<- lag(sp_500_1$hma_81co_sma200,n=18)
# 
# # sp_500_1$hma_9co200_lag19<- lag(sp_500_1$hma_9co200,n=19)
# # sp_500_1$hma_25co200_lag19<- lag(sp_500_1$hma_25co200,n=19)
# # sp_500_1$hma_49co200_lag19<- lag(sp_500_1$hma_49co200,n=19)
# # sp_500_1$hma_81co200_lag19<- lag(sp_500_1$hma_81co200,n=19)
# # sp_500_1$hma_9co_sma50_lag19<- lag(sp_500_1$hma_9co_sma50,n=19)
# # sp_500_1$hma_25co_sma50_lag19<- lag(sp_500_1$hma_25co_sma50,n=19)
# # sp_500_1$hma_49co_sma50_lag19<- lag(sp_500_1$hma_49co_sma50,n=19)
# # sp_500_1$hma_81co_sma50_lag19<- lag(sp_500_1$hma_81co_sma50,n=19)
# # sp_500_1$hma_9co_sma100_lag19<- lag(sp_500_1$hma_9co_sma100,n=19)
# # sp_500_1$hma_25co_sma100_lag19<- lag(sp_500_1$hma_25co_sma100,n=19)
# # sp_500_1$hma_49co_sma100_lag19<- lag(sp_500_1$hma_49co_sma100,n=19)
# # sp_500_1$hma_81co_sma100_lag19<- lag(sp_500_1$hma_81co_sma100,n=19)
# # sp_500_1$hma_9co_sma200_lag19<- lag(sp_500_1$hma_9co_sma200,n=19)
# # sp_500_1$hma_25co_sma200_lag19<- lag(sp_500_1$hma_25co_sma200,n=19)
# # sp_500_1$hma_49co_sma200_lag19<- lag(sp_500_1$hma_49co_sma200,n=19)
# # sp_500_1$hma_81co_sma200_lag19<- lag(sp_500_1$hma_81co_sma200,n=19)
# 
# sp_500_1$hma_9co200_lag20<- lag(sp_500_1$hma_9co200,n=20)
# sp_500_1$hma_25co200_lag20<- lag(sp_500_1$hma_25co200,n=20)
# sp_500_1$hma_49co200_lag20<- lag(sp_500_1$hma_49co200,n=20)
# sp_500_1$hma_81co200_lag20<- lag(sp_500_1$hma_81co200,n=20)
# sp_500_1$hma_9co_sma50_lag20<- lag(sp_500_1$hma_9co_sma50,n=20)
# sp_500_1$hma_25co_sma50_lag20<- lag(sp_500_1$hma_25co_sma50,n=20)
# sp_500_1$hma_49co_sma50_lag20<- lag(sp_500_1$hma_49co_sma50,n=20)
# sp_500_1$hma_81co_sma50_lag20<- lag(sp_500_1$hma_81co_sma50,n=20)
# sp_500_1$hma_9co_sma100_lag20<- lag(sp_500_1$hma_9co_sma100,n=20)
# sp_500_1$hma_25co_sma100_lag20<- lag(sp_500_1$hma_25co_sma100,n=20)
# sp_500_1$hma_49co_sma100_lag20<- lag(sp_500_1$hma_49co_sma100,n=20)
# sp_500_1$hma_81co_sma100_lag20<- lag(sp_500_1$hma_81co_sma100,n=20)
# sp_500_1$hma_9co_sma200_lag20<- lag(sp_500_1$hma_9co_sma200,n=20)
# sp_500_1$hma_25co_sma200_lag20<- lag(sp_500_1$hma_25co_sma200,n=20)
# sp_500_1$hma_49co_sma200_lag20<- lag(sp_500_1$hma_49co_sma200,n=20)
# sp_500_1$hma_81co_sma200_lag20<- lag(sp_500_1$hma_81co_sma200,n=20)
# 

sp_500_1$hma_9co25_lag1 <- lag(sp_500_1$hma_9co25)
sp_500_1$hma_9co25_lag2 <- lag(sp_500_1$hma_9co25,n=2)
sp_500_1$hma_9co25_lag3 <- lag(sp_500_1$hma_9co25,n=3)
sp_500_1$hma_9co25_lag4 <- lag(sp_500_1$hma_9co25,n=4)
# #sp_500_1$hma_9co25_lag5 <- lag(sp_500_1$hma_9co25,n=5)
# sp_500_1$hma_9co25_lag6 <- lag(sp_500_1$hma_9co25, n=6)
# #sp_500_1$hma_9co25_lag7 <- lag(sp_500_1$hma_9co25,n=7)
# sp_500_1$hma_9co25_lag8 <- lag(sp_500_1$hma_9co25,n=8)
# #sp_500_1$hma_9co25_lag9 <- lag(sp_500_1$hma_9co25,n=9)
# sp_500_1$hma_9co25_lag10 <- lag(sp_500_1$hma_9co25,n=10)
# #sp_500_1$hma_9co25_lag11 <- lag(sp_500_1$hma_9co25, n=11)
# sp_500_1$hma_9co25_lag12 <- lag(sp_500_1$hma_9co25,n=12)
# #sp_500_1$hma_9co25_lag13 <- lag(sp_500_1$hma_9co25,n=13)
# sp_500_1$hma_9co25_lag14 <- lag(sp_500_1$hma_9co25,n=14)
# #sp_500_1$hma_9co25_lag15 <- lag(sp_500_1$hma_9co25,n=15)
# sp_500_1$hma_9co25_lag16 <- lag(sp_500_1$hma_9co25, n=16)
# #sp_500_1$hma_9co25_lag17 <- lag(sp_500_1$hma_9co25,n=17)
# sp_500_1$hma_9co25_lag18 <- lag(sp_500_1$hma_9co25,n=18)
# #sp_500_1$hma_9co25_lag19 <- lag(sp_500_1$hma_9co25,n=19)
# sp_500_1$hma_9co25_lag20 <- lag(sp_500_1$hma_9co25,n=20)


sp_500_1$hma_9co49_lag1 <- lag(sp_500_1$hma_9co49)
sp_500_1$hma_9co49_lag2 <- lag(sp_500_1$hma_9co49,n=2)
sp_500_1$hma_9co49_lag3 <- lag(sp_500_1$hma_9co49,n=3)
sp_500_1$hma_9co49_lag4 <- lag(sp_500_1$hma_9co49,n=4)
# #sp_500_1$hma_9co49_lag5 <- lag(sp_500_1$hma_9co49,n=5)
# sp_500_1$hma_9co49_lag6 <- lag(sp_500_1$hma_9co49, n=6)
# #sp_500_1$hma_9co49_lag7 <- lag(sp_500_1$hma_9co49,n=7)
# sp_500_1$hma_9co49_lag8 <- lag(sp_500_1$hma_9co49,n=8)
# #sp_500_1$hma_9co49_lag9 <- lag(sp_500_1$hma_9co49,n=9)
# sp_500_1$hma_9co49_lag10 <- lag(sp_500_1$hma_9co49,n=10)
# #sp_500_1$hma_9co49_lag11 <- lag(sp_500_1$hma_9co49,n=11)
# sp_500_1$hma_9co49_lag12 <- lag(sp_500_1$hma_9co49,n=12)
# #sp_500_1$hma_9co49_lag13 <- lag(sp_500_1$hma_9co49,n=13)
# sp_500_1$hma_9co49_lag14 <- lag(sp_500_1$hma_9co49,n=14)
# #sp_500_1$hma_9co49_lag15 <- lag(sp_500_1$hma_9co49,n=15)
# sp_500_1$hma_9co49_lag16 <- lag(sp_500_1$hma_9co49, n=16)
# #sp_500_1$hma_9co49_lag17 <- lag(sp_500_1$hma_9co49,n=17)
# sp_500_1$hma_9co49_lag18 <- lag(sp_500_1$hma_9co49,n=18)
# #sp_500_1$hma_9co49_lag19 <- lag(sp_500_1$hma_9co49,n=19)
# sp_500_1$hma_9co49_lag20 <- lag(sp_500_1$hma_9co49,n=20)
# 

sp_500_1$hma_9co81_lag1 <- lag(sp_500_1$hma_9co81)
sp_500_1$hma_9co81_lag2 <- lag(sp_500_1$hma_9co81,n=2)
sp_500_1$hma_9co81_lag3 <- lag(sp_500_1$hma_9co81,n=3)
sp_500_1$hma_9co81_lag4 <- lag(sp_500_1$hma_9co81,n=4)
# #sp_500_1$hma_9co81_lag5 <- lag(sp_500_1$hma_9co81,n=5)
# sp_500_1$hma_9co81_lag6 <- lag(sp_500_1$hma_9co81, n=6)
# #sp_500_1$hma_9co81_lag7 <- lag(sp_500_1$hma_9co81,n=7)
# sp_500_1$hma_9co81_lag8 <- lag(sp_500_1$hma_9co81,n=8)
# #sp_500_1$hma_9co81_lag9 <- lag(sp_500_1$hma_9co81,n=9)
# sp_500_1$hma_9co81_lag10 <- lag(sp_500_1$hma_9co81,n=10)
# #sp_500_1$hma_9co81_lag11 <- lag(sp_500_1$hma_9co81, n=11)
# sp_500_1$hma_9co81_lag12 <- lag(sp_500_1$hma_9co81,n=12)
# #sp_500_1$hma_9co81_lag13 <- lag(sp_500_1$hma_9co81,n=13)
# sp_500_1$hma_9co81_lag14 <- lag(sp_500_1$hma_9co81,n=14)
# #sp_500_1$hma_9co81_lag15 <- lag(sp_500_1$hma_9co81,n=15)
# sp_500_1$hma_9co81_lag16 <- lag(sp_500_1$hma_9co81, n=16)
# #sp_500_1$hma_9co81_lag17 <- lag(sp_500_1$hma_9co81,n=17)
# sp_500_1$hma_9co81_lag18 <- lag(sp_500_1$hma_9co81,n=18)
# #sp_500_1$hma_9co81_lag19 <- lag(sp_500_1$hma_9co81,n=19)
# sp_500_1$hma_9co81_lag20 <- lag(sp_500_1$hma_9co81,n=20)

sp_500_1$hma_25co49_lag1 <- lag(sp_500_1$hma_25co49)
sp_500_1$hma_25co49_lag2 <- lag(sp_500_1$hma_25co49,n=2)
sp_500_1$hma_25co49_lag3 <- lag(sp_500_1$hma_25co49,n=3)
sp_500_1$hma_25co49_lag4 <- lag(sp_500_1$hma_25co49,n=4)
# #sp_500_1$hma_25co49_lag5 <- lag(sp_500_1$hma_25co49,n=5)
# sp_500_1$hma_25co49_lag6 <- lag(sp_500_1$hma_25co49,n=6)
# #sp_500_1$hma_25co49_lag7 <- lag(sp_500_1$hma_25co49,n=7)
# sp_500_1$hma_25co49_lag8 <- lag(sp_500_1$hma_25co49,n=8)
# #sp_500_1$hma_25co49_lag9 <- lag(sp_500_1$hma_25co49,n=9)
# sp_500_1$hma_25co49_lag10 <- lag(sp_500_1$hma_25co49,n=10)
# #sp_500_1$hma_25co49_lag11<- lag(sp_500_1$hma_25co49, n=11)
# sp_500_1$hma_25co49_lag12 <- lag(sp_500_1$hma_25co49,n=12)
# #sp_500_1$hma_25co49_lag13 <- lag(sp_500_1$hma_25co49,n=13)
# sp_500_1$hma_25co49_lag14 <- lag(sp_500_1$hma_25co49,n=14)
# #sp_500_1$hma_25co49_lag15 <- lag(sp_500_1$hma_25co49,n=15)
# sp_500_1$hma_25co49_lag16 <- lag(sp_500_1$hma_25co49, n=16)
# #sp_500_1$hma_25co49_lag17 <- lag(sp_500_1$hma_25co49,n=17)
# sp_500_1$hma_25co49_lag18 <- lag(sp_500_1$hma_25co49,n=18)
# #sp_500_1$hma_25co49_lag19 <- lag(sp_500_1$hma_25co49,n=19)
# sp_500_1$hma_25co49_lag20 <- lag(sp_500_1$hma_25co49,n=20)

sp_500_1$hma_25co81_lag1 <- lag(sp_500_1$hma_25co81)
sp_500_1$hma_25co81_lag2 <- lag(sp_500_1$hma_25co81,n=2)
sp_500_1$hma_25co81_lag3 <- lag(sp_500_1$hma_25co81,n=3)
sp_500_1$hma_25co81_lag4 <- lag(sp_500_1$hma_25co81,n=4)
# #sp_500_1$hma_25co81_lag5 <- lag(sp_500_1$hma_25co81,n=5)
# sp_500_1$hma_25co81_lag6 <- lag(sp_500_1$hma_25co81, n=6)
# #sp_500_1$hma_25co81_lag7 <- lag(sp_500_1$hma_25co81,n=7)
# sp_500_1$hma_25co81_lag8 <- lag(sp_500_1$hma_25co81,n=8)
# #sp_500_1$hma_25co81_lag9 <- lag(sp_500_1$hma_25co81,n=9)
# sp_500_1$hma_25co81_lag10 <- lag(sp_500_1$hma_25co81,n=10)
# #sp_500_1$hma_25co81_lag11 <- lag(sp_500_1$hma_25co81, n=11)
# sp_500_1$hma_25co81_lag12 <- lag(sp_500_1$hma_25co81,n=12)
# #sp_500_1$hma_25co81_lag13 <- lag(sp_500_1$hma_25co81,n=13)
# sp_500_1$hma_25co81_lag14 <- lag(sp_500_1$hma_25co81,n=14)
# #sp_500_1$hma_25co81_lag15 <- lag(sp_500_1$hma_25co81,n=15)
# sp_500_1$hma_25co81_lag16 <- lag(sp_500_1$hma_25co81, n=16)
# #sp_500_1$hma_25co81_lag17 <- lag(sp_500_1$hma_25co81,n=17)
# sp_500_1$hma_25co81_lag18 <- lag(sp_500_1$hma_25co81,n=18)
# #sp_500_1$hma_25co81_lag19 <- lag(sp_500_1$hma_25co81,n=19)
# sp_500_1$hma_25co81_lag20 <- lag(sp_500_1$hma_25co81,n=20)

sp_500_1$hma_49co81_lag1 <- lag(sp_500_1$hma_49co81)
sp_500_1$hma_49co81_lag2 <- lag(sp_500_1$hma_49co81,n=2)
sp_500_1$hma_49co81_lag3 <- lag(sp_500_1$hma_49co81,n=3)
sp_500_1$hma_49co81_lag4 <- lag(sp_500_1$hma_49co81,n=4)
# #sp_500_1$hma_49co81_lag5 <- lag(sp_500_1$hma_49co81,n=5)
# sp_500_1$hma_49co81_lag6 <- lag(sp_500_1$hma_49co81, n=6)
# #sp_500_1$hma_49co81_lag7 <- lag(sp_500_1$hma_49co81,n=7)
# sp_500_1$hma_49co81_lag8 <- lag(sp_500_1$hma_49co81,n=8)
# #sp_500_1$hma_49co81_lag9 <- lag(sp_500_1$hma_49co81,n=9)
# sp_500_1$hma_49co81_lag10 <- lag(sp_500_1$hma_49co81,n=10)
# #sp_500_1$hma_49co81_lag11 <- lag(sp_500_1$hma_49co81, n=11)
# sp_500_1$hma_49co81_lag12 <- lag(sp_500_1$hma_49co81,n=12)
# #sp_500_1$hma_49co81_lag13 <- lag(sp_500_1$hma_49co81,n=13)
# sp_500_1$hma_49co81_lag14 <- lag(sp_500_1$hma_49co81,n=14)
# #sp_500_1$hma_49co81_lag15 <- lag(sp_500_1$hma_49co81,n=15)
# sp_500_1$hma_49co81_lag16 <- lag(sp_500_1$hma_49co81, n=16)
# #sp_500_1$hma_49co81_lag17 <- lag(sp_500_1$hma_49co81,n=17)
# sp_500_1$hma_49co81_lag18 <- lag(sp_500_1$hma_49co81,n=18)
# #sp_500_1$hma_49co81_lag19 <- lag(sp_500_1$hma_49co81,n=19)
# sp_500_1$hma_49co81_lag20 <- lag(sp_500_1$hma_49co81,n=20)
# 
# sp_500_1 <- sp_500_1 %>%
#   group_by(symbol)%>%
#   tq_mutate(select = c("high","low","close"),
#             mutate_fun = ATR,
#             n=5,
#             col_rename = c("tr_5","atr_5","truehigh_5","truelow_5"))%>%
#   tq_mutate(select = c("high","low","close"),
#             mutate_fun = ATR,
#             n=15,
#             col_rename = c("tr_15","atr_15","truehigh_15","truelow_15"))
# 
# sp_500_1 <- sp_500_1 %>%
#   mutate(atr_ratio =atr_15/atr_5)
# 
# sp_500_1 <- sp_500_1 %>%
#   group_by(symbol)%>%
#   tq_mutate(select = close,
#             mutate_fun = ROC,
#             n=15,
#             type= "continuous"
#   )



object.size(sp_500_1)/1000000000


cores <- parallel::detectCores()
cores

sp_500_1 <- sp_500_1 %>%
  ungroup(symbol)




stock_split <- initial_split(sp_500_1, prop = 3/4, strata = "target_direction_15")

stock_train_15 <- training(stock_split)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily, close_shift_5, target_5,target_direction_5, close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar)) %>%
  na.omit()
stock_test_15 <- testing(stock_split)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily, close_shift_5, target_5,target_direction_5, close_shift_10, target_10,
            target_direction_10,close_shift_15,target_15,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar)) %>%
  na.omit()

stock_train_15$target_direction_15<- if_else(stock_train_15$target_direction_15 == 1, "buy","sell")

stock_train_15$target_direction_15 <- factor(stock_train_15$target_direction_15, levels=c("buy","sell"))

stock_test_15$target_direction_15<- if_else(stock_test_15$target_direction_15 == 1, "buy","sell")

stock_test_15$target_direction_15 <- factor(stock_test_15$target_direction_15, levels=c("buy","sell"))

remove(stock_split)
stock_recipe_15 <- recipe(target_direction_15~., data = stock_train_15)
object.size(stock_train_15)/1000000000

rf_grid <- expand.grid(
  mtry = c(10,12,14,16,18),
  trees = c(500,750)
)

rf_grid

rf_mod_15 <- 
  rand_forest(mtry = tune(), min_n = 2,trees = tune()) %>% 
  set_engine("ranger",importance = "impurity" ) %>% 
  set_mode("classification")#%>%
  #fit(target_direction_15~., data = stock_train_15)

all_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(3)
registerDoParallel(cl)

rf_workflow <-
  workflow() %>%
  add_model(rf_mod_15) %>%
  add_recipe(stock_recipe_15)

rf_workflow

cv <- vfold_cv(stock_train_15, v = 2)

set.seed(345)
rf_res <-
  rf_workflow %>%
  tune_grid(cv,
            grid = rf_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

stopCluster(cl)

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


train_fit_rf <- fit(last_rf_workflow,stock_train_15)

train_fit_rf%>%
  pull_workflow_fit() %>%
  vip(geom = "point",num_features = 50)

test_pred_rf_15 <- predict(train_fit_rf, stock_test_15, type = "prob") %>%
  bind_cols(predict(train_fit_rf, stock_test_15)) %>%
  bind_cols(select(stock_test_15, target_direction_15)) %>%
  glimpse()

test_pred_rf_15 <- predict(rf_mod_15, stock_test_15, type = "prob") %>%
  bind_cols(predict(rf_mod_15, stock_test_15)) %>%
  bind_cols(select(stock_test_15, target_direction_15)) %>%
  glimpse()


test_pred_rf_15 %>%
  sens(truth = target_direction_15,estimate = .pred_class)

test_pred_rf_15%>%
  spec(truth = target_direction_15,estimate = .pred_class)

test_pred_rf_15%>%
  accuracy(truth = target_direction_15,estimate = .pred_class)

test_pred_rf_15%>%
  precision(truth = target_direction_15,estimate = .pred_class)

test_pred_rf_15%>%
  recall(truth = target_direction_15,estimate = .pred_class)

test_pred_rf_15%>%
  ppv(truth = target_direction_15,estimate = .pred_class)

test_pred_rf_15%>%
  npv(truth = target_direction_15,estimate = .pred_class)

test_pred_rf_15 %>%
  roc_auc(truth = target_direction_15,estimate = .pred_buy)

test_pred_rf_15 %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_15), 
               alpha = 0.5)+
  ggtitle("Random Forest target 15 day ")+
  xlab("Probability of Yes")


rm(rf_mod_15)
rm(stock_train_15)
rm(stock_test_15)

stock_split <- initial_split(sp_500_1, prop = 3/4, strata = "target_direction_20")

stock_train_20 <- training(stock_split)%>%
  select(-c(symbol,date, open, high, low, close, volume, adjusted, return_lag1,
            return_daily, close_shift_5, target_5,target_direction_5,  close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15, target_direction_15,close_shift_20,
            target_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar,day)) %>%
  na.omit()
stock_test_20 <- testing(stock_split)%>%
  select(-c(symbol,date, open, high, low, close, volume, adjusted, return_lag1,
            return_daily, close_shift_5, target_5,target_direction_5,  close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15, target_direction_15,close_shift_20,
            target_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar,day)) %>%
  na.omit()

stock_train_20$target_direction_20<- if_else(stock_train_20$target_direction_20 == 1, "buy","sell")

stock_train_20$target_direction_20 <- factor(stock_train_20$target_direction_20, levels=c("buy","sell"))

stock_test_20$target_direction_20<- if_else(stock_test_20$target_direction_20 == 1, "buy","sell")

stock_test_20$target_direction_20 <- factor(stock_test_20$target_direction_20, levels=c("buy","sell"))

remove(stock_split)
stock_recipe_20 <- recipe(target_direction_20~., data = stock_train_20)


rf_grid <- expand.grid(
  mtry = c(10,12,14,16,18),
  trees = c(500,750)
)

rf_grid

rf_mod_15 <- 
  rand_forest(mtry = tune(), min_n = 2,trees = tune()) %>% 
  set_engine("ranger",importance = "impurity" ,num.threads = cores) %>% 
  set_mode("classification")#%>%
#fit(target_direction_20~., data = stock_train_20)

all_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

rf_workflow <-
  workflow() %>%
  add_model(rf_mod_20) %>%
  add_recipe(stock_recipe_20)

rf_workflow

cv <- vfold_cv(stock_train_20, v = 2)

set.seed(345)
rf_res <-
  rf_workflow %>%
  tune_grid(cv,
            grid = rf_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

stopCluster(cl)

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


train_fit_rf <- fit(last_rf_workflow,stock_train_20)

train_fit_rf%>%
  pull_workflow_fit() %>%
  vip(geom = "point",num_features = 50)

test_pred_rf_20 <- predict(train_fit_rf, stock_test_20, type = "prob") %>%
  bind_cols(predict(train_fit_rf, stock_test_20)) %>%
  bind_cols(select(stock_test_20, target_direction_20)) %>%
  glimpse()

# test_pred_rf_20 <- predict(rf_mod_20, stock_test_20, type = "prob") %>%
#   bind_cols(predict(rf_mod_20, stock_test_20)) %>%
#   bind_cols(select(stock_test_20, target_direction_20)) %>%
#   glimpse()


test_pred_rf_20 %>%
  sens(truth = target_direction_20,estimate = .pred_class)

test_pred_rf_20%>%
  spec(truth = target_direction_20,estimate = .pred_class)

test_pred_rf_20%>%
  accuracy(truth = target_direction_20,estimate = .pred_class)

test_pred_rf_20%>%
  precision(truth = target_direction_20,estimate = .pred_class)

test_pred_rf_20%>%
  recall(truth = target_direction_20,estimate = .pred_class)

test_pred_rf_20%>%
  ppv(truth = target_direction_15,estimate = .pred_class)

test_pred_rf_20%>%
  npv(truth = target_direction_20,estimate = .pred_class)

test_pred_rf_20 %>%
  roc_auc(truth = target_direction_20,estimate = .pred_buy)

test_pred_rf_20 %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_20), 
               alpha = 0.5)+
  ggtitle("Random Forest target 20 day ")+
  xlab("Probability of Yes")
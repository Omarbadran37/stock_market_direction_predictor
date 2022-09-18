
library(tidyverse)
library(tidyquant)
library(tidymodels)
library(randomForest)
library(xgboost)
library(yfR)

rm(list = ls())


start_date <- "2012-01-01"
start_date <- Sys.Date()-365

sp_500 <- tq_index("SP500")

sp_500 <- tq_index("SP500") %>%
  tq_get(get="stock.prices",
         from = start_date)
# remotes::install_github('msperlin/yfR') 
# 

first.date <- Sys.Date()-365
first.date <- Sys.Date()-2190
last.date <- Sys.Date()

df.SP500 <- GetSP500Stocks()
tickers <- df.SP500$Tickers

l.out <- yf_get(tickers = sp_500$symbol,
                         first_date = first.date,
                         last_date = last.date)

n_distinct(l.out$ticker)

sp_500_1 <- l.out %>%
  select(symbol = ticker, date = ref_date, open = price_open, high = price_high,
         low = price_low,close = price_close, volume)

# write.csv(stock_call,"weekly_stock_symbol.csv")
# 
# sp_500 <- tq_get(stock_call$column_label,
#                  from = start_date)

sp_500_1 <- sp_500 %>%
  select(symbol,date,open,high,low,close,volume)

sp_500_1 <- sp_500_1 %>%
  filter(!symbol %in% c("CEG","OGN","AMCR","TTWO","CTVA","DOW","EMBC"))

sp_500_1 <- sp_500_1 %>%
  filter(!symbol %in% c("ACN"))

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

# sp_500_1 <- sp_500_1 %>%
#   mutate(target_direction_5 = if_else(target_5>0.1,1,0))

sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  mutate(close_shift_10 = lead(close, n=10))

sp_500_1 <- sp_500_1%>%
  mutate(target_10 = (close_shift_10-lead(open))/(lead(open))*100)

sp_500_1 <- sp_500_1 %>%
  mutate(target_direction_10 = if_else(target_10>0,1,0))

# sp_500_1 <- sp_500_1 %>%
#   mutate(target_direction_10 = if_else(target_10>0.1,1,0))

sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  mutate(close_shift_15 = lead(close, n=15))

sp_500_1 <- sp_500_1%>%
  mutate(target_15 = (close_shift_15-lead(open))/(lead(open))*100)

sp_500_1 <- sp_500_1 %>%
  mutate(target_direction_15 = if_else(target_15>0,1,0))

# sp_500_1 <- sp_500_1 %>%
#   mutate(target_direction_15 = if_else(target_15>0.1,1,0))

sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  mutate(close_shift_20 = lead(close, n=20))

sp_500_1 <- sp_500_1%>%
  mutate(target_20 = (close_shift_20-lead(open))/(lead(open))*100)

sp_500_1 <- sp_500_1 %>%
  mutate(target_direction_20 = if_else(target_20>0,1,0))

# sp_500_1 <- sp_500_1 %>%
#   mutate(target_direction_20 = if_else(target_20>0.1,1,0))

# sp_500_1 <- sp_500_1 %>%
#   filter(!symbol %in% c("AEVA","AMCR","BTRS","DKNG","LAZR","LPRO","AMLX",
#                         "DOUG","TKLF","TPG","VIGL","ZGN","HCP","IOT","NU"))

# sp_500_1$date <- as.POSIXct(sp_500_1$date)

sp_500_1 %>% ungroup(symbol)%>% count(target_direction_5)
sp_500_1 %>% ungroup(symbol)%>% count(target_direction_10)
sp_500_1 %>% ungroup(symbol)%>% count(target_direction_15)
sp_500_1 %>% ungroup(symbol)%>% count(target_direction_20)


test <- sp_500_1 

test$month_yr <- format_ISO8601(test$date, precision = "ym")
test_1 <-test %>%
  group_by(month_yr,symbol)%>%
  summarise(max_high =max(high),min_low = min(low))


test_2 <-test %>%
  group_by(month_yr,symbol)%>%
  slice_min(date)%>%
  ungroup(month_yr,symbol)%>%
  select(open_month=open)


test_3 <- test %>%
  group_by(month_yr,symbol)%>%
  slice_max(date)%>%
  ungroup(month_yr,symbol)%>%
  select( close_month =close)

test_4 <- bind_cols(test_1,test_2,test_3)


test_4 <- test_4 %>%
  ungroup(symbol,month_yr)
test_4 <- test_4 %>%
  group_by(symbol)



test_4 <- test_4 %>%
  group_by(symbol)%>%
  mutate(pp = (lag(max_high) + lag(min_low) + lag(close_month))/3,
         r1 = (2*lag(pp)) -lag(min_low),
         s1 = (2*lag(pp)) - lag(max_high),
         r2 = lag(pp) + (lag(max_high)-lag(min_low)),
         s2 = lag(pp) - (lag(max_high)-lag(min_low)),
         r3 = lag(max_high) + 2*(lag(pp)-lag(min_low)),
         s3 = lag(min_low) - 2*(lag(max_high)-lag(pp)))%>%
  arrange(symbol)

sp_500_1$month_yr <- format_ISO8601(sp_500_1$date, precision = "ym")

sp_500_1 <- left_join(x=sp_500_1,y=test_4, by=c("symbol","month_yr"))%>%
  arrange(symbol)

sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  mutate(close_r1 = close/r1,
         close_r2 = close/r2,
         close_r3 = close/r3,
         close_s1 = close/s1,
         close_s2 = close/s2,
         close_s3 = close/s3,
         close_pp = close/pp)

rm(test)
rm(test_1)
rm(test_2)
rm(test_3)
rm(test_4)

sp_500_1 <- sp_500_1%>%
  group_by(symbol) %>%
  tq_mutate(select     = close, 
            mutate_fun = MACD, 
            nFast      = 5, 
            nSlow      = 15, 
            nSig       = 9, 
            maType     = "EMA")%>%
  mutate(diff_short = macd - signal)

sp_500_1$diff_short_trend_3 <- sp_500_1$diff_short/lag(sp_500_1$diff_short, n=3)
sp_500_1$diff_short_trend_5 <- sp_500_1$diff_short/lag(sp_500_1$diff_short, n=5)
sp_500_1$diff_short_trend_10 <- sp_500_1$diff_short/lag(sp_500_1$diff_short, n=10)
sp_500_1$diff_short_trend_15 <- sp_500_1$diff_short/lag(sp_500_1$diff_short, n=15)
sp_500_1$diff_short_trend_20 <- sp_500_1$diff_short/lag(sp_500_1$diff_short, n=20)


sp_500_1 <- sp_500_1%>%
  group_by(symbol) %>%
  tq_mutate(select     = close, 
            mutate_fun = MACD, 
            nFast      = 12, 
            nSlow      = 26, 
            nSig       = 9, 
            maType     = "EMA")%>%
  mutate(diff_long = macd - signal)

sp_500_1$diff_long_trend_3 <- sp_500_1$diff_long/lag(sp_500_1$diff_long, n=3)
sp_500_1$diff_long_trend_5 <- sp_500_1$diff_long/lag(sp_500_1$diff_long, n=5)
sp_500_1$diff_long_trend_10 <- sp_500_1$diff_long/lag(sp_500_1$diff_long, n=10)
sp_500_1$diff_long_trend_15 <- sp_500_1$diff_long/lag(sp_500_1$diff_long, n=15)
sp_500_1$diff_long_trend_20 <- sp_500_1$diff_long/lag(sp_500_1$diff_long, n=20)

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

sp_500_1$rsi_trend_3 <- sp_500_1$rsi_14/lag(sp_500_1$rsi_14, n=3)
sp_500_1$rsi_trend_5 <- sp_500_1$rsi_14/lag(sp_500_1$rsi_14, n=5)
sp_500_1$rsi_trend_10 <- sp_500_1$rsi_14/lag(sp_500_1$rsi_14, n=10)
sp_500_1$rsi_trend_15 <- sp_500_1$rsi_14/lag(sp_500_1$rsi_14, n=15)
sp_500_1$rsi_trend_20 <- sp_500_1$rsi_14/lag(sp_500_1$rsi_14, n=20)

sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  tq_mutate(select = c("high","low"),
            mutate_fun = SAR,
            col_rename = "sar")



sp_500_1$sar_ratio <- sp_500_1$close/sp_500_1$sar

sp_500_1$sar_ratio_trend_3 <- sp_500_1$sar_ratio/lag(sp_500_1$sar_ratio,n=3)
sp_500_1$sar_ratio_trend_5 <- sp_500_1$sar_ratio/lag(sp_500_1$sar_ratio,n=5)
sp_500_1$sar_ratio_trend_10 <- sp_500_1$sar_ratio/lag(sp_500_1$sar_ratio,n=10)
sp_500_1$sar_ratio_trend_15<- sp_500_1$sar_ratio/lag(sp_500_1$sar_ratio,n=15)
sp_500_1$sar_ratio_trend_20 <- sp_500_1$sar_ratio/lag(sp_500_1$sar_ratio,n=20)


sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  tq_mutate(select = c("high","low","close"),
            mutate_fun = stoch)

sp_500_1$stoch_diff <- sp_500_1$fastD/sp_500_1$stoch

sp_500_1$stoch_diff_trend_3 <- sp_500_1$stoch_diff/lag(sp_500_1$stoch_diff, n=3)
sp_500_1$stoch_diff_trend_5 <- sp_500_1$stoch_diff/lag(sp_500_1$stoch_diff, n=5)
sp_500_1$stoch_diff_trend_10 <- sp_500_1$stoch_diff/lag(sp_500_1$stoch_diff, n=10)
sp_500_1$stoch_diff_trend_15 <- sp_500_1$stoch_diff/lag(sp_500_1$stoch_diff, n=15)
sp_500_1$stoch_diff_trend_20 <- sp_500_1$stoch_diff/lag(sp_500_1$stoch_diff, n=20)


sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  tq_mutate(select = c("high","low","close"),
            mutate_fun = ADX,
            maType = "EMA")

##sp_500_1$di_ratio <- sp_500_1$DIp/sp_500_1$DIn
##sp_500_1$adx_ratio<- sp_500_1$DIp/sp_500_1$ADX

sp_500_1$adx_trend_3<- sp_500_1$ADX/lag(sp_500_1$ADX,n=3)
sp_500_1$adx_trend_5<- sp_500_1$ADX/lag(sp_500_1$ADX,n=5)
sp_500_1$adx_trend_10<- sp_500_1$ADX/lag(sp_500_1$ADX,n=10)
sp_500_1$adx_trend_15<- sp_500_1$ADX/lag(sp_500_1$ADX,n=15)
sp_500_1$adx_trend_20<- sp_500_1$ADX/lag(sp_500_1$ADX,n=20)

sp_500_1$DIp_trend_3 <- sp_500_1$DIp/lag(sp_500_1$DIp,n=3)
sp_500_1$DIp_trend_5 <- sp_500_1$DIp/lag(sp_500_1$DIp,n=5)
sp_500_1$DIp_trend_10 <- sp_500_1$DIp/lag(sp_500_1$DIp,n=10)
sp_500_1$DIp_trend_15 <- sp_500_1$DIp/lag(sp_500_1$DIp,n=15)
sp_500_1$DIp_trend_20 <- sp_500_1$DIp/lag(sp_500_1$DIp,n=20)

sp_500_1$DIn_trend_3 <- sp_500_1$DIn/lag(sp_500_1$DIn,n=3)
sp_500_1$DIn_trend_5 <- sp_500_1$DIn/lag(sp_500_1$DIn,n=5)
sp_500_1$DIn_trend_10 <- sp_500_1$DIn/lag(sp_500_1$DIn,n=10)
sp_500_1$DIn_trend_15<- sp_500_1$DIn/lag(sp_500_1$DIn,n=15)
sp_500_1$DIn_trend_20 <- sp_500_1$DIn/lag(sp_500_1$DIn,n=20)

sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  tq_mutate(select = c("high","low","close"),
            mutate_fun = BBands)

sp_500_1$lowerBB_candle <- sp_500_1$close/sp_500_1$dn
sp_500_1$upperBB_candle <- sp_500_1$close/sp_500_1$up
sp_500_1$bbands_size <- sp_500_1$up/sp_500_1$dn

sp_500_1$lowerBB_candle_trend_3 <- sp_500_1$lowerBB_candle/lag(sp_500_1$lowerBB_candle,n=3)
sp_500_1$lowerBB_candle_trend_5 <- sp_500_1$lowerBB_candle/lag(sp_500_1$lowerBB_candle,n=5)
sp_500_1$lowerBB_candle_trend_10 <- sp_500_1$lowerBB_candle/lag(sp_500_1$lowerBB_candle,n=10)
sp_500_1$lowerBB_candle_trend_15<- sp_500_1$lowerBB_candle/lag(sp_500_1$lowerBB_candle,n=15)
sp_500_1$lowerBB_candle_trend_20<- sp_500_1$lowerBB_candle/lag(sp_500_1$lowerBB_candle,n=20)

sp_500_1$upperBB_candle_trend_3 <- sp_500_1$upperBB_candle/lag(sp_500_1$upperBB_candle, n =3)
sp_500_1$upperBB_candle_trend_5 <- sp_500_1$upperBB_candle/lag(sp_500_1$upperBB_candle, n =5)
sp_500_1$upperBB_candle_trend_10 <- sp_500_1$upperBB_candle/lag(sp_500_1$upperBB_candle, n =10)
sp_500_1$upperBB_candle_trend_15 <- sp_500_1$upperBB_candle/lag(sp_500_1$upperBB_candle, n =15)
sp_500_1$upperBB_candle_trend_20 <- sp_500_1$upperBB_candle/lag(sp_500_1$upperBB_candle, n =20)

sp_500_1$bbands_size_trend_3 <- sp_500_1$bbands_size/lag(sp_500_1$bbands_size, n=3)
sp_500_1$bbands_size_trend_5 <- sp_500_1$bbands_size/lag(sp_500_1$bbands_size, n=5)
sp_500_1$bbands_size_trend_10 <- sp_500_1$bbands_size/lag(sp_500_1$bbands_size, n=10)
sp_500_1$bbands_size_trend_15 <- sp_500_1$bbands_size/lag(sp_500_1$bbands_size, n=15)
sp_500_1$bbands_size_trend_20 <- sp_500_1$bbands_size/lag(sp_500_1$bbands_size, n=20)

sp_500_1$hma_9_trend_3 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 3)
sp_500_1$hma_9_trend_5 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 5)
sp_500_1$hma_9_trend_10 <- sp_500_1$hma_9 /lag(sp_500_1$hma_9, n = 10)
sp_500_1$hma_9_trend_15 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 15)
sp_500_1$hma_9_trend_20 <- sp_500_1$hma_9/lag(sp_500_1$hma_9, n = 20)

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

sp_500_1$hma_9co200_trend_3<- sp_500_1$hma_9co200/lag(sp_500_1$hma_9co200,n=3)
sp_500_1$hma_25co200_trend_3<- sp_500_1$hma_25co200/lag(sp_500_1$hma_25co200,n=3)
sp_500_1$hma_49co200_trend_3<- sp_500_1$hma_49co200/lag(sp_500_1$hma_49co200,n=3)
sp_500_1$hma_81co200_trend_3<- sp_500_1$hma_81co200/lag(sp_500_1$hma_81co200,n=3)
sp_500_1$hma_9co_sma50_trend_3<- sp_500_1$hma_9co_sma50/lag(sp_500_1$hma_9co_sma50,n=3)
sp_500_1$hma_25co_sma50_trend_3<- sp_500_1$hma_25co_sma50/lag(sp_500_1$hma_25co_sma50,n=3)
sp_500_1$hma_49co_sma50_trend_3<- sp_500_1$hma_49co_sma50/lag(sp_500_1$hma_49co_sma50,n=3)
sp_500_1$hma_81co_sma50_trend_3<- sp_500_1$hma_81co_sma50/lag(sp_500_1$hma_81co_sma50,n=3)
sp_500_1$hma_9co_sma100_trend_3<- sp_500_1$hma_9co_sma100/lag(sp_500_1$hma_9co_sma100,n=3)
sp_500_1$hma_25co_sma100_trend_3<- sp_500_1$hma_25co_sma100/lag(sp_500_1$hma_25co_sma100,n=3)
sp_500_1$hma_49co_sma100_trend_3<- sp_500_1$hma_49co_sma100/lag(sp_500_1$hma_49co_sma100,n=3)
sp_500_1$hma_81co_sma100_trend_3<- sp_500_1$hma_81co_sma100/lag(sp_500_1$hma_81co_sma100,n=3)
sp_500_1$hma_9co_sma200_trend_3<- sp_500_1$hma_9co_sma200/lag(sp_500_1$hma_9co_sma200,n=3)
sp_500_1$hma_25co_sma200_trend_3<- sp_500_1$hma_25co_sma200/lag(sp_500_1$hma_25co_sma200,n=3)
sp_500_1$hma_49co_sma200_trend_3<- sp_500_1$hma_49co_sma200/lag(sp_500_1$hma_49co_sma200,n=3)
sp_500_1$hma_81co_sma200_trend_3<- sp_500_1$hma_81co_sma200/lag(sp_500_1$hma_81co_sma200,n=3)

sp_500_1$hma_9co200_trend_5<- sp_500_1$hma_9co200/lag(sp_500_1$hma_9co200,n=5)
sp_500_1$hma_25co200_trend_5<- sp_500_1$hma_25co200/lag(sp_500_1$hma_25co200,n=5)
sp_500_1$hma_49co200_trend_5<- sp_500_1$hma_49co200/lag(sp_500_1$hma_49co200,n=5)
sp_500_1$hma_81co200_trend_5<- sp_500_1$hma_81co200/lag(sp_500_1$hma_81co200,n=5)
sp_500_1$hma_9co_sma50_trend_5<- sp_500_1$hma_9co_sma50/lag(sp_500_1$hma_9co_sma50,n=5)
sp_500_1$hma_25co_sma50_trend_5<- sp_500_1$hma_25co_sma50/lag(sp_500_1$hma_25co_sma50,n=5)
sp_500_1$hma_49co_sma50_trend_5<- sp_500_1$hma_49co_sma50/lag(sp_500_1$hma_49co_sma50,n=5)
sp_500_1$hma_81co_sma50_trend_5<- sp_500_1$hma_81co_sma50/lag(sp_500_1$hma_81co_sma50,n=5)
sp_500_1$hma_9co_sma100_trend_5<- sp_500_1$hma_9co_sma100/lag(sp_500_1$hma_9co_sma100,n=5)
sp_500_1$hma_25co_sma100_trend_5<- sp_500_1$hma_25co_sma100/lag(sp_500_1$hma_25co_sma100,n=5)
sp_500_1$hma_49co_sma100_trend_5<- sp_500_1$hma_49co_sma100/lag(sp_500_1$hma_49co_sma100,n=5)
sp_500_1$hma_81co_sma100_trend_5<- sp_500_1$hma_81co_sma100/lag(sp_500_1$hma_81co_sma100,n=5)
sp_500_1$hma_9co_sma200_trend_5<- sp_500_1$hma_9co_sma200/lag(sp_500_1$hma_9co_sma200,n=5)
sp_500_1$hma_25co_sma200_trend_5<- sp_500_1$hma_25co_sma200/lag(sp_500_1$hma_25co_sma200,n=5)
sp_500_1$hma_49co_sma200_trend_5<- sp_500_1$hma_49co_sma200/lag(sp_500_1$hma_49co_sma200,n=5)
sp_500_1$hma_81co_sma200_trend_5<- sp_500_1$hma_81co_sma200/lag(sp_500_1$hma_81co_sma200,n=5)

sp_500_1$hma_9co200_trend_10<- sp_500_1$hma_9co200/lag(sp_500_1$hma_9co200,n=10)
sp_500_1$hma_25co200_trend_10<- sp_500_1$hma_25co200/lag(sp_500_1$hma_25co200,n=10)
sp_500_1$hma_49co200_trend_10<- sp_500_1$hma_49co200/lag(sp_500_1$hma_49co200,n=10)
sp_500_1$hma_81co200_trend_10<- sp_500_1$hma_81co200/lag(sp_500_1$hma_81co200,n=10)
sp_500_1$hma_9co_sma50_trend_10<- sp_500_1$hma_9co_sma50/lag(sp_500_1$hma_9co_sma50,n=10)
sp_500_1$hma_25co_sma50_trend_10<- sp_500_1$hma_25co_sma50/lag(sp_500_1$hma_25co_sma50,n=10)
sp_500_1$hma_49co_sma50_trend_10<- sp_500_1$hma_49co_sma50/lag(sp_500_1$hma_49co_sma50,n=10)
sp_500_1$hma_81co_sma50_trend_10<- sp_500_1$hma_81co_sma50/lag(sp_500_1$hma_81co_sma50,n=10)
sp_500_1$hma_9co_sma100_trend_10<- sp_500_1$hma_9co_sma100/lag(sp_500_1$hma_9co_sma100,n=10)
sp_500_1$hma_25co_sma100_trend_10<- sp_500_1$hma_25co_sma100/lag(sp_500_1$hma_25co_sma100,n=10)
sp_500_1$hma_49co_sma100_trend_10<- sp_500_1$hma_49co_sma100/lag(sp_500_1$hma_49co_sma100,n=10)
sp_500_1$hma_81co_sma100_trend_10<- sp_500_1$hma_81co_sma100/lag(sp_500_1$hma_81co_sma100,n=10)
sp_500_1$hma_9co_sma200_trend_10<- sp_500_1$hma_9co_sma200/lag(sp_500_1$hma_9co_sma200,n=10)
sp_500_1$hma_25co_sma200_trend_10<- sp_500_1$hma_25co_sma200/lag(sp_500_1$hma_25co_sma200,n=10)
sp_500_1$hma_49co_sma200_trend_10<- sp_500_1$hma_49co_sma200/lag(sp_500_1$hma_49co_sma200,n=10)
sp_500_1$hma_81co_sma200_trend_10<- sp_500_1$hma_81co_sma200/lag(sp_500_1$hma_81co_sma200,n=10)

sp_500_1$hma_9co200_trend_15<- sp_500_1$hma_9co200/lag(sp_500_1$hma_9co200,n=15)
sp_500_1$hma_25co200_trend_15<- sp_500_1$hma_25co200/lag(sp_500_1$hma_25co200,n=15)
sp_500_1$hma_49co200_trend_15<- sp_500_1$hma_49co200/lag(sp_500_1$hma_49co200,n=15)
sp_500_1$hma_81co200_trend_15<- sp_500_1$hma_81co200/lag(sp_500_1$hma_81co200,n=15)
sp_500_1$hma_9co_sma50_trend_15<- sp_500_1$hma_9co_sma50/lag(sp_500_1$hma_9co_sma50,n=15)
sp_500_1$hma_25co_sma50_trend_15<- sp_500_1$hma_25co_sma50/lag(sp_500_1$hma_25co_sma50,n=15)
sp_500_1$hma_49co_sma50_trend_15<- sp_500_1$hma_49co_sma50/lag(sp_500_1$hma_49co_sma50,n=15)
sp_500_1$hma_81co_sma50_trend_15<- sp_500_1$hma_81co_sma50/lag(sp_500_1$hma_81co_sma50,n=15)
sp_500_1$hma_9co_sma100_trend_15<- sp_500_1$hma_9co_sma100/lag(sp_500_1$hma_9co_sma100,n=15)
sp_500_1$hma_25co_sma100_trend_15<- sp_500_1$hma_25co_sma100/lag(sp_500_1$hma_25co_sma100,n=15)
sp_500_1$hma_49co_sma100_trend_15<- sp_500_1$hma_49co_sma100/lag(sp_500_1$hma_49co_sma100,n=15)
sp_500_1$hma_81co_sma100_trend_15<- sp_500_1$hma_81co_sma100/lag(sp_500_1$hma_81co_sma100,n=15)
sp_500_1$hma_9co_sma200_trend_15<- sp_500_1$hma_9co_sma200/lag(sp_500_1$hma_9co_sma200,n=15)
sp_500_1$hma_25co_sma200_trend_15<- sp_500_1$hma_25co_sma200/lag(sp_500_1$hma_25co_sma200,n=15)
sp_500_1$hma_49co_sma200_trend_15<- sp_500_1$hma_49co_sma200/lag(sp_500_1$hma_49co_sma200,n=15)
sp_500_1$hma_81co_sma200_trend_15<- sp_500_1$hma_81co_sma200/lag(sp_500_1$hma_81co_sma200,n=15)

sp_500_1$hma_9co200_trend_20<- sp_500_1$hma_9co200/lag(sp_500_1$hma_9co200,n=20)
sp_500_1$hma_25co200_trend_20<- sp_500_1$hma_25co200/lag(sp_500_1$hma_25co200,n=20)
sp_500_1$hma_49co200_trend_20<- sp_500_1$hma_49co200/lag(sp_500_1$hma_49co200,n=20)
sp_500_1$hma_81co200_trend_20<- sp_500_1$hma_81co200/lag(sp_500_1$hma_81co200,n=20)
sp_500_1$hma_9co_sma50_trend_20<- sp_500_1$hma_9co_sma50/lag(sp_500_1$hma_9co_sma50,n=20)
sp_500_1$hma_25co_sma50_trend_20<- sp_500_1$hma_25co_sma50/lag(sp_500_1$hma_25co_sma50,n=20)
sp_500_1$hma_49co_sma50_trend_20<- sp_500_1$hma_49co_sma50/lag(sp_500_1$hma_49co_sma50,n=20)
sp_500_1$hma_81co_sma50_trend_20<- sp_500_1$hma_81co_sma50/lag(sp_500_1$hma_81co_sma50,n=20)
sp_500_1$hma_9co_sma100_trend_20<- sp_500_1$hma_9co_sma100/lag(sp_500_1$hma_9co_sma100,n=20)
sp_500_1$hma_25co_sma100_trend_20<- sp_500_1$hma_25co_sma100/lag(sp_500_1$hma_25co_sma100,n=20)
sp_500_1$hma_49co_sma100_trend_20<- sp_500_1$hma_49co_sma100/lag(sp_500_1$hma_49co_sma100,n=20)
sp_500_1$hma_81co_sma100_trend_20<- sp_500_1$hma_81co_sma100/lag(sp_500_1$hma_81co_sma100,n=20)
sp_500_1$hma_9co_sma200_trend_20<- sp_500_1$hma_9co_sma200/lag(sp_500_1$hma_9co_sma200,n=20)
sp_500_1$hma_25co_sma200_trend_20<- sp_500_1$hma_25co_sma200/lag(sp_500_1$hma_25co_sma200,n=20)
sp_500_1$hma_49co_sma200_trend_20<- sp_500_1$hma_49co_sma200/lag(sp_500_1$hma_49co_sma200,n=20)
sp_500_1$hma_81co_sma200_trend_20<- sp_500_1$hma_81co_sma200/lag(sp_500_1$hma_81co_sma200,n=20)

sp_500_1$hma_9co25_trend_3<- sp_500_1$hma_9co25/lag(sp_500_1$hma_9co25,n=3)
sp_500_1$hma_9co25_trend_5<- sp_500_1$hma_9co25/lag(sp_500_1$hma_9co25,n=5)
sp_500_1$hma_9co25_trend_10<- sp_500_1$hma_9co25/lag(sp_500_1$hma_9co25,n=10)
sp_500_1$hma_9co25_trend_15 <- sp_500_1$hma_9co25/lag(sp_500_1$hma_9co25,n=15)
sp_500_1$hma_9co25_trend_20 <- sp_500_1$hma_9co25/lag(sp_500_1$hma_9co25,n=20)

sp_500_1$hma_9co49_trend_3 <- sp_500_1$hma_9co49/lag(sp_500_1$hma_9co49,n=3)
sp_500_1$hma_9co49_trend_5<- sp_500_1$hma_9co49/lag(sp_500_1$hma_9co49,n=5)
sp_500_1$hma_9co49_trend_10 <- sp_500_1$hma_9co49/lag(sp_500_1$hma_9co49,n=10)
sp_500_1$hma_9co49_trend_15<- sp_500_1$hma_9co49/lag(sp_500_1$hma_9co49,n=15)
sp_500_1$hma_9co49_trend_20<- sp_500_1$hma_9co49/lag(sp_500_1$hma_9co49,n=20)

sp_500_1$hma_9co81_trend_3 <- sp_500_1$hma_9co81/lag(sp_500_1$hma_9co81,n=3)
sp_500_1$hma_9co81_trend_5 <- sp_500_1$hma_9co81/lag(sp_500_1$hma_9co81,n=5)
sp_500_1$hma_9co81_trend_10 <- sp_500_1$hma_9co81/lag(sp_500_1$hma_9co81,n=10)
sp_500_1$hma_9co81_trend_15 <- sp_500_1$hma_9co81/lag(sp_500_1$hma_9co81,n=15)
sp_500_1$hma_9co81_trend_20 <- sp_500_1$hma_9co81/lag(sp_500_1$hma_9co81,n=20)

sp_500_1$hma_25co49_trend_3 <- sp_500_1$hma_25co49/lag(sp_500_1$hma_25co49,n=3)
sp_500_1$hma_25co49_trend_5<- sp_500_1$hma_25co49/lag(sp_500_1$hma_25co49,n=5)
sp_500_1$hma_25co49_trend_10<- sp_500_1$hma_25co49/lag(sp_500_1$hma_25co49,n=10)
sp_500_1$hma_25co49_trend_15<- sp_500_1$hma_25co49/lag(sp_500_1$hma_25co49,n=15)
sp_500_1$hma_25co49_trend_20 <- sp_500_1$hma_25co49/lag(sp_500_1$hma_25co49,n=20)

sp_500_1$hma_25co81_trend_3 <- sp_500_1$hma_25co81/lag(sp_500_1$hma_25co81,n=3)
sp_500_1$hma_25co81_trend_5 <- sp_500_1$hma_25co81/lag(sp_500_1$hma_25co81,n=5)
sp_500_1$hma_25co81_trend_10 <- sp_500_1$hma_25co81/lag(sp_500_1$hma_25co81,n=10)
sp_500_1$hma_25co81_trend_15 <- sp_500_1$hma_25co81/lag(sp_500_1$hma_25co81,n=15)
sp_500_1$hma_25co81_trend_20 <- sp_500_1$hma_25co81/lag(sp_500_1$hma_25co81,n=20)

sp_500_1$hma_49co81_trend_3 <- sp_500_1$hma_49co81/lag(sp_500_1$hma_49co81,n=3)
sp_500_1$hma_49co81_trend_5 <- sp_500_1$hma_49co81/lag(sp_500_1$hma_49co81,n=5)
sp_500_1$hma_49co81_trend_10 <- sp_500_1$hma_49co81/lag(sp_500_1$hma_49co81,n=10)
sp_500_1$hma_49co81_trend_15 <- sp_500_1$hma_49co81/lag(sp_500_1$hma_49co81,n=15)
sp_500_1$hma_49co81_trend_20 <- sp_500_1$hma_49co81/lag(sp_500_1$hma_49co81,n=20)

object.size(sp_500_1)/1000000000

date_filter <- sp_500_1 %>%
  filter(date == Sys.Date()-2)

date_filter <- sp_500_1 %>%
  ungroup(symbol)%>%
  slice_max(date)

date_filter <- date_filter %>%
  distinct(symbol, .keep_all = T)

date_filter <- anti_join(x=date_filter, y= bought_stocks, by="symbol")


stock_train_20 <- date_filter%>%
  select(-c(symbol,date, open, high, low, close, volume,  return_lag1,
            return_daily, close_shift_5, target_5,target_direction_5,  close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15, target_direction_15,close_shift_20,
            target_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar)) 

rf_mod_20 <- readRDS("SP500_rfml_trendpick_20.rds")

stock_pred_20_rf <-  predict(rf_mod_20, stock_train_20, type = "prob")

stock_pred_20_rf_symbol  <- bind_cols(stock_pred_20_rf, 
                                      select(date_filter, date,
                                             symbol, close,target_10,target_direction_10,
                                             target_5,target_direction_5, target_15,target_direction_15, 
                                             close_shift_15, close_shift_5, close_shift_20, 
                                             close_shift_15, close_shift_10,
                                             target_20,target_direction_20))

stock_buy_20 <-stock_pred_20_rf_symbol %>%
  group_by(date)%>%
  filter(.pred_buy >=0.595)%>%
  slice_max(.pred_buy, n=60)

buy <- anti_join(x=stock_buy_20, y=look_stocks, by="symbol")

stock_buy_20 <- buy%>%
  slice_max(.pred_buy, n=30)


save_stocks <- stock_buy_20 %>%
  select(date,symbol)

save_stocks$buy_date <- rep(Sys.Date()-2)

look_stocks <- read.csv("stock_holdings.csv")

look_stocks$buy_date <- as.Date(look_stocks$buy_date)

save_stocks <- rbind(look_stocks,save_stocks)
  
  write.csv(save_stocks,"stock_holdings.csv", row.names = F)
  


save_stocks$time_diff <- Sys.Date()-as.Date(save_stocks$buy_date)



train <- sp_500_1 %>% 
  filter(date < "2021-01-01")%>%
  na.omit()


test <- sp_500_1 %>%
  filter(date > "2021-01-01")%>%
  na.omit()


stock_train_15 <- train%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily, close_shift_5, target_5,target_direction_5, close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar))

stock_test_15 <- test%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily, close_shift_5, target_5,target_direction_5, close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar))

stock_train_15$target_direction_15<- if_else(stock_train_15$target_direction_15 == 1, "buy","sell")

stock_train_15$target_direction_15 <- factor(stock_train_15$target_direction_15, levels=c("buy","sell"))

stock_test_15$target_direction_15<- if_else(stock_test_15$target_direction_15 == 1, "buy","sell")

stock_test_15$target_direction_15 <- factor(stock_test_15$target_direction_15, levels=c("buy","sell"))

cores <- 8

rf_mod_15 <- 
  rand_forest( min_n = 2,trees = 750) %>% 
  set_engine("ranger",importance = "impurity",num.threads = cores ) %>% 
  set_mode("classification")%>%
  fit(target_direction_15~., data = stock_train_15)

saveRDS(rf_mod_15,"SP500_rfml_trendpick_15.rds")

library(vip)
rf_mod_15 %>%
  vip(geom = "point",num_features = 50)

test_pred_15_rf <-  predict(rf_mod_15, stock_test_15, type = "prob")%>%
  bind_cols(predict(rf_mod_15, stock_test_15)) %>%
  bind_cols(select(stock_test_15, target_direction_15)) %>%
  glimpse()


test_pred_15_rf %>%
  sens(truth = as.factor(target_direction_15),estimate = .pred_class)

test_pred_15_rf%>%
  spec(truth = target_direction_15,estimate = .pred_class)

test_pred_15_rf%>%
  accuracy(truth = target_direction_15,estimate = .pred_class)

test_pred_15_rf%>%
  precision(truth = target_direction_15,estimate = .pred_class)

test_pred_15_rf%>%
  recall(truth = target_direction_15,estimate = .pred_class)

test_pred_15_rf%>%
  ppv(truth = target_direction_15,estimate = .pred_class)

test_pred_15_rf%>%
  npv(truth = target_direction_15,estimate = .pred_class)

test_pred_15_rf %>%
  roc_auc(truth = target_direction_15,estimate = .pred_buy)

test_pred_15_rf %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_15), 
               alpha = 0.5)+
  ggtitle("Random Forest target 15 day ")+
  xlab("Probability of Yes")

stock_pred_15_rf_symbol  <- bind_cols(test_pred_15_rf, 
                                      select(test, date,
                                             symbol, close,target_10,target_direction_10,
                                             target_5,target_direction_5, target_15,target_direction_15, 
                                             close_shift_15, close_shift_5, close_shift_20, 
                                             close_shift_15, close_shift_10,
                                             target_20,target_direction_20))%>% na.omit()

close_price <- 135



stock_buy_15 <-stock_pred_15_rf_symbol %>%
  group_by(date)%>%
  filter(date >= "2021-01-01" & close <= close_price )%>%
  slice_max(.pred_buy, n=20)

stock_buy_15$price_diff_5 <- stock_buy_15$close_shift_5 -  stock_buy_15$close
stock_buy_15$price_diff_10 <- stock_buy_15$close_shift_10 -  stock_buy_15$close
stock_buy_15$price_diff_15 <- stock_buy_15$close_shift_15 -  stock_buy_15$close
stock_buy_15$price_diff_20 <- stock_buy_15$close_shift_20 -  stock_buy_15$close

sum(stock_buy_15$target_5)
sum(stock_buy_15$target_10)
sum(stock_buy_15$target_15)
sum(stock_buy_15$target_20)

sum(stock_buy_15$price_diff_5)
sum(stock_buy_15$price_diff_10)
sum(stock_buy_15$price_diff_15)
sum(stock_buy_15$price_diff_20)


stock_buy_15$sum_target_5<- cumsum(stock_buy_15$target_5)
stock_buy_15$sum_target_10<- cumsum(stock_buy_15$target_10) 
stock_buy_15$sum_target_15<- cumsum(stock_buy_15$target_15)
stock_buy_15$sum_target_20<- cumsum(stock_buy_15$target_20)

stock_buy_15$sum_price_diff_5<- cumsum(stock_buy_15$price_diff_5)
stock_buy_15$sum_price_diff_10<- cumsum(stock_buy_15$price_diff_10)
stock_buy_15$sum_price_diff_15<- cumsum(stock_buy_15$price_diff_15)
stock_buy_15$sum_price_diff_20<- cumsum(stock_buy_15$price_diff_20)


stock_buy_15 %>%
  ggplot(aes(x=date))+
  geom_line(aes(y = target_5), color = "darkred") + 
  geom_line(aes(y =target_10), color="steelblue", linetype="twodash")+
  geom_line(aes(y = target_15), color = "darkgreen")+
  geom_line(aes(y = target_20), color = "yellow")+
  ggtitle(close_price)


stock_buy_15 %>%
  ggplot(aes(x=date))+
  geom_line(aes(y = sum_target_5), color = "darkred") + 
  geom_line(aes(y =sum_target_10), color="steelblue", linetype="twodash")+
  geom_line(aes(y = sum_target_15), color = "darkgreen")+
  geom_line(aes(y = sum_target_20), color = "yellow")+
  ggtitle(close_price)

stock_buy_15 %>%
  ggplot(aes(x=date))+
  geom_line(aes(y = sum_price_diff_5), color = "darkred") + 
  geom_line(aes(y =sum_price_diff_10), color="steelblue", linetype="twodash")+
  geom_line(aes(y = sum_price_diff_15), color = "darkgreen")+
  geom_line(aes(y = sum_price_diff_20), color = "yellow")+
  ggtitle(close_price)



stock_buy_15$price_and_investment_15 <- (5 * (stock_buy_15$target_15/100))
stock_buy_15$price_and_investment_5 <- (20 * (stock_buy_15$target_5/100))
stock_buy_15$price_and_investment_10 <- (10 * (stock_buy_15$target_10/100))
stock_buy_15$price_and_investment_20 <- (2.5 * (stock_buy_15$target_20/100))
sum(stock_buy_15$price_and_investment_5)
sum(stock_buy_15$price_and_investment_10)
sum(stock_buy_15$price_and_investment_15)
sum(stock_buy_15$price_and_investment_20)

stock_buy_15 %>%
  ggplot(aes(x=date))+
  geom_line(aes(y = cumsum(price_and_investment_5)), color = "darkred") + 
  geom_line(aes(y =cumsum(price_and_investment_10)), color="steelblue", linetype="twodash")+
  geom_line(aes(y = cumsum(price_and_investment_15)), color = "darkgreen")+
  geom_line(aes(y = cumsum(price_and_investment_20)), color = "yellow")




stock_train_20 <- train%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume,  return_lag1,
            return_daily, close_shift_5, target_5,target_direction_5,  close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15, target_direction_15,close_shift_20,
            target_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar,max_high,min_low,close_month,open_month,month_yr,pp,s1,r1,s2,r2,s3,r3)) 

stock_test_20 <- test%>%
  ungroup()%>%
  select(-c(symbol,date, open, high, low, close, volume,  return_lag1,
            return_daily, close_shift_5, target_5,target_direction_5,  close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15, target_direction_15,close_shift_20,
            target_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar,max_high,min_low,close_month,open_month,month_yr,pp,s1,r1,s2,r2,s3,r3))

stock_train_20$target_direction_20<- if_else(stock_train_20$target_direction_20 == 1, "buy","sell")

stock_train_20$target_direction_20 <- factor(stock_train_20$target_direction_20, levels=c("buy","sell"))

stock_test_20$target_direction_20<- if_else(stock_test_20$target_direction_20 == 1, "buy","sell")

stock_test_20$target_direction_20 <- factor(stock_test_20$target_direction_20, levels=c("buy","sell"))


#stock_recipe_20 <- recipe(target_direction_20~., data = stock_train_20)

cores <-8

rf_mod_20 <- 
  rand_forest( min_n = 2,trees = 750) %>% 
  set_engine("ranger",importance = "impurity", num.threads = cores) %>% 
  set_mode("classification")%>%
  fit(target_direction_20~., data = stock_train_20)


saveRDS(rf_mod_20,"SP500_rfml_trendpick_20.rds")

library(vip)
rf_mod_20 %>%
  vip(geom = "point",num_features = 50)

test_pred_20_rf <- predict(rf_mod_20, stock_test_20, type = "prob") %>%
  bind_cols(predict(rf_mod_20, stock_test_20)) %>%
  bind_cols(select(stock_test_20, target_direction_20)) %>%
  glimpse()


test_pred_20_rf %>%
  sens(truth = as.factor(target_direction_20),estimate = .pred_class)

test_pred_20_rf%>%
  spec(truth = target_direction_20,estimate = .pred_class)

test_pred_20_rf%>%
  accuracy(truth = target_direction_20,estimate = .pred_class)

test_pred_20_rf%>%
  precision(truth = target_direction_20,estimate = .pred_class)

test_pred_20_rf%>%
  recall(truth = target_direction_20,estimate = .pred_class)

test_pred_20_rf%>%
  ppv(truth = target_direction_20,estimate = .pred_class)

test_pred_20_rf%>%
  npv(truth = target_direction_20,estimate = .pred_class)

test_pred_20_rf %>%
  roc_auc(truth = target_direction_20,estimate = .pred_buy)

test_pred_20_rf %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_20), 
               alpha = 0.5)+
  ggtitle("Random Forest target 15 day ")+
  xlab("Probability of Yes")


stock_pred_20_rf_symbol  <- bind_cols(test_pred_20_rf, 
                                      select(test, date,
                                             symbol, close,target_10,target_direction_10,
                                             target_5,target_direction_5, target_15,target_direction_15, 
                                             close_shift_15, close_shift_5, close_shift_20, 
                                             close_shift_15, close_shift_10,
                                             target_20,target_direction_20))%>% na.omit()

close_price <- 135

stock_buy_15 <-stock_pred_20_rf_symbol %>%
  group_by(date)%>%
  filter(date >= "2021-01-01" )%>%
  slice_max(.pred_buy, n=20)

stock_buy_15$price_diff_5 <- stock_buy_15$close_shift_5 -  stock_buy_15$close
stock_buy_15$price_diff_10 <- stock_buy_15$close_shift_10 -  stock_buy_15$close
stock_buy_15$price_diff_15 <- stock_buy_15$close_shift_15 -  stock_buy_15$close
stock_buy_15$price_diff_20 <- stock_buy_15$close_shift_20 -  stock_buy_15$close

sum(stock_buy_15$target_5)
sum(stock_buy_15$target_10)
sum(stock_buy_15$target_15)
sum(stock_buy_15$target_20)

sum(stock_buy_15$price_diff_5)
sum(stock_buy_15$price_diff_10)
sum(stock_buy_15$price_diff_15)
sum(stock_buy_15$price_diff_20)


stock_buy_15$sum_target_5<- cumsum(stock_buy_15$target_5)
stock_buy_15$sum_target_10<- cumsum(stock_buy_15$target_10) 
stock_buy_15$sum_target_15<- cumsum(stock_buy_15$target_15)
stock_buy_15$sum_target_20<- cumsum(stock_buy_15$target_20)

stock_buy_15$sum_price_diff_5<- cumsum(stock_buy_15$price_diff_5)
stock_buy_15$sum_price_diff_10<- cumsum(stock_buy_15$price_diff_10)
stock_buy_15$sum_price_diff_15<- cumsum(stock_buy_15$price_diff_15)
stock_buy_15$sum_price_diff_20<- cumsum(stock_buy_15$price_diff_20)


stock_buy_15 %>%
  ggplot(aes(x=date))+
  geom_line(aes(y = target_5), color = "darkred") + 
  geom_line(aes(y =target_10), color="steelblue", linetype="twodash")+
  geom_line(aes(y = target_15), color = "darkgreen")+
  geom_line(aes(y = target_20), color = "yellow")+
  ggtitle(close_price)


stock_buy_15 %>%
  ggplot(aes(x=date))+
  geom_line(aes(y = sum_target_5), color = "darkred") + 
  geom_line(aes(y =sum_target_10), color="steelblue", linetype="twodash")+
  geom_line(aes(y = sum_target_15), color = "darkgreen")+
  geom_line(aes(y = sum_target_20), color = "yellow")+
  ggtitle(close_price)

stock_buy_15 %>%
  ggplot(aes(x=date))+
  geom_line(aes(y = sum_price_diff_5), color = "darkred") + 
  geom_line(aes(y =sum_price_diff_10), color="steelblue", linetype="twodash")+
  geom_line(aes(y = sum_price_diff_15), color = "darkgreen")+
  geom_line(aes(y = sum_price_diff_20), color = "yellow")+
  ggtitle(close_price)



stock_buy_15$price_and_investment_15 <- (5 * (stock_buy_15$target_15/100))
stock_buy_15$price_and_investment_5 <- (20 * (stock_buy_15$target_5/100))
stock_buy_15$price_and_investment_10 <- (10 * (stock_buy_15$target_10/100))
stock_buy_15$price_and_investment_20 <- (5 * (stock_buy_15$target_20/100))
sum(stock_buy_15$price_and_investment_5)
sum(stock_buy_15$price_and_investment_10)
sum(stock_buy_15$price_and_investment_15)
sum(stock_buy_15$price_and_investment_20)

stock_buy_15 %>%
  ggplot(aes(x=date))+
  geom_line(aes(y = cumsum(price_and_investment_5)), color = "darkred") + 
  geom_line(aes(y =cumsum(price_and_investment_10)), color="steelblue", linetype="twodash")+
  geom_line(aes(y = cumsum(price_and_investment_15)), color = "darkgreen")+
  geom_line(aes(y = cumsum(price_and_investment_20)), color = "yellow")





xgb_spec <- boost_tree(
  trees = tune(), 
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune(),                         ## step size
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

xgb_spec

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), stock_train_20),
  learn_rate(),
  trees(),
  size = 30
)

xgb_grid

xgb_wf <- workflow() %>%
  add_formula(target_direction_20 ~ .) %>%
  add_model(xgb_spec)

xgb_wf

set.seed(123)
vb_folds <- vfold_cv(stock_train_20, strata = target_direction_20, n=2)

vb_folds

library(doParallel)

all_cores <- parallel::detectCores(logical = FALSE)
cl <- parallel::makePSOCKcluster(4)
registerDoParallel(cl)

set.seed(234)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = vb_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

xgb_res

collect_metrics(xgb_res)

xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

show_best(xgb_res, "roc_auc")

best_auc <- select_best(xgb_res, "roc_auc")
best_auc

final_xgb <- finalize_workflow(
  xgb_wf,
  best_auc
)

final_xgb

library(vip)

final_xgb %>%
  fit(data = vb_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

train_fit_gb <- fit(final_xgb,stock_train_20)

train_fit_gb%>%
  pull_workflow_fit() %>%
  vip(geom = "point")

test_pred_20_gb <- predict(train_fit_gb, stock_test_20, type = "prob") %>%
  bind_cols(predict(train_fit_gb, stock_test_20)) %>%
  bind_cols(select(stock_test_20, target_direction_20)) %>%
  glimpse()


test_pred_20_gb %>%
  sens(truth = as.factor(target_direction_20),estimate = .pred_class)

test_pred_20_gb%>%
  spec(truth = target_direction_20,estimate = .pred_class)

test_pred_20_gb%>%
  accuracy(truth = target_direction_20,estimate = .pred_class)

test_pred_20_gb%>%
  precision(truth = target_direction_20,estimate = .pred_class)

test_pred_20_gb%>%
  recall(truth = target_direction_20,estimate = .pred_class)

test_pred_20_gb%>%
  ppv(truth = target_direction_20,estimate = .pred_class)

test_pred_20_gb%>%
  npv(truth = target_direction_20,estimate = .pred_class)

test_pred_20_gb %>%
  roc_auc(truth = target_direction_20,estimate = .pred_buy)

test_pred_20_gb %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_20), 
               alpha = 0.5)+
  ggtitle("Random Forest target 20 day ")+
  xlab("Probability of Yes")


start_loop_date <- stock_pred_20_rf_symbol$date

#to start the loop
stock_calls = list()
stock_puts = list()
dat= data.frame()
for (symbol in full_index$symbol) {
  # ... make some data
  tryCatch({
    dat <- getOptionChain(Symbols = symbol, Exp = "2022-03-25")
    dat$symbol <- symbol  # maybe you want to keep track of which iteration produced it?
    row_call <- rownames_to_column(dat$calls, var= "stock_date")
    stock_calls[[symbol]] <- row_call # add it to your list
  },
  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


 start_loop_date <- stock_pred_20_rf_symbol%>%
   distinct(date)%>%
   filter(date >"2021-01-04")
stock_pick <- c()
stock_pick_date <- c()

stock_pick_date <-stock_pred_20_rf_symbol %>%
  filter(date == "2021-01-04")%>%
  slice_max(.pred_buy, n=10)%>%
  select(symbol,date,target_20)

stock_pick_date$time_diff <- rep(0)

#date <- as.Date("2021-01-05")

for (pick in start_loop_date$date){
  stock_buy_20 <-stock_pred_20_rf_symbol %>%
    filter(date == pick)%>%
    slice_max(.pred_buy, n=300)%>%
    select(symbol,date,target_20)
    
    stock_pick <- anti_join(x = stock_buy_20, y=stock_pick_date%>%
                              filter(time_diff <=26), by="symbol" )
    
    stock_pick <- stock_pick %>%
      slice_head( n=10)
    
    stock_pick$time_diff <- stock_pick$date - as.Date(pick)
    #stock_pick$time_diff <- stock_pick$date - as.Date("2021-01-05")
    stock_pick_date$time_diff <-  as.Date(pick)- stock_pick_date$date
    stock_pick_date <- rbind(stock_pick_date, stock_pick)
    
    
    
    ##stock_pick_date  <- stock_pick_date%>%
    ##  filter(time_diff <=26)
    
    ###15 trading days is 19  calendar days
    
    
    
}
 
look <- stock_pick_date %>% count(date)
sum(stock_pick_date$target_20) 
stock_pick_date$price_and_investment_20 <- (5 * (stock_pick_date$target_20/100))
sum(stock_pick_date$price_and_investment_20) 


stock_pick_date$price_and_investment_15 <- (5 * (stock_pick_date$target_15/100))
stock_pick_date$price_and_investment_5 <- (20 * (stock_pick_date$target_5/100))
stock_pick_date$price_and_investment_10 <- (10 * (stock_pick_date$target_10/100))
stock_pick_date$price_and_investment_20 <- (5 * (stock_pick_date$target_20/100))
sum(stock_pick_date$price_and_investment_5)
sum(stock_pick_date$price_and_investment_10)
sum(stock_pick_date$price_and_investment_15)
sum(stock_pick_date$price_and_investment_20)

stock_pick_date %>%
  ggplot(aes(x=date))+
  geom_line(aes(y = cumsum(price_and_investment_20)), color = "yellow")

stock_pick_date$day <- wday(stock_pick_date$date, label = T)

test1 <- stock_pick_date%>%
  filter(day =='Fri')

sum(test1$price_and_investment_20) 

test1%>%
  ggplot(aes(x=date))+
  geom_line(aes(y = cumsum(price_and_investment_20)), color = "green")

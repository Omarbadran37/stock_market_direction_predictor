library(tidyverse)
library(tidyquant)
library(tidymodels)
library(randomForest)
library(xgboost)
library(ranger)

rm(list = ls())

start_date <- "2000-01-01"
start_date <- Sys.Date()-365

sp_500 <- tq_index("SP500") %>%
  filter(sector == "Information Technology")%>%
  tq_get(get="stock.prices",
         from = start_date)

sp_500 <- sp_500 %>%
  filter(sector == "Information Technology")

# sp_500 <- tq_get(test$symbol, get="stock.prices",
#                  from = start_date)

sp_500_1 <- sp_500 %>%
  select(symbol,date,open,high,low,close,volume)

sp_500_1 <- sp_500_1 %>%
  filter(!symbol %in% c("CEG","OGN","AMCR","TTWO","CTVA","DOW","EMBC","ELV"))

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
  mutate(close_shift_3 = lead(close, n=3))

sp_500_1 <- sp_500_1%>%
  mutate(target_3 = (close_shift_3-lead(open))/(lead(open))*100)

sp_500_1 <- sp_500_1 %>%
  mutate(target_direction_3 = if_else(target_3>0,1,0))

# sp_500_1 <- sp_500_1 %>%
#   mutate(target_direction_5 = if_else(target_5>0.1,1,0))

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

# 
blue <-   function(x){
  sp_500_1%>%
    mutate(x = ((lead(close, n =x))-lead(open))/(lead(open))*100)
}

for (i in 1:20){
  test<-blue(i)
  sp_500_1[,ncol(sp_500_1)+1]<- test$x
  colnames(sp_500_1)[ncol(sp_500_1)]<-paste0("lead_",i)



}

buy <- 5
sell <- -5


sp_500_1<- sp_500_1 %>%
  mutate( t1=
            case_when(
              lead_1 >=buy ~1,
              lead_2 >=buy ~2,
              lead_3 >=buy ~3,
              lead_4 >=buy ~4,
              lead_5 >=buy ~5,
              lead_6 >=buy ~6,
              lead_7 >=buy ~7,
              lead_8 >=buy ~8,
              lead_9 >=buy ~9,
              lead_10 >=buy~10,
              lead_11 >=buy~11,
              lead_12 >=buy~12,
              lead_13 >=buy~13,
              lead_14 >=buy~14,
              lead_15 >=buy~15,
              lead_16 >=buy~16,
              lead_17 >=buy~17,
              lead_18 >=buy~18,
              lead_19 >=buy~19,
              lead_20 >=buy~20,
              TRUE ~0
            )
  )

sp_500_1 <- sp_500_1 %>%
  mutate( t2=
            case_when(
              lead_1 <=sell ~1,
              lead_2 <=sell ~2,
              lead_3 <=sell ~3,
              lead_4 <=sell ~4,
              lead_5 <=sell ~5,
              lead_6 <=sell ~6,
              lead_7 <=sell ~7,
              lead_8 <=sell~8,
              lead_9 <=sell ~9,
              lead_10 <=sell~10,
              lead_11 <=sell~11,
              lead_12 <=sell~12,
              lead_13 <=sell~13,
              lead_14 <=sell~14,
              lead_15 <=sell~15,
              lead_16<=sell~16,
              lead_17 <=sell~17,
              lead_18 <=sell~18,
              lead_19 <=sell~19,
              lead_20 <=sell~20,
              TRUE ~0
            )
  )

# sp_500_1$t3 <- if_else(sp_500_1$t1==0 & sp_500_1$t2==0,
#                        if_else(sp_500_1$lead_20 >=0,1,0),0)

sp_500_1 <- sp_500_1%>%
  mutate(t4 = case_when(
    t1==0 & t2!=0 ~0,
    t1!=0 & t2==0~1,
    t1 < t2 ~1,
    t2 < t1~0,
    TRUE ~1
  )
  )



# 
# test <- sp_500_1 
# 
# test$month_yr <- format_ISO8601(test$date, precision = "ym")
# test_1 <-test %>%
#   group_by(month_yr,symbol)%>%
#   summarise(max_high =max(high),min_low = min(low))
# 
# 
# test_2 <-test %>%
#   group_by(month_yr,symbol)%>%
#   slice_min(date)%>%
#   ungroup(month_yr,symbol)%>%
#   select(open_month=open)
# 
# 
# test_3 <- test %>%
#   group_by(month_yr,symbol)%>%
#   slice_max(date)%>%
#   ungroup(month_yr,symbol)%>%
#   select( close_month =close)
# 
# test_4 <- bind_cols(test_1,test_2,test_3)
# 
# 
# test_4 <- test_4 %>%
#   ungroup(symbol,month_yr)
# test_4 <- test_4 %>%
#   group_by(symbol)
# 
# 
# 
# test_4 <- test_4 %>%
#   group_by(symbol)%>%
#   mutate(pp = (lag(max_high) + lag(min_low) + lag(close_month))/3,
#          r1 = (2*lag(pp)) -lag(min_low),
#          s1 = (2*lag(pp)) - lag(max_high),
#          r2 = lag(pp) + (lag(max_high)-lag(min_low)),
#          s2 = lag(pp) - (lag(max_high)-lag(min_low)),
#          r3 = lag(max_high) + 2*(lag(pp)-lag(min_low)),
#          s3 = lag(min_low) - 2*(lag(max_high)-lag(pp)))%>%
#   arrange(symbol)
# 
# sp_500_1$month_yr <- format_ISO8601(sp_500_1$date, precision = "ym")
# 
# sp_500_1 <- left_join(x=sp_500_1,y=test_4, by=c("symbol","month_yr"))%>%
#   arrange(symbol)
# 
# sp_500_1 <- sp_500_1 %>%
#   group_by(symbol)%>%
#   mutate(close_r1 = close/r1,
#          close_r2 = close/r2,
#          close_r3 = close/r3,
#          close_s1 = close/s1,
#          close_s2 = close/s2,
#          close_s3 = close/s3,
#          close_pp = close/pp)
# 
# rm(test)
# rm(test_1)
# rm(test_2)
# rm(test_3)
# rm(test_4)

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
# 
# sp_500_1$adx_trend_3<- sp_500_1$ADX/lag(sp_500_1$ADX,n=3)
# sp_500_1$adx_trend_5<- sp_500_1$ADX/lag(sp_500_1$ADX,n=5)
# sp_500_1$adx_trend_10<- sp_500_1$ADX/lag(sp_500_1$ADX,n=10)
# sp_500_1$adx_trend_15<- sp_500_1$ADX/lag(sp_500_1$ADX,n=15)
# sp_500_1$adx_trend_20<- sp_500_1$ADX/lag(sp_500_1$ADX,n=20)
# 
# sp_500_1$DIp_trend_3 <- sp_500_1$DIp/lag(sp_500_1$DIp,n=3)
# sp_500_1$DIp_trend_5 <- sp_500_1$DIp/lag(sp_500_1$DIp,n=5)
# sp_500_1$DIp_trend_10 <- sp_500_1$DIp/lag(sp_500_1$DIp,n=10)
# sp_500_1$DIp_trend_15 <- sp_500_1$DIp/lag(sp_500_1$DIp,n=15)
# sp_500_1$DIp_trend_20 <- sp_500_1$DIp/lag(sp_500_1$DIp,n=20)
# 
# sp_500_1$DIn_trend_3 <- sp_500_1$DIn/lag(sp_500_1$DIn,n=3)
# sp_500_1$DIn_trend_5 <- sp_500_1$DIn/lag(sp_500_1$DIn,n=5)
# sp_500_1$DIn_trend_10 <- sp_500_1$DIn/lag(sp_500_1$DIn,n=10)
# sp_500_1$DIn_trend_15<- sp_500_1$DIn/lag(sp_500_1$DIn,n=15)
# sp_500_1$DIn_trend_20 <- sp_500_1$DIn/lag(sp_500_1$DIn,n=20)

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

cores <- 8



train <- sp_500_1 %>%
  filter(date < "2021-11-01")%>%
  na.omit()

test <- sp_500_1 %>%
  filter(date > "2021-11-01")%>%
  na.omit()

# this is for using the s and r
# stock_train <- train%>%
#   ungroup(symbol)%>%
#   select(-c(symbol,date, open, high, low, close, volume, return_lag1,
#             return_daily, lead_1:lead_20,t1:t2, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
#             dn,mavg,up,sar,max_high,min_low,close_month,open_month,month_yr,pp,s1,r1,s2,r2,s3,r3))%>%
#   na.omit()

# stock_train <- train%>%
#   ungroup(symbol)%>%
#   select(-c(symbol,date, open, high, low, close, volume, return_lag1,
#             return_daily, lead_1:lead_20,t1:t2, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
#             dn,mavg,up,sar))%>%
#   na.omit()
# 
# 
# # use this for using the s and r
# # stock_test <- test%>%
# #   ungroup(symbol)%>%
# #   select(-c(symbol,date, open, high, low, close, volume, return_lag1,
# #             return_daily, lead_1:lead_20,t1:t2, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
# #             dn,mavg,up,sar,max_high,min_low,close_month,open_month,month_yr,pp,s1,r1,s2,r2,s3,r3))%>%
# #   na.omit()
# 
# stock_test <- test%>%
#   ungroup(symbol)%>%
#   select(-c(symbol,date, open, high, low, close, volume, return_lag1,
#             return_daily, lead_1:lead_20,t1:t2, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
#             dn,mavg,up,sar))%>%
#   na.omit()
# 
# 
# stock_train$t4<- if_else(stock_train$t4 == 1, "buy","sell")
# 
# stock_train$t4 <- factor(stock_train$t4, levels=c("buy","sell"))
# 
# stock_test$t4<- if_else(stock_test$t4 == 1, "buy","sell")
# 
# stock_test$t4 <- factor(stock_test$t4, levels=c("buy","sell"))
# 
# rf_tech <- 
#   rand_forest( min_n = 2, trees =750) %>%
#   set_engine("ranger",importance = "impurity", num.threads = cores) %>%
#   set_mode("classification")%>%
#   fit(t4~., data = stock_train)
# 
# library(vip)
# rf_tech %>%
#   vip(geom = "point",num_features = 50)
# 
# test_pred_3_rf <- predict(rf_tech, stock_test, type = "prob") %>%
#   bind_cols(predict(rf_tech, stock_test)) %>%
#   bind_cols(select(stock_test, t4)) %>%
#   glimpse()
# 
# test_pred_3_rf %>%
#   sens(truth = as.factor(t4),estimate = .pred_class)
# 
# test_pred_3_rf%>%
#   spec(truth = t4,estimate = .pred_class)
# 
# test_pred_3_rf%>%
#   accuracy(truth = t4,estimate = .pred_class)
# 
# test_pred_3_rf%>%
#   precision(truth = t4,estimate = .pred_class)
# 
# test_pred_3_rf%>%
#   recall(truth = t4,estimate = .pred_class)
# 
# test_pred_3_rf%>%
#   ppv(truth = t4,estimate = .pred_class)
# 
# test_pred_3_rf%>%
#   npv(truth = t4,estimate = .pred_class)
# 
# test_pred_3_rf %>%
#   roc_auc(truth = t4,estimate = .pred_buy)
# 
# test_pred_3_rf %>%
#   ggplot() +
#   geom_density(aes(x = .pred_buy, fill = t4), 
#                alpha = 0.5)+
#   ggtitle("Random Forest target 3 day ")+
#   xlab("Probability of Yes")


stock_train_3 <- train%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily, close_shift_3, target_3, close_shift_5, target_5,target_direction_5, close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15,target_direction_15,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar))

stock_test_3 <- test%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily, close_shift_3, target_3,close_shift_5, target_5,target_direction_5, close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15,target_direction_15,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar))

stock_train_3$target_direction_3<- if_else(stock_train_3$target_direction_3 == 1, "buy","sell")

stock_train_3$target_direction_3 <- factor(stock_train_3$target_direction_3, levels=c("buy","sell"))

stock_test_3$target_direction_3<- if_else(stock_test_3$target_direction_3 == 1, "buy","sell")

stock_test_3$target_direction_3 <- factor(stock_test_3$target_direction_3, levels=c("buy","sell"))

# stock_train_15$target_direction_15 <- factor(stock_train_15$target_direction_15, levels=c("buy","neutral","sell"))
# 
# glimpse(stock_train_15$target_direction_15)

# remove(sp_500_1)
# remove(sp_500)

# rf_grid <- expand.grid(
#   mtry = c(12,16,20,24,26,28)
# )
# 
# rf_grid
# rm(stock_split)
# 
# stock_recipe_3 <- recipe(target_direction_3~., data = stock_train_3)
# cores <- 8
# 
# rf_mod_3 <- 
#   rand_forest( min_n = tune(), mtry = tune(),trees =tune()) %>% 
#   set_engine("ranger",importance = "impurity", num.threads = cores) %>% 
#   set_mode("classification")
# 
# 
# rf_workflow <-
#   workflow() %>%
#   add_model(rf_mod_3) %>%
#   add_recipe(stock_recipe_3)
# 
# rf_workflow
# 
# cv <- vfold_cv(stock_train_3, v = 2)
# 
# set.seed(345)
# rf_res <-
#   rf_workflow %>%
#   tune_grid(cv,
#             control = control_grid(save_pred = TRUE),
#             metrics = metric_set(spec))
# 
# rf_res %>%
#   collect_metrics()%>%
#   arrange(desc(mean))
# 
# rf_res %>%
#   collect_predictions()
# 
# 
# autoplot(rf_res)
# 
# 
# rf_res %>%
#   show_best(metric = "roc_auc")
# 
# rf_res %>% 
#   show_best(metric = "spec")
# 
# rf_best <-
#   rf_res %>%
#   select_best(metric = "spec")
# rf_best
# 
# last_rf_workflow <- finalize_workflow(
#   rf_workflow,
#   rf_best
# )
# 
# 
# train_fit_rf <- fit(last_rf_workflow,stock_train_3)
# 
# train_fit_rf%>%
#   pull_workflow_fit() %>%
#   vip(geom = "point",num_features = 50)
# 
# 

# test_pred_3_rf <- predict(train_fit_rf, stock_test_3, type = "prob") %>%
#   bind_cols(predict(train_fit_rf, stock_test_3)) %>%
#   bind_cols(select(stock_test_3, target_direction_3)) %>%
#   glimpse()
rf_mod_3 <-
  rand_forest( min_n = 2,trees = 750) %>%
  set_engine("ranger",importance = "impurity", num.threads = cores) %>%
  set_mode("classification")%>%
  fit(target_direction_3~., data = stock_train_3)


saveRDS(rf_mod_3,"SP500_rfml_trendpick_3.rds")

library(vip)
rf_mod_3 %>%
  vip(geom = "point",num_features = 50)

test_pred_3_rf <- predict(rf_mod_3, stock_test_3, type = "prob") %>%
  bind_cols(predict(rf_mod_3, stock_test_3)) %>%
  bind_cols(select(stock_test_3, target_direction_3)) %>%
  glimpse()


test_pred_3_rf %>%
  sens(truth = as.factor(target_direction_3),estimate = .pred_class)

test_pred_3_rf%>%
  spec(truth = target_direction_3,estimate = .pred_class)

test_pred_3_rf%>%
  accuracy(truth = target_direction_3,estimate = .pred_class)

test_pred_3_rf%>%
  precision(truth = target_direction_3,estimate = .pred_class)

test_pred_3_rf%>%
  recall(truth = target_direction_3,estimate = .pred_class)

test_pred_3_rf%>%
  ppv(truth = target_direction_3,estimate = .pred_class)

test_pred_3_rf%>%
  npv(truth = target_direction_3,estimate = .pred_class)

test_pred_3_rf %>%
  roc_auc(truth = target_direction_3,estimate = .pred_buy)

test_pred_3_rf %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_3), 
               alpha = 0.5)+
  ggtitle("Random Forest target 3 day ")+
  xlab("Probability of Yes")

rm(rf_mod_3)
rm(stock_train_3)
rm(stock_test_3)

stock_train_5 <- train%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily, close_shift_3, target_3, target_direction_3,close_shift_5, target_5, close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15,target_direction_15,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar))

stock_test_5 <- test%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily, close_shift_3, target_3, target_direction_3,close_shift_5, target_5, close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15,target_direction_15,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar))

stock_train_5$target_direction_5<- if_else(stock_train_5$target_direction_5 == 1, "buy","sell")

stock_train_5$target_direction_5 <- factor(stock_train_5$target_direction_5, levels=c("buy","sell"))

stock_test_5$target_direction_5<- if_else(stock_test_5$target_direction_5 == 1, "buy","sell")

stock_test_5$target_direction_5 <- factor(stock_test_5$target_direction_5, levels=c("buy","sell"))

# stock_recipe_5 <- recipe(target_direction_5~., data = stock_train_5)
# cores <- 8
# 
# rf_mod_5 <- 
#   rand_forest( min_n = tune(), mtry = tune(),trees =tune()) %>% 
#   set_engine("ranger",importance = "impurity", num.threads = cores) %>% 
#   set_mode("classification")
# 
# 
# rf_workflow <-
#   workflow() %>%
#   add_model(rf_mod_5) %>%
#   add_recipe(stock_recipe_5)
# 
# rf_workflow
# 
# cv <- vfold_cv(stock_train_5, v = 2)
# 
# set.seed(345)
# rf_res <-
#   rf_workflow %>%
#   tune_grid(cv,
#             control = control_grid(save_pred = TRUE),
#             metrics = metric_set(spec))
# 
# rf_res %>%
#   collect_metrics()%>%
#   arrange(desc(mean))
# 
# rf_res %>%
#   collect_predictions()
# 
# 
# autoplot(rf_res)
# 
# 
# rf_res %>%
#   show_best(metric = "roc_auc")
# 
# rf_res %>% 
#   show_best(metric = "spec")
# 
# rf_best <-
#   rf_res %>%
#   select_best(metric = "spec")
# rf_best
# 
# last_rf_workflow <- finalize_workflow(
#   rf_workflow,
#   rf_best
# )
# 
# 
# train_fit_rf <- fit(last_rf_workflow,stock_train_5)
# 
# train_fit_rf%>%
#   pull_workflow_fit() %>%
#   vip(geom = "point",num_features = 50)

# test_pred_5_rf <- predict(train_fit_rf, stock_test_5, type = "prob") %>%
#   bind_cols(predict(train_fit_rf, stock_test_5)) %>%
#   bind_cols(select(stock_test_5, target_direction_5)) %>%
#   glimpse()

rf_mod_5 <-
  rand_forest( min_n = 2,trees = 750) %>%
  set_engine("ranger",importance = "impurity", num.threads = cores) %>%
  set_mode("classification")%>%
  fit(target_direction_5~., data = stock_train_5)


saveRDS(rf_mod_5,"SP500_rfml_trendpick_5.rds")

library(vip)
rf_mod_5 %>%
  vip(geom = "point",num_features = 50)

test_pred_5_rf <- predict(rf_mod_5, stock_test_5, type = "prob") %>%
  bind_cols(predict(rf_mod_5, stock_test_5)) %>%
  bind_cols(select(stock_test_5, target_direction_5)) %>%
  glimpse()


test_pred_5_rf %>%
  sens(truth = as.factor(target_direction_5),estimate = .pred_class)

test_pred_5_rf%>%
  spec(truth = target_direction_5,estimate = .pred_class)

test_pred_5_rf%>%
  accuracy(truth = target_direction_5,estimate = .pred_class)

test_pred_5_rf%>%
  precision(truth = target_direction_5,estimate = .pred_class)

test_pred_5_rf%>%
  recall(truth = target_direction_5,estimate = .pred_class)

test_pred_5_rf%>%
  ppv(truth = target_direction_5,estimate = .pred_class)

test_pred_5_rf%>%
  npv(truth = target_direction_5,estimate = .pred_class)

test_pred_5_rf %>%
  roc_auc(truth = target_direction_5,estimate = .pred_buy)

test_pred_5_rf %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_5), 
               alpha = 0.5)+
  ggtitle("Random Forest target 5 day ")+
  xlab("Probability of Yes")

rm(rf_mod_5)
rm(stock_train_5)
rm(stock_test_5)

stock_train_10 <- train%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily, close_shift_3, target_3, target_direction_3,close_shift_5, 
            target_5, target_direction_5, close_shift_10, target_10,
            close_shift_15,target_15,target_direction_15,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar))

stock_test_10 <- test%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily, close_shift_3, target_3, target_direction_3,close_shift_5, target_5, target_direction_5, close_shift_10, target_10,
            close_shift_15,target_15,target_direction_15,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar))%>%
  na.omit()

stock_train_10$target_direction_10<- if_else(stock_train_10$target_direction_10 == 1, "buy","sell")

stock_train_10$target_direction_10 <- factor(stock_train_10$target_direction_10, levels=c("buy","sell"))

stock_test_10$target_direction_10<- if_else(stock_test_10$target_direction_10 == 1, "buy","sell")

stock_test_10$target_direction_10 <- factor(stock_test_10$target_direction_10, levels=c("buy","sell"))

# stock_recipe_10 <- recipe(target_direction_10~., data = stock_train_10)
# cores <- 8
# 
# rf_mod_10 <- 
#   rand_forest( min_n = tune(), mtry = tune(),trees =tune()) %>% 
#   set_engine("ranger",importance = "impurity", num.threads = cores) %>% 
#   set_mode("classification")
# 
# 
# rf_workflow <-
#   workflow() %>%
#   add_model(rf_mod_10) %>%
#   add_recipe(stock_recipe_10)
# 
# rf_workflow
# 
# cv <- vfold_cv(stock_train_10, v = 2)
# 
# set.seed(345)
# rf_res <-
#   rf_workflow %>%
#   tune_grid(cv,
#             control = control_grid(save_pred = TRUE),
#             metrics = metric_set(spec))
# 
# rf_res %>%
#   collect_metrics()%>%
#   arrange(desc(mean))
# 
# rf_res %>%
#   collect_predictions()
# 
# 
# autoplot(rf_res)
# 
# 
# rf_res %>%
#   show_best(metric = "roc_auc")
# 
# rf_res %>% 
#   show_best(metric = "spec")
# 
# rf_best <-
#   rf_res %>%
#   select_best(metric = "spec")
# rf_best
# 
# last_rf_workflow <- finalize_workflow(
#   rf_workflow,
#   rf_best
# )
# 
# 
# train_fit_rf <- fit(last_rf_workflow,stock_train_10)
# 
# train_fit_rf%>%
#   pull_workflow_fit() %>%
#   vip(geom = "point",num_features = 50)

# test_pred_10_rf <- predict(train_fit_rf, stock_test_10, type = "prob") %>%
#   bind_cols(predict(train_fit_rf, stock_test_10)) %>%
#   bind_cols(select(stock_test_10, target_direction_10)) %>%
#   glimpse()

rf_mod_10 <-
  rand_forest( min_n = 2,trees = 750) %>%
  set_engine("ranger",importance = "impurity", num.threads = cores) %>%
  set_mode("classification")%>%
  fit(target_direction_10~., data = stock_train_10)


saveRDS(rf_mod_10,"SP500_rfml_trendpick_10.rds")

library(vip)
rf_mod_10 %>%
  vip(geom = "point",num_features = 50)

test_pred_10_rf <- predict(rf_mod_10, stock_test_10, type = "prob") %>%
  bind_cols(predict(rf_mod_10, stock_test_10)) %>%
  bind_cols(select(stock_test_10, target_direction_10)) %>%
  glimpse()


test_pred_10_rf %>%
  sens(truth = as.factor(target_direction_10),estimate = .pred_class)

test_pred_10_rf%>%
  spec(truth = target_direction_10,estimate = .pred_class)

test_pred_10_rf%>%
  accuracy(truth = target_direction_10,estimate = .pred_class)

test_pred_10_rf%>%
  precision(truth = target_direction_10,estimate = .pred_class)

test_pred_10_rf%>%
  recall(truth = target_direction_10,estimate = .pred_class)

test_pred_10_rf%>%
  ppv(truth = target_direction_10,estimate = .pred_class)

test_pred_10_rf%>%
  npv(truth = target_direction_10,estimate = .pred_class)

test_pred_10_rf %>%
  roc_auc(truth = target_direction_10,estimate = .pred_buy)

test_pred_10_rf %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_10), 
               alpha = 0.5)+
  ggtitle("Random Forest target 10 day ")+
  xlab("Probability of Yes")

rm(rf_mod_10)
rm(stock_train_10)
rm(stock_test_10)

stock_train_15 <- train%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily,close_shift_3, target_3, target_direction_3, close_shift_5, target_5,target_direction_5, close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar))

stock_test_15 <- test%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily,close_shift_3, target_3, target_direction_3, close_shift_5, target_5,target_direction_5, close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar))

stock_train_15$target_direction_15<- if_else(stock_train_15$target_direction_15 == 1, "buy","sell")

stock_train_15$target_direction_15 <- factor(stock_train_15$target_direction_15, levels=c("buy","sell"))

stock_test_15$target_direction_15<- if_else(stock_test_15$target_direction_15 == 1, "buy","sell")

stock_test_15$target_direction_15 <- factor(stock_test_15$target_direction_15, levels=c("buy","sell"))

# stock_recipe_15 <- recipe(target_direction_15~., data = stock_train_15)
# cores <- 8
# 
# rf_mod_15 <- 
#   rand_forest( min_n = tune(), mtry = tune(),trees =tune()) %>% 
#   set_engine("ranger",importance = "impurity", num.threads = cores) %>% 
#   set_mode("classification")
# 
# 
# rf_workflow <-
#   workflow() %>%
#   add_model(rf_mod_15) %>%
#   add_recipe(stock_recipe_15)
# 
# rf_workflow
# 
# cv <- vfold_cv(stock_train_15, v = 2)
# 
# set.seed(345)
# rf_res <-
#   rf_workflow %>%
#   tune_grid(cv,
#             control = control_grid(save_pred = TRUE),
#             metrics = metric_set(spec))
# 
# rf_res %>%
#   collect_metrics()%>%
#   arrange(desc(mean))
# 
# rf_res %>%
#   collect_predictions()
# 
# 
# autoplot(rf_res)
# 
# 
# rf_res %>%
#   show_best(metric = "roc_auc")
# 
# rf_res %>% 
#   show_best(metric = "spec")
# 
# rf_best <-
#   rf_res %>%
#   select_best(metric = "spec")
# rf_best
# 
# last_rf_workflow <- finalize_workflow(
#   rf_workflow,
#   rf_best
# )
# 
# 
# train_fit_rf <- fit(last_rf_workflow,stock_train_15)
# 
# train_fit_rf%>%
#   pull_workflow_fit() %>%
#   vip(geom = "point",num_features = 50)

# test_pred_15_rf <-  predict(train_fit_rf, stock_test_15, type = "prob")%>%
#   bind_cols(predict(train_fit_rf, stock_test_15)) %>%
#   bind_cols(select(stock_test_15, target_direction_15)) %>%
#   glimpse()

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

rm(rf_mod_15)
rm(stock_train_15)
rm(stock_test_15)


stock_train_20 <- train%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume,  return_lag1,
            return_daily,close_shift_3, target_3, target_direction_3, close_shift_5, target_5,target_direction_5,  close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15, target_direction_15,close_shift_20,
            target_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar)) 

stock_test_20 <- test%>%
  ungroup()%>%
  select(-c(symbol,date, open, high, low, close, volume,  return_lag1,
            return_daily,close_shift_3, target_3, target_direction_3, close_shift_5, target_5,target_direction_5,  close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15, target_direction_15,close_shift_20,
            target_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar))%>%
  na.omit()

# stock_test_20 <- train%>%
#   ungroup()%>%
#   select(-c(symbol,date, open, high, low, close, volume,  return_lag1,
#             return_daily,close_shift_3, target_3, target_direction_3, close_shift_5, target_5,target_direction_5,  close_shift_10, target_10,
#             target_direction_10, close_shift_15,target_15, target_direction_15,close_shift_20,
#             target_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
#             dn,mavg,up,sar,max_high,min_low,close_month,open_month,month_yr,pp,s1,r1,s2,r2,s3,r3))

stock_train_20$target_direction_20<- if_else(stock_train_20$target_direction_20 == 1, "buy","sell")

stock_train_20$target_direction_20 <- factor(stock_train_20$target_direction_20, levels=c("buy","sell"))

stock_test_20$target_direction_20<- if_else(stock_test_20$target_direction_20 == 1, "buy","sell")

stock_test_20$target_direction_20 <- factor(stock_test_20$target_direction_20, levels=c("buy","sell"))

# stock_recipe_20 <- recipe(target_direction_20~., data = stock_train_20)
# cores <- 8
# 
# rf_mod_20 <- 
#   rand_forest( min_n = tune(), mtry = tune(),trees =tune()) %>% 
#   set_engine("ranger",importance = "impurity", num.threads = cores) %>% 
#   set_mode("classification")
# 
# 
# rf_workflow <-
#   workflow() %>%
#   add_model(rf_mod_20) %>%
#   add_recipe(stock_recipe_20)
# 
# rf_workflow
# 
# cv <- vfold_cv(stock_train_20, v = 2)
# 
# set.seed(345)
# rf_res <-
#   rf_workflow %>%
#   tune_grid(cv,
#             control = control_grid(save_pred = TRUE),
#             metrics = metric_set(spec))
# 
# rf_res %>%
#   collect_metrics()%>%
#   arrange(desc(mean))
# 
# rf_res %>%
#   collect_predictions()
# 
# 
# autoplot(rf_res)
# 
# 
# rf_res %>%
#   show_best(metric = "roc_auc")
# 
# rf_res %>% 
#   show_best(metric = "spec")
# 
# rf_best <-
#   rf_res %>%
#   select_best(metric = "spec")
# rf_best
# 
# last_rf_workflow <- finalize_workflow(
#   rf_workflow,
#   rf_best
# )
# 
# 
# train_fit_rf <- fit(last_rf_workflow,stock_train_20)
# 
# train_fit_rf%>%
#   pull_workflow_fit() %>%
#   vip(geom = "point",num_features = 50)

rf_mod_20 <-
  rand_forest( min_n = 2,trees = 750) %>%
  set_engine("ranger",importance = "impurity", num.threads = cores) %>%
  set_mode("classification")%>%
  fit(target_direction_20~., data = stock_train_20)

# test_pred_20_rf <- predict(train_fit_rf, stock_test_20, type = "prob") %>%
#   bind_cols(predict(train_fit_rf, stock_test_20)) %>%
#   bind_cols(select(stock_test_20, target_direction_20)) %>%
#   glimpse()


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
  ggtitle("Random Forest target 20 day ")+
  xlab("Probability of Yes")

rm(rf_mod_20)
rm(stock_train_20)
rm(stock_test_20)

# blue <-   function(x){
#   sp_500_1%>%
#     mutate(x = ((lead(close, n =x))-lead(open))/(lead(open))*100)
# }
# 
# for (i in 1:20){
#   test<-blue(i)
#   sp_500_1[,ncol(sp_500_1)+1]<- test$x
#   colnames(sp_500_1)[ncol(sp_500_1)]<-paste0("lead_",i)
#   
#   
#   
# }
# 
# buy <- 5
# sell <- -5
# 
# 
# sp_500_1<- sp_500_1 %>%
#   mutate( t1=
#             case_when(
#               lead_1 >=buy ~1,
#               lead_2 >=buy ~2,
#               lead_3 >=buy ~3,
#               lead_4 >=buy ~4,
#               lead_5 >=buy ~5,
#               lead_6 >=buy ~6,
#               lead_7 >=buy ~7,
#               lead_8 >=buy ~8,
#               lead_9 >=buy ~9,
#               lead_10 >=buy~10,
#               lead_11 >=buy~11,
#               lead_12 >=buy~12,
#               lead_13 >=buy~13,
#               lead_14 >=buy~14,
#               lead_15 >=buy~15,
#               lead_16 >=buy~16,
#               lead_17 >=buy~17,
#               lead_18 >=buy~18,
#               lead_19 >=buy~19,
#               lead_20 >=buy~20,
#               TRUE ~0
#             )
#   )
# 
# sp_500_1 <- sp_500_1 %>%
#   mutate( t2=
#             case_when(
#               lead_1 <=sell ~1,
#               lead_2 <=sell ~2,
#               lead_3 <=sell ~3,
#               lead_4 <=sell ~4,
#               lead_5 <=sell ~5,
#               lead_6 <=sell ~6,
#               lead_7 <=sell ~7,
#               lead_8 <=sell~8,
#               lead_9 <=sell ~9,
#               lead_10 <=sell~10,
#               lead_11 <=sell~11,
#               lead_12 <=sell~12,
#               lead_13 <=sell~13,
#               lead_14 <=sell~14,
#               lead_15 <=sell~15,
#               lead_16<=sell~16,
#               lead_17 <=sell~17,
#               lead_18 <=sell~18,
#               lead_19 <=sell~19,
#               lead_20 <=sell~20,
#               TRUE ~0
#             )
#   )
# 
# # sp_500_1$t3 <- if_else(sp_500_1$t1==0 & sp_500_1$t2==0,
# #                        if_else(sp_500_1$lead_20 >=0,1,0),0)
# 
# sp_500_1 <- sp_500_1%>%
#   mutate(t4 = case_when(
#     t1==0 & t2!=0 ~0,
#     t1!=0 & t2==0~1,
#     t1 < t2 ~1,
#     t2 < t1~0,
#     TRUE ~1
#   )
#   )
# 

blue <-   function(x){
  test%>%
    group_by(symbol)%>%
    mutate(y = ((lead(close, n =x))-lead(open))/(lead(open))*100)
}

for (i in 1:20){
  sample<-blue(i)}
  test[,ncol(test)+1]<- sample$y
  colnames(test)[ncol(test)]<-paste0("lead_",i)



}

buy <- 5
sell <- -5


sp_500_1<- sp_500_1 %>%
  mutate( t1=
            case_when(
              lead_1 >=buy ~1,
              lead_2 >=buy ~2,
              lead_3 >=buy ~3,
              lead_4 >=buy ~4,
              lead_5 >=buy ~5,
              lead_6 >=buy ~6,
              lead_7 >=buy ~7,
              lead_8 >=buy ~8,
              lead_9 >=buy ~9,
              lead_10 >=buy~10,
              lead_11 >=buy~11,
              lead_12 >=buy~12,
              lead_13 >=buy~13,
              lead_14 >=buy~14,
              lead_15 >=buy~15,
              lead_16 >=buy~16,
              lead_17 >=buy~17,
              lead_18 >=buy~18,
              lead_19 >=buy~19,
              lead_20 >=buy~20,
              TRUE ~0
            )
  )

sp_500_1 <- sp_500_1 %>%
  mutate( t2=
            case_when(
              lead_1 <=sell ~1,
              lead_2 <=sell ~2,
              lead_3 <=sell ~3,
              lead_4 <=sell ~4,
              lead_5 <=sell ~5,
              lead_6 <=sell ~6,
              lead_7 <=sell ~7,
              lead_8 <=sell~8,
              lead_9 <=sell ~9,
              lead_10 <=sell~10,
              lead_11 <=sell~11,
              lead_12 <=sell~12,
              lead_13 <=sell~13,
              lead_14 <=sell~14,
              lead_15 <=sell~15,
              lead_16<=sell~16,
              lead_17 <=sell~17,
              lead_18 <=sell~18,
              lead_19 <=sell~19,
              lead_20 <=sell~20,
              TRUE ~0
            )
  )

# sp_500_1$t3 <- if_else(sp_500_1$t1==0 & sp_500_1$t2==0,
#                        if_else(sp_500_1$lead_20 >=0,1,0),0)

sp_500_1 <- sp_500_1%>%
  mutate(t4 = case_when(
    t1==0 & t2!=0 ~0,
    t1!=0 & t2==0~1,
    t1 < t2 ~1,
    t2 < t1~0,
    TRUE ~1
  )
  )



stock_test_20 <- sp_500_1%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume,  return_lag1,
            return_daily, close_shift_5, target_5,target_direction_5,  close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15, target_direction_15,close_shift_20,
            target_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar)) %>%
  na.omit()

date_filter <- sp_500_1 %>%
  filter(date == Sys.Date()-1)

stock_test_20 <- date_filter%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume,  return_lag1,
            return_daily, close_shift_5, target_5,target_direction_5,  close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15, target_direction_15,close_shift_20,
            target_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar)) 

rf_mod_3 <- readRDS("SP500_rfml_trendpick_3.rds")

stock_pred_3_rf <-  predict(rf_mod_3, stock_test_20, type = "prob")

stock_pred_3_rf <- stock_pred_3_rf %>%
  rename(pred_buy_3= .pred_buy  ,pred_sell_3 = .pred_sell )

rm(rf_mod_3)

rf_mod_5 <- readRDS("SP500_rfml_trendpick_5.rds")

stock_pred_5_rf <-  predict(rf_mod_5, stock_test_20, type = "prob")

stock_pred_5_rf <- stock_pred_5_rf %>%
  rename(pred_buy_5= .pred_buy  ,pred_sell_5 = .pred_sell )

rm(rf_mod_5)

rf_mod_10 <- readRDS("SP500_rfml_trendpick_10.rds")

stock_pred_10_rf <-  predict(rf_mod_10, stock_test_20, type = "prob")

stock_pred_10_rf <- stock_pred_10_rf %>%
  rename(pred_buy_10= .pred_buy  ,pred_sell_10 = .pred_sell )

rm(rf_mod_10)

rf_mod_15 <- readRDS("SP500_rfml_trendpick_15.rds")

stock_pred_15_rf <-  predict(rf_mod_15, stock_test_20, type = "prob")

stock_pred_15_rf <- stock_pred_15_rf %>%
  rename(pred_buy_15= .pred_buy  ,pred_sell_15 = .pred_sell )

rm(rf_mod_15)

rf_mod_20 <- readRDS("SP500_rfml_trendpick_20.rds")

stock_pred_20_rf <-  predict(rf_mod_20, stock_test_20, type = "prob")

stock_pred_20_rf <- stock_pred_20_rf %>%
  rename(pred_buy_20= .pred_buy  ,pred_sell_20 = .pred_sell )

rm(rf_mod_20)

og_stock_pred <- stock_pred_rf_symbol

look <- sp_500_1 %>%
  na.omit()

test_pred_3_rf <- test_pred_3_rf %>%
  rename(pred_buy_3= .pred_buy  ,pred_sell_3 = .pred_sell )

test_pred_5_rf <- test_pred_5_rf %>%
  rename(pred_buy_5= .pred_buy  ,pred_sell_5 = .pred_sell )

test_pred_10_rf <- test_pred_10_rf %>%
  rename(pred_buy_10= .pred_buy  ,pred_sell_10 = .pred_sell )

test_pred_15_rf <- test_pred_15_rf %>%
  rename(pred_buy_15= .pred_buy  ,pred_sell_15 = .pred_sell )

test_pred_20_rf <- test_pred_20_rf %>%
  rename(pred_buy_20= .pred_buy  ,pred_sell_20 = .pred_sell )%>%
  select(c(1:2))


test_pred_3_rf <- test_pred_3_rf %>%
  select(c(1:2))

test_pred_5_rf <- test_pred_5_rf %>%
  select(c(1:2))

test_pred_10_rf <- test_pred_10_rf %>%
  select(c(1:2))

test_pred_15_rf <- test_pred_15_rf %>%
  select(c(1:2))

test_pred_20_rf <- test_pred_20_rf %>%
  select(c(1:2))

stock_pred_rf_symbol  <- bind_cols(test_pred_3_rf, test_pred_5_rf, 
                                   test_pred_10_rf,test_pred_15_rf,
                                   test_pred_20_rf,
                                   select(test, symbol, date,open, close, target_direction_3, 
                                          target_direction_5,target_direction_10, target_direction_15,target_direction_20,
                                          target_3,target_5,target_10, target_15, target_20,
                                          close_shift_5, close_shift_20, 
                                          close_shift_10, close_shift_15,t4))

# stock_pred_rf_symbol  <- bind_cols(stock_pred_3_rf, stock_pred_5_rf, 
#                                    stock_pred_10_rf,stock_pred_15_rf,
#                                    stock_pred_20_rf,
#                                    select(look, symbol, date, open , close, target_direction_3, 
#                                           target_direction_5,target_direction_10, target_direction_15,target_direction_20,
#                                           target_3,target_5,target_10, target_15, target_20,
#                                           close_shift_5, close_shift_20, 
#                                           close_shift_10, close_shift_15,target_3, target_5, target_10, target_15,target_20,t4))


stock_pred_rf_symbol<- stock_pred_rf_symbol %>%
  select(date,symbol,open,close,pred_buy_3,pred_buy_5,pred_buy_10,pred_buy_15,pred_buy_20,
         target_direction_3,target_direction_5,target_direction_10,
         target_direction_15,target_direction_20,target_3, target_5, target_10, target_15,target_20,t4
  )



stock_pred_rf_symbol$avg <- rowMeans(stock_pred_rf_symbol[,5:7])
stock_pred_rf_symbol<- stock_pred_rf_symbol %>%
  select(date,symbol,open, close,t4,avg,pred_buy_3,pred_buy_5,pred_buy_10,pred_buy_15,pred_buy_20,
         target_direction_3,target_direction_5,target_direction_10,
         target_direction_15,target_direction_20 ,target_3, target_5, target_10, target_15,target_20)

look <- stock_pred_rf_symbol %>%
  filter(avg >= 0.6)

sum(look$t4)



start_date <- "2012-01-01"
start_date <- Sys.Date()-365

test <- tq_get("AAPL",get="stock.prices",
         from = start_date)

test <- test %>%
  group_by(symbol)%>%
  tq_mutate(select = c("high","low","close"),
            mutate_fun = ATR,
            n=7)

multiplier = 3
test <- test %>%
  group_by(symbol)%>%
  mutate(st_up = ((high + low) / 2) + multiplier *atr,
         st_down = ((high + low) / 2) - multiplier *atr)
         

test <- test %>%
  group_by(symbol)%>%
  tq_mutate("close","volume",
            mutate_fun = VWAP,
            n=10)
tes <- test %>%
  group_by(symbol)%>%
  mutate(vwap_10 = VWAP(close,volume),
         vwap_5= VWAP(close,volume, n=5),
         vwap_15= VWAP(close,volume, n=15),
         vwap_20= VWAP(close,volume, n=20))
tes <- tes %>%
  select(symbol,close,vwap_5,vwap_10,vwap_15,vwap_20)

tes <- tes %>%
  mutate(vwap_signal = case_when(
    vwap_5 > vwap_10 & vwap_5 > vwap_15 & vwap_5 > vwap_20 ~ 1,
    TRUE ~0
  ))

test <- test %>%
  group_by(symbol)%>%
  mutate(hfib_ma_1 = EMA(high,n=5),
         hfib_ma_2 = EMA(high,n=8),
         hfib_ma_3 = EMA(high,n=13),
         hfib_ma_4 = EMA(high,n=21),
         hfib_ma_5 = EMA(high,n=34),
         hfib_ma_6 = EMA(high,n=55),
         hfib_ma_7 = EMA(high,n=89),
         hfib_ma_8 = EMA(high,n=144),
         hfib_ma_9 = EMA(high,n=233),
         hfib_ma_10 = EMA(high,n=377),
         hfib_ma_11 = EMA(high,n=610),
         hfib_ma_12 = EMA(high,n=987),
         hfib_ma_13 = EMA(high,n=1597)
         )

test <- test %>%
  group_by(symbol)%>%
  mutate(fib_high = (hfib_ma_1 + hfib_ma_2  + hfib_ma_3 + hfib_ma_4 + hfib_ma_5 +
                     hfib_ma_6 +  hfib_ma_7 + hfib_ma_8 + hfib_ma_9 + hfib_ma_10 +
                     hfib_ma_11  +  hfib_ma_12 + hfib_ma_13) /13)

test <- test %>%
  group_by(symbol)%>%
  mutate(lfib_ma_1 = EMA(high,n=5),
         lfib_ma_2 = EMA(high,n=8),
         lfib_ma_3 = EMA(high,n=13),
         lfib_ma_4 = EMA(high,n=21),
         lfib_ma_5 = EMA(high,n=34),
         lfib_ma_6 = EMA(high,n=55),
         lfib_ma_7 = EMA(high,n=89),
         lfib_ma_8 = EMA(high,n=144),
         lfib_ma_9 = EMA(high,n=233),
         lfib_ma_10 = EMA(high,n=377),
         lfib_ma_11 = EMA(high,n=610),
         lfib_ma_12 = EMA(high,n=987),
         lfib_ma_13 = EMA(high,n=1597)
  )

test <- test %>%
  group_by(symbol)%>%
  mutate(fib_low = (hfib_ma_1 + hfib_ma_2  + hfib_ma_3 + hfib_ma_4 + hfib_ma_5 +
                       hfib_ma_6 +  hfib_ma_7 + hfib_ma_8 + hfib_ma_9 + hfib_ma_10 +
                       hfib_ma_11  +  hfib_ma_12 + hfib_ma_13) /13)



test <- test %>%
  group_by(symbol)%>%
  mutate(fib_signal = case_when(
    close > fib_high & close > fib_low ~1,
    TRUE~0
  ))

test <- test %>%
  group_by(symbol)%>%
  mutate(ema_5= EMA(high,n=5),
         ema_20= EMA(high,n=20))

look <- test %>%
  group_by(symbol)%>%
  mutate(ema_co_signal = case_when(
    ema_5 > ema_20 ~1,
    TRUE~0
  ))

look_1 <- bind_cols(look %>%
                      select(symbol,close,fib_signal,ema_co_signal), 
                    tes %>% 
                      select(vwap_signal))%>%
  na.omit()

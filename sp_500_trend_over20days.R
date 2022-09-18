library(tidyverse)
library(tidyquant)
library(tidymodels)
library(randomForest)
library(xgboost)
library(ranger)

rm(list = ls())

start_date <- "2018-01-01"
start_date <- Sys.Date()-365

sp_500 <- tq_index("SP500") %>%
  tq_get(get="stock.prices",
         from = start_date)



sp_500 <- tq_get(test$symbol, get="stock.prices",
                 from = start_date)

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

blue <-   function(x){
  sp_500_1%>%
    mutate(x = ((lead(close, n =x))-lead(open))/(lead(open))*100)
}

for (i in 1:20){
  test<-blue(i)
  sp_500_1[,ncol(sp_500_1)+1]<- test$x
  colnames(sp_500_1)[ncol(sp_500_1)]<-paste0("lead_",i)
  
  
  
}


sp_500_1<- sp_500_1 %>%
  mutate( t1=
            case_when(
              lead_1 >=7 ~1,
              lead_2 >=7 ~2,
              lead_3 >=7 ~3,
              lead_4 >=7 ~4,
              lead_5 >=7 ~5,
              lead_6 >=7 ~6,
              lead_7 >=7 ~7,
              lead_8 >=7 ~8,
              lead_9 >=7 ~9,
              lead_10 >=7~10,
              lead_11 >=7~11,
              lead_12 >=7~12,
              lead_13 >=7~13,
              lead_14 >=7~14,
              lead_15 >=7~15,
              lead_16 >=7~16,
              lead_17 >=7~17,
              lead_18 >=7~18,
              lead_19 >=7~19,
              lead_20 >=7~20,
              TRUE ~0
            )
  )

sp_500_1 <- sp_500_1 %>%
  mutate( t2=
            case_when(
              lead_1 <=-5 ~1,
              lead_2 <=-5 ~2,
              lead_3 <=-5 ~3,
              lead_4 <=-5 ~4,
              lead_5 <=-5 ~5,
              lead_6 <=-5 ~6,
              lead_7 <=-5 ~7,
              lead_8 <=-5~8,
              lead_9 <=-5 ~9,
              lead_10 <=-5~10,
              lead_11 <=-5~11,
              lead_12 <=-5~12,
              lead_13 <=-5~13,
              lead_14 <=-5~14,
              lead_15 <=-5~15,
              lead_16<=-5~16,
              lead_17 <=-5~17,
              lead_18 <=-5~18,
              lead_19 <=-5~19,
              lead_20 <=-5~20,
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

cores <- 8



cores <- 8

train <- sp_500_1 %>%
  filter(date < "2021-01-01")%>%
  na.omit()

test <- sp_500_1 %>%
  filter(date > "2021-01-01")%>%
  na.omit()

train <- sp_500_1 %>% 
  na.omit()



stock_split <- initial_split(sp_500_1, prop = 3/4, strata = "t4")%>%
  na.omit()

stock_train_3 <- training(stock_split)%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily, lead_1:lead_20,t1:t2, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar,max_high,min_low,close_month,open_month,month_yr,pp,s1,r1,s2,r2,s3,r3))%>%
  na.omit()

stock_test_3 <- testing(stock_split)%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily, lead_1:lead_20,t1:t2, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar,max_high,min_low,close_month,open_month,month_yr,pp,s1,r1,s2,r2,s3,r3))%>%
  na.omit()

stock_train_3$t4<- if_else(stock_train_3$t4 == 1, "buy","sell")

stock_train_3$t4 <- factor(stock_train_3$t4, levels=c("buy","sell"))

stock_test_3$t4<- if_else(stock_test_3$t4 == 1, "buy","sell")

stock_test_3$t4 <- factor(stock_test_3$t4, levels=c("buy","sell"))

# stock_train_15$target_direction_15 <- factor(stock_train_15$target_direction_15, levels=c("buy","neutral","sell"))
# 
# glimpse(stock_train_15$target_direction_15)

# remove(sp_500_1)
# remove(sp_500)

# rf_grid <- expand.grid(
#   mtry = c(12,16,20,24,26,28),
#   trees = c(500,750,1000,1500),
#   min_n = c(2,4,6,8,10,12)
# )

xgb_spec<-
  rand_forest( min_n = tune(), mtry = tune(),trees =tune()) %>%
  set_engine("ranger",importance = "impurity", num.threads = cores) %>%
  set_mode("classification")

xgb_spec

xgb_grid <- grid_latin_hypercube(
  trees(),
  min_n(),
  finalize(mtry(), stock_train_3),
  size = 30
)

xgb_grid

set.seed(123)
vb_folds <- vfold_cv(stock_train_3, v=2)

vb_folds

xgb_wf <- workflow() %>%
  add_formula(t4 ~ .) %>%
  add_model(xgb_spec)

xgb_wf

doParallel::registerDoParallel()

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
  fit(data = stock_train_3) %>%
  pull_workflow_fit() %>%
  vip(geom = "point",num_features = 30)

rf_grid
rm(stock_split)

stock_recipe_3 <- recipe(t4~., data = stock_train_3)
cores <- 8

rf_mod_3 <-
  rand_forest( min_n = tune(), mtry = tune(),trees =tune()) %>%
  set_engine("ranger",importance = "impurity", num.threads = cores) %>%
  set_mode("classification")


rf_workflow <-
  workflow() %>%
  add_model(rf_mod_3) %>%
  add_recipe(stock_recipe_3)

rf_workflow

cv <- vfold_cv(stock_train_3, v = 10)

set.seed(345)
rf_res <-
  rf_workflow %>%
  tune_grid(cv,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

 rf_res %>%
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
  fit(t4~., data = stock_train_3)


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

stock_train_5 <- training(stock_split)%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily, close_shift_3, target_3, target_direction_3,close_shift_5, target_5, close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15,target_direction_15,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar,max_high,min_low,close_month,open_month,month_yr,pp,s1,r1,s2,r2,s3,r3))

stock_test_5 <- testing(stock_split)%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily, close_shift_3, target_3, target_direction_3,close_shift_5, target_5, close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15,target_direction_15,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar,max_high,min_low,close_month,open_month,month_yr,pp,s1,r1,s2,r2,s3,r3))

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

stock_train_10 <- training(stock_split)%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily, close_shift_3, target_3, target_direction_3,close_shift_5, 
            target_5, target_direction_5, close_shift_10, target_10,
            close_shift_15,target_15,target_direction_15,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar,max_high,min_low,close_month,open_month,month_yr,pp,s1,r1,s2,r2,s3,r3))

stock_test_10 <- testing(stock_split)%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily, close_shift_3, target_3, target_direction_3,close_shift_5, target_5, target_direction_5, close_shift_10, target_10,
            close_shift_15,target_15,target_direction_15,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar,max_high,min_low,close_month,open_month,month_yr,pp,s1,r1,s2,r2,s3,r3))%>%
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

stock_train_15 <- training(stock_split)%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily,close_shift_3, target_3, target_direction_3, close_shift_5, target_5,target_direction_5, close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar,max_high,min_low,close_month,open_month,month_yr,pp,s1,r1,s2,r2,s3,r3))

stock_test_15 <- testing(stock_split)%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily,close_shift_3, target_3, target_direction_3, close_shift_5, target_5,target_direction_5, close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar,max_high,min_low,close_month,open_month,month_yr,pp,s1,r1,s2,r2,s3,r3))

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


stock_train_20 <- training(stock_split)%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume,  return_lag1,
            return_daily,close_shift_3, target_3, target_direction_3, close_shift_5, target_5,target_direction_5,  close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15, target_direction_15,close_shift_20,
            target_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar,max_high,min_low,close_month,open_month,month_yr,pp,s1,r1,s2,r2,s3,r3)) 

stock_test_20 <- testing(stock_split)%>%
  ungroup()%>%
  select(-c(symbol,date, open, high, low, close, volume,  return_lag1,
            return_daily,close_shift_3, target_3, target_direction_3, close_shift_5, target_5,target_direction_5,  close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15, target_direction_15,close_shift_20,
            target_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar,max_high,min_low,close_month,open_month,month_yr,pp,s1,r1,s2,r2,s3,r3))%>%
  na.omit()

stock_test_20 <- train%>%
  ungroup()%>%
  select(-c(symbol,date, open, high, low, close, volume,  return_lag1,
            return_daily,close_shift_3, target_3, target_direction_3, close_shift_5, target_5,target_direction_5,  close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15, target_direction_15,close_shift_20,
            target_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar,max_high,min_low,close_month,open_month,month_yr,pp,s1,r1,s2,r2,s3,r3))

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

                       
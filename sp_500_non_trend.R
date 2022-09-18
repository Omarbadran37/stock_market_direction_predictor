# created 7/25 no trend


library(tidyverse)
library(tidyquant)
library(tidymodels)
library(randomForest)
library(xgboost)
library(ranger)
library(rstanarm)

rm(list = ls())

start_date <- "2008-01-01"
# start_date <- Sys.Date()-365

sp_500 <- tq_index("SP500") %>%
  tq_get(get="stock.prices",
         from = start_date)

# sp_500 <- tq_index("SP500") %>%
#   filter(sector == "Information Technology")%>%
#   tq_get(get="stock.prices",
#          from = start_date)


# sp_500 <- tq_get(test$symbol, get="stock.prices",
#                  from = start_date)

sp_500_1 <- sp_500 %>%
  select(symbol,date,open,high,low,close,volume)

sp_500_1 <- sp_500_1 %>%
  filter(!symbol %in% c("CEG","OGN","AMCR","TTWO","CTVA","DOW","EMBC","ELV","CARR","OTIS","FOX","FOXA",
                        "MRNA","CDAY","FTV","HWM","IR","LW","VICI"))

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
  mutate(close_shift_1 = lead(close, n=1))

sp_500_1 <- sp_500_1%>%
  mutate(target_1 = (close_shift_1-lead(open))/(lead(open))*100)

sp_500_1 <- sp_500_1 %>%
  mutate(target_direction_1 = if_else(target_1>0,1,0))

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

sp_500_1$profit_day_3 <- rowSums(sp_500_1[,25:27]>=1)
sp_500_1$profit_day_5 <- rowSums(sp_500_1[,25:29]>=1)
sp_500_1$profit_day_10 <- rowSums(sp_500_1[,25:34]>=1)
sp_500_1$profit_day_15 <- rowSums(sp_500_1[,25:39]>=1)
sp_500_1$profit_day_20 <- rowSums(sp_500_1[,25:44]>=1)

# sp_500_1$target_direction_3 <- if_else((sp_500_1$profit_day_3/3)>= 0.6 ,1,0)
# sp_500_1$target_direction_5 <- if_else((sp_500_1$profit_day_5/5)>= 0.7,1,0)
# sp_500_1$target_direction_10<- if_else((sp_500_1$profit_day_10/10)>= 0.7 ,1,0)
# sp_500_1$target_direction_15 <- if_else((sp_500_1$profit_day_15/15)>= 0.7 ,1,0)
# sp_500_1$target_direction_20 <- if_else((sp_500_1$profit_day_20/20)>= 0.7 ,1,0)

# sp_500_1$target_direction_3 <- if_else((sp_500_1$profit_day_3/3)>= 0.6 | sp_500_1$target_direction_3 == 1,1,0)
# sp_500_1$target_direction_5 <- if_else((sp_500_1$profit_day_5/5)>= 0.6| sp_500_1$target_direction_5 == 1,1,0)
# sp_500_1$target_direction_10<- if_else((sp_500_1$profit_day_10/10)>= 0.6 | sp_500_1$target_direction_10 == 1,1,0)
# sp_500_1$target_direction_15 <- if_else((sp_500_1$profit_day_15/15)>= 0.6 | sp_500_1$target_direction_15 == 1,1,0)
# sp_500_1$target_direction_20 <- if_else((sp_500_1$profit_day_20/20)>= 0.6 | sp_500_1$target_direction_20 == 1,1,0)


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
  mutate(diff_short = macd - signal,
         macd_short_signal = case_when(
           diff_short >0 ~1,
           TRUE~0
         ))

sp_500_1 <- sp_500_1 %>%
  select(-c(macd,signal))#diff_short



sp_500_1 <- sp_500_1%>%
  group_by(symbol) %>%
  tq_mutate(select     = close, 
            mutate_fun = MACD, 
            nFast      = 12, 
            nSlow      = 26, 
            nSig       = 9, 
            maType     = "EMA")%>%
  mutate(diff_long = macd - signal,
         macd_long_signal = case_when(
           diff_long >=0 ~1,
           TRUE~0
         ))


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
  tq_mutate(select = close,
            mutate_fun = EMA,
            n = 5,
            col_rename = "ema_5")

sp_500_1 <- sp_500_1 %>%
  tq_mutate(select = close,
            mutate_fun = EMA,
            n = 21,
            col_rename = "ema_21")

sp_500_1 <- sp_500_1 %>%
  tq_mutate(select = close,
            mutate_fun = EMA,
            n = 50,
            col_rename = "ema_50")

sp_500_1 <- sp_500_1 %>%
  tq_mutate(select = close,
            mutate_fun = EMA,
            n = 200,
            col_rename = "ema_200")

sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  tq_mutate(select = close,
            mutate_fun = RSI,
            n=14,
            col_rename = "rsi_14"
  )%>%
  mutate(rsi_14 =rsi_14/100 )

sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  tq_mutate(select = c("high","low"),
            mutate_fun = SAR,
            col_rename = "sar")%>%
  mutate(sar_ratio = close/sar,
         sar_signal = case_when(
           sar_ratio >=1 ~1,
           TRUE~0
         ))



sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  tq_mutate(select = c("high","low","close"),
            mutate_fun = stoch)%>%
  mutate(stoch_diff = fastD/stoch,
         stoch_signal = case_when(
           stoch_diff >=1 ~1,
           TRUE~0
         ))

# sp_500_1 <- sp_500_1 %>%
#   select(-c(fastD,fastK,stoch,stoch_diff))


sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  mutate(median_price = (high+low)/2,
         ao = SMA(median_price, n=5)-SMA(median_price, n=34),
         ao_mavg = SMA(ao, n=10),
         ao_signal = case_when(
           ao>=0 ~1,
           TRUE~0
         ),
         ao_mavg_signal =case_when(
           ao_mavg>=0 ~1,
           TRUE~0
         ))


sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  tq_mutate(select = c("high","low","close"),
            mutate_fun = ADX,
            maType = "EMA")

sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  mutate(adx_signal = case_when(
    DIp > DIn & ADX>=15 ~1,
    TRUE~0
  ))


sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  tq_mutate(select = c("high","low","close"),
            mutate_fun = BBands)




sp_500_1$ema_5co21 <- sp_500_1$ema_5/sp_500_1$ema_21
sp_500_1$ema_50co200 <- sp_500_1$ema_50/sp_500_1$ema_200

sp_500_1$ema_5co21_signal <- if_else(sp_500_1$ema_5co21>=1,1,0)
sp_500_1$ema_50co200_signal <- if_else(sp_500_1$ema_50co200>=1,1,0)

sp_500_1$ema_5co21_trend_3 <- lag(sp_500_1$ema_5co21_signal, n=3)
sp_500_1$ema_5co21_trend_5 <- lag(sp_500_1$ema_5co21_signal, n=5)
sp_500_1$ema_5co21_trend_10 <- lag(sp_500_1$ema_5co21_signal, n=10)
sp_500_1$ema_5co21_trend_15 <- lag(sp_500_1$ema_5co21_signal, n=15)
sp_500_1$ema_5co21_trend_20 <- lag(sp_500_1$ema_5co21_signal, n=20)

sp_500_1$ema_50co200_trend_3 <- lag(sp_500_1$ema_50co200_signal, n=3)
sp_500_1$ema_50co200_trend_5 <- lag(sp_500_1$ema_50co200_signal, n=5)
sp_500_1$ema_50co200_trend_10 <- lag(sp_500_1$ema_50co200_signal, n=10)
sp_500_1$ema_50co200_trend_15 <- lag(sp_500_1$ema_50co200_signal, n=15)
sp_500_1$ema_50co200_trend_20 <- lag(sp_500_1$ema_50co200_signal, n=20)



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

sp_500_1$hma_9co25_signal <- if_else(sp_500_1$hma_9co25>=1,1,0)
sp_500_1$hma_9co49_signal <- if_else(sp_500_1$hma_9co49>=1,1,0)
sp_500_1$hma_9co81_signal <- if_else(sp_500_1$hma_9co81>=1,1,0)
sp_500_1$hma_25co49_signal<- if_else(sp_500_1$hma_25co49>=1,1,0)
sp_500_1$hma_25co81_signal <- if_else(sp_500_1$hma_25co81>=1,1,0)
sp_500_1$hma_49co81_signal <- if_else(sp_500_1$hma_49co81>=1,1,0)
sp_500_1$hma_9co200_signal<- if_else(sp_500_1$hma_9co200>=1,1,0)
sp_500_1$hma_25co200_signal<- if_else(sp_500_1$hma_25co200>=1,1,0)
sp_500_1$hma_49co200_signal<- if_else(sp_500_1$hma_49co200>=1,1,0)
sp_500_1$hma_81co200_signal<- if_else(sp_500_1$hma_81co200>=1,1,0)
sp_500_1$hma_9co_sma50_signal<- if_else(sp_500_1$hma_9co_sma50>=1,1,0)
sp_500_1$hma_25co_sma50_signal<- if_else(sp_500_1$hma_25co_sma50>=1,1,0)
sp_500_1$hma_49co_sma50_signal<- if_else(sp_500_1$hma_49co_sma50>=1,1,0)
sp_500_1$hma_81co_sma50_signal<- if_else(sp_500_1$hma_81co_sma50>=1,1,0)
sp_500_1$hma_9co_sma100_signal<- if_else(sp_500_1$hma_9co_sma100>=1,1,0)
sp_500_1$hma_25co_sma100_signal<- if_else(sp_500_1$hma_25co_sma100>=1,1,0)
sp_500_1$hma_49co_sma100_signal<- if_else(sp_500_1$hma_49co_sma100>=1,1,0)
sp_500_1$hma_81co_sma100_signal<- if_else(sp_500_1$hma_81co_sma100>=1,1,0)
sp_500_1$hma_9co_sma200_signal<- if_else(sp_500_1$hma_9co_sma200>=1,1,0)
sp_500_1$hma_25co_sma200_signal<- if_else(sp_500_1$hma_25co_sma200>=1,1,0)
sp_500_1$hma_49co_sma200_signal<- if_else(sp_500_1$hma_49co_sma200>=1,1,0)
sp_500_1$hma_81co_sma200_signal<- if_else(sp_500_1$hma_81co_sma200>=1,1,0)


sp_500_1 <- sp_500_1 %>%
  select(-c(hma_9,hma_25,hma_49,hma_81,hma_200,sma_50,sma_100,sma_200,
            ema_5,ema_21,ema_50,ema_200))



sp_500_1  <- sp_500_1  %>%
  group_by(symbol)%>%
  mutate(vwap_5= VWAP(close,volume, n=5),
         vwap_10 = VWAP(close,volume, n=10),
         vwap_15= VWAP(close,volume, n=15),
         vwap_20= VWAP(close,volume, n=20),
         vwap_signal = case_when(
           vwap_5 > vwap_10 & vwap_5 > vwap_15 & vwap_5 > vwap_20 ~ 1,
           TRUE ~0
         )
  )

sp_500_1 <- sp_500_1 %>%
  select(-c(vwap_5, vwap_10, vwap_15, vwap_20))

sp_500_1 <- sp_500_1 %>%
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

sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  mutate(fib_high = (hfib_ma_1 + hfib_ma_2  + hfib_ma_3 + hfib_ma_4 + hfib_ma_5 +
                       hfib_ma_6 +  hfib_ma_7 + hfib_ma_8 + hfib_ma_9 + hfib_ma_10 +
                       hfib_ma_11  +  hfib_ma_12 + hfib_ma_13) /13)

sp_500_1 <- sp_500_1 %>%
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

sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  mutate(fib_low = (lfib_ma_1 + lfib_ma_2  + lfib_ma_3 + lfib_ma_4 + lfib_ma_5 +
                      lfib_ma_6 +  lfib_ma_7 + lfib_ma_8 + lfib_ma_9 + lfib_ma_10 +
                      lfib_ma_11  +  lfib_ma_12 + lfib_ma_13) /13)


sp_500_1 <- sp_500_1 %>%
  group_by(symbol)%>%
  mutate(fib_signal = case_when(
    close > fib_high & close > fib_low ~1,
    TRUE~0
  )
  )
sp_500_1 <- sp_500_1 %>%
  na.omit()


sp_500_1 <- sp_500_1 %>%
  select(-c(macd,signal,sar,ao,ao_mavg,median_price,
            DIp,DIn,DX,ADX,dn,mavg,up,fib_high,fib_low, hfib_ma_1, hfib_ma_2, hfib_ma_3 , hfib_ma_4 , hfib_ma_5 ,
            hfib_ma_6 ,  hfib_ma_7 , hfib_ma_8 , hfib_ma_9 , hfib_ma_10 ,
            hfib_ma_11  ,  hfib_ma_12 , hfib_ma_13,lfib_ma_1 , lfib_ma_2  ,
            lfib_ma_3 , lfib_ma_4 , lfib_ma_5 , lfib_ma_6 ,  lfib_ma_7 , lfib_ma_8 , lfib_ma_9 ,
            lfib_ma_10 , lfib_ma_11  ,  lfib_ma_12 , lfib_ma_13))

object.size(sp_500_1)/1000000000

cores <- 8

# train <- sp_500_1 %>%
#   na.omit()

train <- sp_500_1 %>%
  filter( date < "2021-11-01")%>%
  na.omit()

# train <- train %>%
#   filter(date >"2016-01-01" & date < "2021-11-01")%>%
#   na.omit()

test <- sp_500_1 %>%
  filter(date > "2021-11-01")%>%
  na.omit()

object.size(train)/1000000000

# rm(sp_500_1)

# stock_split <- initial_split(sp_500_1, prop = 3/4, strata = "target_direction_20")%>%
#   na.omit()
stock_train_1 <- train %>%
ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily, close_shift_3, target_3, close_shift_5, target_5,target_direction_5, close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15,target_direction_15,close_shift_20,
            target_20, target_direction_20,target_direction_3,close_shift_1, target_1,"lead_1","lead_2","lead_3", "lead_4","lead_5","lead_6", "lead_7","lead_8",
            "lead_9","lead_10", "lead_11" ,"lead_12",
            "lead_13", "lead_14","lead_15","lead_16","lead_17","lead_18",
            "lead_19" ,"lead_20","t1", "t2","t4","profit_day_3",
            "profit_day_5","profit_day_15","profit_day_20",
            "profit_day_10"))

stock_test_1 <- test%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily, close_shift_3, target_3,close_shift_5, target_5,target_direction_5, close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15,target_direction_15,close_shift_20,
            target_20, target_direction_20,target_direction_3,"lead_1","lead_2","lead_3", "lead_4","lead_5","lead_6", "lead_7","lead_8",
            "lead_9","lead_10", "lead_11" ,"lead_12",
            "lead_13", "lead_14","lead_15","lead_16","lead_17","lead_18",
            "lead_19" ,"lead_20","t1", "t2","t4","profit_day_3",
            "profit_day_5","profit_day_15","profit_day_20",
            "profit_day_10"))

stock_train_1$target_direction_1<- if_else(stock_train_1$target_direction_1 == 1, "buy","sell")

stock_train_1$target_direction_1 <- factor(stock_train_1$target_direction_1, levels=c("buy","sell"))

stock_test_1$target_direction_1<- if_else(stock_test_1$target_direction_1 == 1, "buy","sell")

stock_test_1$target_direction_1 <- factor(stock_test_1$target_direction_1, levels=c("buy","sell"))

rf_mod_1 <-
  rand_forest( min_n = 2,trees = 1000) %>%
  set_engine("ranger",importance = "impurity", num.threads = cores) %>%
  set_mode("classification")%>%
  fit(target_direction_1~., data = stock_train_1)

library(vip)
rf_mod_1 %>%
  vip(geom = "point",num_features = 50)

test_pred_1_rf <- predict(rf_mod_1, stock_test_1, type = "prob") %>%
  bind_cols(predict(rf_mod_1, stock_test_1)) %>%
  bind_cols(select(stock_test_1, target_direction_1)) %>%
  glimpse()

test_pred_1_rf %>%
  sens(truth = as.factor(target_direction_1),estimate = .pred_class)

test_pred_1_rf%>%
  spec(truth = target_direction_1,estimate = .pred_class)

test_pred_1_rf%>%
  accuracy(truth = target_direction_1,estimate = .pred_class)

test_pred_1_rf%>%
  precision(truth = target_direction_1,estimate = .pred_class)

test_pred_1_rf%>%
  recall(truth = target_direction_1,estimate = .pred_class)

test_pred_1_rf%>%
  ppv(truth = target_direction_1,estimate = .pred_class)

test_pred_1_rf%>%
  npv(truth = target_direction_1,estimate = .pred_class)

test_pred_1_rf %>%
  roc_auc(truth = target_direction_1,estimate = .pred_buy)

test_pred_1_rf %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_1), 
               alpha = 0.5)+
  ggtitle("Random Forest target 1 day ")+
  xlab("Probability of Yes")

rm(rf_mod_1)
rm(stock_train_1)
rm(stock_test_1)


stock_train_3 <- train%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily, close_shift_3, target_3, close_shift_5, target_5,target_direction_5, close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15,target_direction_15,close_shift_20,
            target_20, target_direction_20,"lead_1","lead_2","lead_3", "lead_4","lead_5","lead_6", "lead_7","lead_8",
            "lead_9","lead_10", "lead_11" ,"lead_12",
            "lead_13", "lead_14","lead_15","lead_16","lead_17","lead_18",
            "lead_19" ,"lead_20","t1", "t2","t4","profit_day_3",
            "profit_day_5","profit_day_15","profit_day_20",
            "profit_day_10"))

stock_test_3 <- test%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily, close_shift_3, target_3,close_shift_5, target_5,target_direction_5, close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15,target_direction_15,close_shift_20,
            target_20, target_direction_20,"lead_1","lead_2","lead_3", "lead_4","lead_5","lead_6", "lead_7","lead_8",
            "lead_9","lead_10", "lead_11" ,"lead_12",
            "lead_13", "lead_14","lead_15","lead_16","lead_17","lead_18",
            "lead_19" ,"lead_20","t1", "t2","t4","profit_day_3",
            "profit_day_5","profit_day_15","profit_day_20",
            "profit_day_10"))

stock_train_3$target_direction_3<- if_else(stock_train_3$target_direction_3 == 1, "buy","sell")

stock_train_3$target_direction_3 <- factor(stock_train_3$target_direction_3, levels=c("buy","sell"))

stock_test_3$target_direction_3<- if_else(stock_test_3$target_direction_3 == 1, "buy","sell")

stock_test_3$target_direction_3 <- factor(stock_test_3$target_direction_3, levels=c("buy","sell"))

rf_mod_3 <-
  rand_forest( min_n = 2,trees = 1000) %>%
  set_engine("ranger",importance = "impurity", num.threads = cores) %>%
  set_mode("classification")%>%
  fit(target_direction_3~., data = stock_train_3)

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
            target_20, target_direction_20,"lead_1","lead_2","lead_3", "lead_4","lead_5","lead_6", "lead_7","lead_8",
            "lead_9","lead_10", "lead_11" ,"lead_12",
            "lead_13", "lead_14","lead_15","lead_16","lead_17","lead_18",
            "lead_19" ,"lead_20","t1", "t2","t4","profit_day_3",
            "profit_day_5","profit_day_15","profit_day_20",
            "profit_day_10"))

stock_test_5 <- test%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily, close_shift_3, target_3, target_direction_3,close_shift_5, target_5, close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15,target_direction_15,close_shift_20,
            target_20, target_direction_20,"lead_1","lead_2","lead_3", "lead_4","lead_5","lead_6", "lead_7","lead_8",
            "lead_9","lead_10", "lead_11" ,"lead_12",
            "lead_13", "lead_14","lead_15","lead_16","lead_17","lead_18",
            "lead_19" ,"lead_20","t1", "t2","t4","profit_day_3",
            "profit_day_5","profit_day_15","profit_day_20",
            "profit_day_10"))

stock_train_5$target_direction_5<- if_else(stock_train_5$target_direction_5 == 1, "buy","sell")

stock_train_5$target_direction_5 <- factor(stock_train_5$target_direction_5, levels=c("buy","sell"))

stock_test_5$target_direction_5<- if_else(stock_test_5$target_direction_5 == 1, "buy","sell")

stock_test_5$target_direction_5 <- factor(stock_test_5$target_direction_5, levels=c("buy","sell"))

rf_mod_5 <-
  rand_forest( min_n = 2,trees = 750) %>%
  set_engine("ranger",importance = "impurity", num.threads = cores) %>%
  set_mode("classification")%>%
  fit(target_direction_5~., data = stock_train_5)


#saveRDS(rf_mod_5,"SP500_rfml_trendbinary_5day_50.rds")

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
            target_20, target_direction_20,"lead_1","lead_2","lead_3", "lead_4","lead_5","lead_6", "lead_7","lead_8",
            "lead_9","lead_10", "lead_11" ,"lead_12",
            "lead_13", "lead_14","lead_15","lead_16","lead_17","lead_18",
            "lead_19" ,"lead_20","t1", "t2","t4","profit_day_3",
            "profit_day_5","profit_day_15","profit_day_20",
            "profit_day_10"))

stock_test_10 <- test%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily, close_shift_3, target_3, target_direction_3,close_shift_5, target_5, target_direction_5, close_shift_10, target_10,
            close_shift_15,target_15,target_direction_15,close_shift_20,
            target_20, target_direction_20,"lead_1","lead_2","lead_3", "lead_4","lead_5","lead_6", "lead_7","lead_8",
            "lead_9","lead_10", "lead_11" ,"lead_12",
            "lead_13", "lead_14","lead_15","lead_16","lead_17","lead_18",
            "lead_19" ,"lead_20","t1", "t2","t4","profit_day_3",
            "profit_day_5","profit_day_15","profit_day_20",
            "profit_day_10"))

stock_train_10$target_direction_10<- if_else(stock_train_10$target_direction_10 == 1, "buy","sell")

stock_train_10$target_direction_10 <- factor(stock_train_10$target_direction_10, levels=c("buy","sell"))

stock_test_10$target_direction_10<- if_else(stock_test_10$target_direction_10 == 1, "buy","sell")

stock_test_10$target_direction_10 <- factor(stock_test_10$target_direction_10, levels=c("buy","sell"))

rf_mod_10 <-
  rand_forest( min_n = 2,trees = 750) %>%
  set_engine("ranger",importance = "impurity", num.threads = cores) %>%
  set_mode("classification")%>%
  fit(target_direction_10~., data = stock_train_10)

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

#saveRDS(rf_mod_10,"SP500_rfml_trendbinary_10day_50.rds")

rm(rf_mod_10)
rm(stock_train_10)
rm(stock_test_10)

stock_train_15 <- train%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily,close_shift_3, target_3, target_direction_3, close_shift_5, target_5,target_direction_5, close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15,close_shift_20,
            target_20, target_direction_20,"lead_1","lead_2","lead_3", "lead_4","lead_5","lead_6", "lead_7","lead_8",
            "lead_9","lead_10", "lead_11" ,"lead_12",
            "lead_13", "lead_14","lead_15","lead_16","lead_17","lead_18",
            "lead_19" ,"lead_20","t1", "t2","t4","profit_day_3",
            "profit_day_5","profit_day_15","profit_day_20",
            "profit_day_10"))

stock_test_15 <- test%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily,close_shift_3, target_3, target_direction_3, close_shift_5, target_5,target_direction_5, close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15,close_shift_20,
            target_20, target_direction_20,"lead_1","lead_2","lead_3", "lead_4","lead_5","lead_6", "lead_7","lead_8",
            "lead_9","lead_10", "lead_11" ,"lead_12",
            "lead_13", "lead_14","lead_15","lead_16","lead_17","lead_18",
            "lead_19" ,"lead_20","t1", "t2","t4","profit_day_3",
            "profit_day_5","profit_day_15","profit_day_20",
            "profit_day_10"))

stock_train_15$target_direction_15<- if_else(stock_train_15$target_direction_15 == 1, "buy","sell")

stock_train_15$target_direction_15 <- factor(stock_train_15$target_direction_15, levels=c("buy","sell"))

stock_test_15$target_direction_15<- if_else(stock_test_15$target_direction_15 == 1, "buy","sell")

stock_test_15$target_direction_15 <- factor(stock_test_15$target_direction_15, levels=c("buy","sell"))
rf_mod_15 <-
  rand_forest( min_n = 2,trees = 750) %>%
  set_engine("ranger",importance = "impurity",num.threads = cores ) %>%
  set_mode("classification")%>%
  fit(target_direction_15~., data = stock_train_15)



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

#saveRDS(rf_mod_15,"SP500_rfml_trendbinary_15day_50.rds")

rm(rf_mod_15)
rm(stock_train_15)
rm(stock_test_15)


stock_train_20 <- train%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume,  return_lag1,
            return_daily,close_shift_3, target_3, target_direction_3, close_shift_5, target_5,target_direction_5,  close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15, target_direction_15,close_shift_20,
            target_20,"lead_1","lead_2","lead_3", "lead_4","lead_5","lead_6", "lead_7","lead_8",
            "lead_9","lead_10", "lead_11" ,"lead_12",
            "lead_13", "lead_14","lead_15","lead_16","lead_17","lead_18",
            "lead_19" ,"lead_20","t1", "t2","t4","profit_day_3",
            "profit_day_5","profit_day_15","profit_day_20",
            "profit_day_10")) 

stock_test_20 <- test%>%
  ungroup()%>%
  select(-c(symbol,date, open, high, low, close, volume,  return_lag1,
            return_daily,close_shift_3, target_3, target_direction_3, close_shift_5, target_5,target_direction_5,  close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15, target_direction_15,close_shift_20,
            target_20,"lead_1","lead_2","lead_3", "lead_4","lead_5","lead_6", "lead_7","lead_8",
            "lead_9","lead_10", "lead_11" ,"lead_12",
            "lead_13", "lead_14","lead_15","lead_16","lead_17","lead_18",
            "lead_19" ,"lead_20","t1", "t2","t4","profit_day_3",
            "profit_day_5","profit_day_15","profit_day_20",
            "profit_day_10"))


stock_train_20$target_direction_20<- if_else(stock_train_20$target_direction_20 == 1, "buy","sell")

stock_train_20$target_direction_20 <- factor(stock_train_20$target_direction_20, levels=c("buy","sell"))

stock_test_20$target_direction_20<- if_else(stock_test_20$target_direction_20 == 1, "buy","sell")

stock_test_20$target_direction_20 <- factor(stock_test_20$target_direction_20, levels=c("buy","sell"))
rf_mod_20 <-
  rand_forest( min_n = 2,trees = 750) %>%
  set_engine("ranger",importance = "impurity", num.threads = cores) %>%
  set_mode("classification")%>%
  fit(target_direction_20~., data = stock_train_20)

# test_pred_20_rf <- predict(train_fit_rf, stock_test_20, type = "prob") %>%
#   bind_cols(predict(train_fit_rf, stock_test_20)) %>%
#   bind_cols(select(stock_test_20, target_direction_20)) %>%
#   glimpse()




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

#saveRDS(rf_mod_20,"SP500_rfml_trendbinary_20day_50.rds")

rm(rf_mod_20)
rm(stock_train_20)
rm(stock_test_20)


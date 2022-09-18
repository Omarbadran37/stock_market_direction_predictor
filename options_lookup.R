
library(tidyverse)
library(tidyquant)
library(tidymodels)
library(randomForest)
library(xgboost)

rm(list = ls())


start_date<- "2016-01-01"
start_date <- Sys.Date()-365

symbols <- c("AMD","BABA","TGTX","DIS", "LCID",
             "SNOW", "TDOC","FB","COIN","NVDA","TSLA","TWTR",
             "CCJ","CSX","KKR","SQ","PTON","NKLA","UBER","DASH","MARA",
             "VERU","TLT","C","KO","WISH","CHWY","LUMN","ROKU","UPST","AMC")

symbols <- c("PTON","NKLA","UBER","DASH","MARA","VERU","TLT","C","KO")

symbols <- c("SHOP","DAL","AAL","UAL","TWTR","CHPT","HD",
             "KMI","KSS","CCL","M","HON","IOVA","SONO","BAC","UBER","XLF","PTON",
             "LAZR","LFG","CVNA","WAT","AGNC","QS","EEM","SYF","LYFT","KRE","WBD",
             "GLD","UNP","TTWO","MU","APPS","CSX","TECK")
symbols <-c("USO","PLUG", "SNAP", "HYG","ABNB","BAC","C", "NCLH", "CCL","ZIM",
            "XBI","AAPL","META")

symbols <-c("NCLH","DAL")

symbols <-c("UAL","DAL","CCL","EWZ","XOP","CMCSA","MT","UPRO","GDX","KHC","XME",
            "CVNA","ARKK","GLD","DVN","SOFI","EWT","SOXL","SVXY","IWM","XLE","PTON")

symbols <-c("T","EWZ","HYG","ARKK","INTC","F","SLG","GDX","UPS","BAC",
            "AAL","M")

symbols <- c("AMZN","UNG","TIP","MU","ASHR","ICLN","FXI","PLUG",
             "F","IYR","GPS","INTC")

sp_500 <-
  tq_get(symbols, from = start_date)

sp_500 %>%
  count(symbol)%>%
  arrange(n)

library(rameritrade)
login <- td_auth_loginURL('U52FLA9TXZNECPVAS01RP9OGCXNNJAI8', 'https://desmond_td')
token <- td_auth_refreshToken('U52FLA9TXZNECPVAS01RP9OGCXNNJAI8', 'https://desmond_td',
                              "https://desmond_td/?code=ploNlFtiw0MUeKlC08BpFMKsPHRe9oZjBJGMhn%2FaQfoglq2Vh2yKp7fzLFIolVvCRKaymYXCldaT2dZlc5%2BY0oEB1XYLhi4vJKC45LS1rTN%2BvgNk7Nooq59U18VAdmG54AUy62o%2F%2Bet8S%2B6l0sw%2FNQlEDzkdEkURuomHseyp%2FGeN5%2BuDCA2%2BsbFqWS23xKMZjRx50ojoi4FTHEHPsFb5RLLbjkSFGKQ%2Br3cOZZd6dWnDzmtUZLTbW7T6zCapdbKhKHodeFKd3X3JP4%2BacSxFPWVA%2BSAaFgCvXIVH5JZjR7jotSz3PwjEWG8uhtx4Lq6ly4PtBJojHGKBMAcUKXpRMCEYWvLsjnfzfnln%2FX9l5NWv3OvXTNyDwP1DkoemLtZoDaR9UC1QxX8FQCw3Fkm7ArVX5YX6HOndwMO2MmTPY7AIAaKEjiiHUSTv9uL100MQuG4LYrgoVi%2FJHHvlUJF1aEtzlqRB%2BRFQNw8UhSvcIVbPFKJXlZD5CeU9uzjRLsfdoUFWf9qjQJcT72Bl80nBGP29Alw%2F7e%2BlAbxvtqiMnWd8NZUgYGmQtWoifpxR7ADBXH1tk3tYHJu5iEwYUTXK9%2F4rrbYLIN3F%2B7caExhzfodaWrq1rgaiF3Umzf7dykiG5h1kTkiQrNdybYxjxGANqZ797DoiPSlDPZZ5WS%2BTe8%2FhDiHXxq8LvSHZ8%2BnCpqNbaZHYgoOwP7YxaAguydVa0SPn6SeMSsiMvSd0CaUxQTf3rR1rc7nW6c7BJm7zYK06i%2BOdJly8uX1jbNAqNaZrJuiagbS9EJHK5X8OmQYlsJBTKISjqsui1lrhZwpovEw6C1jEzzn6Xny4M50NpF6sEHRVrWWEpBypIi4y%2FBbugkdQAfLpQNQnQFzA24Zz%2FShRpl2e6FO7R8E%3D212FD3x19z9sWBHDJACbC00B75E")
saveRDS(token,'/secure/location/')



refreshToken = readRDS('/secure/location/')
access_token<- td_auth_accessToken('U52FLA9TXZNECPVAS01RP9OGCXNNJAI8', token)

list <- td_priceHistory(tickers = "PTON", startDate =first.date,endDate=last.date,freq = "daily", accessToken = access_token)

dat= data.frame()
for (symbol in df.SP500$Tickers) {
  # ... make some data
  tryCatch({
    list <- td_priceHistory(tickers = symbol, startDate =Sys.Date()-365,endDate=Sys.Date(),freq = "daily", accessToken = access_token)
    ##dat$symbol <- symbol  # maybe you want to keep track of which iteration produced it?
    ##row_call <- rownames_to_column(dat$calls, var= "stock_date")
    #dat[[symbol]] <- list # add it to your list
  },
  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


dat= data.frame()
for (symbol in df.SP500$Tickers) {
  # ... make some data
  tryCatch({
    list <- tq_get(x= symbol, get="stock.prices",
                   from = start_date)
    ##dat$symbol <- symbol  # maybe you want to keep track of which iteration produced it?
    ##row_call <- rownames_to_column(dat$calls, var= "stock_date")
    dat[[symbol]] <- list # add it to your list
  },
  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
tq_get(get="stock.prices",
       from = start_date)

# write.csv(stock_call,"weekly_stock_symbol.csv")
# 
# sp_500 <- tq_get(stock_call$column_label,
#                  from = start_date)

sp_500_1 <- sp_500 %>%
  select(symbol,date,open,high,low,close,volume)

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
  ungroup(symbol)%>%
  filter(date == Sys.Date()-1)

## multitrend Algo
stock_train_20 <- date_filter%>%
  select(-c(symbol,date, open, high, low, close, volume,  return_lag1,
            return_daily, close_shift_5, target_5,target_direction_5,  close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15, target_direction_15,close_shift_20,
            target_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar)) 
rf_mod_3 <- readRDS("SP500_rfml_trendpick_3.rds")

stock_pred_3_rf <-  predict(rf_mod_3, stock_train_20, type = "prob")

stock_pred_3_rf <- stock_pred_3_rf %>%
  rename(pred_buy_3= .pred_buy  ,pred_sell_3 = .pred_sell )

rm(rf_mod_3)

rf_mod_5 <- readRDS("SP500_rfml_trendpick_5.rds")

stock_pred_5_rf <-  predict(rf_mod_5, stock_train_20, type = "prob")

stock_pred_5_rf <- stock_pred_5_rf %>%
  rename(pred_buy_5= .pred_buy  ,pred_sell_5 = .pred_sell )

rm(rf_mod_5)

rf_mod_10 <- readRDS("SP500_rfml_trendpick_10.rds")

stock_pred_10_rf <-  predict(rf_mod_10, stock_train_20, type = "prob")

stock_pred_10_rf <- stock_pred_10_rf %>%
  rename(pred_buy_10= .pred_buy  ,pred_sell_10 = .pred_sell )

rm(rf_mod_10)

rf_mod_15 <- readRDS("SP500_rfml_trendpick_15.rds")

stock_pred_15_rf <-  predict(rf_mod_15, stock_train_20, type = "prob")

stock_pred_15_rf <- stock_pred_15_rf %>%
  rename(pred_buy_15= .pred_buy  ,pred_sell_15 = .pred_sell )

rm(rf_mod_15)

rf_mod_20 <- readRDS("SP500_rfml_trendpick_20.rds")

stock_pred_20_rf <-  predict(rf_mod_20, stock_train_20, type = "prob")

stock_pred_20_rf <- stock_pred_20_rf %>%
  rename(pred_buy_20= .pred_buy  ,pred_sell_20 = .pred_sell )

rm(rf_mod_20)




stock_pred_20_rf_symbol  <- bind_cols(stock_pred_20_rf, 
                                      select(date_filter, date,
                                             symbol, close,target_10,target_direction_10,
                                             target_5,target_direction_5, target_15,target_direction_15, 
                                             close_shift_15, close_shift_5, close_shift_20, 
                                             close_shift_15, close_shift_10,
                                             target_20,target_direction_20))

stock_pred_rf_symbol<- stock_pred_rf_symbol %>%
  select(date,symbol,pred_buy_3,pred_buy_5,pred_buy_10,pred_buy_15,pred_buy_20)

stock_pred_rf_symbol$avg <- rowMeans(stock_pred_rf_symbol[,3:7])




###old verision

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
  slice_max(.pred_buy, n=11)
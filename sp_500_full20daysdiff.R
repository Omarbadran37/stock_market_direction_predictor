
library(tidyverse)
library(tidyquant)
library(tidymodels)
library(randomForest)
library(xgboost)

rm(list = ls())


start_date <- "2012-01-01"
start_date <- Sys.Date()-365

sp_500 <- tq_index("SP500")

sp_500 <- tq_get("AAPL",get="stock.prices",
         from = start_date)
# remotes::install_github('msperlin/yfR') 
# 
library(yfR)

first.date <- Sys.Date()-365
last.date <- Sys.Date()

df.SP500 <- GetSP500Stocks()
tickers <- df.SP500$Tickers

l.out <- yf_get(tickers = sp_500$symbol,
                first_date = first.date,
                last_date = last.date)

n_distinct(l.out$ticker)

sp_500 <- l.out %>%
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

test <- sp_500_1

test$perc_lead <- ((lead(close, n=5))-(lead(open)))/(lead(open))*100


 map_dbl(5, function(x){
  look <- lead(test$close, n =x)
test <- test%>%
  mutate(one = (look-lead(open))/(lead(open))*100)
  
})

 look <- lead(test$close, n =5)
 test <- test%>%
   mutate(one = (look-lead(open))/(lead(open))*100)

 blue <-   function(x){
    stock_pred_rf_symbol%>%
     mutate(x = ((lead(close, n =x))-lead(open))/(lead(open))*100)
 }
blue(1)
lead_2<- blue(2)
lead_3<- blue(3)
lead_4<- blue(4)
lead_5<- blue(5)
lead_6<- blue(6)
lead_7<- blue(7)
lead_8<- blue(8)
lead_9<- blue(9)
lead_10<- blue(10)
lead_11<- blue(11)
lead_12<- blue(12)
lead_13<- blue(13)
lead_14<- blue(14)
lead_15<- blue(15)
lead_16<- blue(16)
lead_17<- blue(17)
lead_18<- blue(18)
lead_19<- blue(19)
lead_20<- blue(20)

output <- NULL

stock_pred_rf_symbol <- stock_pred_rf_symbol %>%
  group_by(symbol)

for (i in 1:20){
  test<-blue(i)
  stock_pred_rf_symbol[,ncol(stock_pred_rf_symbol)+1]<- test$x
  colnames(stock_pred_rf_symbol)[ncol(stock_pred_rf_symbol)]<-paste0("lead_",i)
  
  
  
}

sp_500_1%>%
  group_by(symbol)%>%
  lead(close, n = 20)
stock_pred_rf_symbol<- stock_pred_rf_symbol %>%
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
    lead_20 >=7~20
  )
)

stock_pred_rf_symbol <- stock_pred_rf_symbol %>%
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
              lead_20 <=-5~20
            )
  )

stock_pred_rf_symbol$t3 <- if_else(is.na(stock_pred_rf_symbol$t1) & is.na(stock_pred_rf_symbol$t2),if_else(stock_pred_rf_symbol$lead_20 >=0,1,0),0)
stock_pred_rf_symbol$t4 <- if_else(!is.na(stock_pred_rf_symbol$t1) | stock_pred_rf_symbol$t3 == 1,1,0)

test$yes <- test %>%
  if_else(if_any(lead_1:lead_20,~.>=7),1,0)

test %>%
  filter(if_any(lead_1:lead_20,~.>=7))

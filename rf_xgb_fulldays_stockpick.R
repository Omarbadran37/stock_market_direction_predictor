library(tidyverse)
library(quantmod)
library(tidyquant)
library(tidymodels)
library(randomForest)
library(glmnet)

cores <- parallel::detectCores()
cores

sp_500_1 <- read_csv("stockmodel_techind_data19.csv") 

##dont touch this date filter used for another verision of the 
##rf model with full week data, not just friday
# sp_500_1_datefiltered <- sp_500_1 %>%
#   filter(date >"2021-4-01" & date < "2022-01-21")

sp_500_1_datefiltered <- sp_500_1 %>%
  filter(date < "2022-01-21")

# sp_500_1_datefiltered <- sp_500_1 %>%
#   filter(day == "Friday")

sp_500_1_datefiltered <- sp_500_1_datefiltered %>%
  ungroup(symbol)

# sp_500_1_datefiltered <- sp_500_1_datefiltered %>%
#   filter(date >= "2018-01-01")

# multimetric <- metric_set(accuracy, bal_accuracy, sens, yardstick::spec, precision, recall, ppv, npv)
# multimetric(bind_cols(stock_test_5,test_pred_rf_5), truth = target_direction_5, estimate = .pred_class)

# test_stock_data_full <-sp_500_1 %>%
#   filter(date <"2021-01-01")

stock_split <- initial_split(sp_500_1, prop = 3/4, strata = "target_direction_5")
stock_train_5 <- training(stock_split)%>%
  select(-c(symbol,date, open, high, low, close, volume,  return_lag1,
            return_daily, close_shift_5, target_5, close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15, target_direction_15,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar)) %>%
  na.omit()
stock_test_5 <- testing(stock_split)%>%
  select(-c(symbol,date, open, high, low, close, volume, return_lag1,
            return_daily, close_shift_5, target_5, close_shift_10, target_10,
            target_direction_10, close_shift_15,target_15, target_direction_15,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar)) %>%
  na.omit()


stock_train_5$target_direction_5<- if_else(stock_train_5$target_direction_5 == 1, "buy","sell")

stock_train_5$target_direction_5 <- factor(stock_train_5$target_direction_5, levels=c("buy","sell"))

stock_test_5$target_direction_5<- if_else(stock_test_5$target_direction_5 == 1, "buy","sell")

stock_test_5$target_direction_5 <- factor(stock_test_5$target_direction_5, levels=c("buy","sell"))

# stock_ml <- stock_ml %>% ##sp_500_1 goes here other wise
#   ##ungroup(symbol)%>%
#   select(-c(symbol, open:target_5,close_shift_10:signal,))%>%
#   na.omit()
# 
# stock_recipe <- recipe(target_direction_5~., data = rows_instockprice_train)

rf_mod_5 <- 
  rand_forest(mtry = 20, min_n = 2,trees = 500) %>% 
  set_engine("ranger",importance = "impurity", num.threads = cores) %>% 
  set_mode("classification")%>%
  fit(target_direction_5~., data = stock_train_5)

test_pred_rf_5 <- predict(rf_mod_5, stock_test_5, type = "prob") %>%
  bind_cols(predict(rf_mod_5, stock_test_5)) %>%
  bind_cols(select(stock_test_5, target_direction_5)) %>%
  glimpse()


test_pred_rf_5 %>%
  sens(truth = target_direction_5,estimate = .pred_class)

test_pred_rf_5%>%
  spec(truth = target_direction_5,estimate = .pred_class)

test_pred_rf_5 %>%
  roc_auc(truth = target_direction_5,estimate = .pred_buy)

test_pred_rf_5%>%
  accuracy(truth = target_direction_5,estimate = .pred_class)

test_pred_rf_5%>%
  precision(truth = target_direction_5,estimate = .pred_class)

test_pred_rf_5%>%
  recall(truth = target_direction_5,estimate = .pred_class)

test_pred_rf_5%>%
  ppv(truth = target_direction_5,estimate = .pred_class)

test_pred_rf_5%>%
  npv(truth = target_direction_5,estimate = .pred_class)


test_pred_rf_5 %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_5), 
               alpha = 0.5)+
  ggtitle("Random Forest target day 5 ")+
  xlab("Probability of Yes")

library(vip)
rf_mod_5 %>%
  vip(geom = "point", num_features = 75)

test <- rolling_origin(sp_500,initial = 150000, assess = 75000)

multimetric <- metric_set(accuracy, bal_accuracy, sens, yardstick::spec, precision, recall, ppv, npv)
multimetric(bind_cols(stock_test_5,test_pred_rf_5), truth = target_direction_5, estimate = .pred_class)

saveRDS(rf_mod_5, "stock_pick_model_5day_rf_new.rds")

remove(rf_mod_5)
remove(stock_test_5)
remove(stock_train_5)


test <- sp_500_1 %>%
  filter(date == "2022-01-28")

friday <- Sys.Date()-wday(Sys.Date()+1)

date_filter <- sp_500_1 %>%
  filter(date == "2022-02-04")

rf_mod_5 <- readRDS("stock_pick_model_5day_rf_new.rds")

stock_ml <- test%>%
  ungroup(symbol)%>%
select(-c(symbol,date, open, high, low, close, volume, adjusted, return_lag1,
          return_daily, close_shift_5, target_5, close_shift_10, target_10,
          target_direction_10, close_shift_15,target_15, target_direction_15,close_shift_20,
          target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
          dn,mavg,up,sar))

stock_pred_5_rf <-  predict(rf_mod_5, stock_ml, type = "prob")%>%
  bind_cols(predict(rf_mod_5, stock_ml)) %>%
  bind_cols(select(stock_ml, target_direction_5)) %>%
  glimpse()
  
stock_pred_5_rf_symbol  <- bind_cols(stock_pred_5_rf, select(date_filter, symbol, close,close_shift_5, target_5))

stock_pred_5_rf_symbol$action <- if_else(stock_pred_5_rf_symbol$.pred_buy >=.50,"buy","sell")

stock_buy_2 <-stock_pred_5_rf_symbol %>%
  filter(action =="buy")

stock_buy_1 %>%
  filter(symbol == "HD")

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
  dplyr::slice(1)%>%
  select(1:2)


stock_call$this_week_option <- if_else((stock_call$start_date1-as.Date(friday))<=7,1,0)

stock_call <- stock_call %>%
  filter(start_date1 <= Sys.Date()+7)

stock_call_final <- stock_pred_5_rf_symbol %>%
  filter(symbol %in% stock_call$column_label)%>%
  slice_max(.pred_buy, n=30)

stock_call_final <- inner_join(x=stock_call_final, y=full_index %>%
                                 select(symbol, company, industry), 
                               by= "symbol")

stock_call_final %>%
  count(industry)%>%
  arrange(desc(n))

test <- full_index %>%
  filter(industry=="RETAIL: Building Materials")

stock_pred_10_rf <-  predict(rf_mod_10, stock_ml, type = "prob")

stock_pred_10_rf_symbol  <- bind_cols(stock_pred_10_rf, select(date_filter, symbol, close))

stock_pred_10_rf_symbol$action <- if_else(stock_pred_10_rf_symbol$.pred_buy >=.50,"buy","sell")

stock_buy <-stock_pred_10_rf_symbol %>%
  filter(action =="buy")%>%
  select(symbol)

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
  dplyr::slice(1)%>%
  select(1:2)


stock_call$this_week_option <- if_else((stock_call$start_date1-as.Date(friday))<=7,1,0)

stock_call <- stock_call %>%
  filter(start_date1 <= Sys.Date()+5)

stock_call_final <- stock_pred_5_rf_symbol %>%
  filter(symbol %in% stock_call$column_label)%>%
  slice_max(.pred_buy, n=20)

stock_call_final <- inner_join(x=stock_call_final, y=full_index %>%
                                 select(symbol, company, industry), 
                               by= "symbol")

stock_call_final %>%
  count(industry)
 
stock_pred_15_rf <-  predict(rf_mod_15, stock_ml, type = "prob")

stock_pred_15_rf_symbol  <- bind_cols(stock_pred_15_rf, select(date_filter, symbol, close))

stock_pred_15_rf_symbol$action <- if_else(stock_pred_15_rf_symbol$.pred_buy >=.50,"buy","sell")

stock_buy <-stock_pred_15_rf_symbol %>%
  filter(action =="buy")%>%
  select(symbol)

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



stock_call_15 <- bind_rows(stock_calls, .id = "column_label")


stock_call_15$symbol_char <-nchar(stock_call_15$column_label)+1
stock_call_15$end_symbol_char <- nchar(stock_call_15$column_label)+6

stock_call_15$start_date1 <- ymd(str_sub(stock_call_15$stock_date,
                                      start = stock_call_15$symbol_char,
                                      end =stock_call_15$end_symbol_char))

stock_call_15 <- stock_call_15 %>%
  relocate(start_date1, .after = column_label)
stock_call_15 <- stock_call_15%>%
  group_by(column_label)%>%
  dplyr::slice(1)%>%
  select(1:2)


stock_call_15$this_week_option <- if_else((stock_call_15$start_date1-as.Date(friday))<=15,1,0)

stock_call_15 <- stock_call_15 %>%
  filter(start_date1 <= Sys.Date()+5)

stock_call_final_15 <- stock_pred_15_rf_symbol %>%
  filter(symbol %in% stock_call_15$column_label)%>%
  slice_max(.pred_buy, n=50)

stock_call_final_15 <- inner_join(x=stock_call_final_15, y=full_index %>%
                                 select(symbol, company, industry), 
                               by= "symbol")

stock_call_final %>%
  count(industry)

write.csv(stock_call_final_15,"stock_picks_15day_220121.csv")


gb_mod_5 <- boost_tree(
  trees = 1000, 
  tree_depth = 6, min_n = 7, 
  loss_reduction = 0.0000000232,                     ## first three: model complexity
  sample_size = 0.817, mtry = 155,         ## randomness
  learn_rate = 0.0369,                         ## step size
) %>% 
  set_engine("xgboost", num.threads = cores) %>% 
  set_mode("classification")%>%
  fit(target_direction_5~., data = stock_train_5)

test_pred_gb_5 <- predict(gb_mod_5, stock_test_5, type = "prob") %>%
  bind_cols(predict(gb_mod_5, stock_test_5)) %>%
  bind_cols(select(stock_test_5, target_direction_5)) %>%
  glimpse()


test_pred_gb_5 %>%
  sens(truth = target_direction_5,estimate = .pred_class)

test_pred_gb_5%>%
  spec(truth = target_direction_5,estimate = .pred_class)

test_pred_gb_5 %>%
  roc_auc(truth = target_direction_5,estimate = .pred_buy)

test_pred_gb_5 %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_5), 
               alpha = 0.5)+
  ggtitle("XGB target 5 day ")+
  xlab("Probability of Yes")

saveRDS(gb_mod_5, "stock_pick_model_5day_newgb.rds")

### target direction 10

stock_split <- initial_split(sp_500_1, prop = 3/4, strata = "target_direction_10")
stock_train_10 <- training(stock_split)%>%
  select(-c(symbol,date, open, high, low, close, volume,  return_lag1,
            return_daily, close_shift_5, target_5,target_direction_5, close_shift_10, target_10,
            close_shift_15,target_15, target_direction_15,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar)) %>%
  na.omit()
stock_test_10 <- testing(stock_split)%>%
  select(-c(symbol,date, open, high, low, close, volume,  return_lag1,
            return_daily, close_shift_5, target_5,target_direction_5, close_shift_10, target_10,
            close_shift_15,target_15, target_direction_15,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar)) %>%
  na.omit()

stock_train_10$target_direction_10<- if_else(stock_train_10$target_direction_10 == 1, "buy","sell")

stock_train_10$target_direction_10 <- factor(stock_train_10$target_direction_10, levels=c("buy","sell"))

stock_test_10$target_direction_10<- if_else(stock_test_10$target_direction_10 == 1, "buy","sell")

stock_test_10$target_direction_10 <- factor(stock_test_10$target_direction_10, levels=c("buy","sell"))


##stock_recipe_10 <- recipe(target_direction_10~., data = stock_train_10)

rf_mod_10 <- 
  rand_forest(mtry = 20, min_n = 2,trees = 500) %>% 
  set_engine("ranger",importance = "impurity", num.threads = cores) %>% 
  set_mode("classification")%>%
  fit(target_direction_10~., data = stock_train_10)

test_pred_rf_10 <- predict(rf_mod_10, stock_test_10, type = "prob") %>%
  bind_cols(predict(rf_mod_10, stock_test_10)) %>%
  bind_cols(select(stock_test_10, target_direction_10)) %>%
  glimpse()


test_pred_rf_10 %>%
  sens(truth = target_direction_10,estimate = .pred_class)

test_pred_rf_10%>%
  spec(truth = target_direction_10,estimate = .pred_class)

test_pred_rf_10%>%
  accuracy(truth = target_direction_10,estimate = .pred_class)

test_pred_rf_10%>%
  precision(truth = target_direction_10,estimate = .pred_class)

test_pred_rf_10%>%
  recall(truth = target_direction_10,estimate = .pred_class)

test_pred_rf_10%>%
  ppv(truth = target_direction_10,estimate = .pred_class)

test_pred_rf_10%>%
  npv(truth = target_direction_10,estimate = .pred_class)

test_pred_rf_10 %>%
  roc_auc(truth = target_direction_10,estimate = .pred_buy)

test_pred_rf_10 %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_10), 
               alpha = 0.5)+
  ggtitle("Random Forest target 10 day ")+
  xlab("Probability of Yes")

multimetric <- metric_set( bal_accuracy, sens, yardstick::spec, precision, recall, ppv, npv)
multimetric(bind_cols(stock_test_10,test_pred_rf_10), truth = target_direction_10, estimate = .pred_class)

saveRDS(rf_mod_10, "stock_pick_model_10day_new_rf.rds")
remove(rf_mod_10)
remove(stock_test_10)
remove(stock_train_10)

rf_full_stock_test <-test_stock_data_full%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, adjusted, return_lag1,
            return_daily, close_shift_5, target_5,target_direction_5, close_shift_10, target_10,
            close_shift_15,target_15, target_direction_15,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar,day)) %>%
  na.omit()

test <- test_stock_data_full %>%
  na.omit()

rf_full_stock_test$target_direction_10<- if_else(rf_full_stock_test$target_direction_10 == 1, "buy","sell")

rf_full_stock_test$target_direction_10 <- factor(rf_full_stock_test$target_direction_10, levels=c("buy","sell"))



test_pred_rf_10 <- predict(rf_mod_10, rf_full_stock_test, type = "prob") %>%
  bind_cols(predict(rf_mod_10, rf_full_stock_test)) %>%
  bind_cols(select(rf_full_stock_test, target_direction_10)) %>%
  glimpse()


test_pred_rf_10 %>%
  sens(truth = target_direction_10,estimate = .pred_class)

test_pred_rf_10%>%
  spec(truth = target_direction_10,estimate = .pred_class)

test_pred_rf_10%>%
  accuracy(truth = target_direction_10,estimate = .pred_class)

test_pred_rf_10%>%
  precision(truth = target_direction_10,estimate = .pred_class)

test_pred_rf_10%>%
  recall(truth = target_direction_10,estimate = .pred_class)

test_pred_rf_10%>%
  ppv(truth = target_direction_10,estimate = .pred_class)

test_pred_rf_10%>%
  npv(truth = target_direction_10,estimate = .pred_class)

test_pred_rf_10 %>%
  roc_auc(truth = target_direction_10,estimate = .pred_buy)

test_pred_rf_10 %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_10), 
               alpha = 0.5)+
  ggtitle("Random Forest target 10 day ")+
  xlab("Probability of Yes")

test_pred_rf_10_1 <- test_pred_rf_10
  
test_1 <- test %>% 
  select(symbol,date,target_10,close)

test_pred_rf_10_combined <- bind_cols(test_pred_rf_10_1,test_1)

friday <- Sys.Date()-wday(Sys.Date()+1)

sum_per_symbol <- test_pred_rf_10_combined %>%
  group_by(symbol)%>%
  filter(.pred_class=="buy")%>%
  summarise(sum(target_10))
test_2<- test_pred_rf_10_combined%>%
  group_by(symbol)%>%
slice_max(date)%>%
  select(close)

join_sum_close_stock <- inner_join(x=sum_per_symbol, y= test_2, by="symbol")

remove(rf_mod_10)

gb_mod_10 <- boost_tree(
  trees = 1000, 
  tree_depth = 6, min_n = 7, 
  loss_reduction = 0.0000000232,                     ## first three: model complexity
  sample_size = 0.817, mtry = 155,         ## randomness
  learn_rate = 0.0369,                         ## step size
) %>% 
  set_engine("xgboost", num.threads = cores) %>% 
  set_mode("classification")%>%
  fit(target_direction_10~., data = stock_test_10)

test_pred_gb_10 <- predict(gb_mod_10, stock_test_10, type = "prob") %>%
  bind_cols(predict(gb_mod_10, stock_test_10)) %>%
  bind_cols(select(stock_test_10, target_direction_10)) %>%
  glimpse()


test_pred_gb_10 %>%
  sens(truth = target_direction_10,estimate = .pred_class)

test_pred_gb_10%>%
  spec(truth = target_direction_10,estimate = .pred_class)

test_pred_gb_10 %>%
  roc_auc(truth = target_direction_10,estimate = .pred_buy)

test_pred_gb_10 %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_10), 
               alpha = 0.5)+
  ggtitle("XGB target 10 day ")+
  xlab("Probability of Yes")

saveRDS(gb_mod_10, "stock_pick_model_10day_new_gb.rds")


### target direction 15

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

#stock_ml <- stock_ml %>% ##sp_500_1 goes here other wise
  ##ungroup(symbol)%>%
#  select(-c(symbol, open:target_5,close_shift_10:signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
#            dn,mavg,up,sar))%>%
#  na.omit()

#stock_recipe <- recipe(target_direction_15~., data = stock_train_15)

rf_mod_15 <- 
  rand_forest(mtry = 20, min_n = 2,trees = 500) %>% 
  set_engine("ranger",importance = "impurity", num.threads = cores) %>% 
  set_mode("classification")%>%
  fit(target_direction_15~., data = stock_train_15)

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

saveRDS(rf_mod_15, "stock_pick_model_15day_new_rf.rds")
remove(rf_mod_15)
remove(stock_test_15)
remove(stock_train_15)

rf_full_stock_test_15 <-test_stock_data_full%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, adjusted, return_lag1,
            return_daily, close_shift_5, target_5,target_direction_5, close_shift_10, target_10,
            close_shift_15,target_15, target_direction_10,close_shift_20,
            target_20, target_direction_20, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar,day)) %>%
  na.omit()

test_pred_rf_15 <- predict(rf_mod_15, rf_full_stock_test_15, type = "prob") %>%
  bind_cols(predict(rf_mod_15, rf_full_stock_test_15)) %>%
  bind_cols(select(rf_full_stock_test_15, target_direction_15)) %>%
  glimpse()

test <- test_stock_data_full %>%
  na.omit()

multimetric <- metric_set(accuracy, bal_accuracy, sens, yardstick::spec, precision, recall, ppv, npv)
multimetric(bind_cols(stock_test_15,test_pred_rf_15), truth = target_direction_15, estimate = .pred_class)

saveRDS(rf_mod_15, "stock_pick_model_15day_new_rf.rds")

gb_mod_15 <- boost_tree(
  trees = 1000, 
  tree_depth = 6, min_n = 7, 
  loss_reduction = 0.0000000232,                     ## first three: model complexity
  sample_size = 0.817, mtry = 155,         ## randomness
  learn_rate = 0.0369,                         ## step size
) %>% 
  set_engine("xgboost", num.threads = cores) %>% 
  set_mode("classification")%>%
  fit(target_direction_15~., data = stock_train_15)

test_pred_gb_15 <- predict(gb_mod_15, stock_test_15, type = "prob") %>%
  bind_cols(predict(gb_mod_15, stock_test_15)) %>%
  bind_cols(select(stock_test_15, target_direction_15)) %>%
  glimpse()


test_pred_gb_15 %>%
  sens(truth = target_direction_15,estimate = .pred_class)

test_pred_gb_15%>%
  spec(truth = target_direction_15,estimate = .pred_class)

test_pred_gb_15 %>%
  roc_auc(truth = target_direction_15,estimate = .pred_buy)

test_pred_gb_15 %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_15), 
               alpha = 0.5)+
  ggtitle("XGB target 15 day ")+
  xlab("Probability of Yes")

saveRDS(gb_mod_15, "stock_pick_model_15day_new_rf.rds")

### target direction 20


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


stock_ml <- stock_ml %>% ##sp_500_1 goes here other wise
  ##ungroup(symbol)%>%
  select(-c(symbol, open:target_10,close_shift_15:signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar))%>%
  na.omit()

stock_recipe_20 <- recipe(target_direction_20~., data = stock_train_20)

rf_mod_20 <- 
  rand_forest(mtry = 40, min_n = 2,trees = 500) %>% 
  set_engine("ranger",importance = "impurity", num.threads = cores) %>% 
  set_mode("classification")%>%
  fit(target_direction_20~., data = stock_train_20)

test_pred_rf_20 <- predict(rf_mod_20, stock_test_20, type = "prob") %>%
  bind_cols(predict(rf_mod_20, stock_test_20)) %>%
  bind_cols(select(stock_test_20, target_direction_20)) %>%
  glimpse()


test_pred_rf_20 %>%
  sens(truth = target_direction_20,estimate = .pred_class)

test_pred_rf_20%>%
  spec(truth = target_direction_20,estimate = .pred_class)

test_pred_rf_20 %>%
  roc_auc(truth = target_direction_20,estimate = .pred_buy)

test_pred_rf_20 %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_20), 
               alpha = 0.5)+
  ggtitle("Random Forest target 20 day ")+
  xlab("Probability of Yes")

rf_full_stock_test_20 <-test_stock_data_full%>%
  ungroup(symbol)%>%
  select(-c(symbol,date, open, high, low, close, volume, adjusted, return_lag1,
            return_daily, close_shift_5, target_5,target_direction_5, close_shift_10, target_10,
            close_shift_15,target_15, target_direction_10,close_shift_20,
            target_20, target_direction_15, macd,signal,macd..1:signal..1,hma_9:sma_50,DIp:ADX,
            dn,mavg,up,sar)) %>%
  na.omit()

test_pred_rf_20 <- predict(rf_mod_20, rf_full_stock_test_20, type = "prob") %>%
  bind_cols(predict(rf_mod_20, rf_full_stock_test_20)) %>%
  bind_cols(select(rf_full_stock_test_20, target_direction_20)) %>%
  glimpse()

test_pred_rf_20 %>%
  sens(truth = target_direction_20,estimate = .pred_class)

test_pred_rf_20%>%
  spec(truth = target_direction_20,estimate = .pred_class)

test_pred_rf_20 %>%
  roc_auc(truth = target_direction_20,estimate = .pred_buy)

test_pred_rf_20 %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_20), 
               alpha = 0.5)+
  ggtitle("Random Forest target 20 day ")+
  xlab("Probability of Yes")

multimetric <- metric_set(accuracy, bal_accuracy, sens, yardstick::spec, precision, recall, ppv, npv)
multimetric(bind_cols(stock_test_20,test_pred_rf_20), truth = target_direction_20, estimate = .pred_class)

saveRDS(rf_mod_20, "stock_pick_model_20day_new_rf.rds")

gb_mod_20 <- boost_tree(
  trees = 1000, 
  tree_depth = 6, min_n = 7, 
  loss_reduction = 0.0000000232,                     ## first three: model complexity
  sample_size = 0.817, mtry = 155,         ## randomness
  learn_rate = 0.0369,                         ## step size
) %>% 
  set_engine("xgboost", num.threads = cores) %>% 
  set_mode("classification")%>%
  fit(target_direction_20~., data = stock_train_20)

test_pred_gb_20 <- predict(gb_mod_20, stock_test_20, type = "prob") %>%
  bind_cols(predict(gb_mod_20, stock_test_20)) %>%
  bind_cols(select(stock_test_20, target_direction_20)) %>%
  glimpse()


test_pred_gb_20 %>%
  sens(truth = target_direction_20,estimate = .pred_class)

test_pred_gb_20%>%
  spec(truth = target_direction_20,estimate = .pred_class)

test_pred_gb_20 %>%
  roc_auc(truth = target_direction_20,estimate = .pred_buy)

test_pred_gb_20 %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_20), 
               alpha = 0.5)+
  ggtitle("XGB target 20 day ")+
  xlab("Probability of Yes")

saveRDS(gb_mod_20, "stock_pick_model_20day_new_gb.rds")

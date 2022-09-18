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
  mutate(close_shift_21 = lead(close, n=21))

sp_500_1 <- sp_500_1%>%
  mutate(target_21 = (close_shift_21-lead(open))/(lead(open))*100)

sp_500_1 <- sp_500_1 %>%
  mutate(target_direction_21 = if_else(target_21>0,1,0))


bb_lower_filter_with_direction <- sp_500_1 %>%
  filter(lowerBB_candle <1)

bb_lower_filter_lastfriday <- sp_500_1 %>%
  filter(date == friday)

test <- bb_lower_filter_lastfriday %>%
  filter(lowerBB_candle <1 |lowerBB_candle_lag5: lowerBB_candle_lag5 <1, rsi_14 <=35)

test1 <- bb_lower_filter_lastfriday %>%
  filter(lowerBB_candle <1 , rsi_14 <=35, close <100)

test2 <- bb_lower_filter_lastfriday %>%
  filter(lowerBB_candle <1 , rsi_14 <=30, close <100)

bb_lower_filter_with_direction %>%
  filter(target_direction_5 ==1)

bb_lower_filter_with_direction %>%
  filter(target_direction_10 ==1)

bb_lower_filter_with_direction %>%
  filter(target_direction_15 ==1)

bb_lower_filter_with_direction %>%
  filter(target_direction_21 ==1)

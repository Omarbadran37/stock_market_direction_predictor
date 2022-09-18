
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

look<- test_4 %>%
  arrange(symbol)





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

look <- left_join(x=sp_500_1,y=test_4, by=c("symbol","month_yr"))%>%
  arrange(symbol)

look <- look %>%
  group_by(symbol)%>%
  mutate(close_r1 = close/r1,
         close_r2 = close/r2,
         close_r3 = close/r3,
         close_s1 = close/s1,
         close_s2 = close/s2,
         close_s3 = close/s3)




test_4$pp <- (lag(test_4$max_high) + lag(test_4$min_low) + lag(test_4$close))/3
test_4$r1 <- (2*lag(test_4$pp)) -lag(test_4$min_low)
test_4$s1 <- (2*lag(test_4$pp)) -lag(test_4$max_high)
test_4$r2 <- lag(test_4$pp) + (test_4$max_high-test_4$min_low)
test_4$s2 <- lag(test_4$pp) - (lag(test_4$max_high)-lag(test_4$min_low))
test_4$r3 <- lag(test_4$max_high) - 2*(lag(test_4$pp)-lag(test_4$min_low))
test_4$s3 <- lag(test_4$min_low) - 2*(lag(test_4$max_high)-lag(test_4$pp))



test <- test%>%arrange(symbol)



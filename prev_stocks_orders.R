

prev_orders <- read.csv("~/Downloads/Webull_Orders_Records.csv")


  prev_orders$placed <- as.Date(prev_orders$Placed.Time, format =  "%m/%d/%Y")

prev_orders$time_diff <- Sys.Date() -prev_orders$placed 
glimpse(prev_orders)

# group_stocks <- prev_orders %>%
#   filter(time_diff >=26)
# sold_stocks <- prev_orders %>%
#   filter(Side == "Sell")
# 
# sell_these_stocks <- anti_join(x=group_stocks, y=sold_stocks,by="Symbol")



sell_these_stocks <- prev_orders %>%
  group_by(Symbol)%>%
  slice_max(placed)%>%
  filter(Side == "Buy" & time_diff >=26)



bought_stocks <- prev_orders %>%
  group_by(Symbol)%>%
  slice_max(placed)%>%
  filter(time_diff <26, Side =="Buy")%>%
  rename(symbol = Symbol)

current_holdings_status <- date_filter %>%
  select(symbol,close)

current_holdings_status <- left_join(x=current_holdings_status,
                                      y=bought_stocks %>%
                                        select(symbol,Price),
                                      by="symbol")
current_holdings_status$diff <- (current_holdings_status$close-current_holdings_status$Price)/(current_holdings_status$Price)*100

current_holdings_status%>%
  filter(diff >=0)



start_date <- "2015-01-01"
end_date <- Sys.Date()-7
sp_500 <- tq_index("SP500") %>%
  tq_get(get="stock.prices",
         from = start_date,
         end = end_date)
sp_500_1 <- sp_500 %>% 
  select(symbol, company, sector,date:volume)

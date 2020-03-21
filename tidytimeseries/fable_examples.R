# Example from fable: Tidy forecasting in R
# link: https://www.youtube.com/watch?v=6v3_AsbhqrE&list=PL12D676032C3A6B70&index=3
library(fable)
library(tsibbledata)
library(tsibble)
library(dplyr)
library(ggplot2)
library(lubridate)
library(feasts)
library(tidyr)

aus_retail <- tsibble::as_tsibble(tsibbledata::aus_retail)
vic_cafe <- aus_retail %>% dplyr::filter(State == "Victoria", 
                Industry == "Cafes, restaurants and takeaway food services")
fbl_cafe_fit %>% ggplot() + geom_line(aes(x = Month, y = Turnover))

tsibbledata::vic_elec %>% ggplot() + geom_line(aes(x = Date, y = Demand)) 

fbl_cafe_fit <- vic_cafe %>% model(ets = fable::ETS(log(Turnover) ~ season("A")))


tsbl_elec <- tsibbledata::vic_elec %>% filter(Time < ymd("2014-03-01"))
tbl_elec_fit <- tsbl_elec %>% 
  model(arima=fable::ARIMA(Demand ~ Temperature + I(Temperature^2)  ))
  
gg_arma(tbl_elec_fit)


fbl_cafe_fit %>% augment()
fbl_cafe_fit %>% tidy()
fbl_cafe_fit %>% glance()
fbl_cafe_fit %>% components()

fbl_cafe_fit %>% components() %>% gather(component, value, level, slope, season) %>% 
  ggplot(aes(x = Month, y = value)) + geom_line() + facet_grid(vars(component), scales="free_y")



fbl_cafe_fit %>% forecast(h = 48) %>% autoplot(vic_cafe)








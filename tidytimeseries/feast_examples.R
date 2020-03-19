# Based on useR! talk of Rob Hyndman 
# https://www.youtube.com/watch?v=Ykiuj16P450&list=PL12D676032C3A6B70
# exploring how tidyverse packages work with time series problems
library(tsibble)
library(feasts)
library(fabletools)
library(dplyr)
library(ggplot2)

# Graphicals
holidays <- tourism %>% filter(Purpose =="Holiday") %>% group_by(State) %>% summarise(Trips = sum(Trips))
holidays %>% autoplot(Trips)
holidays %>% gg_season(Trips)
holidays %>% gg_subseries(Trips)
holidays %>% ACF(difference(Trips, 4)) %>% autoplot()


# Decompositions
holidays %>% model(STL(Trips ~ season(window = "periodic"))) %>% components() %>% autoplot()

tourism %>% features(Trips, feature_set(tags="stl")) %>% ggplot(aes(x = trend_strength, y = seasonal_strength_year, col=Purpose)) + 
            geom_point() + facet_wrap(vars(State))

most_seasonal <- tourism %>% features(Trips, feature_set(tags="stl")) %>% filter(seasonal_strength_year == max(seasonal_strength_year))

tourism %>% right_join(most_seasonal, by=c("State","Region","Purpose")) %>% ggplot(aes(x = Quarter, y =Trips)) + 
            geom_line() + facet_grid(vars(State, Region, Purpose))

tourism_features <- tourism %>% features(Trips, feature_set(pkgs="feasts"))

pcs <-tourism_features %>% select(-State, -Region, -Purpose) %>% 
  prcomp(scale=TRUE) %>% augment(tourism_features)

pcs %>% ggplot(aes(x=.fittedPC1, y=.fittedPC2, col=Purpose)) + geom_point() + theme(aspect.ratio=1)


pcs %>% filter(.fittedPC1 == max(.fittedPC1)) %>% left_join(tourism, by= c("State","Region","Purpose")) %>%
  ggplot(aes(x = Quarter, y = Trips)) + geom_line() + facet_grid(vars(State, Region, Purpose)) +
  ggtitle("Outlying time series in PC space") + theme(legend.position = "None")


pcs %>% filter(.fittedPC1 > 10 & .fittedPC2 > 1 & .fittedPC2 < 5) %>% left_join(tourism, by= c("State","Region","Purpose")) %>%
  ggplot(aes(x = Quarter, y = Trips)) + geom_line() + facet_grid(vars(State, Region, Purpose)) +
  ggtitle("Outlying time series in PC space") + theme(legend.position = "None")










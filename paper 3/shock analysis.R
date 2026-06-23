library(tidyverse)
library(countrycode)

# read the data

# food inflation
infl <- read.csv("paper 3/data/food inflation.csv") %>% 
  select(time = TIME_PERIOD, value = OBS_VALUE, entity = REF_AREA_LABEL)  %>%
  mutate(entity = countryname(entity, "country.name")) %>%
  filter(entity %in% adii$entity) %>% mutate(date = as.Date(paste0(time, "-01")))

for(i in c("Poland", "Malaysia", "United States")){
  plot <- infl %>% filter(entity == i, date > as.Date("2020-01-01")) %>%
    ggplot(aes(x = date, y = value)) + geom_line() +
    geom_vline(xintercept = as.Date("2022-01-01")) + geom_vline(xintercept = as.Date("2025-03-01")) +
    labs(title = i)
  
  print(plot)
}




summary(infl)
head(infl)
unique(infl$FREQ_LABEL)


# detecting the shock (Ukraine)

infl %>% filter(date > as.Date("2022-02-01")) %>% group_by(entity) %>%
  mutate(shock = ((value/first(value))-1)*100)


# for each entity

# find the peak (when it stops rising and starts falling for the first time after the war)
# find the end of peak (the moment when it falls below the original value)
# measure the size of the peak (relative to starting value) and the n. of month it took to fall down

shocks <- data.frame(type = NULL, entity = NULL, shock_size = NULL, shock_len = NULL)

for(i in unique(infl$entity)){
  
  pre_war <- infl %>% filter(entity == i, date >= as.Date("2021-01-01") & date < as.Date("2022-01-01")) %>% select(value) %>% unlist()
  
  # Ukraine war
  vals <- infl %>% filter(entity == i, date >= as.Date("2022-02-01") & date < as.Date("2024-01-01")) %>% select(value) %>% unlist()
  start <- vals[1]
  
  if(length(vals) == 0) next
  
  peak_nr <- which.max(vals)
  peak <- vals[peak_nr]
  if(peak-start >= sd(pre_war)){
    diff_start <- vals[peak_nr:length(vals)]-start
    end <- peak_nr + first(which(diff_start<0))
    
    if(is.na(end)) end = length(vals)
    
    size = abs((peak-start))
    len = end # try this version - time to come down from a peak
    
  } else {
    size = NA
    len = NA
  }
  
  shocks <- rbind(shocks, data.frame(type = "Ukraine", entity = i, shock_size = size, shock_len = len))

  # trade war  
  pre_war <- infl %>% filter(entity == i, date >= as.Date("2024-03-01") & date < as.Date("2025-03-01")) %>% select(value) %>% unlist()
  
  vals <- infl %>% filter(entity == i, date >= as.Date("2025-03-01") & date < as.Date("2025-09-01")) %>% select(value) %>% unlist()
  start <- vals[1]
  
  if(length(vals) == 0) next
  
  peak_nr <- which.max(vals)
  peak <- vals[peak_nr]
  if(peak-start >= sd(pre_war)){
    diff_start <- vals[peak_nr:length(vals)]-start
    end <- peak_nr + first(which(diff_start<0))
    
    if(is.na(end)) end = length(vals)
    
    size = abs((peak-start))
    len = end # try this version - time to come down from a peak
    
  } else {
    size = NA
    len = NA
  }
  
  shocks <- rbind(shocks, data.frame(type = "trade_war", entity = i, shock_size = size, shock_len = len))
  
}

cor(na.omit(shocks[,-c(1,2)])) # surprisingly small correlation

summary(shocks)


library(sf)
library(rnaturalearth)
  
shocks <- shocks %>% mutate(code = countrycode(entity, origin = "country.name", destination = "iso3c"))

map_shocks <- world_moll %>% left_join(shocks, by = c("iso_a3_eh"="code"), multiple = "all") %>%
  mutate(shock_size = if_else(shock_size < 0, 0, if_else(shock_size > 10, 10, shock_size))) %>%
  ggplot() + geom_sf(aes(fill = shock_size)) +
  scale_fill_viridis_c(option = "plasma",begin = 0) +
  theme(text = element_text(size = 20, family="serif")) +
  labs(fill = "Food inflation shock")

map_shocks

map_shocks <- world_moll %>% left_join(shocks, by = c("iso_a3_eh"="code"), multiple = "all") %>%
  mutate(shock_size = if_else(shock_size < 0, 0, if_else(shock_size > 10, 10, shock_size))) %>%
  ggplot() + geom_sf(aes(fill = shock_len)) +
  scale_fill_viridis_c(option = "plasma",begin = 0) +
  theme(text = element_text(size = 20, family="serif")) +
  labs(fill = "Food inflation shock")

map_shocks

# shock size can be a moderating variable in the model

import <- read.csv("paper 3/data/goods imports.csv") %>% filter(TIME_PERIOD == 2024) %>%
  select(import = OBS_VALUE, entity = REF_AREA_LABEL)  %>%
  mutate(entity = countryname(entity, "country.name")) %>%
  filter(!is.na(entity))

exports <- read.csv("paper 3/data/exports.csv") %>% filter(TIME_PERIOD == 2023) %>%
  select(export = OBS_VALUE, entity = REF_AREA_LABEL)  %>%
  mutate(entity = countryname(entity, "country.name")) %>%
  filter(!is.na(entity))

summary(exports)

gdp <- read.csv("paper 3/data/gdp in dollars.csv") %>%  filter(TIME_PERIOD == 2024) %>%
  select(gdp = OBS_VALUE, entity = REF_AREA_LABEL)  %>%
  mutate(entity = countryname(entity, "country.name")) %>%
  filter(!is.na(entity))

import_gdp <- import %>% left_join(gdp, by = "entity") %>% mutate(import_dep = import/gdp) %>%
  select(entity, import_dep)
export_gdp <- exports %>% left_join(gdp, by = "entity") %>% mutate(export_dep = export/gdp) %>%
  select(entity, export_dep)

shocks %>% filter(type == "Ukraine", shock_size > 50)
model_data <- shocks %>% left_join(adii, by = "entity") %>% mutate(shock = shock_len/shock_size) %>%
  filter(shock_size > 0 & shock_size < 65) %>% filter(type == "trade_war") %>%
  left_join(export_gdp, by = "entity") #%>% filter(region == "Asia")

summary(model_data)

#%>% filter(type == "trade_war")
 # filter(region %in% c("Asia"))

summary(model_data)
model <- lm(shock_len ~ shock_size + I1 + I2 + I3 + I4 + I5 + I6 + import_dep, model_data)
model <- lm(shock_len ~ Index + shock_size + import_dep, model_data)
model <- lm(shock_len ~ shock_size + I6 + export_dep, model_data)
model <- lm(shock_len ~ Index + export_dep, model_data)

model <- lm(shock_size ~ shock_len + I1 + I2 + I3 + I4 + I5 + I6 + import_dep, model_data)
model <- lm(shock_size ~ shock_len + Index + import_dep, model_data)

summary(model)
plot(model)

cor(na.omit(model_data[,-c(1,4)]))
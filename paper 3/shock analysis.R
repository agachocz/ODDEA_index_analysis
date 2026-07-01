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

shocks <- data.frame(type = NULL, entity = NULL, shock_size = NULL, shock_len = NULL, pre_war = NULL)

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
    # end <- peak_nr + first(which(diff_start<0))
    end <- first(which(diff_start<0))
    
    if(is.na(end)) end = length(vals)
    
    size = abs((peak-start))
    len = end # try this version - time to come down from a peak
    
  } else {
    size = NA
    len = NA
  }
  
  shocks <- rbind(shocks, data.frame(type = "Ukraine", entity = i, shock_size = size, shock_len = len, pre_war = sd(pre_war)))

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
  
  shocks <- rbind(shocks, data.frame(type = "trade_war", entity = i, shock_size = size, shock_len = len, pre_war = sd(pre_war)))
  
}

cor(na.omit(shocks[,-c(1,2)])) # surprisingly small correlation

summary(shocks)


library(sf)
library(rnaturalearth)
  
shocks <- shocks %>% mutate(code = countrycode(entity, origin = "country.name", destination = "iso3c"))

map_shocks <- world_moll %>% left_join(shocks, by = c("iso_a3_eh"="code"), multiple = "all") %>%
  mutate(shock_size = if_else(shock_size < 0, 0, if_else(shock_size > 200, 100, shock_size))) %>%
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

gdp <- read.csv("paper 3/data/gdp in dollars.csv") %>%  filter(TIME_PERIOD == 2021) %>%
  select(gdp = OBS_VALUE, entity = REF_AREA_LABEL)  %>%
  mutate(entity = countryname(entity, "country.name")) %>%
  filter(!is.na(entity))

rus_export <- read.csv("paper 3/data/rus_exports_2021.csv", sep = ";") 
colnames(rus_export) <- c("Reporter", "entity", "Year", "trade_flow", "no_products", "product_share", "export_dollars", "partner_share")

rus_export <- rus_export %>% select(entity, rus_exp = export_dollars)  %>%
  mutate(entity = countryname(entity, "country.name"),
         rus_exp = as.numeric(str_remove_all(rus_exp, ","))) %>%
  filter(!is.na(entity)) %>% left_join(gdp, by = "entity") %>%
  mutate(rus_exp = rus_exp/gdp*1000)

summary(rus_export)


ukr_export <- read.csv("paper 3/data/ukr_exports_2021.csv", sep = ";") 
colnames(ukr_export) <- c("Reporter", "entity", "Year", "trade_flow", "no_products", "product_share", "export_dollars", "partner_share")

ukr_export <- ukr_export %>% select(entity, ukr_exp = export_dollars)  %>%
  mutate(entity = countryname(entity, "country.name"),
         ukr_exp = as.numeric(str_remove_all(ukr_exp, ","))) %>%
  filter(!is.na(entity)) %>% left_join(gdp, by = "entity") %>%
  mutate(ukr_exp = ukr_exp/gdp*1000) %>% select(-gdp)

summary(ukr_export)


shocks %>% filter(type == "Ukraine") %>% summary()
model_data <- shocks %>% left_join(adii, by = "entity") %>% mutate(shock = shock_size/shock_len) %>%
  filter(shock_size > 0 & shock_size < 150) %>% 
  filter(type == "Ukraine", region != "Europe") %>%
  left_join(rus_export, by = "entity") %>% left_join(ukr_export, by = "entity")

#%>% filter(region == "Asia")

summary(model_data)
summary(shocks)

#%>% filter(type == "trade_war")
 # filter(region %in% c("Asia"))

summary(model_data)
model <- lm(shock ~ I1 + I2 + I3 + I4 + I5 + I6 + rus_exp + ukr_exp, model_data)
model <- lm(shock ~ Index + rus_exp + ukr_exp + log(gdp), model_data)

model <- lm(shock_size ~  I1 + I2 + I3 + I4 + I5 + I6 + rus_exp + ukr_exp, model_data)
model <- lm(shock_size ~ Index + ukr_exp + log(gdp), model_data)

summary(model)
plot(model)

library(lmtest)
library(car)

cor(model_data[,c(4,12)])

vif(model) # OK
shapiro.test(model$residuals)
plot(model)

model_data[c(38,61,64),]
boxplot(shocks$shock_size)


# ALTERNATIVE APPROACH - SARIMA MODEL

install.packages("forecast")
library(forecast)

#infl <- infl %>% filter(date >= as.Date("2015-01-01") & date <= as.Date("2024-01-02"))

data <- infl %>% filter(entity %in% adii$entity)

shocks <- data.frame(type = NULL, entity = NULL, shock_size = NULL, shock_len = NULL)

for(i in unique(data$entity)){
  
train <- data %>% filter(entity == i, date < as.Date("2022-02-01"))
test <- data %>% filter(entity == i, date >= as.Date("2022-02-01"))
n <- length(test$value)

if(n == 0) next # skip if no data available

model <- auto.arima(train$value, seasonal = T, stepwise = F, trace = F)
f <- forecast(model, n)

upper <- f$upper[,1] # upper 80% interval
diffs <- test$value-upper

start <- first(which(diffs[1:5] > 0))
if(!is.na(start)){
  end <- first(which(diffs[start:n] < 0))-1
  shock_size = 0
  
  for(m in 1:(end-start)){
    shock_size <- shock_size + diffs[start+m]
  }
  
} else {
  end = NA 
  shock_size = NA
  }

shocks <- rbind(shocks, data.frame(type = "Ukraine", entity = i, shock_size = shock_size, shock_len = end))
print(i)

}


summary(shocks)
cor(na.omit(shocks[,3:4]))
coeftest(model)



plot(c(train$date, test$date), c(train$value, test$value), type = "l")
lines(test$date, test$value, type = "l", col = "blue")
lines(test$date, f$mean, col = "red")
lines(test$date, f$upper[,1], col = "green")

library(tidyverse)
library(countrycode)


# unemployment
unemployment <- read.csv("paper 3/data/unemployment.csv") %>% 
  filter(TIME_FORMAT_LABEL == "Monthly", UNIT_MEASURE_LABEL == "Persons") %>%
  select(time = TIME_PERIOD, value = OBS_VALUE, entity = REF_AREA_LABEL)  %>%
  mutate(entity = countryname(entity, "country.name")) %>%
  filter(entity %in% adii$entity) %>% mutate(date = as.Date(paste0(time, "-01")))
summary(unemployment)

unemployment %>% filter(entity == "United States") %>%
  ggplot(aes(x = date, y = value)) + geom_line() + geom_vline(xintercept = as.Date("2022-03-01"))


shocks <- data.frame(type = NULL, entity = NULL, shock_size = NULL, shock_len = NULL)

for(i in unique(unemployment$entity)){

    # pandemic
  vals <- unemployment %>% filter(entity == i, date >= as.Date("2020-02-01") & date <= as.Date("2022-03-01")) %>% 
    select(value) %>% unlist()
  
  if(length(vals) == 0) next
  
  start <- vals[1]
  
  diffs <- diff(vals)
  peak_nr <- which.max(vals)
  if(!is.na(peak_nr)){
    peak <- vals[peak_nr]
    diff_start <- vals-start
    end <- first(which(diff_start<0))
    
    shocks <- rbind(shocks, data.frame(type = "pandemic", entity = i, shock_size = abs((peak-start)/start), shock_len = end))
  }
}

cor(na.omit(shocks[,-c(1,2)])) # surprisingly small correlation

summary(shocks)

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

model_data <- shocks %>% left_join(adii, by = "entity") %>% mutate(shock = shock_len/shock_size) %>%
  filter(shock_size > 0 & shock_size < 100) #%>% #filter(type == "Ukraine")
  #filter(region %in% c("Asia"))

summary(model_data)
model <- lm(shock_len ~ shock_size + I1 + I2 + I3 + I4 + I5 + I6, model_data)
model <- lm(shock_len ~ shock_size + Index, model_data)
model <- lm(shock_size ~ shock_len + I1 + I2 + I3 + I4 + I5 + I6, model_data)
model <- lm(shock_size ~ shock_len + Index, model_data)

summary(model)
plot(model)

cor(na.omit(model_data[,-c(1,4)]))
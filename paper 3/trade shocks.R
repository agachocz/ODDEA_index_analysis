
# external trade
trade <- read.csv("paper 3/data/external trade.csv") %>% 
  filter(TIME_FORMAT_LABEL == "Quarterly", UNIT_MEASURE_LABEL == "Index") %>%
  select(time = TIME_PERIOD, value = OBS_VALUE, entity = REF_AREA_LABEL)  %>%
  mutate(entity = countryname(entity, "country.name")) %>%
  filter(entity %in% adii$entity) %>% mutate(time = str_replace(time, "Q1", "03"),
                                             time = str_replace(time, "Q2", "06"),
                                             time = str_replace(time, "Q3", "09"),
                                             time = str_replace(time, "Q4", "12")
                                             ) %>%
  mutate(date = as.Date(paste0(time, "-01")))
summary(trade)

unique(trade$TIME_FORMAT_LABEL)
unique(trade$entity)

trade %>% filter(entity == "Russia") %>%
  ggplot(aes(x = date, y = value)) + geom_line() + geom_vline(xintercept = as.Date("2022-01-01"))


shocks <- data.frame(type = NULL, entity = NULL, shock_size = NULL, shock_len = NULL)

for(i in unique(trade$entity)){
  
  # pandemic
  vals <- trade %>% filter(entity == i, date >= as.Date("2019-12-01") & date <= as.Date("2022-03-01")) %>% 
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


# kolokwium

x <- c(1, 3, 2, 3, 7, 5, 1)
y <- c(4, 3, 4, 4, 3, 1, 5)

model <- lm(y ~ x)
summary(model)
mean(x)
mean(y)

sum((x-mean(x))*(y-mean(y)))/(sum((x-mean(x))^2))
mean(y)-mean(x)*(-0.396)
sqrt(sum(model$residuals^2)/5*0.03465347)
X <- matrix(c(rep(1, 7), x), ncol = 2)

solve(t(X)%*%X)


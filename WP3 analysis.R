library(tidyverse)
data <- read.csv("numeric_variables.csv")



names <- c(1:58)[str_detect(colnames(data), "skills")]
skills <- data %>% select(entity = entityName, year = dataYear, all_of(names))

colnames(skills) <- c("entity", "year", "basic_safety", "above_safety", "basic_literacy", "above_literacy",
                      "basic_content", "above_content", "basic_communication", "above_communication",
                      "basic_solving", "above_solving")

skills_agg <- skills %>% filter(year > 2021 & year < 2025) %>%
  pivot_longer(cols = c(-"entity", -"year"), names_to = "var", values_to = "value") %>%
  mutate(level = substr(var, 1, 5)) %>% group_by(entity, level, var) %>%
  summarise(
    value = mean(value, na.rm = T)
  ) %>% group_by(entity, level) %>% summarise(
    value = mean(value, na.rm = T)
  ) %>% pivot_wider(names_from = level, values_from = value)

skills_agg %>% filter(!is.na(value))  # 43 kraje


# Connectivity
colnames(data)[c(5, 7, 24, 35, 39)]
conn <- data %>% select(entityName.x, dataYear, c(5, 7, 24, 35, 39))
colnames(conn) <- c("entity", "year", "mobile", "fixed", "home_internet", "5G", "internet_use")

summary(conn)

conn_agg <- conn %>% filter(year > 2021 & year < 2025) %>%
  pivot_longer(cols = c(-"entity", -"year"), names_to = "var", values_to = "value") %>%
  group_by(entity, var) %>%
  summarise(
    value = mean(value, na.rm = T)
  ) %>% pivot_wider(names_from = var, values_from = value)




# markets
colnames(data)[c(41, 42, 46, 45)]
markets <- data %>% select(entityName.x, dataYear, c(41, 42, 46, 45))
colnames(markets) <- c("entity", "year", "employees", "foreign_investments", 
                       "investments", "revenue")

summary(markets)

markets_agg <- markets %>% filter(year > 2021 & year < 2025) %>%
  pivot_longer(cols = c(-"entity", -"year"), names_to = "var", values_to = "value") %>%
  group_by(entity, var) %>%
  summarise(
    value = mean(value, na.rm = T)
  ) %>% pivot_wider(names_from = var, values_from = value)


data_2 <- read.csv("itu_data_2.csv")
names <- unique(data_2$seriesName)[c(1, 2, 3, 5, 6, 8)]

gov <- data_2 %>% filter(seriesName %in% names) %>% 
  select(seriesName, entityName, dataValue, dataYear) %>%
  mutate(dataValue = if_else(dataValue == "Yes", 1, 0)) %>%
  group_by(entityName, dataYear) %>% summarise(
    value = sum(dataValue)
  ) %>% group_by(entityName) %>% summarise(
    gov_policies = max(value)
  ) %>% rename(entity = entityName)


# Affordability
names <- unique(data_2$seriesName)[c(18, 19)]
afford <- data_2 %>% filter(seriesName %in% names, seriesUnits == "USD") %>% 
  select(seriesName, entityName, dataValue) %>%
  pivot_wider(names_from = seriesName, values_from = dataValue)

afford <- data_2 %>% filter(seriesName %in% names, seriesUnits == "PPP$") %>% 
  select(seriesName, entityName, dataValue) %>%
  pivot_wider(names_from = seriesName, values_from = dataValue)

colnames(afford) <- c("entity", "price_big", "price_small")

unique(afford$seriesUnits)

# combine data

all_data <- skills_agg %>% full_join(conn_agg, by = "entity") %>%
  full_join(markets_agg, by = "entity") %>% full_join(afford, by = "entity") %>%
  full_join(gov, by = "entity") %>% mutate(price_big = as.numeric(price_big),
                                           price_small = as.numeric(price_small))

write.csv(all_data, "all_data.csv")


summary(all_data)

# check which countries have NAs in how many variables

na_prevalence <- all_data %>% pivot_longer(cols = -entity, names_to = "var", values_to = "value") %>%
  group_by(entity) %>% summarise(
    na = sum(is.na(value))
  )

# clustering based on NA
all_data <- read.csv("all_data.csv")[,-1]
data_na <- all_data %>% mutate(across(-entity, is.na))

# choose the best countries (i.e. with the widest coverage)

summary(all_data)

# get 1/0 variable of whether there is data for a particular country and variable
# get every combination of variables and check for how many countries it would be available


comb <- expand.grid(rep(list(c(T, F)), 14))

first = TRUE
for(i in 1:(nrow(comb)-1)){
  
  k <- c(2:15)[unlist(comb[i,])]
  data <- all_data[,k]
  n <- sum(complete.cases(data))
  df <- data.frame(k = paste(k, collapse = ","), n = n, nvar = length(k))
  
  if(first) {
    results <- df
    first = FALSE
  } else {
    results <- rbind(results, df)
  }
}  

results %>% filter(n > 50, nvar > 5) %>% arrange(desc(nvar), desc(n)) %>% head(30)              

# clustering
k <- c(1,5,6,7,8,9,11,12,13,14,15)
data <- all_data[, k]
data <- data[complete.cases(data),]
data <- data %>% mutate(entity = if_else(str_detect(entity, "Bolivia"), "Bolivia", entity)) %>%
  mutate(entity = if_else(str_detect(entity, "Iran"), "Iran", entity)) %>%
  mutate(entity = if_else(str_detect(entity, "Türkiye"), "Turkey", entity)) %>%
  mutate(entity = if_else(str_detect(entity, "Hong Kong"), "Hong Kong", entity)) %>%
  mutate(entity = if_else(str_detect(entity, "Korea"), "South Korea", entity)) %>%
  mutate(entity = if_else(str_detect(entity, "Russia"), "Russia", entity)) %>%
  filter(entity != "State of Palestine")

gdp <- read.csv("GDP.csv", sep = ";") %>% mutate(GDP = str_remove_all(GDP, "\\$")) %>%
  mutate(GDP = as.numeric(str_remove_all(GDP, ",")))


data_gdp <- data %>% left_join(gdp, by = "entity") %>% select(-nr) %>%
  mutate(investments = investments/GDP*100, revenue = revenue/GDP*100)

data_gdp %>% filter(is.na(GDP)) %>% select(entity, GDP)

rownames(data) <- data$entity
d <- dist(scale(data_gdp[,-c(1, 12)]))
hc <- hclust(d, method = "ward.D")



# 
summary(data_skill)

mobile_traffic <- read.csv("Mobile_broadband.csv", sep = ";") %>%
  select(entity = entityName, traffic = dataValue, year = dataYear) %>%
  filter(year > 2020 & year < 2025) %>%
  group_by(entity) %>% summarise(
    traffic = mean(traffic, na.rm = T)
  )

gdp <- read.csv("GDP.csv", sep = ";") %>% mutate(GDP = str_remove_all(GDP, "\\$")) %>%
  mutate(GDP = as.numeric(str_remove_all(GDP, ",")))

pop <- read.csv("population.csv", sep = ";") %>% rename(entity = country) %>%
  mutate(population = as.numeric(str_remove_all(population, ",")))

providers <- read.csv("number_of_internet.csv", sep = ";") %>%
  select(entity = entityName, providers = dataValue, year = dataYear) %>%
  filter(year > 2020 & year < 2025) %>%
  group_by(entity) %>% summarise(
    providers = mean(providers, na.rm = T)
  )

data_skill <- all_data %>% left_join(mobile_traffic,  by = "entity") %>%
  mutate(entity = if_else(str_detect(entity, "Bolivia"), "Bolivia", entity)) %>%
  mutate(entity = if_else(str_detect(entity, "Iran"), "Iran", entity)) %>%
  mutate(entity = if_else(str_detect(entity, "Türkiye"), "Turkey", entity)) %>%
  mutate(entity = if_else(str_detect(entity, "Hong Kong"), "Hong Kong", entity)) %>%
  mutate(entity = if_else(str_detect(entity, "Korea"), "South Korea", entity)) %>%
  mutate(entity = if_else(str_detect(entity, "Russia"), "Russia", entity)) %>%
  mutate(entity = if_else(str_detect(entity, "Taiwan"), "Taiwan", entity)) %>%
  mutate(entity = if_else(str_detect(entity, "Brunei"), "Brunei Darussalam", entity)) %>%
  left_join(gdp, by = "entity") %>% select(-nr) %>%
  left_join(pop, by = "entity") %>% left_join(providers, by = "entity") %>%
  mutate(investments = investments/GDP*100, revenue = revenue/GDP*100) %>%
  mutate(traffic = traffic/population*100000) %>%
  mutate(price_big = price_big/(GDP/population)*1000, 
         price_small = price_small/(GDP/population)*1000) %>%
  filter(!is.na(basic), !is.na(above)) %>%
  filter(entity != "Malawi")

#data_skill %>% filter(is.na(population)) %>% select(entity)


  data_skill <- data_skill %>% #filter(!(entity %in% c("Malawi", "Mexico", "Uruguay", "Dominican Rep.", "Chile", "Canada", "Brazil", "Jamaica"))) %>%
  select(-foreign_investments, -employees, -GDP, -population, -providers) %>%
  mutate(gov_policies = if_else(str_detect(entity, "Taiwan"), 5, gov_policies)) %>%
  mutate(X5G = if_else(str_detect(entity, "Russia"), 0, X5G))%>%
  mutate(X5G = if_else(str_detect(entity, "Bosnia"), 0, X5G))%>%
  mutate(X5G = if_else(str_detect(entity, "Türkiye"), 0, X5G))

summary(data_skill)

rownames(data) <- data$entity
d <- dist(scale(data_skill[,-c(1)]))

#df <- na.omit(data_skill)
#d <- as.dist(sqrt(2*(1-cor(t(df[,-1])))))

hc <- hclust(d, method = "ward.D")

plot(hc, labels = data_skill$entity, main = "", sub = "", xlab = "")


clust <- cutree(hc,k=4)

# analyse the results within clusters

data_clust <- cbind(data_skill, cluster = clust)

names <- data_clust %>% group_by(cluster) %>% summarise(
  countries = paste(entity, collapse = ",")
)

names

cluster_means <- data_clust %>%
  group_by(cluster) %>% summarise(across(-c(entity), \(x) mean(x, na.rm = T)))

data_clust %>% left_join(gdp, by = "entity") %>% 
  left_join(pop, by = "entity") %>% mutate(GDP_pc = GDP/population) %>%
  group_by(cluster) %>%
  summarise(GDP = mean(GDP, na.rm = T), GDP_pc = mean(GDP_pc, na.rm = T))

install.packages("corrplot")
library(corrplot)

cor_data <- data_clust %>% left_join(gdp, by = "entity") %>% 
  left_join(pop, by = "entity") %>% mutate(GDP_pc = GDP/population)
  
cor_data <- cor_data[complete.cases(cor_data),-c(1,15:18)]
colnames(cor_data) <- c("Advanced skills", "Basic skills",
"5G coverage",
"Fixed broadband",
"Internet at home",
"Internet use",
"Mobile broadband",
"ICT investments",
"Telecom. revenue",
"Prices high",
"Prices low",
"Governance & policies",
"Mobile data traffic",
"GDP per capita")

corrplot(cor(cor_data), tl.col = "black")

install.packages("agricolae")
library(agricolae)

first <- T
for(i in 2:14){
  var <- colnames(data_clust)[i]
  df <- data_clust[,c(i,15)]
  colnames(df) <- c("var", "cluster")
  df$cluster <- as.factor(df$cluster)
  #print(kruskal.test(var ~ cluster, df))
  
  a <- aov(var ~ cluster, df)
  p_anova <- summary(a)[[1]][["Pr(>F)"]][1]
  p_kw <- kruskal.test(var ~ cluster, data = df)$p.value
  
  tukey <- TukeyHSD(a, "cluster")$cluster
  tukey_p <- tukey[, "p adj"]
  names(tukey_p) <- rownames(tukey)
  row <- c(zmienna = var, p_ANOVA = p_anova, p_KW = p_kw, tukey_p)
  
  if(first){
    wyniki <- row
    first <- F
  } else{
    wyniki <- rbind(wyniki, row)
  }
}

# konwersja na numeric i zaokrąglenie
wyniki <- as.data.frame(wyniki, stringsAsFactors = FALSE)
wyniki[,-1] <- round(as.numeric(unlist(wyniki[,-1])), 3)

wyniki <- wyniki[,c(1:5,7,6,8,9)]

write.csv(wyniki, "wyniki testow.csv")
write.csv(cluster_means,"cluster means.csv")
write.csv(names, "names.csv")

first = T

for(i in 2:14){
  print(colnames(data_clust)[i])
  df <- data_clust[,c(i,15)] %>% filter(cluster != 4)
  colnames(df) <- c("var", "cluster")
  df$cluster <- as.factor(df$cluster)
  #print(kruskal.test(var ~ cluster, df))
  
  a <- aov(var ~ cluster, df)
  print(summary(a))
  print(scheffe.test(a, "cluster", group = F)$comparison)
  #print(TukeyHSD(a))
  
  if(first){
    
  }
}




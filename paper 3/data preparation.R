library(tidyverse)

#install.packages("countrycode")
library(countrycode)

# ITU data hub

# THE DATA MAY BE TOO NEW - COLLECT NEW VALUES FROM THE DATA HUB!!!
itu_dh <- read.csv("paper 3/data/ITU_all_data.csv") %>% select(entity, internet_use, mobile) %>%
  mutate(internet_use = internet_use/100*20, mobile = mobile/400*20) %>%
  rename(I62_internet_use = internet_use, I61_mobile = mobile) %>%
  mutate(entity = countryname(entity, "country.name", "country.name"))


mobile <- read.csv("paper 3/data/itu_data_1.csv") %>% filter(dataYear <= 2018, seriesUnits == "per 100 people") %>%
  filter(seriesName == "Active mobile-broadband subscriptions") %>%
  mutate(entity = countryname(entityName, "country.name", "country.name")) %>%
  select(entity, year = dataYear, dataValue) %>% group_by(entity) %>% arrange(desc(year)) %>%
  filter(!is.na(entity) & !is.na(dataValue)) %>%
  summarise(dataValue = first(dataValue)) %>% #filter(!(entity %in% c("Macao SAR China", "United Arab Emirates"))) %>%
  mutate(dataValue = if_else(dataValue > 100, 100, dataValue)) %>%
  mutate(I61_mobile = dataValue/max(dataValue)*20) %>% select(entity, I61_mobile)

summary(mobile)

internet <- read.csv("paper 3/data/internet_users.csv") %>% 
  select(entity = REF_AREA_LABEL, year = TIME_PERIOD, dataValue = OBS_VALUE) %>%
  mutate(entity = countryname(entity, "country.name", "country.name")) %>%
  select(entity, year, dataValue) %>% group_by(entity) %>% arrange(desc(year)) %>%
  filter(!is.na(entity) & !is.na(dataValue)) %>%
  summarise(dataValue = first(dataValue)) %>%
  mutate(I62_internet_use = dataValue/100*20) %>% select(entity, I62_internet_use)
 
unique(itu_dh$entity)

# TRPC data protection index

trpc <- read.csv("paper 3/data/TRPC data protection index.txt") %>% 
  select(entity = Economy, I21_data_protection = Total.Score) %>%
  mutate(I21_data_protection = I21_data_protection/72*20) %>%
  mutate(entity = str_remove(entity, "\\*")) %>%
  mutate(entity = countryname(entity, destination = "country.name"))

files <- list.files("paper 3/data/")
files <- files[str_detect(files, "trpc")]

first <- TRUE
for(i in files){
  d <- read.csv(paste0("paper 3/data/", i), sep = ";")
  # print(ncol(d))
  
  colnames(d) <- c("entity", str_remove(i, ".csv"))
  
  if(first){
    trpc_add <- d
    first = FALSE
  } else {
    trpc_add <- full_join(trpc_add, d, by = "entity")
  }
}


trpc_add$I21_data_protection = rowMeans(trpc_add[,-1], na.rm = T)/6*20
trpc_add <- trpc_add %>% mutate(entity = countryname(entity, destination = "country.name"))

additional <- trpc_add[!(trpc_add$entity %in% trpc$entity), c(1,9)]
trpc <- rbind(trpc, additional)



# ITU Global Cybersecurity Index

cybersec <- read.csv("paper 3/data/ITU Global Cybersecurity Index.csv") %>%
  select(entity = Country.name, I22_legal = Legal, I23_Institutional = Organization, 
         I24_Technical = Technical, I25_Cooperation = Cooperation.Measures) %>%
  mutate(entity = countryname(entity, "country.name"))
  

# UN e-government - CAN POTENTIALLY REDUCE NA BY TAKING MULTIPLE YEARS

egov = read.csv("paper 3/data/EGOV_DATA_2022.csv") %>%
  select(entity = Country.Name, I63_gov_services = Online.Service.Index) %>%
  mutate(I63_gov_services = I63_gov_services*20) %>%
  mutate(entity = countryname(entity, "country.name"))


# Global Findex database

findex <- read.csv("paper 3/data/GlobalFindexDatabase2025.csv") %>% filter(group == "all") %>%
  select(entity = countrynewwb, I31_bank_services = g20_any, I32_digital_money = merchant_pay, year) %>% arrange(desc(year)) %>%
  mutate(I32_digital_money = ifelse(is.na(I32_digital_money) & !is.na(I31_bank_services), I31_bank_services, I32_digital_money)) %>%
  filter(!is.na(I31_bank_services)) %>% group_by(entity) %>% summarise(
    I31_bank_services = first(I31_bank_services),
    I32_digital_money = first(I32_digital_money)) %>%
  mutate(I31_bank_services = I31_bank_services*20, I32_digital_money = I32_digital_money*20) %>% 
  mutate(entity = countryname(entity, "country.name")) %>%
  filter(!is.na(entity))

summary(findex)


# 3.1 - using bank services: g20.any
# 3.2 - any digital money: merchant.pay


# ease of starting a new business

ease <- read.csv("paper 3/data/World Bank ease of starting a business.csv", sep = ";") %>%
  filter(entity != "Max Score") %>%
  mutate(I54_ease_of_business = (P1_regulation + P2_services + P3_efficiency)/3/100*20) %>%
  select(entity, I54_ease_of_business) %>%
  mutate(entity = countryname(entity, "country.name"))

#ease_add <- global_competetiveness_index %>% filter(str_detect(Indicator, "I1101_") | str_detect(Indicator, "I1102_")) %>%
#  group_by(entity) %>% summarise(Value = mean(Value, na.rm = T)) %>%
#  mutate(entity = countryname(entity, "country.name", "country.name")) %>% select(entity, I54_ease_of_business = Value) %>%
#  mutate(I54_ease_of_business = (310.65-I54_ease_of_business)/310.65*20)

#missing <- ease_add$entity[!(ease_add$entity %in% ease$entity)]
#additional <- ease_add %>% filter(entity %in% missing)

#ease <- rbind(ease, additional)

# World Bank logistic performance index

logistics_serv = read.csv("paper 3/data/LPI quality of services.csv") %>%
  filter(COMP_BREAKDOWN_1_LABEL == "Metric: Score", TIME_PERIOD %in% c(2016,2018,2023)) %>%
  arrange(desc(TIME_PERIOD)) %>% select(entity = REF_AREA_LABEL, OBS_VALUE) %>%
  filter(!is.na(OBS_VALUE)) %>%
  group_by(entity) %>% summarise(OBS_VALUE = first(OBS_VALUE)) %>%
  rename(I15_logistic_services = OBS_VALUE) %>%
  mutate(I15_logistic_services = I15_logistic_services/5*20) %>%
  mutate(entity = countryname(entity, "country.name"))

logistics_infr = read.csv("paper 3/data/LPI quality of infrastructure.csv") %>%
  filter(COMP_BREAKDOWN_1_LABEL == "Metric: Score", TIME_PERIOD %in% c(2016,2018,2023)) %>%
  arrange(desc(TIME_PERIOD)) %>% select(entity = REF_AREA_LABEL, OBS_VALUE) %>%
  filter(!is.na(OBS_VALUE)) %>%
  group_by(entity) %>% summarise(OBS_VALUE = first(OBS_VALUE)) %>%
  rename(I14_logistic_infrastructure = OBS_VALUE) %>%
  mutate(I14_logistic_infrastructure = I14_logistic_infrastructure/5*20) %>%
  mutate(entity = countryname(entity, "country.name"))

# OECD trade facilitation - LOAD ALL THE FILES AND AGGREGATE

files <- list.files("paper 3/data/")
files <- files[str_detect(files, "OECD_")]

first <- TRUE
for(i in files){
  d <- read.csv(paste0("paper 3/data/", i), sep = ";") %>%
    mutate(X2017 = as.numeric(X2017), X2019 = as.numeric(X2019), X2022 = as.numeric(X2022)) %>%
    mutate(value = if_else(is.na(X2022), if_else(is.na(X2019), X2017, X2019), X2022)) %>%
    select(Country, value)
  
  colnames(d) <- c("entity", str_remove(i, ".csv"))
  
  if(first){
    oecd <- d
    first = FALSE
  } else {
    oecd <- full_join(oecd, d, by = "entity")
  }
}

summary(oecd)
# lots of NA: G1, G13, G2, G3

oecd <- oecd %>% mutate(I13_trade_procedures = OECD_A7/2*20, I12_digital_certificates = OECD_G11/2*20, 
                        I11_digital_trade = (OECD_G10+OECD_G12+OECD_G4+OECD_G6+OECD_G9)/12*20,
                        I33_electronic_transactions = OECD_G5/2*20) %>%
  select(entity, I11_digital_trade, I12_digital_certificates, I13_trade_procedures, I33_electronic_transactions) %>%
  mutate(entity = countryname(entity, "country.name", "country.name"))


# from the Global Competitiveness Index

# 4.3 - multi stakeholder collaboration
# 4.4 - digital skills in active population
# 4.5 - graduates skills
# 5.1 - availability of venture capital
# 5.3 - degree to which innovative companies can grow
# 6.4 - government responsive to disruption and change
# 6.5 - legal framework constructive to innovation

collaboration <- GCI_data %>% select(entity, I43_collaboration = WEF_GCI_MULTISTAKECOLLAB) %>%
  mutate(entity = countrycode(entity, "iso3c", "country.name")) %>%
  mutate(I43_collaboration = as.numeric(I43_collaboration)/100*20) %>%
  filter(!is.na(I43_collaboration))

digital_skills %>% filter(entity %in% c("Singapore", "Thailand", "Vietnam", "United States"))

missing <- data.frame(
  entity = c("Singapore", "Thailand", "Vietnam", "United States"),
  I43_collaboration = c(66.0, 52.1, 43.9, 73.9)/100*20
)

collaboration <- rbind(collaboration, missing)


digital_skills <- GCI_data %>% select(entity, I44_digital_skills = WEF_GCI_EOSQ508) %>%
  mutate(entity = countrycode(entity, "iso3c", "country.name")) %>%
  mutate(I44_digital_skills = as.numeric(I44_digital_skills)/100*20) %>%
  filter(!is.na(I44_digital_skills))

missing <- data.frame(
  entity = c("Thailand", "Vietnam", "United States"),
  I44_digital_skills = c(54.3, 46.1, 71.2)/100*20
)

digital_skills <- rbind(digital_skills, missing)


graduates_skills <- GCI_data %>% select(entity, I45_graduates_skills = WEF_GCI_GRADSKILLS) %>%
  mutate(entity = countrycode(entity, "iso3c", "country.name")) %>%
  mutate(I45_graduates_skills = as.numeric(I45_graduates_skills)/100*20) %>%
  filter(!is.na(I45_graduates_skills))

missing <- data.frame(
  entity = c("Singapore", "Thailand", "Vietnam", "United States"),
  I45_graduates_skills = c(73.4, 49.7, 41.2, 71.2)/100*20
)

graduates_skills <- rbind(graduates_skills, missing)


venture_capital <- GCI_data %>% select(entity, I51_venture_capital = WEF_GCI_EOSQ089) %>%
  mutate(entity = countrycode(entity, "iso3c", "country.name")) %>%
  mutate(I51_venture_capital = as.numeric(I51_venture_capital)/100*20) %>%
  filter(!is.na(I51_venture_capital))

missing <- data.frame(
  entity = c("Singapore", "Thailand", "Vietnam", "United States"),
  I51_venture_capital = c(63.5, 46.1, 37.8, 70.6)/100*20
)

venture_capital <- rbind(venture_capital, missing)


innovative_companies <- GCI_data %>% select(entity, I53_innovative_companies = WEF_GCI_EOSQ507) %>%
  mutate(entity = countrycode(entity, "iso3c", "country.name")) %>%
  mutate(I53_innovative_companies = as.numeric(I53_innovative_companies)/100*20) %>%
  filter(!is.na(I53_innovative_companies))


gov_responses <- GCI_data %>% select(entity, I64_gov_responses = WEF_GCI_EOSQ362) %>%
  mutate(entity = countrycode(entity, "iso3c", "country.name")) %>%
  mutate(I64_gov_responses = as.numeric(I64_gov_responses)/100*20) %>%
  filter(!is.na(I64_gov_responses))

missing <- data.frame(
  entity = c("Singapore", "Thailand", "Vietnam", "United States"),
  I64_gov_responses = c(85.2, 47.9, 49.4, 68.9)/100*20
)

gov_responses <- rbind(gov_responses, missing)

# not sure if this is the correct indicator
innovation_framework <- GCI_data %>% select(entity, I65_innovation_framework = WEF_GCI_EOSQ509) %>%
  mutate(entity = countrycode(entity, "iso3c", "country.name")) %>%
  mutate(I65_innovation_framework = as.numeric(I65_innovation_framework)/100*20) %>%
  filter(!is.na(I65_innovation_framework))


# intellectual property rights protection

intellectual_property <- GCI_data %>% select(entity, I55_intellectual_property = WEF_GCI_EOSQ052) %>%
  mutate(entity = countrycode(entity, "iso3c", "country.name")) %>%
  mutate(I55_intellectual_property = as.numeric(I55_intellectual_property)/100*20) %>%
  filter(!is.na(I55_intellectual_property))

missing <- data.frame(
  entity = c("Singapore", "Thailand", "Vietnam", "United States"),
  I55_intellectual_property = c(89.3, 45.3, 44.4, 78.3)/100*20
)

intellectual_property <- rbind(intellectual_property, missing)

# RnD expenditure

rnd_expenditure <- GCI_data %>% select(entity, I52_rnd_expenditure = WEF_GCI_RDSPENDING) %>%
  mutate(entity = countrycode(entity, "iso3c", "country.name")) %>%
  mutate(I52_rnd_expenditure = as.numeric(I52_rnd_expenditure)/100*20) %>%
  filter(!is.na(I52_rnd_expenditure))

missing <- data.frame(
  entity = c("Thailand", "Vietnam", "United States"),
  I52_rnd_expenditure = c(26.0, 14.7, 91.5)/100*20
)

rnd_expenditure <- rbind(rnd_expenditure, missing)


knowledge_emp <- read.csv("paper 3/data/knowledge intensive employment.csv", sep = ";") %>%
  mutate(I42_knowledge_emp = as.numeric(str_remove(knowledge_emp, "%"))) %>% select(entity, I42_knowledge_emp)  %>%
  mutate(entity = countryname(entity, "country.name", "country.name")) %>%
  mutate(I42_knowledge_emp = I42_knowledge_emp/100*20)

# id and digital id

have_id <- read.csv("paper 3/data/UN id data.csv") %>% filter(str_detect(Series.Name, "ID ownership")) %>%
  select(Country.Name, "X2021..YR2021.", "X2017..YR2017.") %>%
  pivot_longer(-Country.Name, names_to = "year", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%
  group_by(Country.Name) %>% summarise(I34_id_card = mean(value, na.rm = T)/100*20) %>%
  select(entity = Country.Name, I34_id_card) %>%
  mutate(entity = countryname(entity, "country.name", "country.name"))

# Australia does not have a central ID system, so I set the value to 0
have_id[have_id$entity == "Australia", 2] = 0


digital_id <- read.csv("paper 3/data/digital id system.csv") %>% filter(str_detect(Series.Name, "Online digital identity")) %>%
  select(Country.Name, "X2021..YR2021.", "X2017..YR2017.") %>%
  pivot_longer(-Country.Name, names_to = "year", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%
  group_by(Country.Name) %>% summarise(I35_digital_id = mean(value, na.rm = T)*20) %>%
  select(entity = Country.Name, I35_digital_id) %>%
  mutate(entity = countryname(entity, "country.name", "country.name"))


# graduates in STEM - 2021
summary(graduates_STEM)

graduates_STEM <- read.csv("paper 3/data/graduates in STEM.csv") %>% filter(year <= 2023) %>%
  group_by(geoUnit) %>%
  filter(!is.na(value)) %>% arrange(desc(year)) %>%
  summarise(value = first(value)) %>%
  select(entity = geoUnit, I41_stem_graduates = value) %>%
  mutate(entity = countrycode(entity, origin = "iso3c", destination = "country.name")) %>% 
  mutate(I41_stem_graduates = I41_stem_graduates/100*20)


# MERGING DATA

all_data <- graduates_skills %>% full_join(digital_skills, by = "entity") %>%
  full_join(collaboration, by = "entity") %>%
  full_join(rnd_expenditure, by = "entity") %>%
  full_join(gov_responses, by = "entity") %>%
  full_join(innovation_framework, by = "entity") %>%
  full_join(innovative_companies, by = "entity") %>%
  full_join(venture_capital, by = "entity") %>%
  full_join(oecd, by = "entity") %>%
  full_join(logistics_infr, by = "entity") %>%
  full_join(logistics_serv, by = "entity") %>%
  full_join(ease, by = "entity") %>%
  full_join(findex, by = "entity") %>%
  full_join(egov, by = "entity") %>%
  full_join(trpc, by = "entity") %>%
  full_join(cybersec, by = "entity") %>%
  full_join(internet, by = "entity") %>%
  full_join(mobile, by = "entity") %>%
  full_join(knowledge_emp, by = "entity") %>%
  full_join(have_id, by = "entity") %>%
  full_join(digital_id, by = "entity") %>%
  full_join(graduates_STEM, by = "entity") %>%
  full_join(intellectual_property, by = "entity") %>%
  filter(!is.na(entity)) %>% filter(!str_detect(entity, "country"))

#%>% drop_na()

summary(all_data)
sort(colnames(all_data))

clean_data <- all_data %>% drop_na()

all_data %>% filter(entity == "Australia") %>% is.na()
small_na <- all_data$entity[rowSums(is.na(all_data)) < 4]
all_data[rowSums(is.na(all_data)) < 5,] %>% summary()

clean_data <- all_data %>% filter(entity %in% small_na)

# biggest missing: bank services, digital money
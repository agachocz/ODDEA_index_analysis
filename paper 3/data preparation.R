library(tidyverse)

install.packages("countrycode")
library(countrycode)

# ITU data hub

itu_dh <- read.csv("paper 3/data/ITU_all_data.csv") %>% select(entity, internet_use, mobile) %>%
  mutate(internet_use = internet_use/100*20, mobile = mobile/100*20) %>%
  rename(I62_internet_use = internet_use, I61_mobile = mobile) %>%
  mutate(entity = countryname(entity, "country.name", "country.name"))

unique(itu_dh$entity)

# TRPC data protection index

trpc <- read.csv("paper 3/data/TRPC data protection index.txt") %>% 
  select(entity = Economy, I21_data_protection = Total.Score) %>%
  mutate(I21_data_protection = I21_data_protection/72*20) %>%
  mutate(entity = countryname(entity, "country.name", "country.name"))

# add maximum level of protection for the European Union

eu_countries <- subset(countrycode::codelist, 
                       eu28 == "EU", 
                       select = "country.name.en")$country.name.en

eu_trpc = data.frame(entity = eu_countries, I21_data_protection = 20)
trpc <- rbind(trpc, eu_trpc)

# ITU Global Cybersecurity Index

cybersec <- read.csv("paper 3/data/ITU Global Cybersecurity Index.csv") %>%
  select(entity = Country.name, I22_legal = Legal, I23_Institutional = Organization, 
         I24_Technical = Technical, I25_Cooperation = Cooperation.Measures) %>%
  mutate(entity = countryname(entity, "country.name", "country.name"))
  

# UN e-government - CAN POTENTIALLY REDUCE NA BY TAKING MULTIPLE YEARS

egov = read.csv("paper 3/data/EGOV_DATA_2022.csv") %>%
  select(entity = Country.Name, I63_gov_services = Online.Service.Index) %>%
  mutate(I63_gov_services = I63_gov_services*20) %>%
  mutate(entity = countryname(entity, "country.name", "country.name"))


# Global Findex database

findex <- read.csv("paper 3/data/GlobalFindexDatabase2025.csv") %>% filter(year >= 2018, group == "all") %>%
  select(entity = countrynewwb, I31_bank_services = g20_any, I32_digital_money = merchant_pay) %>% group_by(entity) %>%
  summarise(I31_bank_services = mean(I31_bank_services, na.rm = T), I32_digital_money = mean(I32_digital_money, na.rm = T)) %>%
  mutate(I31_bank_services = I31_bank_services*20, I32_digital_money = I32_digital_money*20) %>%
  mutate(I32_digital_money = ifelse(is.na(I32_digital_money) & !is.na(I31_bank_services), I31_bank_services, I32_digital_money)) %>%
    mutate(entity = countryname(entity, "country.name", "country.name"))


# 3.1 - using bank services: g20.any
# 3.2 - any digital money: merchant.pay


# ease of starting a new business

ease <- read.csv("paper 3/data/World Bank ease of starting a business.csv", sep = ";") %>%
  filter(entity != "Max Score") %>%
  mutate(I54_ease_of_business = (P1_regulation + P2_services + P3_efficiency)/3/100*20) %>%
  select(entity, I54_ease_of_business) %>%
  mutate(entity = countryname(entity, "country.name", "country.name"))

ease_add <- global_competetiveness_index %>% filter(str_detect(Indicator, "I1101_") | str_detect(Indicator, "I1102_")) %>%
  group_by(entity) %>% summarise(Value = mean(Value, na.rm = T)) %>%
  mutate(entity = countryname(entity, "country.name", "country.name")) %>% select(entity, I54_ease_of_business = Value) %>%
  mutate(I54_ease_of_business = (310.65-I54_ease_of_business)/310.65*20)

missing <- ease_add$entity[!(ease_add$entity %in% ease$entity)]
additional <- ease_add %>% filter(entity %in% missing)

ease <- rbind(ease, additional)

# World Bank logistic performance index

logistics_serv = read.csv("paper 3/data/LPI quality of services.csv") %>%
  filter(COMP_BREAKDOWN_1_LABEL == "Metric: Score", TIME_PERIOD %in% c(2016,2018,2023)) %>%
  select(entity = REF_AREA_LABEL, I15_logistic_services = OBS_VALUE) %>%
  group_by(entity) %>% summarise(I15_logistic_services = mean(I15_logistic_services, na.rm = T)) %>%
  mutate(I15_logistic_services = I15_logistic_services/5*20) %>%
  mutate(entity = countryname(entity, "country.name", "country.name"))

logistics_infr = read.csv("paper 3/data/LPI quality of infrastructure.csv") %>%
  filter(COMP_BREAKDOWN_1_LABEL == "Metric: Score", TIME_PERIOD %in% c(2016,2018,2023)) %>%
  select(entity = REF_AREA_LABEL, I14_logistic_infrastructure = OBS_VALUE) %>%
  group_by(entity) %>% summarise(I14_logistic_infrastructure = mean(I14_logistic_infrastructure, na.rm = T)) %>%
  mutate(I14_logistic_infrastructure = I14_logistic_infrastructure/5*20) %>%
  mutate(entity = countryname(entity, "country.name", "country.name"))

# OECD trade facilitation - LOAD ALL THE FILES AND AGGREGATE

files <- list.files("paper 3/data/")
files <- files[str_detect(files, "OECD_")]

first <- TRUE
for(i in files){
  d <- read.csv(paste0("paper 3/data/", i), sep = ";") %>%
    mutate(X2017 = as.numeric(X2017), X2019 = as.numeric(X2019), X2022 = as.numeric(X2022)) %>%
    pivot_longer(-Country, names_to = "year", values_to = "value") %>%
    group_by(Country) %>% summarise(value = mean(value, na.rm = T))
  
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
                        I11_digital_trade = (OECD_G10+OECD_G12+OECD_G4+OECD_G5+OECD_G6+OECD_G9)/12*20,
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

collaboration <- global_competetiveness_index %>% filter(str_detect(Indicator, "I1204_")) %>%
  mutate(entity = countryname(entity, "country.name", "country.name")) %>% select(entity, I43_collaboration = Value) %>%
  mutate(I43_collaboration = I43_collaboration/7*20)

digital_skills <- global_competetiveness_index %>% filter(str_detect(Indicator, "I605_")) %>%
  mutate(entity = countryname(entity, "country.name", "country.name")) %>% select(entity, I44_digital_skills = Value) %>%
  mutate(I44_digital_skills = I44_digital_skills/7*20)

graduates_skills <- global_competetiveness_index %>% filter(str_detect(Indicator, "I604_")) %>%
  mutate(entity = countryname(entity, "country.name", "country.name")) %>% select(entity, I45_graduates_skills = Value) %>%
  mutate(I45_graduates_skills = I45_graduates_skills/7*20)

venture_capital <- global_competetiveness_index %>% filter(str_detect(Indicator, "I903_")) %>%
  mutate(entity = countryname(entity, "country.name", "country.name")) %>% select(entity, I51_venture_capital = Value) %>%
  mutate(I51_venture_capital = I51_venture_capital/7*20)

innovative_companies <- global_competetiveness_index %>% filter(str_detect(Indicator, "I1107_")) %>%
  mutate(entity = countryname(entity, "country.name", "country.name")) %>% select(entity, I53_innovative_companies = Value) %>%
  mutate(I53_innovative_companies = I53_innovative_companies/7*20)

gov_responses <- global_competetiveness_index %>% filter(str_detect(Indicator, "I121_")) %>%
  mutate(entity = countryname(entity, "country.name", "country.name")) %>% select(entity, I64_gov_responses = Value) %>%
  mutate(I64_gov_responses = I64_gov_responses/7*20)

# not sure if this is the correct indicator
innovation_framework <- global_competetiveness_index %>% filter(str_detect(Indicator, "I122_")) %>%
  mutate(entity = countryname(entity, "country.name", "country.name")) %>% select(entity, I65_innovation_framework = Value) %>%
  mutate(I65_innovation_framework = I65_innovation_framework/7*20)

# intellectual property rights protection

intellectual_property <- global_competetiveness_index %>% filter(str_detect(Indicator, "I115_")) %>%
  mutate(entity = countryname(entity, "country.name", "country.name")) %>% select(entity, I55_intellectual_property = Value) %>%
  mutate(I55_intellectual_property = I55_intellectual_property/7*20)


# RnD expenditure 20 points for the maximum value
global_competetiveness_index %>% filter(str_detect(Indicator, "I1207_")) %>% summarise(max = max(Value, na.rm = T))

rnd_expenditure <- global_competetiveness_index %>% filter(str_detect(Indicator, "I1207_")) %>%
  mutate(entity = countryname(entity, "country.name", "country.name")) %>% select(entity, I52_rnd_expenditure = Value) %>%
  mutate(I52_rnd_expenditure = I52_rnd_expenditure/4.3*20)

knowledge_emp <- read.csv("paper 3/data/knowledge intensive employment.csv", sep = ";") %>%
  mutate(I42_knowledge_emp = as.numeric(str_remove(knowledge_emp, "%"))) %>% select(entity, I42_knowledge_emp)  %>%
  mutate(entity = countryname(entity, "country.name", "country.name"))

# id and digital id

have_id <- read.csv("paper 3/data/UN id data.csv") %>% filter(str_detect(Series.Name, "ID ownership")) %>%
  select(Country.Name, "X2021..YR2021.", "X2017..YR2017.", "X2024..YR2024.") %>%
  pivot_longer(-Country.Name, names_to = "year", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%
  group_by(Country.Name) %>% summarise(I34_id_card = mean(value, na.rm = T)/100*20) %>%
  select(entity = Country.Name, I34_id_card) %>%
  mutate(entity = countryname(entity, "country.name", "country.name"))


digital_id <- read.csv("paper 3/data/digital id system.csv") %>% filter(str_detect(Series.Name, "Online digital identity")) %>%
  select(Country.Name, "X2021..YR2021.", "X2017..YR2017.", "X2024..YR2024.") %>%
  pivot_longer(-Country.Name, names_to = "year", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%
  group_by(Country.Name) %>% summarise(I35_digital_id = mean(value, na.rm = T)/100*20) %>%
  select(entity = Country.Name, I35_digital_id) %>%
  mutate(entity = countryname(entity, "country.name", "country.name"))


# graduates in STEM - 2021
summary(graduates_STEM)

graduates_STEM <- read.csv("paper 3/data/graduates in STEM.csv") %>% group_by(geoUnit) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  select(entity = geoUnit, I41_stem_graduates = value) %>%
  mutate(entity = countrycode(entity, origin = "iso3c", destination = "country.name")) %>% 
  mutate(I41_stem_graduates = I41_stem_graduates/61.4*20)


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
  full_join(itu_dh, by = "entity") %>%
  full_join(knowledge_emp, by = "entity") %>%
  full_join(have_id, by = "entity") %>%
  full_join(digital_id, by = "entity") %>%
  full_join(graduates_STEM, by = "entity") %>%
  full_join(intellectual_property, by = "entity") %>%
  filter(!is.na(entity))

#%>% drop_na()

summary(all_data)
sort(colnames(all_data))

clean_data <- all_data %>% drop_na()

all_data %>% filter(entity == "Germany") %>% is.na()
small_na <- all_data$entity[rowSums(is.na(all_data)) < 3]
all_data[rowSums(is.na(all_data)) < 3,] %>% summary()

# biggest missing: data protection, knowledge_emp, stem_graduates
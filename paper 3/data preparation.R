library(tidyverse)

install.packages("countrycode")
library(countrycode)

standardized_names <- countrycode(
  sourcevar = messy_countries,
  origin = "country.name",
  destination = "country.name"
)

# ITU data hub

itu_dh <- read.csv("paper 3/data/ITU_all_data.csv") %>% select(entity, internet_use, mobile) %>%
  mutate(internet_use = internet_use/100*20, mobile = mobile/100*20) %>%
  rename(I63_internet_use = internet_use, I62_mobile = mobile) %>%
  mutate(entity = countryname(entity))

unique(itu_dh$entity)

# TRPC data protection index

trpc <- read.csv("paper 3/data/TRPC data protection index.txt") %>% 
  select(entity = Economy, I21_data_protection = Total.Score) %>%
  mutate(I21_data_protection = I21_data_protection/72*20) %>%
  mutate(entity = countryname(entity))

# ITU Global Cybersecurity Index

cybersec <- read.csv("paper 3/data/ITU Global Cybersecurity Index.csv") %>%
  select(entity = Country.name, I22_legal = Legal, I23_Institutional = Organization, 
         I24_Technical = Technical, I25_Cooperation = Cooperation.Measures) %>%
  mutate(entity = countryname(entity))
  

# UN e-government

egov = read.csv("paper 3/data/EGOV_DATA_2022.csv") %>%
  select(entity = Country.Name, I63_gov_services = Online.Service.Index) %>%
  mutate(I63_gov_services = I63_gov_services*20) %>%
  mutate(entity = countryname(entity))


# Global Findex database

findex <- read.csv("paper 3/data/GlobalFindexDatabase2025.csv") %>% filter(year == 2021, group == "all") %>%
  select(entity = countrynewwb, I31_bank_services = g20_any, I32_digital_money = merchant_pay) %>%
  mutate(I31_bank_services = I31_bank_services*20, I32_digital_money = I32_digital_money*20) %>%
  mutate(entity = countryname(entity))

# 3.1 - using bank services: g20.any
# 3.2 - any digital money: merchant.pay

summary(findex)

# ease of starting a new business

ease <- read.csv("paper 3/data/World Bank ease of starting a business.csv", sep = ";") %>%
  filter(entity != "Max Score") %>%
  mutate(I54_ease_of_business = (P1_regulation + P2_services + P3_efficiency)/3/100*20) %>%
  select(entity, I54_ease_of_business) %>%
  mutate(entity = countryname(entity))

# World Bank logistic performance index

logistics_serv = read.csv("paper 3/data/LPI quality of services.csv") %>%
  filter(COMP_BREAKDOWN_1_LABEL == "Metric: Score", TIME_PERIOD == 2023) %>%
  select(entity = REF_AREA_LABEL, I14_logistic_services = OBS_VALUE) %>%
  mutate(I14_logistic_services = I14_logistic_services/5*20) %>%
  mutate(entity = countryname(entity))

logistics_infr = read.csv("paper 3/data/LPI quality of infrastructure.csv") %>%
  filter(COMP_BREAKDOWN_1_LABEL == "Metric: Score", TIME_PERIOD == 2023) %>%
  select(entity = REF_AREA_LABEL, I13_logistic_infrastructure = OBS_VALUE) %>%
  mutate(I13_logistic_infrastructure = I13_logistic_infrastructure/5*20) %>%
  mutate(entity = countryname(entity))

# OECD trade facilitation - LOAD ALL THE FILES AND AGGREGATE

files <- list.files("paper 3/data/")
files <- files[str_detect(files, "OECD_")]

first <- TRUE
for(i in files){
  d <- read.csv(paste0("paper 3/data/", i), sep = ";") %>% select(Country, X2022) %>%
    mutate(X2022 = as.numeric(X2022))
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
                        I11_digital_trade = (OECD_G10+OECD_G12+OECD_G4+OECD_G5+OECD_G6+OECD_G9)/12*20) %>%
  select(entity, I11_digital_trade, I12_digital_certificates, I13_trade_procedures) %>%
  mutate(entity = countryname(entity))


# from the Global Competitiveness Index

knowledge_emp <- read.csv("paper 3/data/knowledge intensive employment.csv", sep = ";") %>%
  mutate(I41_knowledge_emp = as.numeric(str_remove(knowledge_emp, "%"))/100*20) %>%
  select(entity, I41_knowledge_emp) %>%
  mutate(entity = countryname(entity))

# multi-stakeholder collaboration

collaboration <- read.csv("paper 3/data/Multi-stakeholder Collaboration.csv", sep = ";") %>%
  mutate(I43_collaboration = as.numeric(stakeholder_collaboration)/100*20) %>%
  select(entity, I43_collaboration) %>%
  mutate(entity = countryname(entity))

# digital skills in active population

digital_skills <- read.csv("paper 3/data/Digital Skills in Active Population.csv", sep = ";") %>%
  mutate(I44_skills = as.numeric(digital_skills)/7*20) %>%
  select(entity, I44_skills) %>%
  mutate(entity = countryname(entity))

# skills of graduates

grad_skills <- read.csv("paper 3/data/Indicator 12.04_ Skillsets of Graduates.csv", sep = ";") %>%
  mutate(I45_grad_skills = as.numeric(grad_skills)/100*20) %>%
  select(entity, I45_grad_skills) %>%
  mutate(entity = countryname(entity))



# MERGING DATA

all_data <- grad_skills %>% full_join(digital_skills, by = "entity") %>%
  full_join(collaboration, by = "entity") %>%
  full_join(knowledge_emp, by = "entity") %>%
  full_join(oecd, by = "entity") %>%
  full_join(logistics_infr, by = "entity") %>%
  full_join(logistics_serv, by = "entity") %>%
  full_join(ease, by = "entity") %>%
  full_join(findex, by = "entity") %>%
  full_join(egov, by = "entity") %>%
  full_join(trpc, by = "entity") %>%
  full_join(cybersec, by = "entity") %>%
  full_join(itu_dh, by = "entity") %>%
  filter(!is.na(entity))

#%>% drop_na()

all_data %>% filter(entity == "Malaysia")

collaboration %>% filter(entity == "Malaysia")

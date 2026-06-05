library(tidyverse)

# ITU data hub

itu_dh <- read.csv("paper 3/data/ITU_all_data.csv") %>% select(entity, internet_use, mobile) %>%
  rename(I63_internet_use = internet_use, I62_mobile = mobile)

# TRPC data protection index

trpc <- read.csv("paper 3/data/TRPC data protection index.txt") %>% 
  select(entity = Economy, I21_data_protection = Total.Score)

# ITU Global Cybersecurity Index

trpc <- read.csv("paper 3/data/ITU Global Cybersecurity Index.csv") %>%
  select(entity = Country.name, I22_legal = Legal, I23_Institutional = Organization, 
         I24_Technical = Technical, I25_Cooperation = Cooperation.Measures)

# UN e-government

egov = read.csv("paper 3/data/EGOV_DATA_2022.csv") %>%
  select(entity = Country.Name, I63_gov_services = Online.Service.Index)

# Global Findex database - CHECK THE MEANING BEHIND VARIABLE NAMES

findex <- read.csv("paper 3/data/GlobalFindexDatabase2025.csv")

# ease of starting a new business

ease <- read.csv("paper 3/data/World Bank ease of starting a business.csv", sep = ";") %>%
  filter(entity != "Max Score") %>%
  mutate(I54_ease_of_business = (P1_regulation + P2_services + P3_efficiency)/3) %>%
  select(entity, I54_ease_of_business)

# World Bank logistic performance index

logistics_serv = read.csv("paper 3/data/LPI quality of services.csv") %>%
  select(entity = REF_AREA_LABEL, year = TIME_PERIOD, I14_logistic_services = OBS_VALUE)

logistics_infr = read.csv("paper 3/data/LPI quality of infrastructure.csv") %>%
  select(entity = REF_AREA_LABEL, year = TIME_PERIOD, I13_logistic_infrastructure = OBS_VALUE)

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
  select(entity, I11_digital_trade, I12_digital_certificates, I13_trade_procedures)



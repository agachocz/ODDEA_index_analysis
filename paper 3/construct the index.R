library(tidyverse)

# based on clean_data from data preparation.R

sort(colnames(clean_data))
summary(clean_data)

adii <- clean_data %>% mutate(
  P1 = I11_digital_trade + I12_digital_certificates + I13_trade_procedures + I14_logistic_infrastructure + I15_logistic_services,
  P2 = I21_data_protection + I22_legal + I23_Institutional + I24_Technical + I25_Cooperation,
  P3 = I31_bank_services + I32_digital_money + I33_electronic_transactions + I34_id_card + I35_digital_id,
  P4 = I41_stem_graduates + I42_knowledge_emp + I43_collaboration + I44_digital_skills + I45_graduates_skills,
  P5 = I51_venture_capital + I52_rnd_expenditure + I53_innovative_companies + I54_ease_of_business + I55_intellectual_property,
  P6 = I61_mobile + I62_internet_use + I63_gov_services + I64_gov_responses + I65_innovation_framework
  ) %>% mutate(Index = (P1+P2+P3+P5+P6)/6)

adii %>% select(P1:Index) %>% summary()

# compare with the original values

adii_asean <- read.csv("paper 3/data/ADII 2_0 scores.csv")
colnames(adii_asean) <- c("entity", "P1_org", "P2_org", "P3_org", "P4_org", "P5_org", "P6_org")
adii_asean <- adii_asean  %>% mutate(entity = countryname(entity, "country.name", "country.name"))

comparison <- adii_asean %>% left_join(adii, by = "entity") %>% 
  select(entity, P1_org, P1, P2_org, P2, P3_org, P3, P4_org, P4, P5_org, P5, P6_org, P6)

cor(na.omit(comparison[,-1])) # most have pretty high correlation, but the number of cases is low


# draw a map

install.packages("sf")
install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")

library(sf)
library(rnaturalearth)
#library(rnaturalearthdata)


world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != "Antarctica")

target_crs <- "+proj=moll"
world_moll <- world %>% st_transform(crs = target_crs)

export_data <- cbind(world_moll$sovereignt, world_moll$admin, world_moll$iso_a3_eh)

#write.csv(export_data, "map_tariff_data.csv")


adii <- adii %>% mutate(code = countrycode(entity, origin = "country.name", destination = "iso3c"))

map_adii <- world_moll %>% left_join(adii, by = c("iso_a3_eh"="code"), multiple = "all") %>%
  ggplot() + geom_sf(aes(fill = Index)) +
  scale_fill_viridis_c(option = "plasma",begin = 0) +
  theme(text = element_text(size = 20, family="serif")) +
  labs(fill = "ADII")

map_adii

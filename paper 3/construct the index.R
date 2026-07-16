library(tidyverse)

# based on clean_data from data preparation.R

#sort(colnames(clean_data))
summary(clean_data)

#codelist %>% filter(country.name.en %in% adii$entity) %>% select(un.region.name)

adii <- clean_data %>% pivot_longer(-entity, values_to = "value", names_to = "indicator") %>%
  mutate(pillar = substr(indicator, 1, 2)) %>% group_by(entity, pillar) %>% 
  summarise(value = mean(value, na.rm = T)/20*100) %>% pivot_wider(id_cols = entity, names_from = pillar, values_from = value) %>%
  mutate(Index = (I1+I2+I3+I4+I5+I6)/6) %>% arrange(entity)
  
#  mutate(
#  P1 = I11_digital_trade + I12_digital_certificates + I13_trade_procedures + I14_logistic_infrastructure + I15_logistic_services,
#  P2 = I21_data_protection + I22_legal + I23_Institutional + I24_Technical + I25_Cooperation,
#  P3 = I31_bank_services + I32_digital_money + I33_electronic_transactions + I34_id_card + I35_digital_id,
#  P4 = I41_stem_graduates + I42_knowledge_emp + I43_collaboration + I44_digital_skills + I45_graduates_skills,
#  P5 = I51_venture_capital + I52_rnd_expenditure + I53_innovative_companies + I54_ease_of_business + I55_intellectual_property,
#  P6 = I61_mobile + I62_internet_use + I63_gov_services + I64_gov_responses + I65_innovation_framework
#  ) %>% mutate(Index = (P1+P2+P3+P5+P6)/6) %>% arrange(entity)

region_names = codelist %>% filter(country.name.en %in% adii$entity) %>% arrange(country.name.en) %>%
  select(country.name.en, un.region.name)

adii <- adii %>% left_join(region_names, by = c("entity" = "country.name.en")) %>%
  rename(region = un.region.name)

adii %>% mutate(region = if_else(region == "Oceania", "Asia", region)) %>%
  pivot_longer(-c(entity, region), values_to = "value", names_to = "pillar") %>%
  group_by(pillar) %>%
  summarise(n = n(), m = mean(value), sd = sd(value)) %>% print(n = 35, digits = 2)

adii %>% group_by(region) %>% tally()
adii %>% #filter(region == "Asia") %>% 
  select(entity, Index, region) %>% arrange(desc(Index))

europe <- subset(codelist, un.region.name == "Europe", select = "country.name.en")
asia <- subset(codelist, un.region.name == "Asia", select = "country.name.en") 
africa <- subset(codelist, un.region.name == "Africa", select = "country.name.en")
oceania <- subset(codelist, un.region.name == "Oceania", select = "country.name.en")
americas <- subset(codelist, un.region.name == "Americas", select = "country.name.en")

unique(codelist$un.region.name)
colnames(codelist)

# compare with the original values

adii_asean <- read.csv("paper 3/data/ADII 2_0 scores.csv")
colnames(adii_asean) <- c("entity", "P1_org", "P2_org", "P3_org", "P4_org", "P5_org", "P6_org")
adii_asean <- adii_asean  %>% mutate(entity = countryname(entity, "country.name", "country.name"))
adii_asean$Index_org <- rowMeans(adii_asean[,-1])

comparison <- adii_asean %>% left_join(adii, by = "entity") %>% 
  select(entity, P1_org, I1, P2_org, I2, P3_org, I3, P4_org, I4, P5_org, I5, P6_org, I6, Index_org, Index) %>% drop_na()

comparison %>% group_by(entity) %>% summarise(
  RMSE = sqrt(((P1_org-I1)^2 + (P2_org-I2)^2 + (P3_org-I3)^2 + (P4_org-I4)^2 + (P5_org-I5)^2 + (P6_org-I6)^2)/6),
  MAPE = ((abs(P1_org-I1)/P1_org + abs(P2_org-I2)/P2_org + abs(P3_org-I3)/P3_org + 
             abs(P4_org-I4)/P4_org + abs(P5_org-I5)/P5_org + abs(P6_org-I6)/P6_org)/6)
) %>% ungroup() %>% summarise(RMSE = mean(RMSE), MAPE = mean(MAPE))

cor(na.omit(comparison[,-1])) # most have pretty high correlation, but the number of cases is low

cor(comparison$P6, comparison$P6_org)

# for table in the article

mean(abs(comparison$Index_org-comparison$Index)/comparison$Index_org)*100
mean((comparison$Index_org-comparison$Index))

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



# compare with DESI and ICT - overall a pretty good similarity

desi <- read.csv("compound indicators/DESI.csv") %>% filter(time_period == 2022, indicator == "desi_total") %>%
  mutate(ref_area = if_else(ref_area == "EL", "GR", ref_area)) %>%
  mutate(entity = countrycode(ref_area, origin = "iso2c", destination = "country.name"),
         DESI = value*100) %>% filter(!is.na(entity)) %>%
  select(entity, DESI)

#summary(desi)
#unique(desi$indicator)

desi_adii <- desi %>% inner_join(adii, by = "entity")
cor(desi_adii$DESI, desi_adii$Index, method = "p") # pretty good!

idi <- read.csv("paper 3/data/IDI 2023 Scores.csv") %>%
  mutate(entity = countryname(Economy, "country.name")) %>% 
  select(entity, IDI = IDI.Score)

idi_adii <- idi %>% inner_join(adii, by = "entity")
cor(idi_adii$IDI, idi_adii$Index, method = "s")

summary(idi_adii)


dii <- read.csv("compound indicators/DII.csv", sep = ";") %>% 
  select(entity = Entity, DII = Digital.Evolution.Score) %>%
  mutate(entity = countryname(entity, "country.name"))

dii_idii <- dii %>% inner_join(adii, by = "entity")
cor(dii_idii$DII, dii_idii$Index, method = "p")


boxplot(idi_adii$IDI)
boxplot(idi_adii$Index)
boxplot(desi$DESI)


# compare between indicators

desi_idi <- idi %>% inner_join(desi, by = "entity")
cor(desi_idi$IDI, desi_idi$DESI, method = "p")

desi_dii <- dii %>% inner_join(desi, by = "entity")
cor(desi_dii$DII, desi_dii$DESI, method = "s")

dii_idi <- idi %>% inner_join(dii, by = "entity")
cor(dii_idi$IDI, dii_idi$DII, method = "s")

library(tidyverse)

# ADII
adii <- read.csv("datasets/ADII.csv", sep = ";")
colnames(adii) <- c("Entity", "Trade_Logistics", "Cybersecurity",
                    "Digital_Payments", "Human_Skills", "Innovation",
                    "Institutions_Infrastructure")
adii$Score <- rowMeans(adii[,-1])

# DESI
desi <- read.csv("datasets/DESI.csv")

unique(desi$ref_area)

desi_pillars <- desi %>% filter(indicator == "desi") %>%
  select(Year = time_period, Key = ref_area, 
         indicator, breakdown, value) %>% 
  pivot_wider(names_from = "breakdown", values_from = "value") %>%
  rename(Connectivity = desi_conn, Public_Services = desi_dps,
         Human_Capital = desi_hc, Integration = desi_idt) %>%
  mutate(Score = (Connectivity + Public_Services + Human_Capital + Integration)/4)


# DII
dii <- read.csv("datasets/DII.csv", sep = ";") %>%
  select(Year, Entity, Region, Income = Income.Group, Score = Digital.Evolution.Score,
         Supply = Supply.Conditions.Score, Demand = Demand.Conditions.Score,
         Institutions = Institutional.Environment.Score, 
         Innovation = Innovation.and.Change.Score)

score_summary <- dii %>% group_by(Region, Income) %>% summarise(
  Mean_Score = mean(Score)
)

# WDCI
# doesn't include all pillars, probably useless
wdci <- read.csv("datasets/WDCI.csv", sep = ";", na.strings = "-")
colnames(wdci) <- c("Entity", "Key", "Year", "Human_Skills",
                    "Mobile_Users", "E_participation")

wdci_clean <- wdci %>% filter(!is.na(Human_Skills) & 
              !is.na(Mobile_Users) & !is.na(E_participation)) %>%
  mutate(Human_Skills = Human_Skills/max(Human_Skills),
         Mobile_Users = Mobile_Users/max(Mobile_Users),
         E_participation = E_participation/max(E_participation)) %>%
  mutate(Score = (Mobile_Users + Human_Skills + E_participation)/3)

country_key <- wdci %>% select(Entity, Key) %>% unique() %>% 
  slice(-c(66,67))
# Greece
country_key[21,]$Key = "EL"

# map country names in desi

# Malta is not included, but it's also missing from DII
country_key %>% filter(Entity == "Malta")
unique(desi_pillars$Key[!(desi_pillars$Key %in% country_key$Key)])

desi <- desi_pillars %>% left_join(country_key, by="Key")

# check for missing names
unique(dii$Entity)
unique(desi$Entity)
unique(adii$Entity)
unique(wdci$Entity)

dii %>% filter(Region == "WR") %>% select(Entity)
dii %>% filter(Entity == "South Korea") %>% select(Entity)

desi <- desi %>% mutate(Entity = if_else(Entity == "Czech Republic", 
                                         "Czechia", Entity)) %>%
  mutate(Entity = if_else(Entity == "Slovak Republic", 
                          "Slovakia", Entity))

adii <- adii %>% mutate(Entity = if_else(Entity == "Lao PDR", 
                                         "Laos", Entity))

unique(wdci$Entity[!(wdci$Entity %in% dii$Entity)])
unique(dii$Entity[!(dii$Entity %in% wdci$Entity)])

unique(desi$Entity[!(desi$Entity %in% dii$Entity)])
unique(dii$Entity[!(dii$Entity %in% wdci$Entity)])

wdci <- wdci %>% mutate(Entity = if_else(Entity == "Czech Republic", "Czechia", Entity)) %>%
  mutate(Entity = if_else(Entity == "Hong Kong SAR", "Hong Kong", Entity)) %>%
  mutate(Entity = if_else(Entity == "Korea Rep.", "South Korea", Entity)) %>%
  mutate(Entity = if_else(Entity == "Slovak Republic", "Slovakia", Entity)) %>%
  mutate(Entity = if_else(Entity == "Taiwan. Chinga", "Taiwan", Entity)) %>%
  mutate(Entity = if_else(Entity == "UAE", "United Arab Emirates", Entity)) %>%
  mutate(Entity = if_else(Entity == "USA", "United States", Entity))

# compare two global indicators
wdci_dii <- wdci_clean %>% inner_join(dii, by = c("Year", "Entity"))

cor(wdci_dii$Score.x, wdci_dii$Score.y)
plot(wdci_dii$Score.x, wdci_dii$Score.y)


# compare ASEAN to global
adii_wdci <- wdci_clean %>% filter(Year == 2019) %>% 
  inner_join(adii, by = c("Entity"))

cor(adii_wdci$Score.x, adii_wdci$Score.y)
plot(adii_wdci$Score.x, adii_wdci$Score.y)

adii_dii <- dii %>%
  inner_join(adii, by = c("Entity"))

cor(adii_dii$Score.x, adii_dii$Score.y)
plot(adii_dii$Score.x, adii_dii$Score.y)


# compare EU to global
desi_wdci <- wdci_clean %>%
  inner_join(desi_total, by = c("Key", "Year"))

cor(desi_wdci$Score.x, desi_wdci$Score.y)
plot(desi_wdci$Score.x, desi_wdci$Score.y)

desi_dii <- dii %>%
  inner_join(desi, by = c("Entity", "Year"))

cor(desi_dii$Score.x, desi_dii$Score.y)
plot(desi_dii$Score.x, desi_dii$Score.y)



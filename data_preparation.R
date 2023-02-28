library(tidyverse)

# ADII
adii <- read.csv("datasets/ADII.csv", sep = ";")
colnames(adii) <- c("Entity", "Trade_Logistics", "Cybersecurity",
                    "Digital_Payments", "Human Skills", "Innovation",
                    "Institutions_Infrastructure")
adii$Score <- rowMeans(adii[,-1])

# DESI
desi <- read.csv("datasets/DESI.csv")

unique(desi$indicator)

desi_pillars <- desi %>% filter(indicator == "desi") %>%
  select(Year = time_period, Key = ref_area, 
         indicator, breakdown, value)

desi_total <- desi_pillars %>% group_by(Year, Key) %>%
  summarise(Score = mean(value))

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


# correlations at pillars' level
cor(dii[,6:9])
cor(adii[,2:7])

desi_by_pillar <- desi_pillars %>% pivot_wider(names_from = "breakdown",
                                               values_from = "value")
cor(desi_by_pillar[,4:7])
cor_matrix <- cor(adii_dii[,6:9], adii_dii[,10:15])


# Example of orthogonalization to exclude common component from pillars
desi_by_pillar$Score <- rowMeans(desi_by_pillar[,4:7])
model_conn <- lm(desi_conn ~ Score, desi_by_pillar)
model_dps <- lm(desi_dps ~ Score, desi_by_pillar)

cor(desi_by_pillar$desi_conn, desi_by_pillar$desi_dps)
cor(model_conn$residuals, model_dps$residuals)
cor(desi_by_pillar$desi_dps, model_conn$residuals)

plot(desi_by_pillar$desi_conn, desi_by_pillar$desi_dps)
plot(model_conn$residuals, model_dps$residuals)

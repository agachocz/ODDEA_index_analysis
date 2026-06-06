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

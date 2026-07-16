# try principal component analysis to see if ADII needs reduction

# correlations
summary(clean_data)
indicators <- colnames(clean_data)

P1 <- indicators[str_detect(indicators, "I1")]
P2 <- indicators[str_detect(indicators, "I2")]
P3 <- indicators[str_detect(indicators, "I3")]
P4 <- indicators[str_detect(indicators, "I4")]
P5 <- indicators[str_detect(indicators, "I5")]
P6 <- indicators[str_detect(indicators, "I6")]

cor(na.omit(clean_data[,P6]))

pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("proportions of variance:")
  print(x.pvar)
  
  par(mfrow=c(2,2))
  plot(x.pvar,xlab="Principal component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
  plot(cumsum(x.pvar),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
  screeplot(x)
  screeplot(x,type="l")
  par(mfrow=c(1,1))
}
pcaCharts(pc)

# PCA
# I should probably use the data availability as a cue on which indicators should be included...

nas <- colSums(is.na(all_data))

pc <- prcomp(na.omit(clean_data[,P5]), center = TRUE, scale. = TRUE)
summary(pc)

cor(pc$x[,1:3], na.omit(clean_data[,P5]))

# P6: I63_gov_services, I65_innovation_framework, I61_mobile | instead of I64_gov_responses use I65
# P5: I55_intellectual_property, I54_ease_of_business, I52_rnd_expenditure | can change I54 for I53_innovative_companies
# P4: I44_digital_skills, I41_stem_graduates, I42_knowledge_emp
# P3: I32_digital_money, I34_id_card, I33_electronic_transactions
# P2: I22_legal, I21_data_protection, I25_Cooperation
# P1: I15_logistic_services, I13_trade_procedures, I12_digital_certificates

new_set <- c("I63_gov_services", "I65_innovation_framework", "I61_mobile",
             "I55_intellectual_property", "I53_innovative_companies", "I52_rnd_expenditure",
             "I44_digital_skills", "I41_stem_graduates", "I42_knowledge_emp",
             "I32_digital_money", "I34_id_card", "I33_electronic_transactions",
             "I22_legal", "I21_data_protection", "I25_Cooperation",
             "I15_logistic_services", "I13_trade_procedures", "I12_digital_certificates")

reduced_data <- all_data %>% select(entity, all_of(new_set))

small_na <- reduced_data$entity[rowSums(is.na(reduced_data)) < 4]
clean_data_reduced <- reduced_data %>% filter(entity %in% small_na)
  

adii_reduced <- clean_data_reduced %>% pivot_longer(-entity, values_to = "value", names_to = "indicator") %>%
  mutate(pillar = substr(indicator, 1, 2)) %>% group_by(entity, pillar) %>% 
  summarise(value = mean(value, na.rm = T)/20*100) %>% pivot_wider(id_cols = entity, names_from = pillar, values_from = value) %>%
  mutate(Index = (I1+I2+I3+I4+I5+I6)/6) %>% arrange(entity)

adii_reduced_check <- adii_reduced %>% filter(entity %in% adii$entity)
cor(adii_reduced_check[,2:8], adii[,2:8])
# the final index has 99% accuracy!

comparison <- adii_asean %>% left_join(adii_reduced, by = "entity") %>% 
  select(entity, P1_org, I1, P2_org, I2, P3_org, I3, P4_org, I4, P5_org, I5, P6_org, I6, Index_org, Index) %>% drop_na()

comparison %>% group_by(entity) %>% summarise(
  RMSE = sqrt(((P1_org-I1)^2 + (P2_org-I2)^2 + (P3_org-I3)^2 + (P4_org-I4)^2 + (P5_org-I5)^2 + (P6_org-I6)^2)/6),
  MAPE = ((abs(P1_org-I1)/P1_org + abs(P2_org-I2)/P2_org + abs(P3_org-I3)/P3_org + 
             abs(P4_org-I4)/P4_org + abs(P5_org-I5)/P5_org + abs(P6_org-I6)/P6_org)/6)
) %>% ungroup() %>% summarise(RMSE = mean(RMSE), MAPE = mean(MAPE))

cor(comparison$Index_org, comparison$Index)

# for table in the article

mean(abs(comparison$P1_org-comparison$I1)/comparison$P1_org)*100
mean((comparison$P1_org-comparison$I1))


adii_reduced <- adii_reduced %>% mutate(code = countrycode(entity, origin = "country.name", destination = "iso3c"))

map_adii <- world_moll %>% left_join(adii_reduced, by = c("iso_a3_eh"="code"), multiple = "all") %>%
  ggplot() + geom_sf(aes(fill = Index)) +
  scale_fill_viridis_c(option = "plasma",begin = 0) +
  theme(text = element_text(size = 20, family="serif")) +
  labs(fill = "ADII")

map_adii

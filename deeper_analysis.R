# FREE IDEAS - HOW TO COMPARE INDEXES AT PILLARS' LEVEL

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




# How the variables impact difference between scores

# DESI x DII
dii_m <- mean(desi_dii$Score.x)
dii_s <- sd(desi_dii$Score.x)
desi_m <- mean(desi_dii$Score.y)
desi_s <- sd(desi_dii$Score.y)

desi_dii_stand <- desi_dii %>% mutate(DII = (Score.x-dii_m)/dii_s,
                                      DESI = (Score.y-desi_m)/desi_s) %>%
  mutate(Diff = DII - DESI)

model <- lm(Diff ~ Supply + Demand + Institutions + Innovation, desi_dii_stand)
summary(model)

cor(desi_dii_stand[,c(6:9,14)])


# ADII x DII
dii_m <- mean(adii_dii$Score.x)
dii_s <- sd(adii_dii$Score.x)
adii_m <- mean(adii_dii$Score.y)
adii_s <- sd(adii_dii$Score.y)

adii_dii_stand <- adii_dii %>% mutate(DII = (Score.x-dii_m)/dii_s,
                                      ADII = (Score.y-adii_m)/adii_s) %>%
  mutate(Diff = DII - ADII)

model <- lm(Diff ~ Supply + Demand + Institutions + Innovation.x, adii_dii_stand)
summary(model)
model <- lm(Diff ~ Trade_Logistics + Cybersecurity + Digital_Payments + 
              + Human_Skills + Innovation.y + Institutions_Infrastructure, adii_dii_stand)
summary(model)

cor_mat <- cor(adii_dii_stand[,c(6:15, 19)])

# ortho
Res_Pillars <- data.frame(Entity = adii_dii_stand[,2])
Res_Pillars$DII_Supply <- lm(Supply ~ DII, adii_dii_stand)$residuals
Res_Pillars$DII_Demand <- lm(Demand ~ DII, adii_dii_stand)$residuals
Res_Pillars$DII_Institutions <- lm(Institutions ~ DII, adii_dii_stand)$residuals
Res_Pillars$DII_Innovation <- lm(Innovation.x ~ DII, adii_dii_stand)$residuals
Res_Pillars$ADII_Trade_Log <- lm(Trade_Logistics ~ ADII, adii_dii_stand)$residuals
Res_Pillars$ADII_Cybersecurity <- lm(Cybersecurity ~ ADII, adii_dii_stand)$residuals
Res_Pillars$ADII_Payments <- lm(Digital_Payments ~ ADII, adii_dii_stand)$residuals
Res_Pillars$ADII_Human_Skills <- lm(Human_Skills ~ ADII, adii_dii_stand)$residuals
Res_Pillars$ADII_Innovation <- lm(Innovation.y ~ ADII, adii_dii_stand)$residuals
Res_Pillars$ADII_Inst_Infr <- lm(Institutions_Infrastructure ~ ADII, adii_dii_stand)$residuals

cor_mat <- cor(Res_Pillars[,c(2:5)], Res_Pillars[,c(6:11)])
cor_mat <- cor(Res_Pillars[,c(2:11)])
corrplot(cor_mat)
test <- cor.mtest(Res_Pillars[,-1])$p

plot(Res_Pillars$DII_Innovation, Res_Pillars$ADII_Innovation)


# ortho with GDP
gdp <- read.csv("datasets/gdp.csv")
colnames(gdp)[4] <- "GDP_PC"

gdp <- gdp %>% filter(Year == 2019) %>% select(Entity, GDP_PC)

adii_dii_gdp <- adii_dii_stand %>% left_join(gdp, by = "Entity")
cor(adii_dii_gdp$GDP_PC, adii_dii_gdp$DII)

Res_Pillars <- data.frame(Entity = adii_dii_gdp[,2])
Res_Pillars$DII_Supply <- lm(Supply ~ GDP_PC, adii_dii_gdp)$residuals
Res_Pillars$DII_Demand <- lm(Demand ~ GDP_PC, adii_dii_gdp)$residuals
Res_Pillars$DII_Institutions <- lm(Institutions ~ GDP_PC, adii_dii_gdp)$residuals
Res_Pillars$DII_Innovation <- lm(Innovation.x ~ GDP_PC, adii_dii_gdp)$residuals
Res_Pillars$ADII_Trade_Log <- lm(Trade_Logistics ~ GDP_PC, adii_dii_gdp)$residuals
Res_Pillars$ADII_Cybersecurity <- lm(Cybersecurity ~ GDP_PC, adii_dii_gdp)$residuals
Res_Pillars$ADII_Payments <- lm(Digital_Payments ~ GDP_PC, adii_dii_gdp)$residuals
Res_Pillars$ADII_Human_Skills <- lm(Human_Skills ~ GDP_PC, adii_dii_gdp)$residuals
Res_Pillars$ADII_Innovation <- lm(Innovation.y ~ GDP_PC, adii_dii_gdp)$residuals
Res_Pillars$ADII_Inst_Infr <- lm(Institutions_Infrastructure ~ GDP_PC, adii_dii_gdp)$residuals

cor_mat <- cor(Res_Pillars[,c(2:5)], Res_Pillars[,c(6:11)])
cor(Res_Pillars[,c(2:5)])
corrplot(cor_mat)
test <- cor.mtest(Res_Pillars[,-1])$p

cor(adii_dii_stand[,c(6:18)])

# ADII vs DII ranking

adii_dii_ranking <- adii_dii %>% select(Entity, DII = Score.x, ADII = Score.y)
rank_adii <- adii_dii_ranking %>% arrange(desc(ADII)) %>% mutate(Rank_ADII = 1:n()) %>%
  select(Entity, Rank_ADII)
rank_dii <- adii_dii_ranking %>% arrange(desc(DII)) %>% mutate(Rank_DII = 1:n()) %>%
  select(Entity, Rank_DII)

adii_dii_ranking <- adii_dii_ranking %>% left_join(rank_adii, by = "Entity") %>%
  left_join(rank_dii, by = "Entity") %>% arrange(desc(DII))

write.csv(adii_dii_ranking, "ADII DII ranking.csv")

cor(adii_dii$Score.x, adii_dii$Score.y, method = "s")
cor(desi_dii$Score.x, desi_dii$Score.y, method = "s")

# DESI vs DII ranking

desi_dii_ranking <- desi_dii %>% select(Entity, DII = Score.x, DESI = Score.y)
rank_desi <- desi_dii_ranking %>% arrange(desc(DESI)) %>% mutate(Rank_DESI = 1:n()) %>%
  select(Entity, Rank_DESI)
rank_dii <- desi_dii_ranking %>% arrange(desc(DII)) %>% mutate(Rank_DII = 1:n()) %>%
  select(Entity, Rank_DII)

desi_dii_ranking <- desi_dii_ranking %>% left_join(rank_desi, by = "Entity") %>%
  left_join(rank_dii, by = "Entity") %>% arrange(desc(DII))

write.csv(desi_dii_ranking, "DESI DII ranking.csv")

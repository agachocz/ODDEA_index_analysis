
# read the borders data

# Install and load libraries
# install.packages("rvest")
# install.packages("tidyverse")

library(rvest)
library(tidyverse)
library(countrycode)

# 2. Define the Wikipedia target URL
url <- "https://en.wikipedia.org/wiki/List_of_countries_and_territories_by_land_and_maritime_borders"

# 3. Read HTML, locate the table, and convert to a data frame
wiki_table <- read_html(url) %>% 
  html_element(".wikitable") %>%   # Targets the standard Wikipedia table style
  html_table()                     # Automatically parses HTML rows into a dataframe

# 4. View the captured data frame
head(wiki_table)

colnames(wiki_table) <- c("entity", "land", "maritime", "total", "neighbours")
borders <- wiki_table %>% slice(-1) %>% select(entity, neighbours) %>%
  mutate(entity = str_remove_all(entity, "\\[.*?\\]")) %>%
  mutate(entity = str_remove_all(entity, "\\(.*?\\)")) %>%
  mutate(neighbours = str_remove_all(neighbours, "\\[.*?\\]")) %>%
  mutate(neighbours = str_replace_all(neighbours, "\\(.*?\\)", ",")) %>%
  mutate(entity = str_trim(entity, side = "right")) %>%
  mutate(entity = countryname(entity, "country.name")) %>%
  filter(entity %in% adii$entity) %>% group_by(entity) %>%
  summarise(neighbours = str_flatten(neighbours, collapse = ","))

unique(borders$entity)

# select only the ones for which I have ADII
# make a square matrix of that size
# for each entity make a row with 1 where the country has a border
# divide the ones by nr of borders, so that the row sums to 1 (maybe later transform with GDP?)

gdp <- read.csv("paper 3/data/gdp in dollars.csv") %>%  filter(TIME_PERIOD == 2023) %>%
  select(gdp = OBS_VALUE, entity = REF_AREA_LABEL)  %>%
  mutate(entity = countryname(entity, "country.name")) %>%
  filter(!is.na(entity)) %>% filter(entity %in% borders$entity)

border_matrix <- c()
border_matrix_big <- c()

for(i in 1:nrow(borders)){
  n <- str_split(borders[i,"neighbours"], ",")
  n <- lapply(n, str_trim)
  n <- lapply(n, countryname)[[1]]
  n <- unique(na.omit(n))
  b <- rep(0, nrow(borders))
  
  positions <- which(borders$entity %in% n)
  if(length(positions) > 0){
    g_all <- sum(gdp$gdp[positions])
    b[positions] <- gdp$gdp[positions]/g_all
  }
  
  border_matrix <- c(border_matrix, b)

}

# make a matrix
border_matrix <- matrix(border_matrix, nrow = nrow(borders), byrow = T)
rowSums(border_matrix)


# compute the dependencies

adii <- adii %>% filter(entity %in% borders$entity)
adii$entity == borders$entity

scores <- as.matrix(adii[,2:8])
score_matrix <- border_matrix %*% scores
score_df <- as.data.frame(score_matrix)
names(score_df) <- c("N1", "N2", "N3", "N4", "N5", "N6", "NIndex")

# estimate the models for each pillar
adii_model <- cbind(adii, score_df)

index_m <- lm(Index ~ NIndex, adii_model)
summary(index_m)

I1_m <- lm(I1 ~ N1, adii_model)
summary(I1_m)

I2_m <- lm(I2 ~ N2, adii_model)
summary(I2_m)

I3_m <- lm(I3 ~ N3, adii_model)
summary(I3_m)

I4_m <- lm(I4 ~ N4, adii_model)
summary(I4_m)

I5_m <- lm(I5 ~ N5, adii_model)
summary(I5_m)

I6_m <- lm(I6 ~ N6, adii_model)
summary(I6_m)

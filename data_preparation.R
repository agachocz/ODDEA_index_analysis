library(dplyr)

# ADII
adii <- read.csv("datasets/ADII.csv", sep = ";")
colnames(adii) <- c("Country", "Trade_Logistics", "Cybersecurity",
                    "Digital_Payments", "Human Skills", "Innovation",
                    "Institutions_Infrastructure")
adii$Score <- rowMeans(adii[,-1])

# DESI
desi <- read.csv("datasets/DESI.csv")

unique(desi$indicator)

desi_total <- desi %>% filter(indicator == "desi")


# DII
dii <- read.csv("datasets/DII.csv", sep = ";")

# WDCI
wdci <- read.csv("datasets/WDCI.csv", sep = ";", na.strings = "-")

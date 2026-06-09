# using World Bank Data API to collect indicators from the Global Competetiveness Index

library(tidyverse)
library(httr)
library(jsonlite)

q <- 'https://data360api.worldbank.org/data360/indicators?datasetId=WEF_GCI'
res = GET(q)
indicators <- fromJSON(content(res, encoding = "UTF-8", type = "text"))

schema <- 'https://data360api.worldbank.org/data360/data?DATABASE_ID=WEF_GCI&INDICATOR=PLACEHOLDER&TIME_PERION=2019&skip=0'

first = TRUE
for(i in indicators){
  
  q <- str_replace(schema, "PLACEHOLDER", i)
  res = GET(q)
  
  data <- fromJSON(content(res, encoding = "UTF-8", type = "text"))$value %>%
    filter(LATEST_DATA == "TRUE", UNIT_MEASURE == "SCORE") %>%
    select(REF_AREA, OBS_VALUE) 
  
  colnames(data) <- c("entity", i)
  
  if(first){
    GCI_data <- data
    first <- FALSE
  } else {
    GCI_data <- GCI_data %>% full_join(data, by = "entity")
  }
  
  print(i)
}


# stakeholder collaboration: WEF_GCI_MULTISTAKECOLLAB
# digital skills: WEF_GCI_EOSQ508
# graduates skills: WEF_GCI_GRADSKILLS
# ventur capital: WEF_GCIHH_EOSQ089
# government responses: WEF_GCI_EOSQ507
# innovating comapnies: WEF_GCI_EOSQ362
# innovation framework: WEF_GCI_EOSQ509k
# intellectual property: WEF_GCIHH_EOSQ052
# rnd expenditure: WEF_GCI_RDSPENDING

i <- "WEF_GCI_MULTISTAKECOLLAB"


ADII_GCI_indicators <- c("WEF_GCI_MULTISTAKECOLLAB", "WEF_GCI_EOSQ508", "WEF_GCI_GRADSKILLS", "WEF_GCIHH_EOSQ089",
                  "WEF_GCI_EOSQ507", "WEF_GCI_EOSQ362", "WEF_GCI_EOSQ509k", "WEF_GCIHH_EOSQ052", "WEF_GCI_RDSPENDING")


# indicators descriptions

q <- "https://data360api.worldbank.org/data360/metadata&$filter=series_description/idno eq 'WEF_GCI_SRVCTRADERESTRICT'&$select=series_description/database_id,series_description/idno"

q <- "https://data360api.worldbank.org/data360/metadata&$filter=series_description/database_id eq 'WEF_GCI'&$select=series_description/database_id,series_description/idno"

res = POST(q)

meta <- fromJSON(content(res, encoding = "UTF-8", type = "text"))

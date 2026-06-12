# using World Bank Data API to collect indicators from the Global Competetiveness Index

library(tidyverse)
library(httr)
library(jsonlite)

q <- 'https://data360api.worldbank.org/data360/indicators?datasetId=WEF_GCI'
res = GET(q)
indicators <- fromJSON(content(res, encoding = "UTF-8", type = "text"))

schema <- 'https://data360api.worldbank.org/data360/data?DATABASE_ID=WEF_GCI&INDICATOR=PLACEHOLDER&skip=0'

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

library(httr)
library(jsonlite)
library(dplyr)

url_ids <- "https://data360api.worldbank.org/data360/indicators?datasetId=WEF_GCI"
res_ids <- GET(url_ids)

indicator_ids <- fromJSON(content(res_ids, encoding = "UTF-8", type = "text"))

url_metadata <- "https://data360api.worldbank.org/data360/metadata"

library(httr)

headers = c(
  accept = "*/*",
  `Content-Type` = "application/json"
)

first <- TRUE
for(i in indicator_ids){
  data = "{\n  \"query\": \"&$filter=series_description/idno eq 'WEF_GCI_EOSQ470'\"\n}"
  
  data <- str_replace(data, "WEF_GCI_EOSQ470", i)
  
  res <- httr::POST(url = "https://data360api.worldbank.org/data360/metadata", httr::add_headers(.headers=headers), body = data)
  
  json_meta <- fromJSON(content(res, encoding = "UTF-8", type = "text"))
  metadata_df <- json_meta[["value"]][["series_description"]] %>% select(idno, name, measurement_unit)
  
  if(first){
    metadata <- metadata_df
    first = FALSE
  } else {
    metadata <- rbind(metadata, metadata_df)
  }
}


metadata

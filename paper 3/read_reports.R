install.packages("tabulapdf")
library(tabulapdf)
library(dplyr)

# Extract tables from pages 2 and 3 of your PDF

country_names <- read.csv("paper 3/data/Multi-stakeholder Collaboration.csv", sep = ";")
new_names <- c("Bulgaria", "Burkina Faso", "Korea, Rep.", "Mexico", "Taiwan, China") # not written in the file

countries <- c(country_names$entity, new_names)
problems <- vector()


for(i in 0:140){
  
tab <- extract_tables(
  file = "paper 3/data/reports/WEF_TheGlobalCompetitivenessReport2019.pdf", 
  pages = ((i*4)+62):((i*4)+65), 
  output = "tibble"
)

j <- 0

for(l in 1:length(tab)){
  if(length(names(tab[[l]])[1]) != 0){
    if(names(tab[[l]])[1] %in% countries) {
      j <- l
      break
    }
  }
  
}

if(j == 0){
  problems <- append(problems, i)
  next
}

entity = names(tab[[j]])[1]
col_names <- tab[[j]][1,]
empty_cols <- which(is.na(col_names))
if(length(empty_cols) > 0){
  page1 <- tab[[j]][-1,-empty_cols]  
} else {page1 <- tab[[j]][-1,]}

page1 <- page1[,1:2]
colnames(page1) <- c("Indicator", "Value")

col_names <- tab[[j+1]][1,]
empty_cols <- which(is.na(col_names))
if(length(empty_cols) > 0){
  page2 <- tab[[j+1]][-1,-empty_cols]  
} else {page2 <- tab[[j+1]][-1,]}

page2 <- page2[,1:2]
colnames(page2) <- c("Indicator", "Value")

col_names <- tab[[j+2]][1,]
empty_cols <- which(is.na(col_names))
if(length(empty_cols) > 0){
  page3 <- tab[[j+2]][-1,-empty_cols]  
} else {page3 <- tab[[j+2]][-1,]}

page3 <- page3[,1:2]
colnames(page3) <- c("Indicator", "Value")

table <- rbind(page1, page2, page3)
table$entity <- entity

if(i == 0){
  report <- table
} else {
  report <- rbind(report, table)
}

print(paste(i, entity))

}


global_competetiveness_index <- report
write.csv(global_competetiveness_index, "paper 3/data/global_competetiveness_index_data.csv")

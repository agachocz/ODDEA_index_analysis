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
u <- unique(global_competetiveness_index$Indicator)

u[str_detect(u, "wrong")]

# clean the indicators names
global_competetiveness_index <- global_competetiveness_index %>% mutate(
  Indicator = case_when(
  str_detect(Indicator, "1st pillar") ~ "P1_institutions",
  str_detect(Indicator, "2nd pillar") ~ "P2_infrastructure",
  str_detect(Indicator, "3rd pillar") ~ "P3_ICT_adoption",
  str_detect(Indicator, "4th pillar") ~ "P4_macro_stability",
  str_detect(Indicator, "5th pillar") ~ "P5_health",
  str_detect(Indicator, "6th pillar") ~ "P6_skills",
  str_detect(Indicator, "7th pillar") ~ "P7_product_market",
  str_detect(Indicator, "8th pillar") ~ "P8_labour_market",
  str_detect(Indicator, "9th pillar") ~ "P9_financial_system",
  str_detect(Indicator, "10th pillar") ~ "P10_market_size",
  str_detect(Indicator, "11th pillar") ~ "P11_dynamism",
  str_detect(Indicator, "12th pillar") ~ "P12_innovation",
  
  str_detect(Indicator, "Security") ~ "Sub1_Security",
  str_detect(Indicator, "Organized crime") ~ "I101_organized_crime",
  str_detect(Indicator, "Homicide rate") ~ "I102_homicide",
  str_detect(Indicator, "Terrorism incidence") ~ "I103_terrorism",
  str_detect(Indicator, "police services") ~ "I104_police",
  str_detect(Indicator, "Social capital 0–100") ~ "Sub2_Social_Capital",
  str_detect(Indicator, "1.05 Social capital") ~ "I105_social_capital",
  str_detect(Indicator, "Checks and balances") ~ "Sub3_Checks_Balances",
  str_detect(Indicator, "Budget transparency") ~ "I106_transparency",
  str_detect(Indicator, "Judicial independence") ~ "I107_judicial_independence",
  str_detect(Indicator, "framework in challenging regulations") ~ "I108_challenging_regulations",
  str_detect(Indicator, "Freedom of the press") ~ "I09_press_freedom",
  str_detect(Indicator, "Public-sector performance") ~ "Sub4_Public_Sector",
  str_detect(Indicator, "Burden of government") ~ "I110_gov_burden",
  str_detect(Indicator, "framework in settling disputes") ~ "I111_settling_disputes",
  str_detect(Indicator, "E-Participation") ~ "I112_e_participation",
  str_detect(Indicator, "Transparency 0–100") ~ "Sub5_Transparency",
  str_detect(Indicator, "Incidence of corruption") ~ "I113_corruption",
  str_detect(Indicator, "Property rights") ~ "I114_property_rights",
  str_detect(Indicator, "property protection") ~ "I115_intellectual_property_protection",
  str_detect(Indicator, "land administration") ~ "I116_land_administration",
  str_detect(Indicator, "Corporate governance") ~ "Sub6_Corporate_Governance",
  str_detect(Indicator, "auditing and accounting") ~ "I117_auditing_accounting_standards",
  str_detect(Indicator, "Conflict of interest") ~ "I118_interest_conflict_regulation",
  str_detect(Indicator, "Shareholder governance") ~ "I119_shareholder_governance",
  str_detect(Indicator, "orientation of government") ~ "Sub7_Government_Orientation",
  str_detect(Indicator, "Government ensuring policy") ~ "I120_policy_stability",
  str_detect(Indicator, "responsiveness to change") ~ "I121_response_to_change",
  str_detect(Indicator, "adaptability to digital business") ~ "I122_digital_business_laws",
  str_detect(Indicator, "long-term vision") ~ "I123_gov_long_term_vision",
  str_detect(Indicator, "Energy efficiency regulation") ~ "I124_energy_regulations",
  str_detect(Indicator, "Renewable energy") ~ "I125_renewable_energy",
  str_detect(Indicator, "Environment-related") ~ "I126_environment_treaties",
  
  str_detect(Indicator, "Transport infrastructure") ~ "Sub8_Transport_Infrastructure",
  str_detect(Indicator, "Road connectivity") ~ "I201_road_connectivity",
  str_detect(Indicator, "Quality of road") ~ "I202_quality_of_roads",
  str_detect(Indicator, "Railroad density") ~ "I203_railroad_density",
  str_detect(Indicator, "train services") ~ "I204_train_efficiency",
  str_detect(Indicator, "Airport connectivity") ~ "I205_airport_connectivity",
  str_detect(Indicator, "Efficiency of air transport") ~ "I206_air_transport_efficiency",
  str_detect(Indicator, "shipping connectivity") ~ "I207_shipping_connectivity",
  str_detect(Indicator, "seaport services") ~ "I208_seaport_efficiency",
  str_detect(Indicator, "Utility infrastructure") ~ "Sub9_Utility_Infrastructure",
  str_detect(Indicator, "Electricity access") ~ "I209_electricity_access",
  str_detect(Indicator, "Electricity supply") ~ "I210_electricity_supply",
  str_detect(Indicator, "unsafe drinking water") ~ "I211_unsafe_drinking_water",
  str_detect(Indicator, "Reliability of water") ~ "I212_water_supply",
  
  str_detect(Indicator, "telephone subscription") ~ "I301_cellular_subscription",
  str_detect(Indicator, "Mobile-broadband subscriptions") ~ "I302_mobile_broadband",
  str_detect(Indicator, "Fixed-broadband Internet") ~ "I303_fixed_broadband",
  str_detect(Indicator, "Fibre internet") ~ "I304_fibre_internet",
  str_detect(Indicator, "Internet users") ~ "I305_internet_users",
  
  str_detect(Indicator, "Inflation") ~ "I401_inflation",
  str_detect(Indicator, "Debt dynamics") ~ "I402_debt_dynamic",
  
  str_detect(Indicator, "life expectancy") ~ "I501_healthy_life_expectancy",
  str_detect(Indicator, "Current workforce") ~ "Sub10_Workforce",
  str_detect(Indicator, "years of schooling") ~ "I601_years_of_schooling",
  str_detect(Indicator, "Skills of current") ~ "Sub11_Workforce_Skills",
  str_detect(Indicator, "staff training") ~ "I602_staff_training",
  str_detect(Indicator, "vocational training") ~ "I603_vocational_training",
  str_detect(Indicator, "Skillset of graduates") ~ "I604_graduates_skillset",
  str_detect(Indicator, "Digital skills") ~ "I605_digital_skills",
  str_detect(Indicator, "skilled employees") ~ "I606_skilled_employees",
  str_detect(Indicator, "Future workforce") ~ "Sub12_Future_Workforce",
  str_detect(Indicator, "School life expectancy") ~ "I607_school_life_expectancy",
  str_detect(Indicator, "Skills of future workforce") ~ "Sub13_Future_Workforce_Skills",
  str_detect(Indicator, "Critical thinking") ~ "I608_critical_thinking",
  str_detect(Indicator, "Pupil-to-teacher") ~ "I609_pupil_teacher_ratio",
  
  str_detect(Indicator, "Domestic competition") ~ "Sub14_Domestic_Competition",
  str_detect(Indicator, "effect of taxes") ~ "I701_distortive_taxes",
  str_detect(Indicator, "market dominance") ~ "I702_market_dominance",
  str_detect(Indicator, "Competition in services") ~ "I703_competition_services",
  str_detect(Indicator, "Trade openness") ~ "Sub15_Trade_Openess",
  str_detect(Indicator, "non-tariff barriers") ~ "I704_non_tariff_barriers",
  str_detect(Indicator, "Trade tariffs") ~ "I705_trade_tariffs",
  str_detect(Indicator, "Complexity of tariffs") ~ "I706_tariffs_complexity",
  str_detect(Indicator, "Border clearance") ~ "I707_border_clearance",
  
  str_detect(Indicator, "Flexibility 0–100") ~ "Sub16_Flexibility",
  str_detect(Indicator, "Redundancy costs") ~ "I801_redundancy_costs",
  str_detect(Indicator, "Hiring and firing") ~ "I802_hiring_firing_practices",
  str_detect(Indicator, "labour-employer relations") ~ "I803_labour_employer_relations",
  str_detect(Indicator, "Flexibility of wage") ~ "I804_wage_flexibility",
  str_detect(Indicator, "labour market policies") ~ "I805_labour_market_policies",
  str_detect(Indicator, "Workers' rights") ~ "I806_workers_rights",
  str_detect(Indicator, "foreign labour") ~ "I807_foreign_labour_hiring",
  str_detect(Indicator, "labour mobility") ~ "I808_internal_labour_mobility",
  str_detect(Indicator, "Meritocracy and incentivization") ~ "Sub17_Meritocracy",
  str_detect(Indicator, "professional management") ~ "I809_reliance_on_management",
  str_detect(Indicator, "Pay and productivity") ~ "I810_pay_productivity",
  str_detect(Indicator, "salaried female workers") ~ "I811_gender_pay_gap",
  str_detect(Indicator, "Labour tax rate") ~ "I812_labour_tax",
  
  str_detect(Indicator, "Depth 0–100") ~ "Sub18_Depth",
  str_detect(Indicator, "credit to private sector") ~ "I901_credit_to_private_sector",
  str_detect(Indicator, "Financing of SMEs") ~ "I902_financing_SME",
  str_detect(Indicator, "Venture capital") ~ "I903_venture_capital",
  str_detect(Indicator, "Market capitalization") ~ "I904_market_capitalization",
  str_detect(Indicator, "Insurance premium") ~ "I905_insurance_premium",
  str_detect(Indicator, "Stability 0–100") ~ "Sub19_Stability",
  str_detect(Indicator, "Soundness of banks") ~ "I906_banks_soundness",
  str_detect(Indicator, "Non-performing loans") ~ "I907_non_performing_loans",
  str_detect(Indicator, "Credit gap") ~ "I908_credit_gap",
  str_detect(Indicator, "regulatory capital") ~ "I909_regulatory_capital",

  str_detect(Indicator, "Gross domestic product") ~ "I1001_GDP_PPP",
  str_detect(Indicator, "Imports of goods") ~ "I1002_imports",
  str_detect(Indicator, "Administrative requirements") ~ "Sub20_Administrative_Requirements",
  str_detect(Indicator, "Cost of starting") ~ "I1101_starting_business_cost",
  str_detect(Indicator, "Time to start") ~ "I1102_starting_business_time",
  str_detect(Indicator, "Insolvency recovery rate") ~ "I1103_insolvency_recovery_rate",
  str_detect(Indicator, "Insolvency regulatory framework") ~ "I1104_insolvency_regulations",
  str_detect(Indicator, "Entrepreneurial culture") ~ "Sub21_Entrepreneurial_Culture",
  str_detect(Indicator, "Attitudes towards entrepreneurial") ~ "I1105_risk_attitudes",
  str_detect(Indicator, "delegate authority") ~ "I1106_delegating_authority",
  str_detect(Indicator, "innovative companies") ~ "I1107_innovating_companies",
  str_detect(Indicator, "disruptive ideas") ~ "I1108_disruptive_ideas",
  
  str_detect(Indicator, "Interaction and diversity") ~ "Sub22_Diversity",
  str_detect(Indicator, "Diversity of workforce") ~ "I1201_workforce_diversity",
  str_detect(Indicator, "cluster development") ~ "I1202_cluster_development",
  str_detect(Indicator, "International co-inventions") ~ "I1203_international_inventions",
  str_detect(Indicator, "Multi-stakeholder collaboration") ~ "I1204_stakeholder_collaboration",
  str_detect(Indicator, "Research and development") ~ "Sub23_Research_Development",
  str_detect(Indicator, "Scientific publications") ~ "I1205_scientific_publications",
  str_detect(Indicator, "Patent applications") ~ "I1206_patents",
  str_detect(Indicator, "R&D expenditures") ~ "I1207_RnD_expenditures",
  str_detect(Indicator, "Research institutions") ~ "I1208_research_institutions",
  str_detect(Indicator, "Commercialization") ~ "Sub24_Commercialization",
  str_detect(Indicator, "Buyer sophistication") ~ "I1209_buyer_sophistication",
  str_detect(Indicator, "Trademark applications") ~ "I1210_trademarks",
  
  TRUE ~ paste("wrong: ", Indicator)
  ))

global_competetiveness_index$Value = as.numeric(str_remove_all(global_competetiveness_index$Value, ","))
summary(global_competetiveness_index)

write.csv(global_competetiveness_index, "paper 3/data/global_competitive_index_dataset.csv")

global_competetiveness_index %>% filter(entity == "Singapore")

# This R script loads in, processes, analyses and visualises data from the 
# Prescribing Information System (PIS) to report on prescribing patterns 
# for each GP practice in Lothian per practice population on 9 indicators.

# Load libraries
library(data.table)
library(readr)
library(plyr)
library(dplyr)
library(dbplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(plotly)
library(shiny)
library(magrittr)
library(stringr)
library(openxlsx)
library(scales)

source("N:/03-Reference/R Functions/loadPharmacyFunctions.R")

#==============================================================#
# Specify File Paths
#==============================================================#

filePath <- "N:\\Prescribing Indicators\\2020-21 Q4\\Data"
outputPath <- "N:/Prescribing Indicators/2020-21 Q4/Outputs"
locality_reports_name <- "/Prescribing Indicators - 2020-21 Q4 - "

#==============================================================#
# Load Data (PIS extracts)
#==============================================================#

#-------------------------------  PIS extracts-------------------------------#

antibiotics <- fread(paste0(filePath, "\\1-Antibiotics.csv"))
respiratory <- readRDS("N:/Projects/Primary Care Pharmacy/02-Reports/Respiratory/Data/rawData/activityData.rda")
anticholinergics2020 <- fread(paste0(filePath, "\\9-Anticholinergics.csv"))
anticholinergics2019 <- fread(paste0(filePath, "\\9-Anticholinergics2019.csv"))
antidiabetics2020 <- fread(paste0(filePath, "\\10-Antidiabetics.csv"))
antidiabetics2019 <- fread(paste0(filePath, "\\10-Antidiabetics2019.csv"))
gaba <- fread(paste0(filePath, "\\6-Gaba.csv"))
opioids <- fread(paste0(filePath, "\\7-Opioids.csv"))
serial <- read_excel("N:\\NSS-MCR activity2020_Q4.xlsx")

#------------------------------- Lookup Information to be joined to raw data -------------------------------#

# Load list of ICS inhalers used to calculate Indicator 4
ics_inhalers <- fread("N:\\03-Reference\\Lookup Information\\ICS_inhalers_list.csv")

#Load a list of active GPs during the reporting period and map their hscp / localities / clusters / practice population
practiceLookup <- read_excel("N:\\03-Reference\\Lookup Information\\practice_information.xlsx")

# Add practice population for 75+ age band
practice_population_over75 <- read_excel("N:\\03-Reference\\Lookup Information\\Practice_pop_age_banded_2021-02-09.xlsx")

# Load DDD conversion values
DDD_full <- read.csv(file = "N:\\03-Reference\\Lookup Information\\DDD List.csv", header = TRUE, na.strings = "", stringsAsFactors = T) %>%
  # Rename the columns
  rename_with(snakecase::to_snake_case)

#=============================================
# Process DDDs dataset
#=============================================

# Select item code, description and conversion value from DDD
DDD <- DDD_full %>%
  select(pi_bnf_item_code, pi_bnf_item_description, con_val) %>%
  # Remove duplicates
  distinct() %>%
  rename(DDDconversion_value = con_val)

#=============================================
# Process the Antibiotics Dataset (for Indicators 1-3)
#=============================================

antibiotics %<>% 
  #Rename columns
  rename_with(snakecase::to_snake_case) %>%
  # Change some strings to numeric and dates (be careful of century when converting 2-digit years to 4-digit years)
  mutate(pat_date_of_birth_c = mdy(pat_date_of_birth_c),
                        pat_date_of_birth_c = ifelse(year(pat_date_of_birth_c) > 2021,
                                                     paste(subtract((year(pat_date_of_birth_c)), 100),
                                                           format(pat_date_of_birth_c, "%m-%d"),
                                                           sep = "-"),
                                                     as.character(pat_date_of_birth_c)),
                        pat_date_of_birth_c = as.POSIXct(pat_date_of_birth_c),
                        paid_date = as.POSIXct(paid_date, format = "%m/%d/%y"),
                        paid_quantity = as.numeric(gsub(",", "", paid_quantity)),
                        pd_paid_gic_excl_bb = as.numeric(gsub(",", "", pd_paid_gic_excl_bb)),
                        paid_month_year = zoo::as.yearmon(paid_date, "%b %Y"),
                        paid_financial_quarter = case_when(
                          paid_date >= "2019-01-01" & paid_date <= "2019-03-31" ~ "Q4 18/19",
                          paid_date >= "2019-04-01" & paid_date <= "2019-06-30" ~ "Q1 19/20",
                          paid_date >= "2019-07-01" & paid_date <= "2019-09-30" ~ "Q2 19/20",
                          paid_date >= "2019-10-01" & paid_date <= "2019-12-31" ~ "Q3 19/20",
                          paid_date >= "2020-01-01" & paid_date <= "2020-03-31" ~ "Q4 19/20",
                          paid_date >= "2020-04-01" & paid_date <= "2020-06-30" ~ "Q1 20/21",
                          paid_date >= "2020-07-01" & paid_date <= "2020-09-30" ~ "Q2 20/21",
                          paid_date >= "2020-10-01" & paid_date <= "2020-12-31" ~ "Q3 20/21",
                          paid_date >= "2021-01-01" & paid_date <= "2021-03-31" ~ "Q4 20/21")
                        ) %>%
  #Use the gpLookup file to match on locality/cluster
  left_join(practiceLookup, by = c("presc_location_code" = "PracticeCode")) %>%
  # Remove NAs from locality and practice_population
  filter(!is.na(locality), !is.na(practice_population), locality != "No HSCP Mapping Found")

# Create object for Indicator 2
co_amoxiclav <- antibiotics %>% filter(pi_approved_name == "CO-AMOXICLAV")

# Create object for Indicator 3 (antibiotics filtered by BNF sub-section 050112)
quinolone <- antibiotics %>% filter(pi_bnf_sub_section_code == 050112)

#=============================================
# Process the Respiratory Dataset (for Indicators 4-5)
#=============================================

respiratory %<>% rename_with(snakecase::to_snake_case) %>%
  left_join(practiceLookup, by = c("presc_location_code" = "PracticeCode"))

ics_inhalers %<>% rename_with(snakecase::to_snake_case)

# Select the names of the ICS inhalers used in calculating Indicator 4
pi4_approved_name <- ics_inhalers %>% select(approved_name) %>% distinct()

# Create object for Indicator 4 (filter respiratory data to use data just for the list of -ICS inhalers)
respiratory_ind4 <- respiratory %>% filter(date >= "2019-01-01", locality != "No HSCP Mapping Found", !is.na(locality),
                                           pi_approved_name %in% pull(pi4_approved_name))
# Create object for Indicator 5
respiratory_ind5 <- respiratory %>% filter(date >= "2018-10-01", locality != "No HSCP Mapping Found", !is.na(locality), 
                                           str_detect(inhaler_class, "SABA"))

#=============================================
# Process the Anticholinergics Dataset (for Indicator 9)
#=============================================

# Merge data
anticholinergics <- rbind(anticholinergics2020, anticholinergics2019)

anticholinergics %<>% 
  rename_with(snakecase::to_snake_case) %>%
  left_join(practiceLookup, by = c("presc_location_code" = "PracticeCode")) %>%
  mutate(pat_date_of_birth_c = mdy(pat_date_of_birth_c)) %>%
  mutate(pat_date_of_birth_c = ifelse(year(pat_date_of_birth_c) > 2021,
                                      paste(subtract((year(pat_date_of_birth_c)), 100),
                                            format(pat_date_of_birth_c, "%m-%d"),
                                            sep = "-"),
                                      as.character(pat_date_of_birth_c))) %>%
  mutate(pat_date_of_birth_c = as.POSIXct(pat_date_of_birth_c),
         paid_date = as.POSIXct(paid_date, format = "%m/%d/%y"),
         paid_quantity = as.numeric(gsub(",", "", paid_quantity)),
         pd_paid_gic_excl_bb = as.numeric(gsub(",", "", pd_paid_gic_excl_bb)),
         paid_month_year = zoo::as.yearmon(paid_date, "%b %Y"),
         paid_financial_quarter = case_when(
           paid_date >= "2019-01-01" & paid_date <= "2019-03-31" ~ "Q4 18/19",
           paid_date >= "2019-04-01" & paid_date <= "2019-06-30" ~ "Q1 19/20",
           paid_date >= "2019-07-01" & paid_date <= "2019-09-30" ~ "Q2 19/20",
           paid_date >= "2019-10-01" & paid_date <= "2019-12-31" ~ "Q3 19/20",
           paid_date >= "2020-01-01" & paid_date <= "2020-03-31" ~ "Q4 19/20",
           paid_date >= "2020-04-01" & paid_date <= "2020-06-30" ~ "Q1 20/21",
           paid_date >= "2020-07-01" & paid_date <= "2020-09-30" ~ "Q2 20/21",
           paid_date >= "2020-10-01" & paid_date <= "2020-12-31" ~ "Q3 20/21",
           paid_date >= "2021-01-01" & paid_date <= "2021-03-31" ~ "Q4 20/21")
  ) %>% 
  filter(locality != "No HSCP Mapping Found", !is.na(locality))

# Calculate patient ages at time of prescription and currently
mostRecentMonth <- floor_date(Sys.Date(), unit = "months") %m-% months(3) #pick out most recent data in the activity file to calculate patients current ages
anticholinergics %<>% 
  mutate(pat_date_of_birth_c = as.POSIXct(pat_date_of_birth_c),
         paid_date = as.POSIXct(paid_date),
         prescribed_age = as.period(interval(start = pat_date_of_birth_c, end = paid_date))$year,
         current_age = as.period(interval(start = pat_date_of_birth_c, end = ymd(mostRecentMonth)))$year,
         # Create age band variable to subset patients age 75+
         age_group = case_when(
           current_age < 18 ~ "<18",
           current_age >= 18 & current_age < 40 ~ "18-39",
           current_age >= 40 & current_age < 60 ~ "40-59",
           current_age >= 60 & current_age < 75 ~ "60-74",
           current_age >= 75 ~ "75+"
                            ))

# Calculate practice population for 75+ age bands
practice_population_over75 %<>% 
  rename_with(snakecase::to_snake_case) %>%
  filter(age_band %in% c("75-84", "85+")) %>%
  group_by(location_code) %>%
  summarise(over75 = sum(practice_population))

#=============================================
# Process the Antidiabetics Dataset (for Indicator 10)
#=============================================

# Merge data
antidiabetics <- rbind(antidiabetics2020, antidiabetics2019)

antidiabetics %<>% rename_with(snakecase::to_snake_case) %>%
  left_join(practiceLookup, by = c("presc_location_code" = "PracticeCode")) %>%
  mutate(pat_date_of_birth_c = mdy(pat_date_of_birth_c)) %>%
  mutate(pat_date_of_birth_c = ifelse(year(pat_date_of_birth_c) > 2021,
                                      paste(subtract((year(pat_date_of_birth_c)), 100),
                                            format(pat_date_of_birth_c, "%m-%d"),
                                            sep = "-"),
                                      as.character(pat_date_of_birth_c))) %>%
  mutate(pat_date_of_birth_c = as.POSIXct(pat_date_of_birth_c),
         paid_date = as.POSIXct(paid_date, format = "%m/%d/%y"),
         paid_quantity = as.numeric(gsub(",", "", paid_quantity)),
         pd_paid_gic_excl_bb = as.numeric(gsub(",", "", pd_paid_gic_excl_bb)),
         paid_month_year = zoo::as.yearmon(paid_date, "%b %Y"),
         paid_financial_quarter = case_when(
           paid_date >= "2019-01-01" & paid_date <= "2019-03-31" ~ "Q4 18/19",
           paid_date >= "2019-04-01" & paid_date <= "2019-06-30" ~ "Q1 19/20",
           paid_date >= "2019-07-01" & paid_date <= "2019-09-30" ~ "Q2 19/20",
           paid_date >= "2019-10-01" & paid_date <= "2019-12-31" ~ "Q3 19/20",
           paid_date >= "2020-01-01" & paid_date <= "2020-03-31" ~ "Q4 19/20",
           paid_date >= "2020-04-01" & paid_date <= "2020-06-30" ~ "Q1 20/21",
           paid_date >= "2020-07-01" & paid_date <= "2020-09-30" ~ "Q2 20/21",
           paid_date >= "2020-10-01" & paid_date <= "2020-12-31" ~ "Q3 20/21",
           paid_date >= "2021-01-01" & paid_date <= "2021-03-31" ~ "Q4 20/21")
  ) %>% 
  # Remove dummy CHIs
  filter(pat_chi_number_c !=  "7777777777") %>% filter(pat_chi_number_c != "8888888888") %>%
  filter(locality != "No HSCP Mapping Found", !is.na(locality))

#=============================================
# Process the Gabapentanoids Dataset (for Indicator 6)
#=============================================

gaba %<>% 
  rename_with(snakecase::to_snake_case) %>%
  # Add localities
  left_join(practiceLookup,
            by = c("presc_location_code" = "PracticeCode")) %>%
  mutate(pat_date_of_birth_c = mdy(pat_date_of_birth_c)) %>%
  mutate(pat_date_of_birth_c = ifelse(year(pat_date_of_birth_c) > 2021,
                                      paste(subtract((year(pat_date_of_birth_c)), 100),
                                            format(pat_date_of_birth_c, "%m-%d"),
                                            sep = "-"),
                                      as.character(pat_date_of_birth_c))) %>%
  mutate(pat_date_of_birth_c = as.POSIXct(pat_date_of_birth_c),
         paid_date = as.POSIXct(paid_date, format = "%m/%d/%y"),
         paid_quantity = as.numeric(gsub(",", "", paid_quantity)),
         pd_paid_gic_excl_bb = as.numeric(gsub(",", "", pd_paid_gic_excl_bb)),
         paid_month_year = zoo::as.yearmon(paid_date, "%b %Y"),
         paid_financial_quarter = case_when(
           paid_date >= "2019-01-01" & paid_date <= "2019-03-31" ~ "Q4 18/19",
           paid_date >= "2019-04-01" & paid_date <= "2019-06-30" ~ "Q1 19/20",
           paid_date >= "2019-07-01" & paid_date <= "2019-09-30" ~ "Q2 19/20",
           paid_date >= "2019-10-01" & paid_date <= "2019-12-31" ~ "Q3 19/20",
           paid_date >= "2020-01-01" & paid_date <= "2020-03-31" ~ "Q4 19/20",
           paid_date >= "2020-04-01" & paid_date <= "2020-06-30" ~ "Q1 20/21",
           paid_date >= "2020-07-01" & paid_date <= "2020-09-30" ~ "Q2 20/21",
           paid_date >= "2020-10-01" & paid_date <= "2020-12-31" ~ "Q3 20/21",
           paid_date >= "2021-01-01" & paid_date <= "2021-03-31" ~ "Q4 20/21")
  ) %>%
  # Join with DDD file by item code to calculate the DDDs for paid quantities
  filter(locality != "No HSCP Mapping Found", !is.na(locality)) %>%
  left_join(DDD[c(1,3)], by = "pi_bnf_item_code") %>%
  # Remove missing DDD conversion values
  filter(!is.na(DDDconversion_value)) %>%
  # Calculate DDDs
  mutate(DDD = paid_quantity / DDDconversion_value)

#=============================================
# Process the Opioids Dataset (for Indicator 7)
#=============================================

opioids %<>% 
  rename_with(snakecase::to_snake_case) %>% 
  # Add localities
  left_join(practiceLookup,
            by = c("presc_location_code" = "PracticeCode")) %>%
  mutate(pat_date_of_birth_c = mdy(pat_date_of_birth_c)) %>%
  mutate(pat_date_of_birth_c = ifelse(year(pat_date_of_birth_c) > 2021,
                                      paste(subtract((year(pat_date_of_birth_c)), 100),
                                            format(pat_date_of_birth_c, "%m-%d"),
                                            sep = "-"),
                                      as.character(pat_date_of_birth_c))) %>%
  mutate(pat_date_of_birth_c = as.POSIXct(pat_date_of_birth_c),
         paid_date = as.POSIXct(paid_date, format = "%m/%d/%y"),
         paid_quantity = as.numeric(gsub(",", "", paid_quantity)),
         pd_paid_gic_excl_bb = as.numeric(gsub(",", "", pd_paid_gic_excl_bb)),
         paid_month_year = zoo::as.yearmon(paid_date, "%b %Y"),
         paid_financial_quarter = case_when(
           paid_date >= "2019-01-01" & paid_date <= "2019-03-31" ~ "Q4 18/19",
           paid_date >= "2019-04-01" & paid_date <= "2019-06-30" ~ "Q1 19/20",
           paid_date >= "2019-07-01" & paid_date <= "2019-09-30" ~ "Q2 19/20",
           paid_date >= "2019-10-01" & paid_date <= "2019-12-31" ~ "Q3 19/20",
           paid_date >= "2020-01-01" & paid_date <= "2020-03-31" ~ "Q4 19/20",
           paid_date >= "2020-04-01" & paid_date <= "2020-06-30" ~ "Q1 20/21",
           paid_date >= "2020-07-01" & paid_date <= "2020-09-30" ~ "Q2 20/21",
           paid_date >= "2020-10-01" & paid_date <= "2020-12-31" ~ "Q3 20/21",
           paid_date >= "2021-01-01" & paid_date <= "2021-03-31" ~ "Q4 20/21")
  ) %>% 
  # Join with DDD file by item code to calculate the DDDs for paid quantities
  filter(locality != "No HSCP Mapping Found", !is.na(locality)) %>%
  left_join(DDD[c(1,3)], by = "pi_bnf_item_code") %>%
  # Remove missing DDD conversion values
  filter(!is.na(DDDconversion_value)) %>%
  # Calculate DDDs
  mutate(DDD = paid_quantity / DDDconversion_value)


#==============================================================#
# Create Objects for Targets
#==============================================================#

# Specify the target for each indicator
target_ind1 <- 1.64 
target_ind2 <- 6.00
target_ind3 <- 4.50 
target_ind4 <- 16.16
target_ind5 <- 8.62 
target_ind9 <- 6.00
target_ind10 <- 11.14
target_ind6 <- 15.17
target_ind7 <- 15

# Specify the target shift from baseline (a 10% or 5% reduction)
target_shift <- -10
target_shift5 <- -5

#==============================================================#
# Create Data Objects
#==============================================================#


#-------------------- Specify the baseline dates -----------------------#

# Specify the baseline quarter
baseline_quarter <- "Q3 19/20"

# Specify the start and end dates for 12 month baselines
baseline_year_start <- "2019-01-01"
baseline_year_end <- "2019-12-31"

# Respiratory dates have a different format
baseline_quarter_ind4 <- "2019/20 Q3"

#---------------------- Automatically identify latest dates --------------#

# Remaining date objects  
current_month <- max(antibiotics$paid_date)
current_quarter <- antibiotics %>% filter(paid_date %in% current_month) %>% distinct(paid_financial_quarter)
full_back12Mth <- format(as.Date(current_month) %m-% months(11), "%Y-%m-%d")

# Respiratory dates have a different format
current_month_respiratory <- max(respiratory$date)
current_quarter_respiratory <- max(respiratory$fin_quarter)
fullback12Mth_respiratory <- format(as.Date(current_month_respiratory) %m-% months(11), "%Y-%m-%d")


#=============================================
# Indicator 1 - Antibiotic Items per 1000 list size per day <= 1.64
#=============================================

# Summarise by financial quarter
indicator1_byquarter <- antibiotics %>% group_by(locality, hscp, presc_location_code, practice_population, paid_financial_quarter) %>%
  summarise(antibiotics_items = sum(number_of_paid_items)) %>%
  mutate(items_per_1000_listsize = round((antibiotics_items*1000)/practice_population,2),
         items_per_1000_listsize_perday = round(items_per_1000_listsize/(365/4),2),
         target = target_ind1,
         target_met = if_else(items_per_1000_listsize_perday <= target, "Target Met", "Target Not Met"))

# Extract baseline figures
ind1_baseline <- indicator1_byquarter %>% filter(paid_financial_quarter == baseline_quarter) %>% 
  select(presc_location_code, items_per_1000_listsize_perday) %>%
  mutate(baseline = round(items_per_1000_listsize_perday,2)) %>%
  ungroup() %>%
  select(presc_location_code, baseline)

# Make table of Indicator 1
indicator1_table <- indicator1_byquarter %>% filter(paid_financial_quarter %in% current_quarter) %>% 
  ungroup() %>%
  select(-paid_financial_quarter) %>%
  left_join(ind1_baseline, by = "presc_location_code") %>%
  mutate(baseline_change = items_per_1000_listsize_perday - baseline,
         percentage_change = round(baseline_change / baseline * 100,2),
         reduction_from_baseline_target = if_else(percentage_change <= target_shift, "Shift Met", "")) %>%
  ungroup() %>%
  select(hscp, presc_location_code, items_per_1000_listsize_perday, antibiotics_items, practice_population, target_met) %>%
  arrange(desc(items_per_1000_listsize_perday))

#=============================================
# Indicator 2 - Number of Co-Amoxiclav Items per 1000 List Size per 100 Days <= 6.00
#=============================================

# Summarise by financial quarter
indicator2_byquarter <- co_amoxiclav %>% group_by(paid_financial_quarter, locality, hscp, presc_location_code, practice_population) %>%
  summarise(antibiotics_items = sum(number_of_paid_items)) %>%
  mutate(items_per_1000_listsize = round((antibiotics_items*1000)/practice_population,2),
         items_per_1000_listsize_per100days = round((items_per_1000_listsize/(365/4)*100),2),
         target = target_ind2,
         target_met = if_else(items_per_1000_listsize_per100days <= target, "Target Met", "Target Not Met"))

# Extract baseline figures
ind2_baseline <- indicator2_byquarter %>% filter(paid_financial_quarter == baseline_quarter) %>% 
  select(presc_location_code, items_per_1000_listsize_per100days) %>%
  mutate(baseline = round(items_per_1000_listsize_per100days,2)) %>%
  ungroup() %>%
  select(presc_location_code, baseline)

# Make table of Indicator 2
indicator2_table <- indicator2_byquarter %>% filter(paid_financial_quarter %in% current_quarter) %>% 
  ungroup() %>%
  select(-paid_financial_quarter) %>%
  left_join(ind2_baseline, by = "presc_location_code") %>%
  mutate(baseline_change = items_per_1000_listsize_per100days - baseline,
         percentage_change = round(baseline_change / baseline * 100,2),
         reduction_from_baseline_target = if_else(percentage_change <= target_shift, "Shift Met", "")) %>%
  ungroup() %>%
  select(hscp, presc_location_code, items_per_1000_listsize_per100days, antibiotics_items, practice_population, target_met) %>%
  arrange(desc(items_per_1000_listsize_per100days))

#=============================================
# Indicator 3 - Number of Quinolone Items per 1000 List Size per 100 Days <= 4.50
#=============================================

# Extract baseline figures
ind3_baseline <- quinolone %>% filter(paid_financial_quarter == baseline_quarter) %>% 
  group_by(hscp, presc_location_code, practice_population, paid_financial_quarter) %>%
  summarise(antibiotics_items = sum(number_of_paid_items)) %>%
  mutate(items_per_1000_listsize = round((antibiotics_items*1000)/practice_population,2),
         # items per 1000 listsize per 100 days
         baseline = round((items_per_1000_listsize/(365/4)*100),2)) %>%
  ungroup() %>%
  select(hscp, presc_location_code, practice_population, baseline)

ind3_current_quarter <- quinolone %>% filter(paid_financial_quarter %in% current_quarter) %>% 
  group_by(hscp, presc_location_code, practice_population, paid_financial_quarter) %>%
  summarise(antibiotics_items = sum(number_of_paid_items)) %>%
  mutate(items_per_1000_listsize = round((antibiotics_items*1000)/practice_population,2),
         items_per_1000_listsize_per100days = round((items_per_1000_listsize/(365/4)*100),2)) %>%
  select(presc_location_code, items_per_1000_listsize_per100days, antibiotics_items)
  

indicator3_table <- ind3_baseline %>%
  left_join(ind3_current_quarter) %>%
  mutate(items_per_1000_listsize_per100days = tidyr::replace_na(items_per_1000_listsize_per100days, 0),
         antibiotics_items = tidyr::replace_na(antibiotics_items, 0),
         target = target_ind3,
         target_met = if_else(items_per_1000_listsize_per100days <= target, "Target Met", "Target Not Met")) %>%
  select(hscp, presc_location_code, items_per_1000_listsize_per100days, antibiotics_items, practice_population, target_met) %>%
  arrange(desc(items_per_1000_listsize_per100days))

#=============================================
# Indicator 4 - High Strength ICS: 
# % of Inhaled Corticosteriod Inhalers Classed as High Strength <= 16.16% 
#or 10% shift from baseline
#=============================================

# Summarise by financial quarter
indicator4_byquarter <- respiratory_ind4 %>%
group_by(fin_quarter, locality, hscp, presc_location_code, inhaler_class, hscs_class) %>%
  summarise(quantity = sum(paid_quantity)) %>%
  mutate(highstrength_flag = ifelse(str_detect(hscs_class, "High"), "High", "Other")) %>%
  ungroup() %>%
  group_by(fin_quarter, locality, hscp, presc_location_code) %>%
  summarise(highstrengthquantity = sum(quantity[highstrength_flag == "High"]),
            otherstrengthquantity = sum(quantity[highstrength_flag == "Other"]),
            total_quantity = sum(quantity),
            percent_highstrength = highstrengthquantity / total_quantity * 100) %>%
  mutate(percent_highstrength = round(percent_highstrength,2),
         target = target_ind4,
         target_met = if_else(percent_highstrength <= target, "Target Met", "Target Not Met"))

# Extract baseline figures
ind4_baseline <- indicator4_byquarter %>% filter(fin_quarter == baseline_quarter_ind4) %>% 
  select(presc_location_code, percent_highstrength) %>%
  mutate(baseline = round(percent_highstrength,2)) %>%
  ungroup() %>%
  select(presc_location_code, baseline)

# Make table of Indicator 4
indicator4_table <- indicator4_byquarter %>% filter(fin_quarter == current_quarter_respiratory) %>% 
  ungroup() %>%
  select(-fin_quarter) %>%
  left_join(ind4_baseline, by = "presc_location_code") %>%
  mutate(baseline_change = percent_highstrength - baseline,
         percentage_change = round(baseline_change / baseline * 100,2),
         reduction_from_baseline_target = if_else(percentage_change <= target_shift, "Shift Met", "")) %>%
  ungroup() %>%
  select(hscp, presc_location_code, percent_highstrength, highstrengthquantity, otherstrengthquantity, target_met, baseline, baseline_change, reduction_from_baseline_target, total_quantity) %>%
  arrange(desc(percent_highstrength))

#=============================================
# Indicator 5 - SABA: Number of Patients Prescribed More than 12 SABA Inhalers a Year
# As a Percentage of All Patients Prescribed a SABA <= 8.62% or 10% Shift from Baseline
#=============================================

# Calculate latest quarter
 indicator5 <- respiratory_ind5 %>% filter(date >= fullback12Mth_respiratory) %>%
   group_by(locality, hscp, presc_location_code, pat_upi_c) %>%
   summarise(quantity = sum(adjusted_quantity)) %>%
   mutate(overuse_flag = ifelse(quantity > 12, 1, 0)) %>%
   ungroup() %>%
   group_by(locality, hscp, presc_location_code) %>%
   summarise(patients = n_distinct(pat_upi_c),
             overuseSABA = sum(overuse_flag)) %>%
   mutate(percent = round(overuseSABA / patients * 100,2),
          target = target_ind5,
          target_met = if_else(percent <= target, "Target Met", "Target Not Met"))

# Calculate baseline
indicator5_baseline <- respiratory_ind5 %>% filter(date >= baseline_year_start & date <= baseline_year_end) %>%
  group_by(locality, hscp, presc_location_code, pat_upi_c) %>%
  summarise(quantity = sum(adjusted_quantity)) %>%
  mutate(overuse_flag = ifelse(quantity > 12, 1, 0)) %>%
  ungroup() %>%
  group_by(locality, hscp, presc_location_code) %>%
  summarise(patients = n_distinct(pat_upi_c),
            overuseSABA = sum(overuse_flag)) %>%
  mutate(baseline = round(overuseSABA / patients * 100,2)) %>%
  ungroup() %>%
  select(presc_location_code, baseline)

# Make table of Indicator 5
indicator5_table <- indicator5 %>%
  left_join(indicator5_baseline, by = "presc_location_code") %>%
  mutate(baseline_change = round(percent - baseline,2),
         percentage_change = round(baseline_change / baseline * 100,2),
         reduction_from_baseline_target = if_else(percentage_change <= target_shift, "Shift Met", "")) %>%
  ungroup() %>%
  select(hscp, presc_location_code, percent, overuseSABA, patients, target_met, baseline, baseline_change, reduction_from_baseline_target) %>%
  arrange(desc(percent))

#=============================================
# Indicator 9 - Anticholinergics: Number of Patients 75 Years or Older Dispensed Over 10 Items of
# Strong or Very Strong Anticholinergics (mARS 3 and 12) in 12 months as a % of registered patients
# aged 75 years or older <= 6.00%
#=============================================

# Calculate baseline
indicator9_baseline <- anticholinergics %>% filter(paid_date >= baseline_year_start & paid_date <= baseline_year_end, age_group == "75+") %>%
  # Add practice population over 75
  left_join(practice_population_over75, by = c("presc_location_code" = "location_code")) %>%
  group_by(hscp, presc_location_code, over75, practice_population, pat_chi_number_c) %>%
  summarise(patients = n_distinct(pat_chi_number_c),
            items = sum(number_of_paid_items)) %>%
  ungroup() %>%
  filter(items >=10) %>%
  group_by(hscp, presc_location_code, over75, practice_population, items) %>%
  summarise(num_patients = sum(patients)) %>%
  ungroup() %>%
  group_by(hscp, presc_location_code, over75, practice_population) %>%
  summarise(patients_over10items = sum(num_patients)) %>%
  ungroup() %>%
  mutate(baseline = round(patients_over10items / over75 * 100,2)) %>%
  select(hscp, presc_location_code, presc_location_code, over75, practice_population, baseline)

# Calculate current quarter
indicator9_current_quarter <- anticholinergics %>% filter(paid_date >= full_back12Mth, age_group == "75+") %>%
  group_by(presc_location_code, pat_chi_number_c) %>%
  summarise(patients = n_distinct(pat_chi_number_c),
            items = sum(number_of_paid_items)) %>%
  ungroup() %>%
  filter(items >=10) %>%
  group_by(presc_location_code, items) %>%
  summarise(num_patients = sum(patients)) %>%
  ungroup() %>%
  group_by(presc_location_code) %>%
  summarise(patients_over10items = sum(num_patients)) %>%
  ungroup()

# Indicator 9 table
indicator9_table <- indicator9_baseline %>%
  left_join(indicator9_current_quarter) %>%
  mutate(patients_over10items = tidyr::replace_na(patients_over10items,0),
         ind9 = round(patients_over10items / over75 * 100,2),
         target = target_ind9,
         target_met = if_else(ind9 <= target, "Target Met", "Target Not Met"),
         baseline_change = round(ind9 - baseline,2),
         percentage_change = round(baseline_change / baseline * 100,2),
         reduction_from_baseline_target = if_else(percentage_change <= target_shift, "Shift Met", "")) %>%
  ungroup() %>%
  select(hscp, presc_location_code, practice_population, over75, patients_over10items, ind9, baseline, patients_over10items,target_met, reduction_from_baseline_target,) %>%
  arrange(desc(ind9))

#=============================================
# Indicator 10 - Antidiabetics: Number of people prescribed self monitoring blood glucose (SMBG) test strips
# who are not prescribed treatments for diabetes or are only prescribed metformin as a % of all people
# prescribed SMBG test strips in 12 months <= 11.14% or a 5% shift from baseline
#=============================================

# Calculate baseline
indicator10_baseline <- antidiabetics %>% filter(paid_date >= baseline_year_start & paid_date <= baseline_year_end) %>%
  group_by(locality, hscp, presc_location_code) %>%
  # Identify all patients on SMBG and metformin with intersect() function
  summarise(
    smbg_and_metformin_all_patients = n_distinct(intersect(pat_chi_number_c[str_detect(pi_approved_name, "METFORMIN")], 
                                                           pat_chi_number_c[pi_approved_name == "BLOOD GLUCOSE TESTING STRIPS"])),
    # Numerator part 1 - Calculate number of patients prescribed SMBG test trips and metformin only but no other diabetes drugs with setdiff() function
    smbg_and_metformin_only_patients = n_distinct(setdiff((intersect(pat_chi_number_c[str_detect(pi_approved_name, "METFORMIN")], 
                                                                     pat_chi_number_c[pi_approved_name == "BLOOD GLUCOSE TESTING STRIPS"])),
                                                          pat_chi_number_c[!str_detect(pi_approved_name, "METFORMIN") & pi_approved_name != "BLOOD GLUCOSE TESTING STRIPS"])),
    # Numerator part 2 - Calculate number of patients prescribed SMBG test strips and no treatments for diabetes with setdiff() function
    smbg_only = n_distinct(setdiff(pat_chi_number_c[pi_approved_name == "BLOOD GLUCOSE TESTING STRIPS"], 
                                   pat_chi_number_c[pi_approved_name != "BLOOD GLUCOSE TESTING STRIPS"])),
    # Add numerators together
    numerator = sum(smbg_and_metformin_only_patients, smbg_only),
    # Denominator - Calculate total number of patients prescribed SMBG test strips
    all_smbg = n_distinct(pat_chi_number_c[pi_approved_name == "BLOOD GLUCOSE TESTING STRIPS"]),
    baseline = round((numerator / all_smbg * 100),2),
  ) %>%
  ungroup() %>%
  filter(!is.na(baseline)) %>%
  select(presc_location_code, baseline)

#str_detect(pi_approved_name, "METFORMIN") - filter all drugs with Metformin in their name vs pi_approved_name == "METFORMIN HYDROCHLORIDE"
#antidiabetics %>% filter(str_detect(pi_approved_name, "METFORMIN")) %>% select(pi_approved_name, pi_bnf_item_description) %>% distinct()

# Indicator 10 table
indicator10_table <- antidiabetics %>% filter(paid_date >= full_back12Mth) %>%
  group_by(locality, hscp, presc_location_code, presc_location_name) %>%
  # Identify all patients on SMBG and metformin with intersect() function
  summarise(
    smbg_and_metformin_all_patients = n_distinct(intersect(pat_chi_number_c[str_detect(pi_approved_name, "METFORMIN")], 
                                                               pat_chi_number_c[pi_approved_name == "BLOOD GLUCOSE TESTING STRIPS"])),
    # Numerator part 1 - Calculate number of patients prescribed SMBG test trips and metformin only but no other diabetes drugs with setdiff() function
    smbg_and_metformin_only_patients = n_distinct(setdiff((intersect(pat_chi_number_c[str_detect(pi_approved_name, "METFORMIN")], 
                                                                             pat_chi_number_c[pi_approved_name == "BLOOD GLUCOSE TESTING STRIPS"])),
                                                                   pat_chi_number_c[!str_detect(pi_approved_name, "METFORMIN") & pi_approved_name != "BLOOD GLUCOSE TESTING STRIPS"])),
    # Numerator part 2 - Calculate number of patients prescribed SMBG test strips and no treatments for diabetes with setdiff() function
    smbg_only = n_distinct(setdiff(pat_chi_number_c[pi_approved_name == "BLOOD GLUCOSE TESTING STRIPS"], 
                                          pat_chi_number_c[pi_approved_name != "BLOOD GLUCOSE TESTING STRIPS"])),
    # Add numerators together
    numerator = sum(smbg_and_metformin_only_patients, smbg_only),
    # Denominator - Calculate total number of patients prescribed SMBG test strips
    all_smbg = n_distinct(pat_chi_number_c[pi_approved_name == "BLOOD GLUCOSE TESTING STRIPS"]),
    ind10 = round((numerator / all_smbg * 100),2),
            ) %>%
  mutate(
    target = target_ind10,
    target_met = if_else(ind10 <= target, "Target Met", "Target Not Met")
  ) %>%
  left_join(indicator10_baseline, by = "presc_location_code") %>%
  mutate(baseline_change = round(ind10 - baseline,2),
         percentage_change = round(baseline_change / baseline * 100,2),
         reduction_from_baseline_target = if_else(percentage_change <= target_shift5, "Shift Met", "")) %>%
  ungroup() %>%
  select(hscp, presc_location_code, ind10, all_smbg, smbg_and_metformin_only_patients, smbg_only, target_met, baseline, baseline_change, reduction_from_baseline_target) %>%
  arrange(desc(ind10))

#=============================================
# Indicator 6 - Gabapentanoids: Pregabalin and gabapentin DDDs per 1000 patients per day <= 15.17
# or a 5% shift from Q3 2019/20
#=============================================           

# Summarise data by quarter
indicator6_byquarter <- gaba %>% filter(!is.na(practice_population)) %>%
  group_by(locality, hscp, presc_location_code, presc_location_name, practice_population, paid_financial_quarter) %>%
  summarise(gaba_DDDs = sum(DDD)) %>%
  mutate(DDDs_per_1000_listsize = round((gaba_DDDs*1000)/practice_population,2),
         DDDs_per_1000_listsize_perday = round(DDDs_per_1000_listsize/(365/4),2),
         target = target_ind6,
         target_met = if_else(DDDs_per_1000_listsize_perday <= target, "Target Met", "Target Not Met"))

# Calculate baselines
indicator6_baseline <- indicator6_byquarter %>% filter(paid_financial_quarter == baseline_quarter) %>%
  rename(baseline = DDDs_per_1000_listsize_perday) %>%
  ungroup() %>%
  select(presc_location_code, baseline)

# Make table of Indicator 6
indicator6_table <- indicator6_byquarter %>% filter(paid_financial_quarter %in% current_quarter) %>%
  left_join(indicator6_baseline, by = "presc_location_code") %>%
  mutate(baseline_change = round(DDDs_per_1000_listsize_perday - baseline,2),
         percentage_change = round(baseline_change / baseline * 100,2),
         reduction_from_baseline_target = if_else(percentage_change <= target_shift5, "Shift Met", "")) %>%
  ungroup() %>%
  select(hscp, presc_location_code, DDDs_per_1000_listsize_perday, DDDs_per_1000_listsize, practice_population, target_met, baseline, baseline_change, reduction_from_baseline_target, gaba_DDDs) %>%
  arrange(desc(DDDs_per_1000_listsize_perday))

#=============================================
# Indicator 7 - Opioids: Number of Defined Daily Doses of Strong Opioids per 1000 List Size per Day <= 15.00
# BNF 4.7.2 and Approved Name: buprenorphine, fentanyl, hydromorphone hydrochloride, morphine, oxycodone, ocycodone and naloxone,
# paracetamol with tramadol hydrochloride, pentazocine, pethidine hydrochloride, tapentadol, tramadol hydrochloride
#=============================================    

# Group data by quarter
indicator7_byquarter <- opioids %>% group_by(locality, hscp, presc_location_code, presc_location_name, practice_population, paid_financial_quarter) %>%
  summarise(opioid_DDDs = sum(DDD)) %>%
  mutate(DDDs_per_1000_listsize = round((opioid_DDDs*1000)/practice_population,2),
         DDDs_per_1000_listsize_perday = round(DDDs_per_1000_listsize/(365/4),2),
         target = target_ind7,
         target_met = if_else(DDDs_per_1000_listsize_perday <= target, "Target Met", "Target Not Met"))

# Calculate baselines
indicator7_baseline <- indicator7_byquarter %>% filter(paid_financial_quarter == baseline_quarter) %>%
  rename(baseline = DDDs_per_1000_listsize_perday) %>%
  ungroup() %>%
  select(presc_location_code, baseline)

# Make table of Indicator 7
indicator7_table <- indicator7_byquarter %>% filter(paid_financial_quarter %in% current_quarter) %>%
  left_join(indicator7_baseline, by = "presc_location_code") %>%
  mutate(baseline_change = round(DDDs_per_1000_listsize_perday - baseline,2),
         percentage_change = round(baseline_change / baseline * 100,2),
         reduction_from_baseline_target = if_else(percentage_change <= target_shift, "Shift Met", "")) %>%
  ungroup() %>%
  select(hscp, presc_location_code, DDDs_per_1000_listsize_perday, DDDs_per_1000_listsize, practice_population, target_met, baseline, baseline_change, reduction_from_baseline_target, opioid_DDDs) %>%
  arrange(desc(DDDs_per_1000_listsize_perday))


# --------------------------------------------- Exporting Outputs ----------------------------------------#

#=============================================
# Export tables to Excel workbook
#=============================================   

# Combine all the tables in a list (each table will be on a separate tab in the outputted Excel file)
list_of_indicators <-
  list("01-Antibiotics" = indicator1_table, "02-Co-Amoxiclav" = indicator2_table,"03-Quinolones" = indicator3_table,"04-High Strength ICS" = indicator4_table,
       "05-SABA" = indicator5_table, "06-Gabapentanoids" = indicator6_table, "07-CNS Opioids" = indicator7_table, "09-Anticholinergics" = indicator9_table,
       "10-Antidiabetics" = indicator10_table)

write.xlsx(list_of_indicators, paste0(outputPath, "/IndicatorTables.xlsx"))

#=============================================
# Export locality reports to Excel
#=============================================
# Format the tables
t1 <- indicator1_table %>%
  left_join(practiceLookup[, 1:2],
            by = c("presc_location_code" = "PracticeCode")) %>%
  relocate(location_name, .after = presc_location_code) %>%
  arrange(hscp, presc_location_code) %>%
  rename(`HSCP` = hscp,
         `Practice Code` = presc_location_code,
         `Practice` = location_name,
         `01 Number of Antibiotic Items` = antibiotics_items,
         `01 Antibiotic Items per 1000 patients per day` = items_per_1000_listsize_perday,
         `01 Indicator Attainment` = target_met) %>%
  select(-practice_population)
  
t2 <- indicator2_table %>%
  left_join(practiceLookup[, 1:2],
            by = c("presc_location_code" = "PracticeCode")) %>%
  relocate(location_name, .after = presc_location_code) %>%
  arrange(hscp, presc_location_code) %>%
  rename(`HSCP` = hscp,
         `Practice Code` = presc_location_code,
         `Practice` = location_name,
         `02 Co-Amoxiclav Items` = antibiotics_items,
         `02 Co-Amoxiclav Items per 1000 LS per 100 days ` = items_per_1000_listsize_per100days,
         `02 Indicator Attainment` = target_met) %>%
  select(-practice_population)

t3 <- indicator3_table %>%
  left_join(practiceLookup[, 1:2],
            by = c("presc_location_code" = "PracticeCode")) %>%
  relocate(location_name, .after = presc_location_code) %>%
  arrange(hscp, presc_location_code) %>%
  rename(`HSCP` = hscp,
         `Practice Code` = presc_location_code,
         `Practice` = location_name,
         `03 Quinolone Items` = antibiotics_items,
         `03 Quinolone Items per 1000 LS per 100 days ` = items_per_1000_listsize_per100days,
         `03 Indicator Attainment` = target_met) %>%
  select(-practice_population)

t4 <- indicator4_table %>%
  left_join(practiceLookup[, 1:2],
            by = c("presc_location_code" = "PracticeCode")) %>%
  relocate(location_name, .after = presc_location_code) %>%
  relocate(total_quantity, .after = highstrengthquantity) %>%
  arrange(hscp, presc_location_code) %>%
  rename(`HSCP` = hscp,
         `Practice Code` = presc_location_code,
         `Practice` = location_name,
         `04 ICS High Dose Items` = highstrengthquantity,
         `04 Number of ICS Items` = total_quantity,
         `04 ICS Percentage High Strength` = percent_highstrength,
         `04 Indicator Attainment` = target_met,
         `04 ICS Reduction in Baseline` = reduction_from_baseline_target) %>%
  select(-baseline, -baseline_change, -otherstrengthquantity)

t5 <- indicator5_table %>%
  left_join(practiceLookup[, 1:2],
            by = c("presc_location_code" = "PracticeCode")) %>%
  relocate(location_name, .after = presc_location_code) %>%
  arrange(hscp, presc_location_code) %>%
  rename(`HSCP` = hscp,
         `Practice Code` = presc_location_code,
         `Practice` = location_name,
         `05 Number of Patients Prescribed >12 SABA in a Year` = overuseSABA,
         `05 Number of Patients Prescribed a SABA in a Year` = patients,
         `05 Percentage Patients Prescribed >12 SABA` = percent,
         `05 Indicator Attainment` = target_met,
         `05 SABA Reduction in Baseline` = reduction_from_baseline_target) %>%
  select(-baseline, -baseline_change)

t6 <- indicator6_table %>%
  left_join(practiceLookup[, 1:2],
            by = c("presc_location_code" = "PracticeCode")) %>%
  relocate(location_name, .after = presc_location_code) %>%
  relocate(gaba_DDDs, .after = location_name) %>%
  arrange(hscp, presc_location_code) %>%
  rename(`HSCP` = hscp,
         `Practice Code` = presc_location_code,
         `Practice` = location_name,
         `06 CNS Gabapentanoid DDD` = gaba_DDDs,
         `06 CNS Gabapentanoid DDDs per 1000 patients per day` = DDDs_per_1000_listsize_perday,
         `06 CNS Gaba Reduction from Baseline` = reduction_from_baseline_target,
         `06 Indicator Attainment` = target_met) %>%
  select(-baseline, -baseline_change, -practice_population, -DDDs_per_1000_listsize)

t7 <- indicator7_table %>%
  left_join(practiceLookup[, 1:2],
            by = c("presc_location_code" = "PracticeCode")) %>%
  relocate(location_name, .after = presc_location_code) %>%
  relocate(opioid_DDDs, .after = location_name) %>%
  arrange(hscp, presc_location_code) %>%
  rename(`HSCP` = hscp,
         `Practice Code` = presc_location_code,
         `Practice` = location_name,
         `07 CNS Opioid DDD` = opioid_DDDs,
         `07 CNS Opioid DDDs per 1000 patients per day` = DDDs_per_1000_listsize_perday,
         `07 CNS Opioid Reduction from Baseline` = reduction_from_baseline_target,
         `07 Indicator Attainment` = target_met) %>%
  select(-baseline, -baseline_change, -practice_population, -DDDs_per_1000_listsize)

t9 <- indicator9_table %>%
  left_join(practiceLookup[, 1:2],
            by = c("presc_location_code" = "PracticeCode")) %>%
  relocate(location_name, .after = presc_location_code) %>%
  arrange(hscp, presc_location_code) %>%
  rename(`HSCP` = hscp,
         `Practice Code` = presc_location_code,
         `Practice` = location_name,
         `09 Number of Patients over 75 prescribed >10 Strong or Very Strong Anticholinergics` = patients_over10items,
         `09 Number of Registered Patients over 75` = over75,
         `09 CNS Anticholinergics Rate` = ind9,
         `09 Indicator Attainment` = target_met) %>%
  select(-practice_population)

t10 <- indicator10_table %>%
  left_join(practiceLookup[, 1:2],
            by = c("presc_location_code" = "PracticeCode")) %>%
  relocate(location_name, .after = presc_location_code) %>%
  arrange(hscp, presc_location_code) %>%
  rename(`HSCP` = hscp,
         `Practice Code` = presc_location_code,
         `Practice` = location_name,
         `10 Number of Patients Prescribed SMBG Test Strips and Metformin Only` = smbg_and_metformin_only_patients,
         `10 Number of Patients Prescribed SMBG Test Strips and no treatments for diabetes` = smbg_only,
         `10 Total Number of Patients Prescribed SMBG Test Strips` = all_smbg,
         `10 SMBG Rate` = ind10,
         `10 Indicator Attainment` = target_met,
         `10 SMBG Reduction in Baseline` = reduction_from_baseline_target) %>%
  select(-baseline, -baseline_change)

summary_table <- merge(t1, t2) %>%
  merge(t3, all = T) %>% merge(t4) %>% merge(t5) %>%
  merge(t6) %>% merge(t7) %>% merge(t9) %>% merge(t10) %>%
  select(-`01 Number of Antibiotic Items`, -`02 Co-Amoxiclav Items`, -`03 Quinolone Items`,-`04 ICS High Dose Items`, -`04 Number of ICS Items`,
         -`05 Number of Patients Prescribed >12 SABA in a Year`, -`05 Number of Patients Prescribed a SABA in a Year`,
         -`06 CNS Gabapentanoid DDD`, -`07 CNS Opioid DDD`, -`09 Number of Patients over 75 prescribed >10 Strong or Very Strong Anticholinergics`,
         -`09 Number of Registered Patients over 75`, -`10 Total Number of Patients Prescribed SMBG Test Strips`,
         -`10 Number of Patients Prescribed SMBG Test Strips and Metformin Only`, -`10 Number of Patients Prescribed SMBG Test Strips and no treatments for diabetes`) %>%
  mutate(target4_met = if_else(`04 Indicator Attainment` == "Target Not Met" & `04 ICS Reduction in Baseline` == "Shift Met", "Target Met", ""),
         target5_met = if_else(`05 Indicator Attainment` == "Target Not Met" & `05 SABA Reduction in Baseline` == "Shift Met", "Target Met", ""),
         target6_met = if_else(`06 Indicator Attainment` == "Target Not Met" & `06 CNS Gaba Reduction from Baseline` == "Shift Met", "Target Met", ""),
         target7_met = if_else(`07 Indicator Attainment` == "Target Not Met" & `07 CNS Opioid Reduction from Baseline` == "Shift Met", "Target Met", ""),
         target10_met = if_else(`10 Indicator Attainment` == "Target Not Met" & `10 SMBG Reduction in Baseline` == "Shift Met", "Target Met", "")) 

summary_table %<>%
  mutate(`Number of Indicators Attained (Out of 9)` = rowSums(summary_table == "Target Met")) %>%
  select(-target4_met, -target5_met, -target6_met, -target7_met, -target10_met)

# Join the tables

list_of_tables <-
  list("Summary" = summary_table,"01-Antibiotics" = t1, "02-Co-Amoxiclav" = t2,"03-Quinolones" = t3,"04-High Strength ICS" = t4,
       "05-SABA" = t5, "06-Gabapentanoids" = t6, "07-CNS Opioids" = t7, "09-Anticholinergics" = t9,
       "10-Antidiabetics" = t10)

write.xlsx(list_of_tables, paste0(outputPath, locality_reports_name, "All Localities.xlsx"))

# Filter by locality

tables_EL <- lapply(list_of_tables, function(t) t[which(t$HSCP %in% c("EL-E", "EL-W")),])
tables_ML <- lapply(list_of_tables, function(t) t[which(t$HSCP %in% c("ML-E", "ML-W")),])
tables_NE <- lapply(list_of_tables, function(t) t[which(t$HSCP == "NE"),])
tables_NW <- lapply(list_of_tables, function(t) t[which(t$HSCP == "NW"),])
tables_SE <- lapply(list_of_tables, function(t) t[which(t$HSCP == "SE"),])
tables_SW <- lapply(list_of_tables, function(t) t[which(t$HSCP == "SW"),])
tables_WL <- lapply(list_of_tables, function(t) t[which(t$HSCP %in% c("WL-E", "WL-W")),])

# Export locality tables
write.xlsx(tables_EL, paste0(outputPath, locality_reports_name, "EL.xlsx"))
write.xlsx(tables_ML, paste0(outputPath, locality_reports_name, "ML.xlsx"))
write.xlsx(tables_NE, paste0(outputPath, locality_reports_name, "NE.xlsx"))
write.xlsx(tables_NW, paste0(outputPath, locality_reports_name, "NW.xlsx"))
write.xlsx(tables_SE, paste0(outputPath, locality_reports_name, "SE.xlsx"))
write.xlsx(tables_SW, paste0(outputPath, locality_reports_name, "SW.xlsx"))
write.xlsx(tables_WL, paste0(outputPath, locality_reports_name, "WL.xlsx"))


#=============================================
# Bar charts
#============================================= 
ind1_chart <- ggplot(indicator1_table)+
  geom_col(aes(x = reorder(presc_location_code, items_per_1000_listsize_perday), y = items_per_1000_listsize_perday, fill = target_met))+
  geom_hline(yintercept = target_ind1, colour = "#FAE100", size = 1.5)+
  geom_text(aes(15, target_ind1, label = paste("Target (", format(target_ind1), ")"), vjust = -1))+
  # geom_hline(yintercept = 1.30, linetype = "dashed", colour = "#7C2855", size = 1.3)+
  # geom_hline(yintercept = 1.40, linetype = "dotted", colour = "#330072", size = 1.3)+
  scale_fill_manual(values = c("#41B6E6", "#003087"))+
  scale_x_discrete(name = "Items per 1000 LS per Day", breaks = NULL)+
  scale_y_continuous(name = "")+
  theme_minimal()+
  theme(axis.text.x = element_blank())+
  theme(legend.title = element_blank())+
  #theme(axis.text.x = element_text(size = rel(0.6), angle = 45, vjust = 1, hjust = 1))+
  ggtitle("01-Antibiotics \nAntibiotics Items Prescribed Per 1000 List Size")

ind2_chart <- ggplot(indicator2_table)+
  geom_col(aes(x = reorder(presc_location_code, items_per_1000_listsize_per100days), y = items_per_1000_listsize_per100days, fill = target_met))+
  geom_hline(yintercept = target_ind2, colour = "#FAE100", size = 1.5)+
  geom_text(aes(15, target_ind2, label = paste("Target (", format(target_ind2), ")"), vjust = -1))+
  scale_fill_manual(values = c("#41B6E6", "#003087"))+
  scale_x_discrete(name = "Items per 1000 LS per 100 Days", breaks = NULL)+
  scale_y_continuous(name = "")+
  theme_minimal()+
  theme(axis.text.x = element_blank())+
  theme(legend.title = element_blank())+
  ggtitle("02-Co-Amoxiclav \nCo-Amoxiclav Items Prescribed Per 1000 List Size")

ind3_chart <- ggplot(indicator3_table)+
  geom_col(aes(x = reorder(presc_location_code, items_per_1000_listsize_per100days), y = items_per_1000_listsize_per100days, fill = target_met))+
  geom_hline(yintercept = target_ind3, colour = "#FAE100", size = 1.5)+
  geom_text(aes(15, target_ind3, label = paste("Target (", format(target_ind3), ")"), vjust = -1))+
  #geom_hline(yintercept = 4, linetype = "dashed", colour = "#7C2855", size = 1.3)+
  #geom_text(aes(15, 4, label = "Proposed Target (4.00)", vjust = -1))+
  scale_fill_manual(values = c("#41B6E6", "#003087"))+
  scale_x_discrete(name = "Items per 1000 LS per 100 Days", breaks = NULL)+
  scale_y_continuous(name = "")+
  theme_minimal()+
  theme(axis.text.x = element_blank())+
  theme(legend.title = element_blank())+
  ggtitle("03-Quinolones \nQuinolone Items Prescribed Per 1000 List Size")

ind4_chart <- ggplot(indicator4_table)+
  geom_col(aes(x = reorder(presc_location_code, percent_highstrength), y = percent_highstrength, fill = target_met))+
  geom_hline(yintercept = target_ind4, colour = "#FAE100", size = 1.5)+
  geom_text(aes(15, target_ind4, label = paste("Target (", format(target_ind4), ")"), vjust = -1))+
  scale_fill_manual(values = c("#41B6E6", "#003087"))+
  scale_x_discrete(name = "% High Dose ICS", breaks = NULL)+
  scale_y_continuous(name = "")+
  theme_minimal()+
  theme(axis.text.x = element_blank())+
  theme(legend.title = element_blank())+
  ggtitle("04-High Strength ICS \nICS % High Dose ICS (Adults)")

ind5_chart <- ggplot(indicator5_table)+
  geom_col(aes(x = reorder(presc_location_code, percent), y = percent, fill = target_met))+
  geom_hline(yintercept = target_ind5, colour = "#FAE100", size = 1.5)+
  geom_text(aes(15, target_ind5, label = paste("Target (", format(target_ind5), ")"), vjust = -1))+
  scale_fill_manual(values = c("#41B6E6", "#003087"))+
  scale_x_discrete(name = "% Patients > 12 SABA", breaks = NULL)+
  scale_y_continuous(name = "")+
  theme_minimal()+
  theme(axis.text.x = element_blank())+
  theme(legend.title = element_blank())+
  ggtitle("05-SABA \nPercentage of patients dispensed over 12 quantity of SABA inhalers in 12 month period")

ind6_chart <- ggplot(indicator6_table)+
  geom_col(aes(x = reorder(presc_location_code, DDDs_per_1000_listsize_perday), y = DDDs_per_1000_listsize_perday, fill = target_met))+
  geom_hline(yintercept = target_ind6, colour = "#FAE100", size = 1.5)+
  geom_text(aes(15, target_ind6, label = paste("Target (", format(target_ind6), ")"), vjust = -1))+
  scale_fill_manual(values = c("#41B6E6", "#003087"))+
  scale_x_discrete(name = "DDDs per 1000 LS per Day", breaks = NULL)+
  scale_y_continuous(name = "")+
  theme_minimal()+
  theme(axis.text.x = element_blank())+
  theme(legend.title = element_blank())+
  ggtitle("06-Gabapentanoids \nPregabalin and gabapentin DDDs per 1,000 list size per day")

ind7_chart <- ggplot(indicator7_table)+
  geom_col(aes(x = reorder(presc_location_code, DDDs_per_1000_listsize_perday), y = DDDs_per_1000_listsize_perday, fill = target_met))+
  geom_hline(yintercept = target_ind7, colour = "#FAE100", size = 1.5)+
  geom_text(aes(15, target_ind7, label = paste("Target (", format(target_ind7), ")"), vjust = -1))+
  scale_fill_manual(values = c("#41B6E6", "#003087"))+
  scale_x_discrete(name = "DDDs per 1000 LS per Day", breaks = NULL)+
  scale_y_continuous(name = "")+
  theme_minimal()+
  theme(axis.text.x = element_blank())+
  theme(legend.title = element_blank())+
  ggtitle("07-Opioids \nStrong opioids DDDs per 1,000 list size per day")

ind9_chart <- ggplot(indicator9_table)+
  geom_col(aes(x = reorder(presc_location_code, ind9), y = ind9, fill = target_met))+
  geom_hline(yintercept = target_ind9, colour = "#FAE100", size = 1.5)+
  geom_text(aes(15, target_ind9, label = paste("Target (", format(target_ind9), ")"), vjust = -1))+
  scale_fill_manual(values = c("#41B6E6", "#003087"))+
  scale_x_discrete(name = "% of Patients >75 years dispensed > 10 items of strong or very strong anticholinergics", breaks = NULL)+
  scale_y_continuous(name = "")+
  theme_minimal()+
  theme(axis.text.x = element_blank())+
  theme(legend.title = element_blank())+
  ggtitle("09-Anticholinergics \nNumber of patients > 75 years dispensed over 10 items of strong or very strong anticholinergics\nin 12 months as a percentage of registered patients aged > 75 years")

ind10_chart <- ggplot(indicator10_table)+
  geom_col(aes(x = reorder(presc_location_code, ind10), y = ind10, fill = target_met))+
  geom_hline(yintercept = target_ind10, colour = "#FAE100", size = 1.5)+
  geom_text(aes(15, target_ind10, label = paste("Target (", format(target_ind10), ")"), vjust = -1))+
  scale_fill_manual(values = c("#41B6E6", "#003087"))+
  scale_x_discrete(name = "% of Patients prescribed blood glucose test strips \nwho are not prescribed treatments for diabetes or are only prescribed metformin", breaks = NULL)+
  scale_y_continuous(name = "")+
  theme_minimal()+
  theme(axis.text.x = element_blank())+
  theme(legend.title = element_blank())+
  ggtitle("10-Antidiabetics \nSelf monitoring of blood glucose (number of people prescribed SMBG test strips\n who are not prescribed treatments for diabetes (6.1.1 and/or 6.1.2)\n or are only prescribed metformin as a % of all people prescribed SMBG test strips)")

#=============================================
# Serial Prescribing
#============================================= 

#------- Process Serial Prescribing Data---------------#

colnames(serial) <- snakecase::to_snake_case(colnames(serial))

serial %<>% filter(str_detect(presc_health_board_name, "NHS")) %>%
  mutate(lothian = if_else(presc_health_board_name == "NHS LOTHIAN", "Lothian", "Other"))

serial_lothian <- serial %>% filter(presc_health_board_name == "NHS LOTHIAN") 

serial_by_healthboard <- serial %>%
  group_by(presc_health_board_name) %>%
  summarise(mean = mean(percentage_of_serial_rx_patients_in_last_12_months_vs_repeat_patients)) %>%
  mutate(lothian = if_else(presc_health_board_name == "NHS LOTHIAN", "Lothian", "Other"))

#------- Visualise Serial Prescribing Data---------------#

# Bar chart of practices in Lothian
ggplot(serial_lothian)+
  geom_col(aes(x = reorder(prescribing_location, percentage_of_serial_rx_patients_in_last_12_months_vs_repeat_patients), 
               y = percentage_of_serial_rx_patients_in_last_12_months_vs_repeat_patients))+
  geom_hline(yintercept = .1, linetype = "dashed", colour = "#7C2855", size = 1.3)+
  geom_text(aes(15, .1, label = "Proposed Target (10.00%)", vjust = -1))+
  geom_hline(yintercept = mean(serial_lothian$percentage_of_serial_rx_patients_in_last_12_months_vs_repeat_patients), colour = "#78BE20", size = 1.5)+
  geom_text(aes(15, mean(percentage_of_serial_rx_patients_in_last_12_months_vs_repeat_patients), label = "Lothian Mean (1.23%)", vjust = -1))+
  geom_vline(xintercept = which.min(abs(sort(serial_lothian$percentage_of_serial_rx_patients_in_last_12_months_vs_repeat_patients) - quantile(serial_lothian$percentage_of_serial_rx_patients_in_last_12_months_vs_repeat_patients,.75))), colour = "#ED8B00", size = .5)+
  geom_vline(xintercept = which.min(abs(sort(serial_lothian$percentage_of_serial_rx_patients_in_last_12_months_vs_repeat_patients) - quantile(serial_lothian$percentage_of_serial_rx_patients_in_last_12_months_vs_repeat_patients,.5))), colour = "#FFB81C", size = .5)+
  geom_vline(xintercept = which.min(abs(sort(serial_lothian$percentage_of_serial_rx_patients_in_last_12_months_vs_repeat_patients) - quantile(serial_lothian$percentage_of_serial_rx_patients_in_last_12_months_vs_repeat_patients,.25))), colour = "#FAE100", size = .5)+
  scale_x_discrete(name = "% of serial rx patients in last 12 months vs repeat patients", breaks = NULL)+
  scale_y_continuous(name = "", labels = percent)+
  theme_minimal()+
  theme(axis.text.x = element_blank())+
  ggtitle("Serial Prescribing by Practice in NHS Lothian")

# Bar chart of health boards in Scotland 
ggplot(serial_by_healthboard)+
  geom_col(aes(x = reorder(presc_health_board_name, mean), 
               y = mean, fill = lothian), show.legend = F)+
  geom_hline(yintercept = mean(serial_by_healthboard$mean), colour = "#AE2573", size = 1.5)+
  geom_text(aes(2, mean(mean), label = "Scotland Mean", vjust = -1))+
  scale_fill_manual(values = c("#003087", "lightgrey"))+
  scale_x_discrete(name = "Mean % of serial rx patients in last 12 months vs repeat patients")+
  scale_y_continuous(name = "", labels = percent)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  ggtitle("Serial Prescribing by NHS Scotland Health Board")
  
# Box plot of health boards in Scotland 
ggplot(serial_by_healthboard)+
  geom_col(aes(x = reorder(presc_health_board_name, mean), 
               y = mean, fill = lothian), show.legend = F)+
  geom_hline(aes(yintercept = mean(mean), linetype = "Scotland Mean"))+
  scale_fill_manual(values = c("blue", "grey"))+
  scale_x_discrete(name = "Mean % of serial rx patients in last 12 months vs repeat patients")+
  scale_y_continuous(name = "", labels = percent)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  ggtitle("Serial Prescribing by NHS Scotland Health Board")



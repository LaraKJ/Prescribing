# This report summarises work done on the xx- funded project
#to pilot a consistent, patient-centred approach in undertaking chronic pain assessment. 
# Adult patients who had been taking pain medications between 3 and 6 months
#and who did not have a cancer diagnosis or were receiving palliative care were invited
#to attend a comprehensive chronic pain review with their GP.  
# Reviews were conducted according to guidelines and following a set template, 
# with pharmacist support for changing dose. By putting in place a plan 
#for managing pain at a relatively early stage, the project aims to reduce 
#the number of patients continuing on pain medication inappropriately in the long term.  
#Each practice participating in the pilot phase aimed to conduct 
#X pain review appointments each week, aiming to conduct up to a total of XXX
#each across the 3-month project pilot phase (MM to MM YYYY).

# The measures used to monitor and evaluate this project are:
 ## <li>Number of prescriptions for pain medications
 ##Volume of pain medications prescribed (DDDs)
 ##Number of patients with a prescription for pain medication
 ##Spend on pain medications
 ##Duration of chronic pain prescribing
 ##Quantity of medication per prescription
 ##Number of times the template is used

#We have evaluated the project cohort against 3 points of comparison:
 ##Baseline data for the project cohort. We used MM YYYY data 
  ###(immediately prior to the pilot's MM start date) for the purposes of establishing the baseline.
 ##All patients in the pilot practices who have been prescribed pain medication for medium duration (between 3 and 6 months).
 ##A control group of patients in 3 other practices that were not selected for this pilot 
  ###but which prescribed comparable levels of opioid DDDs per 1000 list size per day.

# All prescribing data has been taken from the Prescribing Information System (PIS).

library(data.table)
library(dplyr)
library(dbplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(sqldf)
library(RSQLite)
library(plotly)
library(magrittr)
library(stringr)
library(openxlsx)
library(scales)
library(kableExtra)
library(tidyr)
library(networkD3)

# Load source functions
source("N:/03-Reference/R Functions/loadPharmacyFunctions.R")
source("N:/03-Reference/R Functions/chronicPain_functions.R")

#=============================================
# Load Data
#=============================================

# Load PIS data
chronic_pilot <- fread("N:\\Chronic Pain\\PIS Data\\Project\\210616_PIS_data_extract_project3_WLpilot.csv")
chronic_comparative <- fread("N:\\Chronic Pain\\PIS Data\\Project\\210616_PIS_data_extract_project3_comparativepractices.csv")

# Load in GP data (CHI numbers of patients who attended a pain review)
gpdata_cohort_chis <- read_excel("N:\\Chronic Pain\\GP Data\\CHI_numbers_pain_reviews.xlsx")

# Load practice information (list size, hscp)
practiceLookup <- read_excel("N:\\03-Reference\\Lookup Information\\practice_information.xlsx") %>%
  select(-locality, -cluster)

# Load list of agreed pain medications
pain_medications <- read_excel("N:\\Chronic Pain\\Project Lookup\\210201_agreed_list_of_medications.xlsx")

# Load DDDs
DDD_full <- read.csv(file = "N:\\03-Reference\\Lookup Information\\DDD List.csv", header = TRUE, na.strings = "", stringsAsFactors = T) %>%
  # Rename the columns
  rename_with(snakecase::to_snake_case)

# Load DDD conversion values for missing DDDs
DDD_additional <- read_excel("N:\\Chronic Pain\\Project Lookup\\additionalDDDs.xlsx") %>%
  select(pi_bnf_item_code, pi_bnf_item_description, DDDconversion_value)

# Load packsize cutoff values for classifying pack sizes as small, medium or large
packsizes <- read_excel("N:\\Chronic Pain\\Project Lookup\\packsizes.xlsx") %>%
  select(pi_bnf_item_code, packsize_cutoff)

#=============================================
# Process PIS data 
#=============================================

#Combine PIS data for pilot and comparative practices
chronic <- rbind(chronic_pilot, chronic_comparative)

#----------------------(to be deleted after data extract updated)-----------#
# Load the new monthly data
chronic <- as.data.table(chronic) %>%
  mutate(`Presc Location Code` = as.character(`Presc Location Code`),
         `Pat Date of Birth [C]` = mdy(`Pat Date of Birth [C]`),
         `PD Paid GIC excl. BB` = as.numeric(gsub(",", "", `PD Paid GIC excl. BB`)),
         `Paid Quantity` = as.numeric(gsub(",", "", `Paid Quantity`))) %>%
  select(-`PI Daily Dose Conversion`)

# Create Date of Paid Month variable
chronic[, Date:= parse_date_time2(paste(1, `Paid Calendar Month and Year`), "%d %m %Y")]

# Create financial year and quarter variables
chronic <- as.data.table(getFinancialDates(chronic))

# Classify drugs
chronic <- as.data.table(classifyDrug(chronic))

# Calculate patient ages at time of prescription and currently
mostRecentMonth <- floor_date(Sys.Date(), unit = "months") %m-% months(3) #pick out most recent data in the activity file to calculate patients current ages
chronic[, prescribedAge:= as.period(interval(start = `Pat Date of Birth [C]`, end = Date))$year][
  , currentAge:= as.period(interval(start = `Pat Date of Birth [C]`, end = ymd(mostRecentMonth)))$year]

# Create age groups variable
chronic[, ageGroup:= case_when(
  currentAge < 18 ~ "<18",
  currentAge >= 18 & currentAge < 40 ~ "18-39",
  currentAge >= 40 & currentAge < 60 ~ "40-59",
  currentAge >= 60 & currentAge < 75 ~ "60-74",
  currentAge >= 75 ~ "75+"
)
]

# Create variable calculating the morphine equivalent dose
chronic <- morphineEquivalent(chronic)

# Rename column names
colnames(chronic) <- snakecase::to_snake_case(colnames(chronic))

# Change date format in dataframe to string
chronic$date <- format(chronic$date, "%Y-%m-%d")
chronic$pat_date_of_birth_c <- format(chronic$pat_date_of_birth_c, "%Y-%m-%d")

#=============================================
# Process Data 
#=============================================

# Select item code, description and conversion value from DDD
DDD <- DDD_full %>%
  select(pi_bnf_item_code, pi_bnf_item_description, con_val) %>%
  # Remove duplicates
  distinct() %>%
  dplyr::rename(DDDconversion_value = con_val) %>%
  filter(!is.na(DDDconversion_value)) %>%
  # Add conversion values for missing DDDs
  rbind(DDD_additional)

chronic %<>%
  filter(
    # Filter to retain only the pilot practices that conducted pain review and comparable practices
    presc_location_code %in% c(00001, 00002, 00003, 00004, 00005, 00006),
    # Filter to retain list of agreed medications for chronic pain work
    pi_approved_name %in% pain_medications$medication,
    # Filter to remove paediatric medications
    !pi_bnf_item_description %in% c("ALVEDON_PAED SUPPOS 60MG", "CALPOL SIX PLUS FASTMELTS_TAB 250MG", 
                                    "CALPOL_INFANT SUSP 120MG/5ML S/F", "PARACET_ORAL SOLN 500MG/5ML S/F",
                                    "PARACET_ORAL SUSP PAED 120MG/5ML", "ALVEDON_PAED SUPPOS 250MG", "ALVEDON_PAED SUPPOS 125MG",
                                    "PARACET_ORAL SOLN PAED 120MG/5ML S/F", "PARACET_ORAL SUSP 120MG/5ML C/F S/F",
                                    "PARACET_ORAL SUSP 250MG/5ML", "PARACET_ORAL SUSP 250MG/5ML S/F",
                                    "PARACET_SUPPOS 125MG                 @GN", "PARACET_SUPPOS 240MG",
                                    "PARACET_SUPPOS 250MG                 @GN", "VOLTAROL_INJ I/M 25MG/ML 3ML AMP", "VOLTAROL_SUPPOS PAED 12.5MG",
                                    "MIGRALEVE COMPLETE_TAB", "MIGRALEVE_PINK TAB", "MIGRALEVE_YELLOW TAB"),
    # Filter to remove injections and suppositories (more likely to be prescribed for palliative patients)
    !pi_drug_formulation %in% c ("INJ", "SUPPS")) %>%
  # Join to practice information
  left_join(practiceLookup,
            by = c("presc_location_code" = "PracticeCode")) %>%
  # Join and to DDDs
  left_join(DDD[c(1,3)], by = "pi_bnf_item_code") %>%
  # Join to packsize cutoff values
  left_join(packsizes, by = "pi_bnf_item_code") %>%
  # Calculate DDDs
  mutate(
    pat_upi_c = as.character(pat_upi_c),
    date = as.Date(date),
    # Calculate DDDs
    DDD = paid_quantity / DDDconversion_value,
    # Shorten the practice name
    presc_location_name = tm::removeWords(presc_location_name, c(" MEDICAL", " GROUP", " CENTRE"," PRACTICE")),
    # Distinguish between pilot and comparative practices
    practice_type = case_when(
      presc_location_code %in% c("00001", "00002", "00003") ~ "Pilot",
      presc_location_code %in% c("00004", "00005", "00006") ~ "Comparative"),
    # Calculate where the pack size is small, medium or large, based on cut-off values (small is typically a week's supply, and medium is twice that)
    packsize = case_when(
      paid_quantity <= packsize_cutoff ~ "small",
      paid_quantity > packsize_cutoff & paid_quantity <= (packsize_cutoff * 2) ~ "medium",
      paid_quantity > (packsize_cutoff * 2) ~ "large"
    ))

# Change drug classification names
chronic$class[chronic$class == "Other"] <- "Adjuvant Analgesics"
chronic$class[chronic$class == "Adjuvant Analgesics"] <- "Neuropathic Agents"
chronic$class[chronic$class == "Strong Opiod"] <- "Strong Opioid"
chronic$class[chronic$class == "Weak Opiod"] <- "Weak Opioid"
chronic$class[chronic$class == "Non-opiod Analgesics"] <- "Non-opioid Analgesics"

# Anonymise names of the comparative practices
chronic$presc_location_name[chronic$presc_location_code == 00004] <- "Comparative Practice 1"
chronic$presc_location_name[chronic$presc_location_code == 00005] <- "Comparative Practice 2"
chronic$presc_location_name[chronic$presc_location_code == 00006] <- "Comparative Practice 3"

#=============================================
# Create date objects 
#=============================================

# Create objects for dates
current_month <- max(chronic$date)
latest_date <- format.Date(current_month, "%B %Y")
full_back12Mth <- format(as.Date(current_month) %m-% months(11), "%Y-%m-%d")
full_back12Mth_pretty <- format(as.Date(current_month) %m-% months(11), "%B %Y")
full_back3Mth <- format(as.Date(current_month) %m-% months(2), "%Y-%m-%d")
full_back3Mth_pretty <- format(as.Date(current_month) %m-% months(2), "%B %Y")

baseline_month <- as.Date("2021-02-01")
project_start_month <- as.Date("2021-03-01")
project_end_month <- as.Date("2021-05-01")

#=============================================
# Create summary data
#=============================================

#----------------------- Patient Level Summaries ---------------------------#

# Table of patients, start and most recent date of prescription by class of drug
table_of_all_patients <- chronic %>% group_by(practice_type, presc_location_name, practice_population, pat_upi_c, class, pi_approved_name) %>%
  summarise(start_date = min(date),
            last_date = max(date),
            totalDDDs = sum(DDD),
            num_prescriptions = sum(number_of_paid_items),
            cost = sum(pd_paid_gic_excl_bb)) %>%
  mutate(interval = as.Date(last_date) - as.Date(start_date),
         interval_months = interval(start_date, last_date) %/% months(1),
         # Calculate the number of prescriptions between starting and stopping a medicine
         items_per_interval_month = num_prescriptions / abs(interval_months),
         # Replace "Inf" values (from dividing 0 by 0) with NAs
         items_per_interval_month = na_if(items_per_interval_month, "Inf"))

cohort_chis <- gpdata_cohort_chis %>%
  distinct(presc_location_name, presc_location_code, pat_upi_c)

# Identify patients on medication between 3-6 months by the time of the baseline
patients_3_6months <- table_of_all_patients %>% 
     filter(start_date >= 2020-09-01,
       interval_months >= 3 & interval_months <=6,
    # Include patients who've averaged at least 1 prescription every 2 months between starting and stopping a medication (to avoid those with a gap)
            items_per_interval_month > .5)

# Number of patients prescribed chronic pain medication in the last 2 years
patients_by_practice <- chronic %>% group_by(presc_location_name) %>% summarise(all_chronic_pain_patients = n_distinct(pat_upi_c))

cohort_patients_by_practice <- cohort_chis %>% group_by(presc_location_name) %>% summarise(all_patients_reviewed = n_distinct(pat_upi_c))

#----------------------- Section 1 - Summary Information ---------------------------#

# Size of project_cohort - Summary Table
cohort_summary_table <- patients_3_6months %>% group_by(practice_type, presc_location_name) %>% 
  summarise(`Number of Patients on Pain Medications >3 and < 6 months` = n_distinct(pat_upi_c)) %>%
  left_join(patients_by_practice, by = "presc_location_name") %>%
  left_join(cohort_patients_by_practice, by = "presc_location_name") %>%
  rename(`Number of Patients Who Have Had a Chronic Pain Assessment Between March and May 2021` = all_patients_reviewed,
         `Number of Patients with at least 1 Pain Medication Prescription Since January 2019` = all_chronic_pain_patients,
         `Practice Name` = presc_location_name) %>%
  arrange(desc(practice_type)) %>%
  select(-practice_type)
cohort_summary_table$`Number of Patients Who Have Had a Chronic Pain Assessment Between March and May 2021`[is.na(cohort_summary_table$`Number of Patients Who Have Had a Chronic Pain Assessment Between March and May 2021`)] <- 0

#----------------------- Section 3 - Prescribing Trends ---------------------------#
# Trends for All Chronic Pain Patients (By Drug Class)
trends_by_date <- chronic %>%
  group_by(practice_type, presc_location_name, practice_population, class, date) %>%
  mutate(date = as.Date(date)) %>%
  summarise(
    # Summarise data for all chronic pain patients
    num_prescriptions_all = sum(number_of_paid_items),
    num_patients_all = n_distinct(pat_upi_c),
    cost_all = sum(pd_paid_gic_excl_bb),
    quantity_all = sum(paid_quantity),
    totalDDDs_all = sum(!is.na(DDD)),
    cost_per_patient_all = cost_all/num_prescriptions_all,
    DDDs_per_patient_all = totalDDDs_all/num_prescriptions_all,
    # Summarise data for patients on medication >3 and < 6 months
    num_prescriptions_medium_duration = sum(number_of_paid_items[pat_upi_c %in% patients_3_6months$pat_upi_c]),
    num_patients_medium_duration = n_distinct(pat_upi_c[pat_upi_c %in% patients_3_6months$pat_upi_c]),
    cost_medium_duration = sum(pd_paid_gic_excl_bb[pat_upi_c %in% patients_3_6months$pat_upi_c]),
    quantity_medium_duration = sum(paid_quantity[pat_upi_c %in% patients_3_6months$pat_upi_c]),
    totalDDDs_medium_duration = sum(!is.na(DDD[pat_upi_c %in% patients_3_6months$pat_upi_c])),
    cost_per_patient_medium_duration = cost_medium_duration/num_patients_medium_duration,
    DDDs_per_patient_medium_duration = totalDDDs_medium_duration/num_patients_medium_duration,
    # Summarise data for project cohort
    num_prescriptions_project_cohort = sum(number_of_paid_items[pat_upi_c %in% cohort_chis$pat_upi_c]),
    num_patients_project_cohort = n_distinct(pat_upi_c[pat_upi_c %in% cohort_chis$pat_upi_c]),
    cost_project_cohort = sum(pd_paid_gic_excl_bb[pat_upi_c %in% cohort_chis$pat_upi_c]),
    quantity_project_cohort = sum(paid_quantity[pat_upi_c %in% cohort_chis$pat_upi_c]),
    totalDDDs_project_cohort = sum(!is.na(DDD[pat_upi_c %in% cohort_chis$pat_upi_c])),
    cost_per_patient_project_cohort = cost_project_cohort/num_prescriptions_project_cohort,
    DDDs_per_patient_project_cohort = totalDDDs_project_cohort/num_prescriptions_project_cohort
  )

# Overall trends (not broken down by drug class)
trends_by_date_overall <- chronic %>%
  group_by(practice_type, presc_location_name, practice_population, date) %>%
  mutate(date = as.Date(date)) %>%
  summarise(
    # Summarise data for all chronic pain patients
    num_prescriptions_all = sum(number_of_paid_items),
    num_patients_all = n_distinct(pat_upi_c),
    cost_all = sum(pd_paid_gic_excl_bb),
    quantity_all = sum(paid_quantity),
    totalDDDs_all = sum(!is.na(DDD)),
    cost_per_patient_all = cost_all/num_prescriptions_all,
    DDDs_per_patient_all = totalDDDs_all/num_prescriptions_all,
    # Summarise data for patients on medication >3 and < 6 months
    num_prescriptions_medium_duration = sum(number_of_paid_items[pat_upi_c %in% patients_3_6months$pat_upi_c]),
    num_patients_medium_duration = n_distinct(pat_upi_c[pat_upi_c %in% patients_3_6months$pat_upi_c]),
    cost_medium_duration = sum(pd_paid_gic_excl_bb[pat_upi_c %in% patients_3_6months$pat_upi_c]),
    quantity_medium_duration = sum(paid_quantity[pat_upi_c %in% patients_3_6months$pat_upi_c]),
    totalDDDs_medium_duration = sum(!is.na(DDD[pat_upi_c %in% patients_3_6months$pat_upi_c])),
    cost_per_patient_medium_duration = cost_medium_duration/num_patients_medium_duration,
    DDDs_per_patient_medium_duration = totalDDDs_medium_duration/num_patients_medium_duration,
    # Summarise data for project cohort
    num_prescriptions_project_cohort = sum(number_of_paid_items[pat_upi_c %in% cohort_chis$pat_upi_c]),
    num_patients_project_cohort = n_distinct(pat_upi_c[pat_upi_c %in% cohort_chis$pat_upi_c]),
    cost_project_cohort = sum(pd_paid_gic_excl_bb[pat_upi_c %in% cohort_chis$pat_upi_c]),
    quantity_project_cohort = sum(paid_quantity[pat_upi_c %in% cohort_chis$pat_upi_c]),
    totalDDDs_project_cohort = sum(!is.na(DDD[pat_upi_c %in% cohort_chis$pat_upi_c])),
    cost_per_patient_project_cohort = cost_project_cohort/num_prescriptions_project_cohort,
    DDDs_per_patient_project_cohort = totalDDDs_project_cohort/num_prescriptions_project_cohort
  )

# Trends by date broken down by pi_approved_name
trends_by_date_approved_name <- chronic %>%
  group_by(practice_type, presc_location_name, practice_population, class, pi_approved_name, date) %>%
  mutate(date = as.Date(date)) %>%
  summarise(
    # Summarise data for all chronic pain patients
    num_prescriptions_all = sum(number_of_paid_items),
    num_patients_all = n_distinct(pat_upi_c),
    cost_all = sum(pd_paid_gic_excl_bb),
    quantity_all = sum(paid_quantity),
    totalDDDs_all = sum(!is.na(DDD)),
    cost_per_patient_all = cost_all/num_prescriptions_all,
    DDDs_per_patient_all = totalDDDs_all/num_prescriptions_all,
    # Summarise data for patients on medication >3 and < 6 months
    num_prescriptions_medium_duration = sum(number_of_paid_items[pat_upi_c %in% patients_3_6months$pat_upi_c]),
    num_patients_medium_duration = n_distinct(pat_upi_c[pat_upi_c %in% patients_3_6months$pat_upi_c]),
    cost_medium_duration = sum(pd_paid_gic_excl_bb[pat_upi_c %in% patients_3_6months$pat_upi_c]),
    quantity_medium_duration = sum(paid_quantity[pat_upi_c %in% patients_3_6months$pat_upi_c]),
    totalDDDs_medium_duration = sum(!is.na(DDD[pat_upi_c %in% patients_3_6months$pat_upi_c])),
    cost_per_patient_medium_duration = cost_medium_duration/num_patients_medium_duration,
    DDDs_per_patient_medium_duration = totalDDDs_medium_duration/num_patients_medium_duration,
    # Summarise data for project cohort
    num_prescriptions_project_cohort = sum(number_of_paid_items[pat_upi_c %in% cohort_chis$pat_upi_c]),
    num_patients_project_cohort = n_distinct(pat_upi_c[pat_upi_c %in% cohort_chis$pat_upi_c]),
    cost_project_cohort = sum(pd_paid_gic_excl_bb[pat_upi_c %in% cohort_chis$pat_upi_c]),
    quantity_project_cohort = sum(paid_quantity[pat_upi_c %in% cohort_chis$pat_upi_c]),
    totalDDDs_project_cohort = sum(!is.na(DDD[pat_upi_c %in% cohort_chis$pat_upi_c])),
    cost_per_patient_project_cohort = cost_project_cohort/num_prescriptions_project_cohort,
    DDDs_per_patient_project_cohort = totalDDDs_project_cohort/num_prescriptions_project_cohort
  )

# Overall data for all pilot vs comparative practices (not broken down by practice)
trends_by_date_approved_name_summary <- chronic %>%
  group_by(practice_type, class, pi_approved_name, date) %>%
  mutate(date = as.Date(date)) %>%
  summarise(
    # Summarise data for all chronic pain patients
    num_prescriptions_all = sum(number_of_paid_items),
    num_patients_all = n_distinct(pat_upi_c),
    cost_all = sum(pd_paid_gic_excl_bb),
    quantity_all = sum(paid_quantity),
    totalDDDs_all = sum(!is.na(DDD)),
    cost_per_patient_all = cost_all/num_prescriptions_all,
    DDDs_per_patient_all = totalDDDs_all/num_prescriptions_all,
    # Summarise data for patients on medication >3 and < 6 months
    num_prescriptions_medium_duration = sum(number_of_paid_items[pat_upi_c %in% patients_3_6months$pat_upi_c]),
    num_patients_medium_duration = n_distinct(pat_upi_c[pat_upi_c %in% patients_3_6months$pat_upi_c]),
    cost_medium_duration = sum(pd_paid_gic_excl_bb[pat_upi_c %in% patients_3_6months$pat_upi_c]),
    quantity_medium_duration = sum(paid_quantity[pat_upi_c %in% patients_3_6months$pat_upi_c]),
    totalDDDs_medium_duration = sum(!is.na(DDD[pat_upi_c %in% patients_3_6months$pat_upi_c])),
    cost_per_patient_medium_duration = cost_medium_duration/num_patients_medium_duration,
    DDDs_per_patient_medium_duration = totalDDDs_medium_duration/num_patients_medium_duration,
    # Summarise data for project cohort
    num_prescriptions_project_cohort = sum(number_of_paid_items[pat_upi_c %in% cohort_chis$pat_upi_c]),
    num_patients_project_cohort = n_distinct(pat_upi_c[pat_upi_c %in% cohort_chis$pat_upi_c]),
    cost_project_cohort = sum(pd_paid_gic_excl_bb[pat_upi_c %in% cohort_chis$pat_upi_c]),
    quantity_project_cohort = sum(paid_quantity[pat_upi_c %in% cohort_chis$pat_upi_c]),
    totalDDDs_project_cohort = sum(!is.na(DDD[pat_upi_c %in% cohort_chis$pat_upi_c])),
    cost_per_patient_project_cohort = cost_project_cohort/num_prescriptions_project_cohort,
    DDDs_per_patient_project_cohort = totalDDDs_project_cohort/num_prescriptions_project_cohort
  )

# Per patient info
trends_by_date_patient_level <- chronic %>%
  group_by(practice_type, presc_location_name, class, pi_approved_name, pat_upi_c, date) %>%
  mutate(date = as.Date(date)) %>%
  summarise(cost = sum(pd_paid_gic_excl_bb),
            totalDDDs = sum(sum(!is.na(DDD))),
            items = sum(number_of_paid_items))

# Baseline data
baseline_patients <- chronic %>% 
  mutate(comparison_groups = case_when(
    pat_upi_c %in% cohort_chis$pat_upi_c ~ "Project Cohort",
    practice_type == "Comparative" ~ "Comparative Practices",
    practice_type == "Pilot" & !(pat_upi_c %in% cohort_chis$pat_upi_c) ~ "Pilot Practices"
  )) %>%
  group_by(practice_type, comparison_groups, pat_upi_c, date) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= "2021-02-01" & date <= "2021-06-01", pat_upi_c %in% patients_3_6months$pat_upi_c) %>%
  summarise(patients = n_distinct(pat_upi_c),
            cost = sum(pd_paid_gic_excl_bb),
            totalDDDs = sum(DDD),
            items = sum(number_of_paid_items)) %>%
  ungroup() %>%
  group_by(comparison_groups, date) %>%
  summarise(mean_cost = round(mean(cost),2),
            min_cost = min(cost),
            max_cost = max(cost),
            sd_cost = round(sd(cost),2),
            mean_DDDs = round(mean(totalDDDs, na.rm = T),2),
            min_DDDs = round(min(totalDDDs, na.rm = T),2),
            max_DDDs = round(max(totalDDDs, na.rm = T),2),
            sd_DDDs = round(sd(totalDDDs, na.rm = T),2),
            mean_items = round(mean(items),2),
            min_items = min(items),
            max_items = max(items),
            sd_items = round(sd(items)),2) %>% 
  mutate(date = format(date, "%b %Y"))

#----------------------- Section 4 - Number of Patients / Duration ---------------------------#

#Duration of Chronic Pain Prescribing
duration <- table_of_all_patients %>% group_by (practice_type, class, interval_months) %>% summarise(patients = n_distinct(pat_upi_c))

duration_by_category <- table_of_all_patients %>% filter(practice_type == "Pilot") %>%
  mutate(start_date = as.Date(start_date),
         last_date = as.Date(last_date)) %>%
  #group_by(presc_location_name, interval, class) %>%
  mutate(duration = case_when(
    interval_months == "0" ~ "0 (One-off)",
    interval_months > 0 & interval_months <= 3 ~ "1-3 months",
    interval_months > 3 & interval_months < 6 ~ "4-6 months",
    interval_months >=6 ~ "7+ months")) %>%
  group_by(presc_location_name, class, duration) %>%
  summarise(patients = n_distinct(pat_upi_c))


#----------------------- Section 5 - Quantity per Prescription ---------------------------#

prescription_quantities <- chronic[chronic$pat_upi_c %in% cohort_chis$pat_upi_c,] %>%
  mutate(date = as.Date(date),
         quantity_per_prescription = paid_quantity / number_of_paid_items,
         DDDs_per_prescription = DDD / number_of_paid_items)

# Number of Prescriptions by Pack Sizes
prescriptions_by_packsize <- chronic %>% filter(!is.na(packsize)) %>% group_by(class) %>%
  summarise(small = sum(number_of_paid_items[packsize == "small"]),
            medium = sum(number_of_paid_items[packsize == "medium"]),
            large = sum(number_of_paid_items[packsize == "large"])) %>%
  mutate(`Percentage of Prescriptions that are Small Pack Size` = round(small / (small + medium + large) * 100,2),
         small = formatC(small, big.mark = ","),
         medium = formatC(medium, big.mark = ","),
         large = format(large, big.mark = ","))

# Number of Prescriptions by Pack Sizes - Project Cohort
prescriptions_by_packsize_cohort <- chronic %>% filter(pat_upi_c %in% cohort_chis$pat_upi_c) %>% group_by(class) %>%
  summarise(small = sum(number_of_paid_items[packsize == "small"], na.rm = T),
            medium = sum(number_of_paid_items[packsize == "medium"], na.rm = T),
            large = sum(number_of_paid_items[packsize == "large"], na.rm = T)) %>%
  mutate(`Percentage of Prescriptions that are Small Pack Size` = round(small / (small + medium + large) * 100,2)) %>%
  rename(`Drug Class` = class,
         `Small Packsize` = small,
         `Medium Packsize` = medium,
         `Large Packsize` = large)

#=============================================
# Sankey Diagram - Data Processing
#=============================================

#----------------------Process Data for nodes and flows ----------------#

sankey_raw_data <- chronic %>% filter(pat_upi_c %in% cohort_chis$pat_upi_c, 
                                      date >= "2020-02-01" & date <= "2020-06-01") %>%
  select(pat_upi_c, date, class, pi_approved_name, number_of_paid_items, paid_quantity, DDD) %>%
  select(-pi_approved_name, -paid_quantity, -DDD)

# Use row_number function to rank the number of times an item has been prescribed per drug class and per person
sankey_pivot_wide <- sankey_raw_data %>%
  arrange(date, pat_upi_c) %>%
  transform(id = as.numeric(factor(pat_upi_c))) %>%
  # Retain distinct values so a drug class is counted only once in a month
  distinct() %>%
  mutate(row_id = 1:n()) %>%
  group_by(id, class) %>%
  mutate(flow_num = row_number(),
         date = month(as.Date(date)),
         date = date - 1,
         class2 = class) %>%
  # Use pivot_wider function to create separate flow column for each month
  pivot_wider(names_from = date, values_from = class, names_prefix = "flow_") %>%
  ungroup() %>%
  group_by(id) %>%
  # Create unique id for each patient and the nth time they've been prescribed an item per drug class
  mutate(presc_id_class = paste0(id, sep = '_', class2, sep = '_', flow_num),
         presc_id = paste0(id, sep = '_', flow_num)) %>%
  # Create variable to capture the order and number of entities - max number of flows per person per drug class
  mutate(FlowNumN = max(flow_num)) %>%
  ungroup() %>%
  # Cap number of entities to 7
  filter(flow_num <= 7)  %>%
  select(-pat_upi_c, -row_id) %>%
  select(order(colnames(.))) %>%
  arrange(id, presc_id)

# Put the flows in one row per patient and per class
sankey_pivot_wide_collapsed <- sankey_pivot_wide %>% select(-presc_id_class, -number_of_paid_items, -presc_id) %>% group_by(id, class2) %>%
  summarise_all(~first(na.omit(.))) %>%
  select(id, FlowNumN, class2, flow_1, flow_2, flow_3, flow_4, flow_5)

# Add a number to character variable to specify the order of the entity in 
#     - the sequence. This is important for ensuring the related nodes are
#     - linked when we summarise the data to generate the frequency
sankey_flow_numbered <- sankey_pivot_wide_collapsed %>% 
  mutate(flow_1 = case_when(flow_1 != " " ~ paste0(flow_1, sep = '_1'))) %>%
  mutate(flow_2 = case_when(flow_2 != " " ~ paste0(flow_2, sep = '_2'))) %>%
  mutate(flow_3 = case_when(flow_3 != " " ~ paste0(flow_3, sep = '_3'))) %>%
  mutate(flow_4 = case_when(flow_4 != " " ~ paste0(flow_4, sep = '_4'))) %>%
  mutate(flow_5 = case_when(flow_5 != " " ~ paste0(flow_5, sep = '_5')))

# Summarise the frequencies of each prescribing flow
sankey_flow_freq_chronic <- sankey_flow_numbered %>%
  group_by(flow_1, flow_2, flow_3, flow_4, flow_5) %>%
  summarise(n = n()) %>%
  ungroup()

# Create subsets of the above tibble so that each one includes a source
#     - and target nodes and the frequency of each occurrence
flow_1_2_chronic <- sankey_flow_freq_chronic %>%
  select(In = 1, Out = 2, 6) 

flow_2_3_chronic <- sankey_flow_freq_chronic %>%
  select(In = 2, Out = 3, 6)

flow_3_4_chronic <- sankey_flow_freq_chronic %>%
  select(In = 3, Out = 4, 6) 

flow_4_5_chronic <- sankey_flow_freq_chronic %>%
  select(In = 4, Out = 5, 6) 

# Use rbind to combine into one tibble and summarise again to get the  
#     - frequency of each source and target node combination

sankey_plot_data_chronic <- rbind(flow_1_2_chronic, flow_2_3_chronic, flow_3_4_chronic, flow_4_5_chronic) %>%
  filter(!is.na(Out)) %>%
  group_by(In, Out) %>%
  summarise(Freq = sum(n))

# Change NA's to "None" (for patients that aren't initially on medication but start later)
sankey_plot_data_chronic$In[is.na(sankey_plot_data_chronic$In)] <- "None"

#******************************* Create node ID *******************************# 

#### - The graph is generated using an ID, not the entity names, so we need to
#### - create an id to match each entity name

### 1 - Create a node data frame which lists every entity involved in the flow
#     - Create 2 cols: the name of the entity with _n (needed for link ID), and 
#     - the name of the entity with the number removed (data labels)

nodes_chronic <- sankey_plot_data_chronic %>%
  select(In, Out) %>%
  pivot_longer(c("In", "Out"), names_to = "col_name", 
               values_to = "name_match") %>%
  select(-1) %>% distinct() %>%
  mutate(name = str_sub(name_match, end =-3),
         IDIn = row_number()-1,
         IDOut = row_number()-1)

nodes_chronic <- data.frame(nodes_chronic)

### 2 - Create ID using match function
#     - Match function returns the position of the order of the matched name
#     - specified in the node dataframe

sankey_plot_id_chronic <- sankey_plot_data_chronic %>%
  mutate(IDIn = match(In, nodes_chronic$name_match)-1,
         IDOut = match(Out, nodes_chronic$name_match)-1)

sankey_plot_id_chronic <- data.frame(sankey_plot_id_chronic)


#******************************* Create graph *******************************# 

### 1 Basic

sankeyNetwork(Links = sankey_plot_id_chronic, Nodes = nodes_chronic,
              Source = "IDIn", Target = "IDOut",
              Value = "Freq", NodeID = "name"
)

### 2 Format nodes, font and flow

sankeyNetwork(Links = sankey_plot_id_chronic, Nodes = nodes_chronic,
              Source = "IDIn", Target = "IDOut",
              Value = "Freq", NodeID = "name",
              nodeWidth = 20, nodePadding = 10, 
              fontSize = 12, fontFamily = "Arial", 
              height = 370, width = 600,
              sinksRight = FALSE
)
### 3 Edit colours
## Use d3.scaleOrdinal() to constructs a new ordinal scale of colours and their names

node_colour_chronic <- 'd3.scaleOrdinal() .domain(["NHS_Blue","NHS_Light_Blue","NHS_Green","NHS_Light_Green",
                                              "NHS_Yellow","Grey"]) 
                .range(["#005EB8", "#41B6E6", "#009639", "#78BE20", 
                         "#FAE100", "#817f82"])'


nodes_chronic <- nodes_chronic %>% 
  mutate(col_group = case_when(name == "Neuropathic Agents" ~ "NHS_Blue",
                               name == "Non-opioid Analgesics" ~ "NHS_Light_Blue",
                               name == "NSAIDS" ~ "NHS_Yellow",
                               name == "Strong Opioid" ~ "NHS_Green",
                               name == "Weak Opioid" ~ "NHS_Light_Green",
                               name == "No" ~ "Grey"))

#=============================================
# Visualisations
#=============================================


#====================== Section 3 - Prescribing Trends ===========================#

#------------------------ Visualisations by Class of Drug----------------------------#

##------------------------ Number of Prescriptions ----------------------------#

### Fig 1.1 Number of Prescriptions of Pain Medications - Project Cohort - Overall (Not Broken Down by Practice)
ggplot(trends_by_date[trends_by_date$practice_type == "Pilot",])+
  geom_col(aes(x = date, y = num_patients_project_cohort, fill = class), alpha = .5)+
  stat_summary(aes(x = date, y = num_prescriptions_project_cohort, colour = class),fun = "sum", geom = "line", size = 1.5)+
  annotate("rect", xmin = project_start_month, xmax = project_end_month, 
           ymin = 0, ymax = max(trends_by_date$num_prescriptions_project_cohort) * 2.3, alpha = .2)+
  theme_classic()+
  scale_y_continuous(name = "Number of Prescriptions",
                     sec.axis = sec_axis(~., name = "Number of Patients\n with a Prescription"))+
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", name = " ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  facet_wrap(~class)+
  labs(title = paste("Fig 1.1 Overall Number of Prescriptions: Project Cohort,\n", 
                     format(min(trends_by_date$date),'%b %Y'), "-", format(max(trends_by_date$date),'%b %Y'), sep = " "), 
       subtitle = "The shaded area indicates the months when the project was live",
       caption = "Data Source: PIS")

### Fig 1.2 Number of Prescriptions of Pain Medications - Project Cohort - by Practice
ggplot(trends_by_date[trends_by_date$practice_type == "Pilot",])+
  geom_line(aes(x = date, y = num_prescriptions_project_cohort, group = presc_location_name, colour = presc_location_name), size = 1, alpha = .8)+
  stat_summary(aes(x = date, y = num_prescriptions_project_cohort, colour = "Mean"),fun = "mean", geom = "line", size = 1.5)+
  annotate("rect", xmin = project_start_month, xmax = project_end_month, 
           ymin = 0, ymax = max(trends_by_date$num_prescriptions_project_cohort), alpha = .2)+
  theme_classic()+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+
  scale_y_continuous(name = "Number of Prescriptions")+
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", name = " ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  facet_wrap(~class, nrow = 3)+
  guides(colour=guide_legend(nrow=1))+
  labs(title = paste("Fig 1.2 Overall Number of Prescriptions per Practice: Project Cohort,\n", 
                     format(min(trends_by_date$date),'%b %Y'), "-", format(max(trends_by_date$date),'%b %Y'), sep = " "), 
       subtitle = "The shaded area indicates the months when the project was live",
       caption = "Data Source: PIS")

#------------------------ Quantity and DDDs ----------------------------#

# Overall Monthly DDDs
ggplot(trends_by_date[trends_by_date$practice_type == "Pilot",])+
  geom_col(aes(x = date, y = num_patients_project_cohort, fill = class), alpha = .5)+
  stat_summary(aes(x = date, y = totalDDDs_project_cohort, colour = class),fun = "sum", geom = "line", size = 1.5)+
  annotate("rect", xmin = project_start_month, xmax = project_end_month, 
           ymin = 0, ymax = max(trends_by_date$num_prescriptions_project_cohort) * 2.3, alpha = .2)+
  theme_classic()+
  scale_y_continuous(name = "Total Monthly DDDs",
                     sec.axis = sec_axis(~., name = "Number of Patients\n with a Prescription"))+
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", name = " ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  facet_wrap(~class)+
  labs(title = paste("Fig 1.3 Total Monthly DDDs: Project Cohort,\n", 
                     format(min(trends_by_date$date),'%b %Y'), "-", format(max(trends_by_date$date),'%b %Y'), sep = " "), 
       subtitle = "The shaded area indicates the months when the project was live",
       caption = "Data Source: PIS")

# Per Patient Monthly DDDs for the project cohort
ggplot(trends_by_date[trends_by_date$practice_type == "Pilot",])+
  geom_line(aes(x = date, y = DDDs_per_patient_project_cohort, group = presc_location_name, colour = presc_location_name), size = 1, alpha = .8)+
  stat_summary(aes(x = date, y = DDDs_per_patient_project_cohort, colour = "Mean"),fun = "mean", geom = "line", size = 1.5)+
  annotate("rect", xmin = project_start_month, xmax = project_end_month, 
           ymin = 0, ymax = max(!is.na(trends_by_date$DDDs_per_patient_project_cohort)), alpha = .2)+
  theme_classic()+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+
  scale_y_continuous(name = "Monthly DDDs per Patient")+
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", name = " ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  facet_wrap(~class, nrow = 3)+
  guides(colour=guide_legend(nrow=1))+
  labs(title = paste("Fig 1.4a Per Patient Monthly DDDs per Practice: Project Cohort,\n", 
                     format(min(trends_by_date$date),'%b %Y'), "-", format(max(trends_by_date$date),'%b %Y'), sep = " "), 
       subtitle = "The shaded area indicates the months when the project was live",
       caption = "Data Source: PIS")

# Total Monthly DDDs for the project cohort
ggplot(trends_by_date[trends_by_date$practice_type == "Pilot",])+
  geom_line(aes(x = date, y = totalDDDs_project_cohort, group = presc_location_name, colour = presc_location_name), size = 1, alpha = .8)+
  stat_summary(aes(x = date, y = totalDDDs_project_cohort, colour = "Mean"),fun = "mean", geom = "line", size = 1.5)+
  annotate("rect", xmin = project_start_month, xmax = project_end_month, 
           ymin = 0, ymax = max(trends_by_date$totalDDDs_project_cohort), alpha = .2)+
  theme_classic()+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+
  scale_y_continuous(name = "Total Monthly DDDs for Project Cohort")+
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", name = " ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  facet_wrap(~class, nrow = 3)+
  guides(colour=guide_legend(nrow=1))+
  labs(title = paste("Fig 1.4b Total Monthly DDDs per Practice: Project Cohort,\n", 
                     format(min(trends_by_date$date),'%b %Y'), "-", format(max(trends_by_date$date),'%b %Y'), sep = " "), 
       subtitle = "The shaded area indicates the months when the project was live",
       caption = "Data Source: PIS")

#------------------------ Number of Patients ----------------------------#

# Overall Number of Patients with a Prescription - Not Broken Down per Practice
ggplot(trends_by_date[trends_by_date$practice_type == "Pilot",])+
  geom_col(aes(x = date, y = num_patients_project_cohort, fill = class), alpha = .5)+
  annotate("rect", xmin = project_start_month, xmax = project_end_month, 
           ymin = 0, ymax = max(trends_by_date$num_patients_project_cohort) * 2.5, alpha = .2)+
  theme_classic()+
  scale_y_continuous(name = "Total Monthly DDDs",
                     sec.axis = sec_axis(~., name = "Number of Patients\n with a Prescription"))+
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", name = " ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  facet_wrap(~class)+
  labs(title = paste("Fig 1.5a Number of Patients with a Prescription: Project Cohort,\n", 
                     format(min(trends_by_date$date),'%b %Y'), "-", format(max(trends_by_date$date),'%b %Y'), sep = " "), 
       subtitle = "The shaded area indicates the months when the project was live",
       caption = "Data Source: PIS")

# Broken Down per Practice
# Number of Unique Patients in the Project Cohort with a Pain Medication Prescription each month
ggplot(trends_by_date[trends_by_date$practice_type == "Pilot",])+
  geom_line(aes(x = date, y = num_patients_project_cohort, group = presc_location_name, colour = presc_location_name), size = 1, alpha = .8)+
  stat_summary(aes(x = date, y = num_patients_project_cohort, colour = "Mean"),fun = "mean", geom = "line", size = 1.5)+
  annotate("rect", xmin = project_start_month, xmax = project_end_month, 
           ymin = 0, ymax = 14, alpha = .2)+
  theme_classic()+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+
  scale_y_continuous(name = "Number of Distinct Patients with  Prescription", breaks=seq(0, 14, by = 2))+
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", name = " ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  facet_wrap(~class, nrow = 3)+
  guides(colour=guide_legend(nrow=1))+
  labs(title = paste("Fig 1.5b Number of Patients with a Prescription per Practice: Project Cohort,\n", 
                     format(min(trends_by_date$date),'%b %Y'), "-", format(max(trends_by_date$date),'%b %Y'), sep = " "), 
       subtitle = "The shaded area indicates the months when the project was live",
       caption = "Data Source: PIS")

#------------------------ Spend ----------------------------#

### Spend on Pain Medications - Project Cohort - Overall (Not Broken Down by Practice)
ggplot(trends_by_date[trends_by_date$practice_type == "Pilot",])+
  geom_col(aes(x = date, y = num_patients_project_cohort * 2, fill = class), alpha = .5)+
  stat_summary(aes(x = date, y = cost_project_cohort, colour = class),fun = "sum", geom = "line", size = 1.5)+
  annotate("rect", xmin = project_start_month, xmax = project_end_month, 
           ymin = 0, ymax = max(trends_by_date$cost_project_cohort * 1.3), alpha = .2)+
  theme_classic()+
  scale_y_continuous(name = "Total Spend", labels = scales::dollar_format(prefix = "£"),
                     sec.axis = sec_axis(~./2, name = "Number of Patients\n with a Prescription", breaks = seq(25,100,25)))+
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", name = " ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  facet_wrap(~class)+
  labs(title = paste("Fig 1.6 Overall Spend on Prescriptions: Project Cohort,\n", 
                     format(min(trends_by_date$date),'%b %Y'), "-", format(max(trends_by_date$date),'%b %Y'), sep = " "), 
       subtitle = "The shaded area indicates the months when the project was live",
       caption = "Data Source: PIS")

# Comparative and Pilot Practice Per Patient Total Spend on Pain Medications for All Patients - All Drugs
ggplot(trends_by_date_overall)+
  geom_line(aes(x = date, y = cost_per_patient_all, colour = presc_location_name), size = 1)+
  stat_summary(aes(x = date, y = cost_per_patient_all),fun = "mean", colour = "black", geom = "line", size = 1.2)+
  annotate("rect", xmin = project_start_month, xmax = project_end_month, 
           ymin = 0, ymax = 13, alpha = .2)+
  theme_classic()+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+
  scale_y_continuous(name = "Spend per Patient", breaks=seq(0, 14, by = 2), labels = scales::dollar_format(prefix = "£"))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", name = " ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  facet_wrap(~practice_type)+
  guides(colour=guide_legend(nrow=2))+
  labs(title = paste("Fig 1.7 Per Patient Spend on Pain Medications,\n", 
                     format(min(trends_by_date$date),'%b %Y'), "-", format(max(trends_by_date$date),'%b %Y'), sep = " "), 
       subtitle = "The black bold lines indicate means, with thin lines representing individual practices\nThe shaded area indicates the months when the project was live",
       caption = "Data Source: PIS")

# Spend Per Patient - Project Cohort
ggplot(trends_by_date_overall[trends_by_date_overall$practice_type == "Pilot",])+
  geom_line(aes(x = date, y = cost_per_patient_project_cohort, colour = presc_location_name), size = 1, alpha = .8)+
  stat_summary(aes(x = date, y = cost_per_patient_project_cohort, colour = "Project Cohort Mean Spend"),fun = "mean", geom = "line", size = 1.5)+
  annotate("rect", xmin = project_start_month, xmax = project_end_month, 
           ymin = 0, ymax = 20, alpha = .2)+
  theme_classic()+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+
  scale_y_continuous(name = "Spend per Patient", labels = scales::dollar_format(prefix = "£"))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", name = " ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  guides(colour=guide_legend(nrow=2))+
  labs(title = paste("Fig 1.8 Per Patient Spend on All Pain Medications by Practice: Project Cohort,\n", 
                     format(min(trends_by_date$date),'%b %Y'), "-", format(max(trends_by_date$date),'%b %Y'), sep = " "), 
       subtitle = "The black bold lines indicate means, with thin lines representing individual practices\nThe shaded area indicates the months when the project was live",
       caption = "Data Source: PIS")

# Per Patient Spend on Pain Medications for Project Cohort Patients - by Drug Class
ggplot(trends_by_date[trends_by_date$practice_type == "Pilot",])+
  stat_summary(aes(x = date, y = cost_per_patient_project_cohort, group = class, colour = class),fun = "mean", geom = "line", size = 1.2)+
  annotate("rect", xmin = project_start_month, xmax = project_end_month, 
           ymin = 0, ymax = 40, alpha = .2)+
  theme_classic()+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+
  scale_y_continuous(name = "Spend per Project Patient", labels = scales::dollar_format(prefix = "£"))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", name = " ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  guides(colour=guide_legend(nrow=1))+
  labs(title = paste("Fig 1.9 Per Patient Spend on Pain Medications by Drug Class: Project Cohort,\n", 
                     format(min(trends_by_date$date),'%b %Y'), "-", format(max(trends_by_date$date),'%b %Y'), sep = " "), 
       subtitle = "The shaded area indicates the months when the project was live",
       caption = "Data Source: PIS")

# Spend by Drug Class and Approved Name, Project Cohort
fig_spend_neuropathic <- ggplot(trends_by_date_approved_name_summary[trends_by_date_approved_name_summary$practice_type == "Pilot" & trends_by_date_approved_name_summary$class == "Neuropathic Agents",])+
  geom_col(aes(x = date, y = cost_project_cohort, fill = pi_approved_name), size = 1)+
  annotate("rect", xmin = project_start_month, xmax = project_end_month, 
           ymin = 0, ymax = max(trends_by_date_approved_name_summary$cost_project_cohort) * 1.5, alpha = .2)+
  theme_classic()+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+
  scale_y_continuous(name = "Spend on Prescriptions", labels = scales::dollar_format(prefix = "£"))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", name = " ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="right")+
  labs(title = "Neuropathic Agents",
       caption = "Data Source: PIS")

fig_spend_nsaids <- ggplot(trends_by_date_approved_name_summary[trends_by_date_approved_name_summary$practice_type == "Pilot" & trends_by_date_approved_name_summary$class == "NSAIDS",])+
  geom_col(aes(x = date, y = cost_project_cohort, fill = pi_approved_name), size = 1)+
  annotate("rect", xmin = project_start_month, xmax = project_end_month, 
           ymin = 0, ymax = max(trends_by_date_approved_name_summary$cost_project_cohort) / 3, alpha = .2)+
  theme_classic()+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+
  scale_y_continuous(name = "Spend on Prescriptions", labels = scales::dollar_format(prefix = "£"))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", name = " ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="right")+
  labs(title = "NSAIDS",
       caption = "Data Source: PIS")

fig_spend_weak_opioids <- ggplot(trends_by_date_approved_name_summary[trends_by_date_approved_name_summary$practice_type == "Pilot" & trends_by_date_approved_name_summary$class == "Weak Opioid",])+
  geom_col(aes(x = date, y = cost_project_cohort, fill = pi_approved_name), size = 1)+
  annotate("rect", xmin = project_start_month, xmax = project_end_month, 
           ymin = 0, ymax = max(trends_by_date_approved_name_summary$cost_project_cohort) / 3, alpha = .2)+
  theme_classic()+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+
  scale_y_continuous(name = "Spend on Prescriptions", labels = scales::dollar_format(prefix = "£"))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", name = " ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="right")+
  labs(title = "Weak Opioids",
       caption = "Data Source: PIS")

fig_spend_non_opioids <- ggplot(trends_by_date_approved_name_summary[trends_by_date_approved_name_summary$practice_type == "Pilot" & trends_by_date_approved_name_summary$class == "Non-opioid Analgesics",])+
  geom_col(aes(x = date, y = cost_project_cohort, fill = pi_approved_name), size = 1)+
  annotate("rect", xmin = project_start_month, xmax = project_end_month, 
           ymin = 0, ymax = max(trends_by_date_approved_name_summary$cost_project_cohort) / 3, alpha = .2)+
  theme_classic()+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+
  scale_y_continuous(name = "Spend on Prescriptions", labels = scales::dollar_format(prefix = "£"))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", name = " ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="right")+
  labs(title = "Non-Opioid Analgesics",
       caption = "Data Source: PIS")

fig_spend_strong_opioids <- ggplot(trends_by_date_approved_name_summary[trends_by_date_approved_name_summary$practice_type == "Pilot" & trends_by_date_approved_name_summary$class == "Strong Opioid",])+
  geom_col(aes(x = date, y = cost_project_cohort, fill = pi_approved_name), size = 1)+
  annotate("rect", xmin = project_start_month, xmax = project_end_month, 
           ymin = 0, ymax = max(trends_by_date_approved_name_summary$cost_project_cohort) * 1.1, alpha = .2)+
  theme_classic()+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+
  scale_y_continuous(name = "Spend on Prescriptions", labels = scales::dollar_format(prefix = "£"))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", name = " ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="right")+
  labs(title = "Strong Opioids",
       caption = "Data Source: PIS")

# Arrange the plots in a grid layout
gridExtra::grid.arrange(fig_spend_neuropathic, fig_spend_nsaids, fig_spend_weak_opioids, fig_spend_non_opioids, fig_spend_strong_opioids, ncol = 2)


#----------------------- Section 4 - Number of Patients / Duration ---------------------------#

# Visualise duration of prescribing for all repeat patients (not broken down by practice)
ggplot(duration[duration$interval_months != 0 & duration$practice_type == "Pilot",])+
  geom_col(aes(x = interval_months, y = patients, fill = class), show.legend = F)+
  theme_classic()+
  scale_y_continuous(name = "Number of Patients")+
  scale_x_continuous(name = "Number of Months Between First and Last Prescriptions")+
  facet_wrap(~class, nrow = 3)+
  labs(title = "Duration of Pain Medication Prescribing\n for All Patients Receiving 2 or More Prescriptions",
       caption = "Data Source: PIS")

# Bar plot - Length of time patients are on agreed medications - All Patients in Pilot Practices
ggplot(duration_by_category)+
  geom_col(aes(x = duration, y = patients, fill = class), position = "dodge")+
  theme_classic()+
  scale_y_continuous(name = "Number of Patients")+
  scale_x_discrete(name = "")+
  theme(legend.title = element_blank(), legend.position="none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  facet_wrap(~class)+
  labs(title = "Length of Time Patients are on Pain Medications: All Patients",
       caption = "Data Source: PIS")


#----------------------- Section 5 - Quantity per Prescription ---------------------------#

# Quantity per Prescription
ggplot(prescription_quantities[!is.na(prescription_quantities$DDDs_per_prescription)])+
  geom_violin(aes(x = date, y = DDDs_per_prescription, group = date, fill = class, colour = class), show.legend = F)+
  stat_summary(aes(x = date, y = DDDs_per_prescription, group = 1, colour = class), fun = mean, geom = "line", size = 1.2)+
  annotate("rect", xmin = project_start_month, xmax = project_end_month, 
           ymin = 0, ymax = max(prescription_quantities$DDDs_per_prescription[!is.na(prescription_quantities$DDDs_per_prescription)]), alpha = .2)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  scale_y_continuous(name = "DDDs per Prescription")+
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", name = " ")+
  facet_wrap(~class, nrow = 3)+
  labs(title = "Volume of Pain Medication in Each Prescription: Project Cohort",
       caption = "Data Source: PIS")

# Items by Pack Size
formattable::formattable(prescriptions_by_packsize_cohort)

# Items by Pack Size over time
ggplot(chronic[chronic$pat_upi_c%in% cohort_chis$pat_upi_c,])+
  stat_summary(aes(x = date, y = number_of_paid_items, group = packsize, colour = packsize), fun = "sum", geom = "line")+
  annotate("rect", xmin = project_start_month, xmax = project_end_month, 
           ymin = 0, ymax = 30, alpha = .2)+
  theme_classic()+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+
  scale_y_continuous(name = "Number of Prescription Items")+
  scale_colour_manual(values = c("slategray2", "slategray3", "red"))+
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", name = " ")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  facet_wrap(~class)+
  labs(title = paste("Number of Prescription Items by Pack Size: Project Cohort,\n", 
                     format(min(chronic$date),'%b %Y'), "-", format(max(chronic$date),'%b %Y'), sep = " "), 
       subtitle = "The shaded area indicates the months when the project was live",
       caption = "Data Source: PIS")


#----------------------- Section 6 - Sankey Diagram Visualising Pain Prescribing Flows ---------------------------#
sankeyNetwork(Links = sankey_plot_id_chronic, Nodes = nodes_chronic,
              Source = "IDIn", Target = "IDOut",
              Value = "Freq", NodeID = "name",
              nodeWidth = 20, nodePadding = 10, 
              fontSize = 12, fontFamily = "Arial", 
              height = 500, width = 750,
              sinksRight = T,
              NodeGroup = "col_group",
              colourScale = node_colour_chronic,
)

#=============================================
# Appendix
#=============================================
# List of Agreed Medications by Class, Approved Name and Item Description Included in the Analysis
appendix1 <- chronic %>%
  select(class, pi_approved_name, pi_bnf_item_description, DDDconversion_value, packsize_cutoff) %>% distinct() %>%
  arrange(class, pi_approved_name, pi_bnf_item_description) %>%
  mutate(DDDconversion_value = round(DDDconversion_value, 2)) %>%
  rename(`Drug Class` = class,
         `Drug Name` = pi_approved_name,
         `Item Description` = pi_bnf_item_description,
         `DDD Conversion Value` = DDDconversion_value,
         `Quantity in a Small Packsize` = packsize_cutoff)



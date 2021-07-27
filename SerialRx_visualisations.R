# This script loads, analyses and visualises data data in 3 practices with comparable demographics
# in the same locality but which have different rates of serial prescribing (only Practice1 issues SerialRx).
# Measures assessed included number of prescriptions, quantity of medication dispensed and cost
# for (a) the serialrx vs. comparative practices, (b) serialrx vs. other patients, and
# (c) drugs which can be prescribed on serialrx vs. other drugs.
# Time trends are looked at over a 4 year time frame (Jan 2017 to Jan 2021)
# to assess differences before and after switching to serial prescribing.

library(data.table)
library(dplyr)
library(dbplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(plotly)
library(magrittr)
library(stringr)
library(openxlsx)
library(scales)
library(tidyr)

#==============================================================#
# Load Data
#==============================================================#

# Load raw data for Practice 1 (Serial Rx practice) and 2 comparative practices (no serialrx) (Jan 2017-Jan 2021)
practice1 <- fread("N:\\SerialRx\\Data\\practice1_data.csv")
practice2 <- fread("N:\\SerialRx\\Data\\practice2_data.csv")
practice3 <- fread("N:\\SerialRx\\Data\\practice3_data.csv")

#Load practice populations
practiceLookup <- read_excel("N:\\03-Reference\\Lookup Information\\practice_information.xlsx") %>%
  select(PracticeCode, practice_population)

#==============================================================#
# Process Data
#==============================================================#

# Create new variable
eastlothian <- 
  # combines raw practice data into one dataframe
  bind_rows(practice1, practice2, practice3) %>% 
  # Rename the columns
  rename_with(snakecase::to_snake_case) %>%
  # Change the data class, add flag for Serial Prescriptions
  mutate(presc_location_code = as.character(presc_location_code),
         presc_location_name = tm::removeWords(presc_location_name, c(" MEDICAL", " PRACTICE")),
         paid_date = as.Date(paid_date, "%m/%d/%y"),
         paid_quantity = as.numeric(gsub(",","", paid_quantity)),
         pd_paid_gic_excl_bb = as.numeric(gsub(",","", pd_paid_gic_excl_bb)),
         # Classify each prescription as serial (barcode starts with a K) or nonserial (a catch-all for acutes and repeats)
         prescribingtype = if_else(str_detect(pd_form_barcode,"^K"), "serial", "nonserial")) %>%
  # Exclude dummy prescriptions
  filter(pi_approved_name != "DUMMY REJECTED") %>%
  left_join(practiceLookup,
            by = c("presc_location_code" = "PracticeCode"))

#==============================================================#
# Summarise Data - High Level
#==============================================================#

# Create objects for dates
current_month <- max(eastlothian$paid_date)
latest_date <- format.Date(current_month, "%B %Y")
earliest_month <- min(eastlothian$paid_date)
earliest_date <- format.Date(earliest_month, "%B %Y")
full_back12Mth_date <- format(as.Date(current_month) %m-% months(11), "%Y-%m-%d")
full_back12Mth <- format(as.Date(current_month) %m-% months(11), "%B %Y")
full_current_month <- format(as.Date(current_month), "%B %Y")
full_back3Mth_date <- format(as.Date(current_month) %m-% months(2), "%Y-%m-%d")
latest_full_quarter <- eastlothian %>% group_by(paid_calendar_year_quarter) %>%
  summarise(months_in_quarter = n_distinct(paid_date)) %>%
  filter(months_in_quarter == 3) %>%
  filter(paid_calendar_year_quarter == max(paid_calendar_year_quarter)) %>% select(paid_calendar_year_quarter)
latest_incomplete_quarter <- eastlothian %>% group_by(paid_calendar_year_quarter) %>%
  summarise(months_in_quarter = n_distinct(paid_date)) %>%
  filter(months_in_quarter < 3) %>% select(paid_calendar_year_quarter)

#---------- Create objects for flagging serialrx patients and drugs ----#

# List of chi numbers of all patients who have ever had a serial prescription
serialrx_chis <- eastlothian %>% filter(prescribingtype == "serial") %>%
  group_by(pat_upi_c) %>%
  # Start date of first Serial Prescription
  summarise(serialrx_start = min(paid_date),
            serialrx_latest = max(paid_date)) %>% 
  distinct() %>%
  # Categorise patients as new, returning or previous SerialRx patients
  mutate(serialrx_flag = case_when(
    serialrx_start > as.Date(current_month) %m-% months(12) ~ "SerialRx New Patient",
    serialrx_start <= as.Date(current_month) %m-% months(12) &
      serialrx_latest > as.Date(current_month) %m-% months(12) ~ "SerialRx Return Patient",
    serialrx_start <= as.Date(current_month) %m-% months(12) &
      serialrx_latest <= as.Date(current_month) %m-% months(12) ~ "SerialRx Previous Patient"
  ))

# Create table of frequencies of SerialRx patients
serialrx_patient_freq <- as.data.frame(table(serialrx_chis$serialrx_flag))

# List of drugs for which serial prescriptions have been issued
serialrx_drugs <- eastlothian %>% filter(prescribingtype == "serial") %>%
  select(pi_bnf_section_description, pi_approved_name) %>% distinct() %>% arrange(pi_bnf_section_description, pi_approved_name)

# Create table of practice level information for last 12 months
practice_table <- eastlothian %>% filter(paid_date >= full_back12Mth_date) %>%
  group_by(presc_location_code, presc_location_name, practice_population) %>%
  summarise(`Number of SerialRx Prescriptions` = n_distinct(pd_form_barcode[prescribingtype == "serial"]),
            `Total Number of Prescriptions` = n_distinct(pd_form_barcode)) %>%
  rename(`Practice Code` = presc_location_code,
         `Practice Name` = presc_location_name,
         `List Size` = practice_population)

#---------- Create summaries of data -----------------------------------#

# Create summary of number of items and cost per patient per quarter
eastlothian_patient_level <- eastlothian %>% filter(paid_date < "2021-01-01", !is.na(pat_upi_c)) %>%
  group_by(presc_location_name, paid_calendar_year_quarter, pat_upi_c, prescribingtype) %>%
  summarise(items = sum(number_of_paid_items),
            cost = sum(pd_paid_gic_excl_bb))

# Summarise Data by Quarter
allpatients_by_quarter <- eastlothian %>%
  filter(!paid_calendar_year_quarter %in% latest_incomplete_quarter) %>%
  mutate(# Add flag for patient type, if the patient has ever been issued a serial prescription
         patient_type = ifelse(pat_upi_c %in% serialrx_chis$pat_upi_c, "Serial Patient", "Not Serial Patient")) %>%
  group_by(paid_calendar_year_quarter, patient_type, prescribingtype) %>%
  summarise(items = sum(number_of_paid_items),
            cost = sum(pd_paid_gic_excl_bb),
            quantity = sum(paid_quantity),
            patients = n_distinct(pat_upi_c)) %>%
  mutate(items_per_patient = items / patients,
         cost_per_patient = cost / patients,
         cost_per_item = items / cost,
         quantity_per_item = quantity / items)

total_by_quarter <- eastlothian %>%
  filter(!paid_calendar_year_quarter %in% latest_incomplete_quarter) %>%
  group_by(paid_calendar_year_quarter, presc_location_name) %>%
  summarise(items = sum(number_of_paid_items),
            cost = sum(pd_paid_gic_excl_bb),
            quantity = sum(paid_quantity),
            patients = n_distinct(pat_upi_c)) %>%
  mutate(items_per_patient = items / patients,
         cost_per_patient = cost / patients,
         cost_per_item = items / cost,
         quantity_per_item = quantity / items)
  
serialrx_patients_by_quarter <- eastlothian %>%
  filter(pat_upi_c %in% serialrx_chis$pat_upi_c, !paid_calendar_year_quarter %in% latest_incomplete_quarter) %>%
  group_by(paid_calendar_year_quarter) %>%
  summarise(items = sum(number_of_paid_items),
            cost = sum(pd_paid_gic_excl_bb),
            quantity = sum(paid_quantity),
            patients = n_distinct(pat_upi_c))

serialrx_patients_first_prescription <- eastlothian %>%
  filter(pat_upi_c %in% serialrx_chis$pat_upi_c) %>%
  group_by(pat_upi_c) %>%
  summarise(date_of_first_prescription = min(paid_date)) %>%
  ungroup() %>%
  group_by(date_of_first_prescription) %>%
  summarise(patients = n_distinct(pat_upi_c))

#-- Summarise Data by Year for Cohort of Patients on SerialRx and Compare to All Patients before and after switch--#

allpatients <- eastlothian %>% filter(paid_date < "2021-01-01") %>%
  mutate(year = year(paid_date),
         # Add flag for patient type, if the patient has ever been issued a serial prescription
         patient_type = ifelse(pat_upi_c %in% serialrx_chis$pat_upi_c, "Serial Patient", "Not Serial Patient"),
         # Add flag to identify drugs for which serial prescriptions have been issued
         serial_drugs = ifelse(pi_approved_name %in% serialrx_drugs$pi_approved_name, "Serial Drug", "Not Serial Drug")) %>%
  group_by(year, patient_type, serial_drugs, prescribingtype) %>%
  summarise(items = sum(number_of_paid_items),
            cost = sum(pd_paid_gic_excl_bb),
            quantity = sum(paid_quantity),
            patients = n_distinct(pat_upi_c)) %>%
  mutate(items_per_patient = items / patients,
         cost_per_patient = cost / patients,
         cost_per_item = items / cost,
         quantity_per_item = quantity / items)

#---- Summarise the quarterly cost, items and quantity per serialrx patient by before and after the switch to serialrx----#

eastlothian_quarterly_summary <- eastlothian %>% filter(pat_upi_c %in% serialrx_chis$pat_upi_c, !paid_calendar_year_quarter %in% latest_incomplete_quarter) %>%
  left_join(serialrx_chis, by = "pat_upi_c") %>%
  # Calculate the months and quarters before and after switching to serial prescriptions
  mutate(
    # Add flag to identify drugs for which serial prescriptions have been issued
    serial_drugs = ifelse(pi_approved_name %in% serialrx_drugs$pi_approved_name, "Serial Drug", "Not Serial Drug"),
    months_from_serialrx_start = interval(serialrx_start, paid_date) %/% months(1),
    quarters_from_serialrx_start = interval(serialrx_start, paid_date) %/% months(3)) %>%
  # Group the data by the number of quarters before the switch to serialrx
  group_by(quarters_from_serialrx_start, serial_drugs) %>%
  summarise(items = sum(number_of_paid_items),
            cost = sum(pd_paid_gic_excl_bb),
            patients = n_distinct(pat_upi_c),
            quantity = sum(paid_quantity)) %>%
  mutate(items_per_patient = items / patients,
         cost_per_patient = cost / patients,
         quantity_per_patient = quantity / patients,
         quantity_per_item = quantity / items)

eastlothian_serialrx_cohort_prescriptions <- eastlothian %>%
  filter(pat_upi_c %in% serialrx_chis$pat_upi_c,
         pi_approved_name %in% serialrx_drugs$pi_approved_name,
         paid_date < "2021-01-01") %>%
  group_by(paid_calendar_year_quarter, prescribingtype) %>%
  summarise(items = sum(number_of_paid_items))

eastlothian_signing_workload <- eastlothian_serialrx_cohort_prescriptions %>%
  group_by(paid_calendar_year_quarter) %>%
  mutate(signing_workload = items[prescribingtype == "nonserial"] + (items[prescribingtype == "serial"]/6)) %>% ungroup %>%
  select(paid_calendar_year_quarter, signing_workload) %>% distinct() 

serialrx_switch <- eastlothian_quarterly_summary %>% filter(serial_drugs == "Serial Drug", quarters_from_serialrx_start >= -12 & quarters_from_serialrx_start <= 12)

#==============================================================#
# Visualise Data for All Patients in the 3 practices
#==============================================================#

fig_labels = c("2017 Q1" = "2017   Q1", "2017 Q2" = "Q2", "2017 Q3" = "Q3", "2017 Q4" = "Q4",  
               "2018 Q1" = "2018   Q1", "2018 Q2" = "Q2", "2018 Q3" = "Q3", "2018 Q4" = "Q4",
               "2019 Q1" = "2019   Q1", "2019 Q2" = "Q2", "2019 Q3" = "Q3", "2019 Q4" = "Q4",
               "2020 Q1" = "2020   Q1", "2020 Q2" = "Q2", "2020 Q3" = "Q3", "2020 Q4" = "Q4",
               "2021 Q1" = "2021   Q1", "2021 Q2" = "Q2", "2021 Q3" = "Q3", "2021 Q4" = "Q4")

fig_labels_years = c("2017 Q1" = "2017", "2017 Q2" = "", "2017 Q3" = "", "2017 Q4" = "",  
               "2018 Q1" = "2018", "2018 Q2" = "", "2018 Q3" = "", "2018 Q4" = "",
               "2019 Q1" = "2019", "2019 Q2" = "", "2019 Q3" = "", "2019 Q4" = "",
               "2020 Q1" = "2020", "2020 Q2" = "", "2020 Q3" = "", "2020 Q4" = "",
               "2021 Q1" = "2021", "2021 Q2" = "", "2021 Q3" = "", "2021 Q4" = "")

#==============================================================#
# 4 key messages
#==============================================================#

# Practice3 and Practice1 are comparable practices (# of patients over 65, population growth) but in Practice3 number of items is increasing and Practice1 is not
ggplot(eastlothian_patient_level)+
  stat_summary(aes(x = paid_calendar_year_quarter, y = items, group = presc_location_name, colour = presc_location_name), fun = "mean", geom = "line", size = 1.2)+
  scale_y_continuous(name = "Number of Paid Items per Patient per Quarter")+
  scale_x_discrete(name = "", labels = fig_labels_years)+
  scale_colour_manual(values = c("slategray2", "red", "slategray3"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  #facet_wrap(~presc_location_name)+
  labs(title = paste("Number of Prescription Items per Patient per Quarter,", 
                     format(min(eastlothian$paid_date),'%b %Y'), "-", format(max(eastlothian$paid_date),'%b %Y'), sep = " "),
       subtitle = "The number of prescriptions per patient has decreased in The Orchard since starting serial prescribing",
       caption = "Data Source: PIS")

# Anonymised version of previous figure
ggplot(eastlothian_patient_level)+
  stat_summary(aes(x = paid_calendar_year_quarter, y = items, group = presc_location_name, colour = presc_location_name), fun = "mean", geom = "line", size = 1.2)+
  scale_y_continuous(name = "Number of Paid Items per Patient per Quarter")+
  scale_x_discrete(name = "", labels = fig_labels_years)+
  #scale_colour_discrete(labels = c("a", "b", "c"))+
  scale_colour_manual(labels = c("Comparison Practice 1", "SerialRx Practice", "Comparison Practice 2"), values = c("slategray2", "red", "slategray3"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  #facet_wrap(~presc_location_name)+
  labs(title = paste("Number of Prescription Items per Patient per Quarter,", 
                     format(min(eastlothian$paid_date),'%b %Y'), "-", format(max(eastlothian$paid_date),'%b %Y'), sep = " "),
       subtitle = "The number of prescriptions per patient has decreased in the SerialRx practice since starting serial prescribing",
       caption = "Data Source: PIS")

# GP time saved for SerialRx (from Xk to Xk prescriptions per quarter)
# Overall Number of Items for SerialRx Cohort
ggplot(eastlothian[eastlothian$pat_upi_c %in% serialrx_chis$pat_upi_c & pi_approved_name %in% serialrx_drugs$pi_approved_name & paid_date < "2021-01-01",])+
  stat_summary(aes(x = paid_calendar_year_quarter, y = number_of_paid_items, group = prescribingtype, colour = prescribingtype), fun = "sum", geom = "line", size = 1.2)+
  annotate(geom = "text", x = "2020 Q2", y = 2500, label = "Difference in the number of\n prescriptions a GP has to sign")+
  scale_y_continuous(name = "Number of Prescriptions", labels = scales::comma)+
  scale_x_discrete(name = "", labels = fig_labels)+
  scale_colour_manual(values = c("navyblue", "dodgerblue"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  labs(title = paste("Decrease in the Number of Prescription Items a GP has to sign,\n", 
                     format(min(eastlothian$paid_date),'%b %Y'), "-", format(max(eastlothian$paid_date),'%b %Y'), sep = " "), 
       subtitle = "Number of Serial and Non-Serial Prescriptions per Quarter: SerialRx Cohort",
       caption = "Data Source: PIS")

# With line for signing workload
ggplot(eastlothian_serialrx_cohort_prescriptions)+
  stat_summary(aes(x = paid_calendar_year_quarter, y = items, group = prescribingtype, colour = prescribingtype), fun = "sum", geom = "line", size = 1)+
  stat_summary(data = eastlothian_signing_workload, aes(x = paid_calendar_year_quarter, y = signing_workload, group = 1), colour = "red", linetype = "dashed", fun = "sum", geom = "line", size = 1.5)+
  annotate(geom = "text", x = "2020 Q3", y = 2100, label = "Estimated prescription\n signing workload for GPs", colour = "red")+
  scale_y_continuous(name = "Number of Prescriptions", labels = scales::comma)+
  scale_x_discrete(name = "", labels = fig_labels)+
  scale_colour_manual(labels = c("Acute and Repeat Prescription Items", "SerialRx Items"), values = c("navyblue", "dodgerblue"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  labs(title = paste("Number of Serial and Non-Serial Prescriptions per Quarter: SerialRx Cohort,\n", 
                     format(min(eastlothian$paid_date),'%b %Y'), "-", format(max(eastlothian$paid_date),'%b %Y'), sep = " "), 
       subtitle = "Switching to SerialRx results in a decrease in the number of prescription Items a GP has to sign",
       caption = "Data Source: PIS")

# Starting SerialRx and cost per patient decreases
# Visualisation of Cost per Patient before and after switch

ggplot(serialrx_switch)+
  geom_line(aes(x = quarters_from_serialrx_start, y = cost_per_patient, colour = "Actual"), size = 1, alpha = .5)+
  annotate(geom = "text", x = -2, y = 85, label = "More items are prescribed\n during the switch")+
  geom_smooth(aes(x = quarters_from_serialrx_start, y = cost_per_patient, colour = "Smoothed"), size = 1.5, se = F)+
  annotate(geom = "text", x = -10, y = 64, label = "£62 Before Switch to SerialRx")+
  geom_hline(yintercept = serialrx_switch$cost_per_patient[serialrx_switch$quarters_from_serialrx_start == -12], linetype = "dashed", alpha = .5)+
  annotate(geom = "text", x = 10, y = 44, label = "£41 After Switch to SerialRx")+
  geom_hline(yintercept = serialrx_switch$cost_per_patient[serialrx_switch$quarters_from_serialrx_start == 12], linetype = "dashed", alpha = .5)+
  theme_classic()+
  scale_colour_manual(labels = c("Actual Cost", "Moving Average"), values = c("dodgerblue", "red"))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  scale_y_continuous(name = "Cost per Patient", labels = scales::dollar_format(prefix = "£"), breaks = seq(0,90, 20))+
  scale_x_continuous(name = "Quarters Before (-) and After (+) Switch to SerialRx", breaks = seq(-12, 12, 6))+
  labs(title = paste("Quarterly Cost per Patient for SerialRx Drugs Decreases After Switch to SerialRx,\n", 
                     format(min(eastlothian$paid_date),'%b %Y'), "-", format(max(eastlothian$paid_date),'%b %Y'), sep = " "), 
       caption = "Data Source: PIS")

# SerialRx avoids panic spike because of Covid
ggplot(allpatients[allpatients$serial_drugs == "Serial Drug",])+
  stat_summary(aes(x = year, y = cost_per_patient, group = patient_type, colour = patient_type), fun = "mean", geom = "line", size = 1.2)+
  #stat_summary(aes(x = year, y = items_per_patient * 5, group = patient_type, colour = patient_type, linetype = patient_type), fun = "mean", geom = "line", size = 1.2)+
  #stat_summary(aes(x = year, y = quantity / patients * .1, group = patient_type, colour = "Quantity"), fun = "mean", geom = "line", size = 1.2)+
  annotate(geom = "text", x = 2020, y = 160, label = "Covid-19\n Lockdown")+
  scale_y_continuous(name = "Cost per Patient", labels = scales::dollar_format(prefix = "£"))+
  scale_colour_manual(labels = c("Patients on Acute or Repeat Prescriptions", "SerialRx Patients"), values = c("navyblue", "dodgerblue"))+
  theme_classic()+
  coord_cartesian(ylim = c(75,160))+
  scale_x_continuous(name = "")+
  #facet_wrap(~patient_type)+
  theme(legend.title = element_blank(), legend.position="bottom")+
  labs(title = "Annual Cost per Patient with a Prescription, SerialRx Drugs", 
       subtitle = "The annual cost per patient is lower for patients with SerialRx, even for the same drugs.\n SerialRx also prevents a variation in overordering (there is no panic spike in 2020)",
       caption = "Data Source: PIS")


#==============================================================#
# Other Visualisations
#==============================================================#

#---------------------Number of Items -------------------#

# Overall Number of Paid Items over time, total
ggplot(eastlothian[!eastlothian$paid_calendar_year_quarter %in% latest_incomplete_quarter,])+
  stat_summary(aes(x = paid_calendar_year_quarter, y = number_of_paid_items, group = presc_location_name, colour = presc_location_name), fun = "sum", geom = "line", size = 1.2)+
  #scale_y_continuous(name = "Overall Number of Paid Items", labels = scales::comma, limits = c(0, 21000))+
  scale_x_discrete(name = "", labels = fig_labels_years)+
  scale_colour_manual(labels = c("Comparison Practice 1", "SerialRx Practice", "Comparison Practice 2"), values = c("slategray2", "red", "slategray3"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  labs(title = paste("Overall Number of Paid Items,", 
                     format(min(eastlothian$paid_date),'%b %Y'), "-", format(max(eastlothian$paid_date),'%b %Y'), sep = " "), 
       caption = "Data Source: PIS")

# Overall Cost per quarter
ggplot(eastlothian[!eastlothian$paid_calendar_year_quarter %in% latest_incomplete_quarter,])+
  stat_summary(aes(x = paid_calendar_year_quarter, y = pd_paid_gic_excl_bb, group = presc_location_name, colour = presc_location_name), fun = "sum", geom = "line", size = 1.2)+
  scale_y_continuous(name = "Overall Cost", labels = scales::comma)+
  scale_x_discrete(name = "", labels = fig_labels_years)+
  scale_colour_manual(labels = c("Comparison Practice 1", "SerialRx Practice", "Comparison Practice 2"), values = c("slategray2", "red", "slategray3"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  labs(title = paste("Overall Cost,", 
                     format(min(eastlothian$paid_date),'%b %Y'), "-", format(max(eastlothian$paid_date),'%b %Y'), sep = " "), 
       caption = "Data Source: PIS")

# Overall Number of Paid Items over time, total and broken down by serial vs nonserial
ggplot(eastlothian[!eastlothian$paid_calendar_year_quarter %in% latest_incomplete_quarter,])+
  geom_col(data = total_by_quarter, aes(x = paid_calendar_year_quarter, y = patients * 1), alpha = .1)+
  stat_summary(aes(x = paid_calendar_year_quarter, y = number_of_paid_items, group = prescribingtype, colour = prescribingtype), fun = "sum", geom = "line", size = 1.2)+
  stat_summary(aes(x = paid_calendar_year_quarter, y = number_of_paid_items, group = 1, colour = "total prescriptions"), fun = "sum", geom = "line", size = 1.2)+
  scale_y_continuous(name = "Overall Number of Paid Items", labels = scales::comma,
                     sec.axis = sec_axis(~. / 1, name = "Number of Patients with a Prescription", breaks = seq(1000,4000, 1000)))+
  scale_x_discrete(name = "", labels = fig_labels_years)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  facet_wrap(~presc_location_name)+
  labs(title = paste("Overall Number of Paid Items,", 
                     format(min(eastlothian$paid_date),'%b %Y'), "-", format(max(eastlothian$paid_date),'%b %Y'), sep = " "), 
       subtitle = "Grey Bars Indicate the Number of Patients with a Prescription\n Lines Indicate Number of Items (Total and by Type of Prescription)",
       caption = "Data Source: PIS")

# Number of Paid Serial vs Nonserial Items per Patient
ggplot(eastlothian_patient_level)+
  stat_summary(aes(x = paid_calendar_year_quarter, y = items, group = prescribingtype, colour = prescribingtype), fun = "mean", geom = "line", size = 1.2)+
  scale_y_continuous(name = "Number of Paid Items per Patient per Quarter")+
  scale_x_discrete(name = "", labels = fig_labels_years)+
  scale_colour_discrete(name = "Type of Prescription")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  facet_wrap(~presc_location_name)+
  labs(title = paste("Number of Paid Items per Patient with a Prescription,\n", 
                     format(min(eastlothian$paid_date),'%b %Y'), "-", format(max(eastlothian$paid_date),'%b %Y'), sep = " "), 
       caption = "Data Source: PIS")

#--------------------- Quantity -------------------#

# Overall Paid Quantity over time, total and broken down by serial vs nonserial
ggplot(eastlothian[!eastlothian$paid_calendar_year_quarter %in% latest_incomplete_quarter,])+
  geom_col(data = total_by_quarter, aes(x = paid_calendar_year_quarter, y = patients * 100), alpha = .1)+
  stat_summary(aes(x = paid_calendar_year_quarter, y = paid_quantity, group = prescribingtype, colour = prescribingtype), fun = "sum", geom = "line", size = 1.2)+
  stat_summary(aes(x = paid_calendar_year_quarter, y = paid_quantity, group = 1, colour = "total"), fun = "sum", geom = "line", size = 1.2)+
  scale_y_continuous(name = "Paid Quantity", labels = scales::comma,
                     sec.axis = sec_axis(~. / 100, name = "Number of Patients with a Prescription", breaks = seq(1000,4000, 1000)))+
  scale_x_discrete(name = "", labels = fig_labels_years)+
  scale_colour_discrete(name = "Type of Prescription")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  facet_wrap(~presc_location_name)+
  labs(title = paste("Overall Paid Quantity,", 
                     format(min(eastlothian$paid_date),'%b %Y'), "-", format(max(eastlothian$paid_date),'%b %Y'), sep = " "), 
       subtitle = "Grey Bars Indicate the Number of Patients with a Prescription\n Lines Indicate Quantity (Total and by Type of Prescription)",
       caption = "Data Source: PIS")

#--------------------- Cost -------------------#

# Overall cost, with number of patients with a prescription on secondary axis
ggplot(eastlothian[!eastlothian$paid_calendar_year_quarter %in% latest_incomplete_quarter,])+
  geom_col(data = total_by_quarter, aes(x = paid_calendar_year_quarter, y = patients * 100), alpha = .1)+
  stat_summary(aes(x = paid_calendar_year_quarter, y = pd_paid_gic_excl_bb, group = prescribingtype, colour = prescribingtype), fun = "sum", geom = "line", size = 1.2)+
  stat_summary(aes(x = paid_calendar_year_quarter, y = pd_paid_gic_excl_bb, group = 1, colour = "total"), fun = "sum", geom = "line", size = 1.2)+
  scale_y_continuous(name = "Cost", labels = scales::dollar_format(prefix = "£"),
                     sec.axis = sec_axis(~. / 100, name = "Number of Patients with a Prescription", breaks = seq(1000,4000, 1000)))+
  scale_x_discrete(name = "", labels = fig_labels_years)+
  scale_colour_discrete(name = "Type of Prescription")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  facet_wrap(~presc_location_name)+
  labs(title = paste("Overall Cost and Number of Rx Patients,", 
                     format(min(eastlothian$paid_date),'%b %Y'), "-", format(max(eastlothian$paid_date),'%b %Y'), sep = " "), 
       subtitle = "Grey Bars Indicate the Number of Patients with a Prescription\n Lines Indicate Cost (Total and by Type of Prescription)",
       caption = "Data Source: PIS")

# Bar chart of number of patients with a prescription per quarter
ggplot(total_by_quarter)+
  geom_col(data = total_by_quarter, aes(x = paid_calendar_year_quarter, y = patients))+
  facet_wrap(~presc_location_name)

#==============================================================#
# Visualise Data for Cohort of Patients on SerialRx
#==============================================================#

# Bar chart showing the number of patients with a SerialRx per quarter
ggplot(allpatients_by_quarter[allpatients_by_quarter$patient_type == "Serial Patient",])+
  geom_col(aes(x = paid_calendar_year_quarter, y = patients, group = prescribingtype, fill = prescribingtype), position = "dodge")+
  geom_line(data = serialrx_patients_by_quarter, aes(x = paid_calendar_year_quarter, y = patients, group = 1, colour = "Total"), size = 1.2)+
  scale_colour_manual(values = c("Total" = "black"))+
  scale_y_continuous(name = "Number of Patients")+
  scale_x_discrete(name = "", labels = fig_labels)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  labs(title = paste("SerialRx Cohort: Number of Patients with a Prescription,\n", 
                     format(min(eastlothian$paid_date),'%b %Y'), "-", format(max(eastlothian$paid_date),'%b %Y'), sep = " "), 
       subtitle = "By Type of Prescription",
       caption = "Data Source: PIS")

# New Patients at the Practice
## Bar chart showing the first date of prescription
ggplot(serialrx_patients_first_prescription)+
  geom_col(aes(x = date_of_first_prescription, y = patients))+
  scale_y_continuous(name = "Number of Patients")+
  scale_x_date(name = "Month of First Prescription at the Practice")+
  theme_classic()+
  labs(title = "SerialRx Cohort: Number of New Patients at the Practice", 
       subtitle = paste(sum(serialrx_patients_first_prescription$patients[serialrx_patients_first_prescription$date_of_first_prescription >= "2017-07-31"]), 
                        "out of", sum(serialrx_patients_first_prescription$patients), 
                        "patients in the SerialRx Cohort received their first prescription\n in the practice after July 2017"),
       caption = "Data Source: PIS")

# Overall Number of Items for SerialRx Cohort
ggplot(eastlothian[eastlothian$pat_upi_c %in% serialrx_chis$pat_upi_c & pi_approved_name %in% serialrx_drugs$pi_approved_name & paid_date < "2021-01-01",])+
  stat_summary(aes(x = paid_calendar_year_quarter, y = number_of_paid_items, group = prescribingtype, colour = prescribingtype), fun = "sum", geom = "line", size = 1.2)+
  stat_summary(aes(x = paid_calendar_year_quarter, y = number_of_paid_items, group = 1, colour = "total"), fun = "sum", geom = "line", size = 1.2)+
  scale_y_continuous(name = "Number of Prescriptions", labels = scales::comma)+
  scale_x_discrete(name = "", labels = fig_labels)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  labs(title = paste("Overall Number of Paid Items for SerialRx Cohort,\n", 
                     format(min(eastlothian$paid_date),'%b %Y'), "-", format(max(eastlothian$paid_date),'%b %Y'), sep = " "), 
       subtitle = "Total and by Type of Prescription",
       caption = "Data Source: PIS")

# Fig with secondary axis showing the number of patients with a SerialRx
ggplot(eastlothian[eastlothian$pat_upi_c %in% serialrx_chis$pat_upi_c & pi_approved_name %in% serialrx_drugs$pi_approved_name & paid_date < "2021-01-01",])+
  geom_col(data = allpatients_by_quarter[allpatients_by_quarter$patient_type == "Serial Patient",],
           aes(x = paid_calendar_year_quarter, y = patients, group = prescribingtype, fill = prescribingtype), position = "dodge", alpha = .3)+
  stat_summary(aes(x = paid_calendar_year_quarter, y = number_of_paid_items, group = prescribingtype, colour = prescribingtype), fun = "sum", geom = "line", size = 1.2)+
  stat_summary(aes(x = paid_calendar_year_quarter, y = number_of_paid_items, group = 1, colour = "total"), fun = "sum", geom = "line", size = 1.2)+
  geom_line(data = serialrx_patients_by_quarter, aes(x = paid_calendar_year_quarter, y = patients, group = 1, colour = "Total_Patients"), alpha = .3, size = 1.2)+
  #geom_col(data = allpatients_by_quarter[allpatients_by_quarter$patient_type == "Serial Patient" & allpatients_by_quarter$prescribingtype == "serial",],
  #         aes(x = paid_calendar_year_quarter, y = patients), position = "dodge")+
  scale_y_continuous(name = "Number of Prescriptions", labels = scales::comma,
                     sec.axis = sec_axis(~. , name = "Number of Patients with a Prescription\n by Type of Prescription", breaks = seq(0,1000, 500)))+
  scale_x_discrete(name = "", labels = fig_labels)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  labs(title = paste("Overall Number of Paid Items for SerialRx Cohort,\n", 
                     format(min(eastlothian$paid_date),'%b %Y'), "-", format(max(eastlothian$paid_date),'%b %Y'), sep = " "), 
       subtitle = "Total and by Type of Prescription",
       caption = "Data Source: PIS")


# Overall Paid Quantity for SerialRx Cohort
##NB: includes both serial and nonserial prescriptions for everyone who has received at least 1 serialrx
ggplot(eastlothian[eastlothian$pat_upi_c %in% serialrx_chis$pat_upi_c & pi_approved_name %in% serialrx_drugs$pi_approved_name & paid_date < "2021-01-01",])+
  geom_col(data = allpatients_by_quarter[allpatients_by_quarter$patient_type == "Serial Patient",],
           aes(x = paid_calendar_year_quarter, y = patients * 100, group = prescribingtype, fill = prescribingtype), position = "dodge", alpha = .3)+
  stat_summary(aes(x = paid_calendar_year_quarter, y = paid_quantity, group = prescribingtype, colour = prescribingtype), fun = "sum", geom = "line", size = 1.2)+
  stat_summary(aes(x = paid_calendar_year_quarter, y = paid_quantity, group = 1, colour = "total"), fun = "sum", geom = "line", size = 1.2)+
  geom_line(data = serialrx_patients_by_quarter, aes(x = paid_calendar_year_quarter, y = patients * 100, group = 1, colour = "Total_Patients"), alpha = .3, size = 1.2)+
  scale_y_continuous(name = "Paid Quantity", labels = scales::comma,
                     sec.axis = sec_axis(~. / 100, name = "Number of Patients with a Prescription\n by Type of Prescription", breaks = seq(0,1000, 500)))+
  scale_x_discrete(name = "")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  labs(title = paste("Overall Paid Quantity for SerialRx Cohort,\n", 
                     format(min(eastlothian$paid_date),'%b %Y'), "-", format(max(eastlothian$paid_date),'%b %Y'), sep = " "), 
       subtitle = "Total and by Type of Prescription",
       caption = "Data Source: PIS")

# Overall Cost for SerialRx Cohort for Drugs for which serial prescriptions have been issued
ggplot(eastlothian[eastlothian$pat_upi_c %in% serialrx_chis$pat_upi_c & pi_approved_name %in% serialrx_drugs$pi_approved_name & paid_date < "2021-01-01",])+
  geom_col(data = allpatients_by_quarter[allpatients_by_quarter$patient_type == "Serial Patient",],
           aes(x = paid_calendar_year_quarter, y = patients * 10, group = prescribingtype, fill = prescribingtype), position = "dodge", alpha = .3)+
  stat_summary(aes(x = paid_calendar_year_quarter, y = pd_paid_gic_excl_bb, group = prescribingtype, colour = prescribingtype), fun = "sum", geom = "line", size = 1.2)+
  stat_summary(aes(x = paid_calendar_year_quarter, y = pd_paid_gic_excl_bb, group = 1, colour = "total"), fun = "sum", geom = "line", size = 1.2)+
  geom_line(data = serialrx_patients_by_quarter, aes(x = paid_calendar_year_quarter, y = patients * 10, group = 1, colour = "Total_Patients"), alpha = .3, size = 1.2)+
  scale_y_continuous(name = "Total Cost", labels = scales::dollar_format(scale = 1/1000, prefix = "£", suffix = "K"),
                     sec.axis = sec_axis(~. / 10, name = "Number of Patients with a Prescription\n by Type of Prescription", breaks = seq(0,1000, 500)))+
  scale_x_discrete(name = "", labels = fig_labels)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  labs(title = paste("Overall Cost for SerialRx Cohort,", 
                     format(min(eastlothian$paid_date),'%b %Y'), "-", format(max(eastlothian$paid_date),'%b %Y'), sep = " "), 
       subtitle = "Drugs for which > 1 SerialRx prescription has been issued",
       caption = "Data Source: PIS")

# Cost per Patient on Serial vs Non Serial Drugs, broken down by Serial vs. Not Serial Patients, and SerialRx Items only

plot1 <- ggplot(allpatients[allpatients$serial_drugs == "Not Serial Drug",])+
  stat_summary(aes(x = year, y = cost_per_patient, group = patient_type, colour = patient_type), fun = "mean", geom = "line", size = 1.2)+
  theme_classic()+
  scale_y_continuous(name = "Cost per Patient", labels = scales::dollar_format(prefix = "£"), limits = c(0, 250), breaks = seq(0,200,50))+
  scale_x_continuous(name = "")+
  theme(legend.title = element_blank(), legend.position = "bottom")+
  labs(title = "Cost per Patient by Type of Drug", 
       subtitle = "Not Serial Drugs",
       caption = "")

plot2 <- ggplot(allpatients[allpatients$serial_drugs == "Serial Drug",])+
  geom_col(data = allpatients[allpatients$serial_drugs == "Serial Drug" & allpatients$patient_type == "Serial Patient",], 
           aes(x = year, y = items, group = prescribingtype, fill = prescribingtype), position = "fill", alpha = .3)+
  stat_summary(aes(x = year, y = cost_per_patient / 100, group = patient_type, colour = patient_type), 
               fun = "mean", geom = "line", size = 1.2, show.legend = F)+
  theme_classic()+
  scale_y_continuous(name = "Cost per Patient", labels = scales::dollar_format(prefix = "£", scale = 100), breaks = seq(0, 2, .5),
                     sec.axis = sec_axis(~. / 1, name = "% of Serial Drugs Prescribed to the SerialRx Cohort\n That Are SerialRx", 
                                         labels = scales::percent_format(scale = 100), breaks =seq(0, 1, .5)))+
  scale_x_continuous(name = "")+
  coord_cartesian(ylim = c(0,2.5))+
  theme(legend.title = element_blank(), legend.position="bottom")+
  labs(title = "", 
       subtitle = "Serial Drugs",
       caption = "Data Source: PIS")

gridExtra::grid.arrange(plot1, plot2, ncol = 2)


# Change in Cost, Items and Quantity per Patient Over Time for Serial Drugs Only
ggplot(allpatients[allpatients$serial_drugs == "Serial Drug",])+
  stat_summary(aes(x = year, y = cost_per_patient, group = patient_type, colour = "Cost"), fun = "mean", geom = "line", size = 1.2)+
  stat_summary(aes(x = year, y = items_per_patient * 5, group = patient_type, colour = "Items"), fun = "mean", geom = "line", size = 1.2)+
  stat_summary(aes(x = year, y = quantity / patients * .1, group = patient_type, colour = "Quantity"), fun = "mean", geom = "line", size = 1.2)+
  scale_y_continuous(name = "Cost and Quantity per Patient", breaks = seq(60, 160, 20),
                     sec.axis = sec_axis(~./5, name = "Items per Patient", breaks = seq(10,30,5)))+
  theme_classic()+
  coord_cartesian(ylim = c(75,160))+
  scale_x_continuous(name = "")+
  facet_wrap(~patient_type)+
  theme(legend.title = element_blank(), legend.position="bottom")+
  labs(title = "Annual Cost, Quantity and Items per Patient with  Prescription", 
       subtitle = "Serial Drugs",
       caption = "Data Source: PIS")


#==============================================================#
# Visualise Data for Cohort of Patients on SerialRx before and after switch
#==============================================================#

# Visualisation of Cost per Patient before and after switch
ggplot(eastlothian_quarterly_summary)+
  geom_col(aes(x = quarters_from_serialrx_start, y = patients / 50, group = serial_drugs, fill = serial_drugs), alpha = .3)+
  geom_line(aes(x = quarters_from_serialrx_start, y = cost_per_patient, group = serial_drugs, colour = serial_drugs), size = 1.2)+
  theme_minimal()+
  theme(legend.title = element_blank(), legend.position="bottom")+
  scale_y_continuous(name = "Cost per Patient", labels = scales::dollar_format(prefix = "£"),
                     sec.axis = sec_axis(~. * 50, name = "Number of Patients with a Prescription\n by Type of Prescription", breaks = seq(250,750, 250)))+
  scale_x_continuous(name = "Quarters Before and After Switch to SerialRx")+
  labs(title = paste("Cost per Patient Before (-) and After (+) Switch to SerialRx,\n", 
                     format(min(eastlothian$paid_date),'%b %Y'), "-", format(max(eastlothian$paid_date),'%b %Y'), sep = " "), 
       caption = "Data Source: PIS")

ggplot(eastlothian_quarterly_summary)+
  geom_col(aes(x = quarters_from_serialrx_start, y = patients, group = serial_drugs, fill = serial_drugs))

# Visualisation of Paid Quantity per Patient before and after switch
ggplot(eastlothian_quarterly_summary)+
  geom_col(aes(x = quarters_from_serialrx_start, y = patients / 10, group = serial_drugs, fill = serial_drugs), alpha = .3)+
  geom_line(aes(x = quarters_from_serialrx_start, y = quantity_per_patient, group = serial_drugs, colour = serial_drugs), size = 1.2)+
  theme_minimal()+
  theme(legend.title = element_blank(), legend.position="bottom")+
  scale_y_continuous(name = "Paid Quantity per Patient",
                     sec.axis = sec_axis(~. * 10, name = "Number of Patients with a Prescription\n by Type of Prescription", breaks = seq(250,750, 250)))+
  scale_x_continuous(name = "Quarters Before (-) and After (+) Switch to SerialRx")+
  labs(title = paste("Number of Paid Items per Patient Before and After Switch to SerialRx,\n", 
                     format(min(eastlothian$paid_date),'%b %Y'), "-", format(max(eastlothian$paid_date),'%b %Y'), sep = " "), 
       caption = "Data Source: PIS")





# Who are the main users of pain medications by sex and age?
# What happens after patients receive their first pain prescription?
# What are typical patterns of chronic pain polypharmacy
# Are their differences by sex or age?

library(data.table)
library(dplyr)
library(plyr)
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
library(ggthemes)
library(ellipse)
library(RColorBrewer)
library(GGally)
library(corrgram)
library(ggpubr)
library(waffle)
library(extrafont)
library(sparkline)
library(shades)

# Load source functions
source("N:/loadPharmacyFunctions.R")
source("N:/Chronic Pain/scripts/functions.R")

#=============================================
# Load Data
#=============================================

# Load PIS data
chronic <- fread("N:\\Projects\\Primary Care Pharmacy\\Chronic Pain\\Project\\GPdata\\210315_PIS_data_extract_3WLpractices.csv")

# Load practice information (list size, hscp)
practiceLookup <- read_excel("N:\\practice_information.xlsx") %>%
  select(-locality, -cluster)

# Load list of agreed pain medications
pain_medications <- read_excel("N:\\210201_agreed_list_of_medications.xlsx")

# Load DDDs
DDD_full <- read.csv(file = "N:\\DDDs\\DDD List.csv", header = TRUE, na.strings = "", stringsAsFactors = T) %>%
  # Rename the columns
  rename_with(snakecase::to_snake_case)

# Load DDD conversion values for missing DDDs
DDD_additional <- read_excel("N:\\additionalDDDs.xlsx") %>%
  select(pi_bnf_item_code, pi_bnf_item_description, DDDconversion_value)

# Load packsize cutoff values for classifying pack sizes as small, medium or large
packsizes <- read_excel("N:\\packsizes.xlsx") %>%
  select(pi_bnf_item_code, packsize_cutoff)

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

# Classify drugs
chronic <- as.data.table(classifyDrug(chronic))

# Change drug classification names
chronic$class[chronic$class == "Other"] <- "Adjuvant Analgesics"
chronic$class[chronic$class == "Adjuvant Analgesics"] <- "Neuropathic Agents"
chronic$class[chronic$class == "Strong Opiod"] <- "Strong Opioid"
chronic$class[chronic$class == "Weak Opiod"] <- "Weak Opioid"
chronic$class[chronic$class == "Non-opiod Analgesics"] <- "Non-opioid Analgesics"

chronic %<>% 
  rename_with(snakecase::to_snake_case) %>%
  filter(
    # Filter to retain list of agreed medications for chronic pain work
    pi_approved_name %in% pain_medications$medication,
    # Filter to remove injections and suppositories (more likely to be used by palliative patients)
    !pi_drug_formulation %in% c ("INJ", "SUPPS")) %>%
  mutate(pat_date_of_birth_c = mdy(pat_date_of_birth_c)) %>%
  mutate(pat_date_of_birth_c = ifelse(year(pat_date_of_birth_c) > 2021,
                                      paste(subtract((year(pat_date_of_birth_c)), 100),
                                            format(pat_date_of_birth_c, "%m-%d"),
                                            sep = "-"),
                                      as.character(pat_date_of_birth_c))) %>%
  mutate(pat_date_of_birth_c = as.POSIXct(pat_date_of_birth_c),
         #paid_date = as.POSIXct(paid_date, format = "%m/%d/%y"),
         date = parse_date_time2(paste(1, paid_calendar_month_and_year), "%d %m %Y"),
         paid_quantity = as.numeric(gsub(",", "", paid_quantity)),
         pd_paid_gic_excl_bb = as.numeric(gsub(",", "", pd_paid_gic_excl_bb)),
         paid_month_year = zoo::as.yearmon(date, "%b %Y"),
         prescribedAge = as.period(interval(start = pat_date_of_birth_c, end = date))$year,
         currentAge = as.period(interval(start = pat_date_of_birth_c, end = max(date)))$year,
         age_group = case_when(
           currentAge < 18 ~ "<18",
           currentAge >= 18 & currentAge < 30 ~ "18-29",
           currentAge >= 30 & currentAge < 40 ~ "30-39",
           currentAge >= 40 & currentAge < 50 ~ "40-49",
           currentAge >= 50 & currentAge < 60 ~ "50-59",
           currentAge >= 60 & currentAge < 70 ~ "60-69",
           currentAge >= 70 & currentAge < 80 ~ "70-79",
           currentAge >= 80 & currentAge < 90 ~ "80-89",
           currentAge >= 90 ~ "90+"
         )) %>%
  # Join and to DDDs
  left_join(DDD[c(1,3)], by = "pi_bnf_item_code") %>%
  # Join to packsize cutoff values
  left_join(packsizes, by = "pi_bnf_item_code") %>%
  # Calculate DDDs
  mutate(
    date = as.Date(date),
    # Calculate DDDs
    DDD = paid_quantity / DDDconversion_value,
    # Shorten the practice name
    presc_location_name = tm::removeWords(presc_location_name, c(" MEDICAL", " GROUP", " CENTRE"," PRACTICE")),
    # Calculate where the pack size is small, medium or large, based on cut-off values (small is typically a week's supply, and medium is twice that)
    packsize = case_when(
      paid_quantity <= packsize_cutoff ~ "small",
      paid_quantity > packsize_cutoff & paid_quantity <= (packsize_cutoff * 2) ~ "medium",
      paid_quantity > (packsize_cutoff * 2) ~ "large"
    ))

#=============================================
# Summarise Data 
#=============================================

# Objects for dates
earliest_month <- min(chronic$date)
last_month <- max(chronic$date)
cutoff_month_min <- format(as.Date(earliest_month) %m+% months(4), "%Y-%m-%d")
cutoff_month_max <- format(as.Date(last_month) %m-% months(11), "%Y-%m-%d")

# Subset data to pull out just new starts
chis <- chronic %>%
  group_by(pat_upi_c, pat_date_of_birth_c) %>%
  summarise(start_date = min(date),
            end_date = max(date),
            total_items = sum(number_of_paid_items),
            total_DDDs = sum(DDD, na.rm = T)) %>%
  filter(start_date >= cutoff_month_min & start_date < cutoff_month_max) %>%
  # add gender flag (CHI number even for females, odd for males)
  mutate(
    gender = case_when(
    (pat_upi_c%%2) == 0 ~ "Female",
    (pat_upi_c%%2) != 0 ~ "Male")
  ) %>% ungroup() %>%
  arrange(desc(pat_date_of_birth_c)) %>%
  mutate(id = row_number(),
         id = as.factor(id))

chronic_newstarts <- chronic %>% filter(
  #prescribedAge > 16,
  pat_upi_c %in% chis$pat_upi_c) %>%
  left_join(chis, by = "pat_upi_c") %>%
  # Calculate months since first prescription
  mutate(months_since_1st_rx = interval(start_date, date) %/% months(1)) %>%
  # Include just the first 12 months of data since first pain prescription
  filter(months_since_1st_rx <= 12)

#------------------------- Percentage of Patients who are High Users-----------------------------------#

# Categorise the number of patients by number of pain prescriptions in a year
users <- chronic_newstarts %>% summarise(patients = n_distinct(pat_upi_c),
                                         patients_1rx = n_distinct(pat_upi_c[total_items == 1]),
                                         patients_2_4rx = n_distinct(pat_upi_c[total_items >1 & total_items <5]),
                                         patients_5_8rx = n_distinct(pat_upi_c[total_items >=5 & total_items <8]),
                                         patients_9_12rx = n_distinct(pat_upi_c[total_items >=8 & total_items <12]),
                                patients_12plus_rx_or_365plusDDDs = n_distinct(pat_upi_c[total_items > 12 | total_DDDs > 365]),
                                patients_12plus_rx = n_distinct(pat_upi_c[total_items > 12 & total_DDDs < 365])) %>%
  mutate(high_users_percent = round(patients_12plus_rx_or_365plusDDDs / patients * 100, 0))

# Percentage of Patients broken down by low, medium and high users - Alpha
users_percent <- users %>% pivot_longer(!c(patients, high_users_percent, patients_12plus_rx), names_to = "user_category", values_to = "count") %>%
  mutate(percent = round(count / patients * 100,0))

#--------------------------- All Patients by Sex and Age Group ----------------#

# Population - by category
population <- chronic_newstarts %>% group_by(age_group, gender, pat_upi_c) %>%
  summarise(num_items = sum(number_of_paid_items)) %>%
  mutate(items = case_when(
    num_items == 1 ~ "1 Rx",
    num_items > 1 & num_items < 5 ~ "2-4 Rx",
    num_items >= 5 & num_items < 8 ~ "5-8 Rx",
    num_items >= 8 & num_items < 12 ~ "9-12 Rx",
    num_items >= 12 ~ "12+ Rx"
  ),
  items = factor(items, levels = c("1 Rx", "2-4 Rx", "5-8 Rx", "9-12 Rx", "12+ Rx"))) %>%
  ungroup() %>%
  group_by(age_group, gender, items) %>%
  summarise(patients = n_distinct(pat_upi_c))

population$patients[population$gender == "Male"] <- -1 * population$patients[population$gender == "Male"]

population_labels <- population %>%
  group_by(age_group, gender) %>%
  summarise(total_patients = sum(patients))

# ----------------- Individual Trajectories for the Outliers ----------------#

# Sparklines of Outliers (> 365 DDDs)
ordered_id <- chronic_newstarts %>% filter(total_DDDs > 365, months_since_1st_rx >= 0) %>%
  distinct(id, gender, age_group, total_DDDs, total_items) %>% arrange(gender, age_group) %>%
  mutate(id_ordered = row_number()) %>% select(id, id_ordered)

persona_top_outliers <- chronic_newstarts %>% filter(total_DDDs > 365, months_since_1st_rx >= 0) %>% arrange(months_since_1st_rx) %>%
  select(id, gender, age_group, pi_approved_name, class, DDD, number_of_paid_items, packsize, months_since_1st_rx) %>%
  left_join(ordered_id) %>%
  arrange(gender, age_group) %>%
  mutate(id_formatted = paste0("ID ", id_ordered, " (", gender, ", ", age_group, ")"),
         id_formatted = as.factor(id_formatted))

# Reorder factors
persona_top_outliers$id_formatted <- forcats::fct_reorder(persona_top_outliers$id_formatted, persona_top_outliers$id_ordered, min)

# Summarise items for sparkline
top_outliers_items <- persona_top_outliers %>% group_by(id_ordered, id_formatted, gender, age_group, months_since_1st_rx) %>%
  summarise(total_DDDs = sum(DDD),
            total_items = sum(number_of_paid_items))

# Order of drug classes for outliers
rx_ordered_by_class_outliers <- persona_top_outliers %>%
  mutate(packsize_quantity = case_when(
    packsize == "small" ~ 1,
    packsize == "medium" ~ 2,
    packsize == "large" ~ 4
  ),
  packsize_quantity = packsize_quantity * number_of_paid_items,
  class = factor(class, levels = c("Neuropathic Agents", "NSAIDS", "Non-opioid Analgesics", "Weak Opioid", "Strong Opioid"))) %>%
  arrange(id_formatted, months_since_1st_rx)

# Detailed timeline (by drug class) for persona of the highest user
persona1 <- chronic_newstarts %>% filter(total_DDDs == max(total_DDDs)) %>% arrange(months_since_1st_rx) %>%
  select(pi_approved_name, class, DDD, number_of_paid_items, packsize, months_since_1st_rx) %>%
  mutate(packsize_quantity = case_when(
    packsize == "small" ~ 1,
    packsize == "medium" ~ 2,
    packsize == "large" ~ 4
  ),
  packsize_quantity = packsize_quantity * number_of_paid_items)
group_by(months_since_1st_rx, class, packsize) %>%
  summarise(items = sum(number_of_paid_items))


# ------------------------- Most Common Drugs --------------------------#

## Most common drugs prescribed for all patients
drugs_rx <- chronic_newstarts %>% group_by(pi_approved_name, class) %>%
  summarise(items = sum(number_of_paid_items)) %>%
  arrange(desc(items)) %>% ungroup() %>%
  mutate(order = row_number(),
         order = as.factor(order),
         class = factor(class, levels = c("Neuropathic Agents", "NSAIDS", "Non-opioid Analgesics", "Weak Opioid", "Strong Opioid")))

## Most common drugs prescribed for outliers
drugs_rx_outliers <- chronic_newstarts %>% filter(total_items > 12 | total_DDDs > 365) %>% group_by(pi_approved_name, class) %>%
  summarise(items_outliers = sum(number_of_paid_items)) %>%
  arrange(desc(items_outliers)) %>% ungroup() %>%
  mutate(order_outliers = row_number(),
         order_outliers = as.factor(order_outliers),
         class = factor(class, levels = c("Neuropathic Agents", "NSAIDS", "Non-opioid Analgesics", "Weak Opioid", "Strong Opioid"))) %>%
  left_join(drugs_rx)

## Drugs prescribed as a percentage of all prescriptions (easier to compare normal vs. outlier patients)
drugs_percentage_of_rx <- chronic_newstarts %>% mutate(patient_type = case_when(
  total_items > 12 | total_DDDs > 365 ~ "outlier",
  total_items <= 12 | total_DDDs <= 365 ~ "normal")) %>%
  group_by(patient_type, pi_approved_name, class) %>%
  summarise(items = sum(number_of_paid_items)) %>%
  arrange(patient_type, desc(items)) %>% ungroup()

drugs_percentage_of_rx %<>%
  mutate(order_outliers = row_number(),
         order_outliers = as.factor(order_outliers),
         class = factor(class, levels = c("Neuropathic Agents", "NSAIDS", "Non-opioid Analgesics", "Weak Opioid", "Strong Opioid")),
         percentage_of_items = case_when(
           patient_type == "normal" ~ items / sum(items[patient_type == "normal"]),
           patient_type == "outlier" ~ items / sum(items[patient_type == "outlier"])
         ),
         label = paste0(pi_approved_name, " ", percent(percentage_of_items, accuracy = .1))) %>%
  select(-c(items, order_outliers))

### Rename categories for prettier formatting
drugs_percentage_of_rx$pi_approved_name[drugs_percentage_of_rx$pi_approved_name == "TRAMADOL HYDROCHLORIDE"] <- "TRAMADOL"
drugs_percentage_of_rx$patient_type[drugs_percentage_of_rx$patient_type == "normal"] <- "Normal Patient"
drugs_percentage_of_rx$patient_type[drugs_percentage_of_rx$patient_type == "outlier"] <- "Outlier"


#=============================================
# Visualise Data 
#=============================================

#--------- Waffle chart of the percentage of patients who are low, medium or high users--------#

## Select colours from the "PuRd" colour palette
brewer.pal(6, "PuRd")

waffle(c(`> 12 Rx = 9%` = 9, `9-12 Rx = 7%` = 7,
         `5-8 Rx = 12%` = 12, `2-4 Rx = 38%` = 38, `1 Rx = 33%` = 34), 
       rows = 10, flip = T, reverse = T, legend_pos = "left",
       title = "Patients Starting Prescription Pain Medication\n Categorised as Low, Medium or High Users",
       colors = c("#006D2C", "#2CA25F", "#66C2A4", "#99D8C9", "#CCECE6")
       #colors = c("#980043", "#DD1C77", "#DF65B0", "#C994C7", "#E7CAED")
       )

#------------------- Who are the main users of pain medications by sex and age?------#

# Population Pyramid by Age Group for Males vs. Females, colour gradient according to how many prescriptions patients have received
ggplot()+
  geom_bar(data = population[population$gender == "Female",], aes(x = age_group, y = patients, group = items, fill = items), stat = "identity")+
  geom_bar(data = population[population$gender == "Male",], aes(x = age_group, y = patients, group = items, fill = items), stat = "identity")+
  #geom_text(data = population_labels[population_labels$gender == "Male",], aes(x = age_group, y = total_patients, label = abs(total_patients), hjust = 1.5))+
  #geom_text(data = population_labels[population_labels$gender == "Female",], aes(x = age_group, y = total_patients, label = total_patients, hjust = -.5))+
  annotate(geom = "text", x = "90+", y = -75, label = "Males")+
  annotate(geom = "text", x = "90+", y = 75, label = "Females")+
  geom_hline(yintercept = 0, size = 3, colour = "white")+
  scale_x_discrete(name = "Age \n")+
  scale_y_continuous(name = "Number of New Patients", breaks = seq(-100, 100, 25), labels = c(seq(100,0,-25), seq(25,100,25)))+
  scale_fill_manual(name = "Number of Pain Prescriptions the Patient Has Received in the Last 12 Months", values = c("#E7CAED", "#C994C7", "#DF65B0", "#DD1C77", "#980043"))+
  #scale_fill_gradient(high = "#132B43", low = "#56B1F7")+ # Reverses the colour gradient
  theme_tufte()+
  theme(legend.position = "bottom", legend.box = "vertical", axis.text.y = element_text(size = 10), legend.title.align = .5, text =element_text(size=14))+
  guides(fill=guide_legend(title.position = "top", title.hjust = .5, label.hjust = .5))+
  coord_flip()


#------------------- Volume of Pain Medication per Patient in first 12 months ------#

# Circular stacked bar chart
pat_class_DDD <- chronic_newstarts %>% filter(prescribedAge > 16) %>% group_by(age_group, gender, id, class) %>%
  summarise(items = sum(number_of_paid_items),
            DDDs = sum(DDD)) %>%
  filter(!is.na(DDDs))

# Circular bar chart with space
pat_class_DDD_totals <- pat_class_DDD %>% filter(age_group != "<18") %>%
  group_by(age_group, gender, id) %>%
  summarise(items = sum(items),
            DDDs = sum(DDDs)) %>%
  mutate(age_group = as.factor(age_group)) %>%
  select(-id)

empty_bar <- 10
to_add_totals <- data.frame(matrix(NA, empty_bar * nlevels(pat_class_DDD_totals$age_group), ncol(pat_class_DDD_totals)))
colnames(to_add_totals) <- colnames(pat_class_DDD_totals)
to_add_totals$age_group <- rep(levels(pat_class_DDD_totals$age_group), each = empty_bar)
to_add_totals %<>% mutate(age_group = as.factor(age_group))
pat_class_DDD_totals <- rbind(pat_class_DDD_totals, as.data.frame(to_add_totals)) %>%
  arrange(age_group)

pat_class_DDD_totals_male <- pat_class_DDD_totals %>% filter(gender == "Male" | is.na(gender))
pat_class_DDD_totals_male$id <- seq(1, nrow(pat_class_DDD_totals_male))
pat_class_DDD_totals_female <- pat_class_DDD_totals %>% filter(gender == "Female" | is.na(gender))
pat_class_DDD_totals_female$id <- seq(1, nrow(pat_class_DDD_totals_female))

# prepare a data frame for base lines
base_data_males <- pat_class_DDD_totals_male %>%
  group_by(age_group, gender) %>% 
  summarise(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end))) %>% filter(!is.na(gender))
base_data_females <- pat_class_DDD_totals_female %>%
  group_by(age_group, gender) %>% 
  summarise(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end))) %>% filter(!is.na(gender))

# prepare a data frame for grid (scales)
grid_data_males <- base_data_males
grid_data_males$end <- grid_data_males$end[ c( nrow(grid_data_males), 1:nrow(grid_data_males)-1)] + 1
grid_data_males$start <- grid_data_males$start - 1
grid_data_males <- grid_data_males[-1,]
grid_data_females <- base_data_females
grid_data_females$end <- grid_data_females$end[ c( nrow(grid_data_females), 1:nrow(grid_data_females)-1)] + 1
grid_data_females$start <- grid_data_females$start - 1
grid_data_females <- grid_data_females[-1,]

# Male only - DDDs
ggplot(pat_class_DDD_totals_male, aes(x = as.factor(id), y = DDDs, fill = DDDs))+
  geom_bar(stat = "identity")+
  geom_segment(data=grid_data_males, aes(x = end, y = 365, xend = start, yend = 365), colour = "grey", alpha=1, size=0.5 , inherit.aes = FALSE )+
  annotate("text", x = max(pat_class_DDD_totals_male$id), y = 365, label = "365 DDDs", color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  annotate("text", x = min(pat_class_DDD_totals_male$id), y = -1400, label = "Males", color="black", size=5 , angle=0, fontface="bold", hjust=.5) +
  ylim(-1500,max(pat_class_DDD_totals_male$DDDs, na.rm = T))+
  theme_minimal() +
  scale_fill_gradient(low = "blue", high = "red")+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = unit(rep(-2,4), "cm") 
  )+
  coord_polar(start = 0)+
  geom_segment(data=base_data_males, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=1 , inherit.aes = F)+
  geom_text(data=base_data_males, aes(x = title, y = -18, label=age_group), hjust=c(1,1,1,0,0,0,0,0),  vjust=c(1,1,0,0,0,1,1,1), colour = "black", alpha=0.8, size=3.5, fontface="bold", inherit.aes = FALSE)

# Female only - DDDs
ggplot(pat_class_DDD_totals_female, aes(x = as.factor(id), y = DDDs, fill = DDDs))+
  geom_bar(stat = "identity")+
  geom_segment(data=grid_data_females, aes(x = end, y = 365, xend = start, yend = 365), colour = "grey", alpha=1, size=0.5 , inherit.aes = FALSE )+
  annotate("text", x = max(pat_class_DDD_totals_female$id), y = 365, label = "365 DDDs", color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  annotate("text", x = min(pat_class_DDD_totals_female$id), y = -1400, label = "Females", color="black", size=5 , angle=0, fontface="bold", hjust=.5) +
  ylim(-1500,max(pat_class_DDD_totals_female$DDDs, na.rm = T))+
  theme_minimal() +
  scale_fill_gradient(low = "blue", high = "red")+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = unit(rep(-2,4), "cm") 
  )+
  coord_polar(start = 0)+
  geom_segment(data=base_data_females, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=1 , inherit.aes = F)+
  geom_text(data=base_data_females, aes(x = title, y = -18, label=age_group), hjust=c(1,1,1,0,0,0,0,0),  vjust=c(1,1,0,0,0,1,1,1), colour = "black", alpha=0.8, size=3.5, fontface="bold", inherit.aes = FALSE)


#------------------- Timeline of prescribing for outliers in first 12 months?------#


# --------------------------- Bar Chart Matrices - Total DDDs per Month for Outliers --------#

## Bar Chart Matrix - Males
ggplot(top_outliers_items[top_outliers_items$gender == "Male",], aes(x = months_since_1st_rx, y = total_DDDs, group = id_formatted))+
  geom_col(alpha = .8, aes(fill = total_DDDs), show.legend = F)+
  #geom_line(size = 2)+
  theme_void()+
  scale_fill_gradient(low = "blue", high = "red")+
  #scale_fill_viridis_c()+
  facet_wrap(~id_formatted, strip.position = "bottom")

## Bar Chart Matrix - Females
ggplot(top_outliers_items[top_outliers_items$gender == "Female",], aes(x = months_since_1st_rx, y = total_DDDs, group = id_formatted))+
  geom_col(alpha = .8, aes(fill = total_DDDs), show.legend = F)+
  #geom_line(size = 2)+
  theme_void()+
  scale_fill_gradient(low = "blue", high = "red")+
  #scale_fill_viridis_c()+
  facet_wrap(~id_formatted, strip.position = "bottom")

#--------------- Bar Chart Matrix of Drug Class and Order of Prescriptions -----------------# 

## Males

ggplot(rx_ordered_by_class_outliers[rx_ordered_by_class_outliers$gender == "Male",])+
  geom_col(aes(x = months_since_1st_rx, y = DDD, group = DDD, fill = class), position = "dodge2")+
  #stat_summary(aes(x = months_since_1st_rx, y = number_of_paid_items), fun = sum, geom = "line")+
  #scale_x_continuous(name = "Months Since First Prescription", breaks = seq(0,12,1))+
  #scale_y_continuous(name = "DDD per Prescription")+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "bottom", legend.title = element_blank())+
  facet_wrap(~id_formatted, strip.position = "bottom")

## Females

ggplot(rx_ordered_by_class_outliers[rx_ordered_by_class_outliers$gender == "Female",])+
  geom_col(aes(x = months_since_1st_rx, y = DDD, group = DDD, fill = class), position = "dodge2")+
  #stat_summary(aes(x = months_since_1st_rx, y = number_of_paid_items), fun = sum, geom = "line")+
  #scale_x_continuous(name = "Months Since First Prescription", breaks = seq(0,12,1))+
  #scale_y_continuous(name = "DDD per Prescription")+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "bottom", legend.title = element_blank())+
  facet_wrap(~id_formatted, strip.position = "bottom")


#-------------------------- Timeline of Pain Prescribing for a Persona ---------------------------------#

# Bar chart - Order of prescriptions by drug class and DDDs
ggplot(persona1)+
  geom_col(aes(x = months_since_1st_rx, y = DDD, group = DDD, fill = class), position = "dodge2")+
  #stat_summary(aes(x = months_since_1st_rx, y = number_of_paid_items), fun = sum, geom = "line")+
  scale_x_continuous(name = "Months Since First Prescription", breaks = seq(0,12,1))+
  scale_y_continuous(name = "DDD per Prescription")+
  scale_fill_viridis_d()+
  theme_tufte()+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), text = element_text(size=14))

# Total DDDs per Month by Drug Class
ggplot(persona1)+
  geom_col(aes(x = months_since_1st_rx, y = DDD, group = DDD, fill = class), position = "stack")+
  #stat_summary(aes(x = months_since_1st_rx, y = number_of_paid_items), fun = sum, geom = "line")+
  scale_x_continuous(name = "Months Since First Prescription", breaks = seq(0,12,1))+
  scale_y_continuous(name = "DDD per Prescription")+
  scale_fill_viridis_d()+
  theme_tufte()+
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), text = element_text(size=14))


# --------------- Most Common Drugs ----------------------------------------------------#

# Bar chart of Drugs ordered by number of prescriptions - all patients
ggplot(drugs_rx)+
  geom_col(aes(x = reorder(pi_approved_name, +items), y = items, fill = class))+
  scale_x_discrete(name = "")+
  scale_y_continuous(name = "Number of Prescriptions", limits = c(0, 1300))+
  #geom_text(aes(x = reorder(pi_approved_name, +items), y = items, label = items), hjust = -.1)+
  theme_tufte()+
  scale_fill_viridis_d()+
  #theme(legend.position = "bottom", legend.box = "vertical", axis.text.y = element_text(size = 10), legend.title.align = .5, text =element_text(size=14))+
  theme(legend.position = "bottom", legend.box = "vertical", legend.title = element_blank(), text = element_text(size = 14), 
        legend.text=element_text(size=14))+
  coord_flip()

# Bar chart of Drugs ordered by number of prescriptions - outlier patients
ggplot(drugs_rx_outliers)+
  geom_col(aes(x = reorder(pi_approved_name, +items_outliers), y = items_outliers, fill = class))+
  scale_x_discrete(name = "")+
  scale_y_continuous(name = "Number of Prescriptions", limits = c(0, 400))+
  #geom_text(aes(x = reorder(pi_approved_name, +items_outliers), y = items_outliers, label = items_outliers), hjust = -.1)+
  theme_tufte()+
  scale_fill_viridis_d()+
  theme(legend.position = "bottom", legend.title = element_blank(), text = element_text(size = 14), legend.text=element_text(size=14))+
  coord_flip()

# Slopegraph of the difference in drugs prescribed for normal patients vs. outliers
ggplot(drugs_percentage_of_rx, aes(x = patient_type, y = percentage_of_items, group = pi_approved_name, colour = class, label = label), show.legend = F)+
  geom_line(size = 1)+
  geom_point(size = 2)+
  scale_colour_viridis_d()+
  scale_x_discrete(name = "", position = "top")+
  scale_y_continuous(name = "")+
  geom_text(aes(label = ifelse(patient_type == "Normal Patient" & percentage_of_items > .09, label, "")), hjust = 1.1, vjust = 0, size = 5, alpha = 2)+
  geom_text(aes(label = ifelse(patient_type == "Outlier" & percentage_of_items > .055 & pi_approved_name != "PARACETAMOL", label, "")), hjust = -.1, vjust = 0, size = 5)+
  theme_bw()+
  theme(legend.position = "none", legend.title = element_blank(), text =element_text(size=14),
        axis.ticks = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), panel.border = element_blank())



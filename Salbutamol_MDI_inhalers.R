# Report on the quantity of Salbutamol MDI inhalers prescribed per month for children and adults

library(data.table)
library(readr)
library(dplyr)
library(dbplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(plotly)
library(shiny)
library(magrittr)
library(DT)

# Load Respiratory Data
respiratory <- readRDS("N:/Respiratory/Data/activityData.rda") %>%
  rename_with(snakecase::to_snake_case)

# Load classification of inhalers as DPI or MDI
inhaler_types <- read_excel("N:\\03-Reference\\Lookup Information\\DPI vs MDI inhalers.xlsx")

# Calculate patient ages at time of prescription and currently
mostRecentMonth <- floor_date(Sys.Date(), unit = "months") %m-% months(3) #pick out most recent data in the activity file to calculate patients current ages
respiratory %<>%
  mutate(prescribed_age = as.period(interval(start = pat_date_of_birth_c, end = date))$year,
         current_age = as.period(interval(start = pat_date_of_birth_c, end = mostRecentMonth))$year)

# Total Salbutamol MDI prescribed in Lothian per month since Jan 2018 (quantity)
SABA_MDI <- respiratory %>% filter(date >= "2018-01-01") %>%
  filter(pi_drug_formulation == "INHAL") %>%
  left_join(inhaler_types, by = "pi_bnf_item_description") %>%
  filter(inhaler_class == "SABA", category == "MDI") %>%
  group_by(date) %>%
  summarise(monthly_quantity = sum(paid_quantity),
            monthly_quantity_under18 = sum(paid_quantity[!is.na(prescribed_age) & prescribed_age < 18]),
            monthly_quantity_over18 = sum(paid_quantity[!is.na(prescribed_age) & prescribed_age >= 18]),
            quantity_missing_ages = sum(paid_quantity[is.na(prescribed_age)])) %>%
  mutate(date = format.Date(date, "%d/%m/%Y"))

# Export to Excel
writexl::write_xlsx(SABA_MDI, "N:\\Respiratory\\SABA MDI\\SABA_MDI_quantities.xlsx")


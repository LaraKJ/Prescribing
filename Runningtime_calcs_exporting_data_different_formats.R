# Check running times for exporting data to different formats (incl. SQLite database)

library(data.table)
library(readr)
library(dplyr)
library(dbplyr)
library(sqldf)
library(RSQLite)
library(dbplyr)

# Load the chronic pain data and check the running time
system.time(fulldata <- readRDS(
  "N:/activityData.rda")) # User 44.72, system .30, elapsed 226.05

# Create a randomly sampled subset of the data (100k out of 9,463,986 observations)
subset <- fulldata[sample(1:nrow(fulldata), 100000, replace = F),]

# Write subset to csv file using write.csv() function
system.time(write.csv(subset, file = "H:\\subset.csv")) 
#Time: 10 min. user 14.14, system 1.71, elapsed 606.12; File size: 30.937MB

# Write subset to csv file using write_csv() function (marginally faster)
system.time(write_csv(subset, file = "H:\\subset2.csv"))
#Time: 9 min. user 3.53, system 1.83, elapsed 522.86; File size: 28.279MB

# Write subset to csv file using write.table() function (faster?)
system.time(fwrite(subset, file = "H:\\subset3.csv"))
#Time: 1 min. user 0.19, system 0.05, elapsed 40.37; File size: 28.104MB

# Load in subset3 file to check data
subset3 <- fread("H:\\subset3.csv") 
# Compare data to original subset
all_equal(subset, subset3) #Some classes of data have changed

#Save subset as an R object (RDA format)
system.time(saveRDS(subset, file = "H:\\subset4.csv"))
#Time: 1min. user 1.66, system 0.07, elapsed 34.83.; File size: 3.597MB


system.time(read_excel("C:/sample_dataset.xlsx"))
system.time(fread("C://sample_dataset.xlsx"))

#------- Write data into SQLite database-------#

# Get the dataframe frame
# Rename the column names
colnames(subset) <- c("prescHBName", "prescLocationCode", "prescLocationName",
                      "patUpiC", "patDateOfBirthC", "paidCalMonthYear",
                      "piBnfChapterCode", "piBnfChapterDesc", "piBnfSectionCode",
                      "piBnfSectionDesc", "piBnfItemDescription", "piApprovedName",
                      "piPrescItemName", "piDrugFormulation", "piItemStrength",
                      "piDailyDoseConversion", "FormTypeCode", "pdClassCode",
                      "pdPaidGicExclBb", "numberOfPaidItems", "paidQuantity",
                      "Date", "finQuarter", "finYear",
                      "class", "prescribedAge", "currentAge",
                      "ageGroup", "imputedDDD", "morphineEquivalentDose")

# Change date format in dataframe
subset$patDateOfBirthC <- format(subset$patDateOfBirthC, "%Y-%m-%d")
subset$Date <- format(subset$Date, "%Y-%m-%d")

#subset$patDateOfBirthC <- as.Date(format(subset$patDateOfBirthC, "%Y%m%d"), format = "%Y%m%d")
#subset$Date <- as.Date(format(subset$Date, "%Y%m%d"), format = "%Y%m%d")
class(subset$patDateOfBirthC)

# Set working directory
getwd()
setwd("H:/Training datasets")

# Create new empty database
dataset_db <- dbConnect(SQLite(), dbname = "dataset.sqlite")

#Import data frame into database
dbWriteTable(conn = chronic_db, name = "Dataset", value = subset, row.names = F)
dbListTables(chronic_db) # The tables in the database
dbListFields(chronic_db, "Dataset") # The columns in a table
dbReadTable(chronic_db, "Dataset")
src_dbi(dataset_db)

# Query the database with SQL syntax
tbl(dataset_db, sql("SELECT patUpiC, patDateOfBirthC, Date FROM Dataset"))
numpatients <- tbl(dataset_db, sql("SELECT prescLocationName, patUpiC, patDateOfBirthC, Date 
                                   FROM Dataset"))
numpatients %>% group_by(prescLocationName) %>%
  summarise(patients = n_distinct(patUpiC)) %>%
  collect()

# Experiment with date datatypes
class(numpatients)
numpatients <- data.frame(numpatients)
numpatients$newDate <- as.Date(numpatients$Date)
dbWriteTable(conn = dataset_db, name = "Dates", value = numpatients, row.names = F)
tbl(dataset_db, sql("SELECT * FROM Dates"))

# Check the data classes in database
tbl(dataset_db, sql("SELECT * FROM Dataset"))
tbl(dataset_db, sql("SELECT patDateOfBirthC FROM Dataset"))
head(subset$patDateOfBirthC, n = 10)

#To do - change datatype of patDateOfBirthC and Date from chr to date


#Append data to table in database
system.time(dbWriteTable(conn=dataset_db, name = "Dataset", subset, append = T, row.names = F))
# User 0.70, system 0.06, elapsed 18.35

#Close connection with database
dbDisconnect(dataset_db)

#---------Test time to append subset to fulldata-------
system.time(dbWriteTable(conn = dataset_db, name = "FullData", value = fulldata, row.names = F))
system.time(dbWriteTable(conn=dataset_db, name = "FullData", subset, append = T, row.names = F))


# Connect to database
getwd()
setwd("N:/Dataset/")
dataset_db <- dbConnect(SQLite(), dbname = "dataset.sqlite")

dbReadTable(dataset_db, "Dataset")
dbGetQuery(dataset_db, "SELECT piBnfSectionDesc, sum(pdPaidGicExclBb) AS 'total_cost', count(numberOfPaidItems) FROM Dataset
           GROUP BY piBnfSectionDesc
           ORDER BY total_cost")

#Close connection with database
dbDisconnect(dataset_db)


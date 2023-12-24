library(countrycode) # convert country code between different systems
library(data.table) # data manipulation libraries
library(HeadR) # library by Prof Keith
library(ggplot2) # create visualizations
library(ggrepel) # make visualizations beautiful
library(fixest) # estimating linear models with multiple fixed-effects
library(readxl) # reading Excel file into R

rm(list = ls()) # remove everything from the workspace


otc <- fread("/Users/namnguyen/Library/CloudStorage/OneDrive-HKUSTConnect/NEWT/RA/BIS_FXTurnover_2023.csv")
setDT(otc)

otc_net_gross<-otc[DER_INSTR=="B" & DER_RISK== "B" & DER_REP_CTY!="5J"
                   & DER_SECTOR_CPY == "A" & DER_CPC == "5J" 
                   & DER_ISSUE_MAT == "A" & DER_RATING == "A"
                   & DER_EX_METHOD == "3" & DER_BASIS == "B"]

setnames(otc_net_gross, gsub("X", "", colnames(otc_net_gross)))

otc_net_gross <- melt(otc, id.vars = which(unlist(lapply(otc, function(x) !all(is.numeric(x))))), variable.name = "year", value.name = "amount", variable.factor = FALSE)
otc_net_gross$year <- as.numeric(otc_net_gross$year)

# Select necessary columns
otc_net_gross <- otc_net_gross[, c("Reporting country", "DER_CURR_LEG1", "DER_CURR_LEG2", "year", "amount")]

# Set names again for easy manipulation
setnames(otc_net_gross,c("countryname", "cur1", "cur2","year", "amount"))

# If the amount is NA, kick out of otc_net_gross
otc_net_gross <- otc_net_gross[!is.na(otc_net_gross$amount)]

# Find unique country in the data.table
unique_country <- otc_net_gross[, .(unique_countries = unique(countryname)), by = year]

# Put out all "All countries (unique)"
unique_country <- unique_country[unique_countries != "All countries (total)"]

# Use geddit (:=), add a new column with all value 1
unique_country <- unique_country[, count := 1]

# Calculate the sum of unique country in different years
unique_country <- unique_country[, sum(count), by = year]

# Sort cur 1, we do not want to have TO1 in unique currenty
otc_net_gross_without_TO1 <- otc_net_gross[cur1 != "TO1"]

# Find unique currency in data.table
unique_currency <- otc_net_gross_without_TO1[, .(unique_currency = unique(cur1)), by = year]

# Use geddit (:=), add a new column with all value 1
unique_currency <- unique_currency[, count := 1]

# Calculate the sum of unique currency in different years
unique_currency <- unique_currency[, sum(count), by = year]

# Put on new row to make the shape of unique_country and unique_currency
# compatible
new_row <- data.table(year = 1986, V1 = 0)
unique_currency <- rbind(unique_currency, new_row)

# Sort the year
unique_country <- unique_country[order(year)]
unique_currency <- unique_currency[order(year)]

# Start the merge!!!
merged_unique <- merge(unique_country, unique_currency, by = c("year"))

# Change the name of the columns
setnames(merged_unique, c("year", "unique_country", "unique_currency"))

# Output to a tex file
merged_unique_tex <- xtable(merged_unique)
print(merged_unique_tex, file = "task1-2-unique.tex")

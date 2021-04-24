#### Preamble ####
# Purpose: Clean original data
# Author: Hong Shi
# Date:  11 April 2021
# Contact: lancehong.shi@mail.utoronto.ca
# License: MIT
# Pre-requisites: Need to understand the logic to clean data as in "00_EDA.R"


#### Workspace set-up ####

# Load the original data in .dta format

# install.packages("haven")
# install.packages("rdd")
library(tidyverse)
library(haven)
library(here)


#### Import Data ####

# The original data can be found at: https://www.openicpsr.org/openicpsr/project/113751/version/V1/view

original_data <- read_dta(here("inputs/data/original_data.dta"))

#### Clean data ####

clean_data <- original_data
# check na values

# Reference the following to check No. of NA in each column: 
# https://stackoverflow.com/questions/24027605/determine-the-number-of-na-values-in-a-column

na_count <- sapply(original_data, function(y) sum(length(which(is.na(y)))))
na_count<- data.frame(na_count)
na_count

# Replace NAs in bpl_asia with 1 as in "00_EDA.R"

clean_data$bpl_asia <- clean_data$bpl_asia %>% replace_na(1)


# Replace NAs in prog_science prog_socsci_bus prog_socscihum with 0 as in "00_EDA.R"

clean_data$prog_science <- clean_data$prog_science %>% replace_na(0)
clean_data$prog_socsci_bus <- clean_data$prog_socsci_bus %>% replace_na(0)
clean_data$prog_socscihum <- clean_data$prog_socscihum %>% replace_na(0)

# Note: Student programs may change after first entering the university. 
# And there is no information about whether the program enrollment is recorded at the first year or by the end of graduation.
# Besides, there are many students with program unknown. So I would like not to proceed on these data columns in this study.

# Save the cleaned original data

write_csv(clean_data,here::here("inputs/data/cleaned_original.csv"))






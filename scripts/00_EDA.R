#### Preamble ####
# Purpose: Explanatory Data Analysis
# Author: Hong Shi
# Date:  11 April 2021
# Contact: lancehong.shi@mail.utoronto.ca
# License: MIT
# Pre-requisites: None

#### Workspace set-up ####

# Load the original data in .dta format

# install.packages("haven")
# install.packages("rdd")
library(tidyverse)
library(haven)
library(here)
library(rdd)


#### Import Data ####

# The original data can be found at: https://www.openicpsr.org/openicpsr/project/113751/version/V1/view

original_data <- read_dta(here("inputs/data/original_data.dta"))

#### Exploring Missing Values ####

#### Note: The original paper does not provide descriptions about variables in the data (Very Unfortunate) ####
####       I take some conventions when handling these missing values #### 

# Reference the following to check No. of NA in each column: 
# https://stackoverflow.com/questions/24027605/determine-the-number-of-na-values-in-a-column

na_count <- sapply(original_data, function(y) sum(length(which(is.na(y)))))
na_count<- data.frame(na_count)
na_count

# There are NAs in GPA_year2 CGPA_year2 totcredits_year2 bpl_asia yearstodegree gradin4 gradin5 gradin6 gradin4orcont
# prog_science prog_socsci_bus prog_socscihum year2_dist_from_cut CGPA_final nextGPA nextCGPA 
# probation_year2 probation_summer2 suspended_year2 suspended_summer2

# Second year related data such as GPA_year2 CGPA_year2 totcredits_year2 year2_dist_from_cut CGPA_final nextGPA nextCGPA 
# probation_year2 probation_summer2 suspended_year2 suspended_summer2 year2_dist_from_cut
# may not be available due to dropoff, gap year, etc. so will leave these NAs as is for now.

# now check bpl_asia
original_data %>% 
  filter(is.na(bpl_asia)) %>% 
  select(bpl_asia, bpl_other, bpl_north_america)

# bpl_asia, bpl_other, bpl_north_america are only indicators to show student birth of place. 
# If students are not born in North America and regions other than Asia, born in Asia should be the only possibility of these students
# So I would replace NAs in bpl_asia as 1.

# Check yearstodegree gradin4 gradin5 gradin6 gradin4orcont
# Similar to second year related data, the graduation related data may not be available due to dropoff, gap year, etc. 

graddata <- original_data %>% select(gradin4, gradin5, gradin6)
sapply(graddata,function(x) sum(is.na(x)))

# I notice a considerable amount of missing values related to graduation, I would like to leave these NAs "as-is", and use the rest as graduation samples

### IMPORTANT: After a glance of the original data, I notice that some observations violate the logic of graduation dummy coding 
# (e.g. gradin4 gradin5 gradin6 appears to be "1 0 1" as in identifier 372) I could attribute this as my misunderstanding of this coding or the lack knowledge about the original data.

# Check  prog_science prog_socsci_bus prog_socscihum

original_data %>% 
  filter(is.na(prog_science) | is.na(prog_socsci_bus) | is.na(prog_socscihum)) %>% 
  select(prog_science, prog_socsci_bus, prog_socscihum, prog_unknown)

# It is easy to see that NAs in  prog_science prog_socsci_bus prog_socscihum means unknown programs, so I would replace these NAs in prog as 0




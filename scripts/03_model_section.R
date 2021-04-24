#### Preamble ####
# Purpose: Codes for analysis in Model Section
# Author: Hong Shi
# Date:  18 April 2021
# Contact: lancehong.shi@mail.utoronto.ca
# License: MIT
# Pre-requisites: Need to have downloaded the "summary_data.csv" and "bandwidth02.csv" in "02_data_section.R" and saved it to inputs/data


##### Workspace set-up #####

library(tidyverse)
library(here)

##### Import Data #####

# Read in cleaned data

clean_for_summary <- read.csv(here("inputs/data/summary_data.csv"))


##### Subsection: Test on Regression Discontinuity Design Assumptions  #####

# Histogram of the distribution of GPA from cutoff

ggplot(clean_for_summary, aes(x = dist_from_cut))+
  geom_histogram(aes(y  = stat(count)), binwidth = 0.125, color = "black", fill = "#33CCCC")+
  geom_vline(xintercept = 0, linetype = "dotted")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Frequency Headcount")+
  xlab("Year 1 GPA minus Probation Cutoff")+
  ggtitle("Distribution of Year 1 GPA relative to Probation Cutoff for All Students")

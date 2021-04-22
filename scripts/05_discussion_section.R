#### Preamble ####
# Purpose: Codes for analysis in Discussion Section
# Author: Hong Shi
# Date:  23 April 2021
# Contact: lancehong.shi@mail.utoronto.ca
# License: MIT
# Pre-requisites: Need to have downloaded the "summary_data.csv", "bandwidth02.csv" in "02_data_section.R" and saved it to inputs/data


#### Workspace set-up ####

library(tidyverse)
library(here)
library(kableExtra)
library(rdrobust)
library(broom)
library(modelsummary)


#### Import Data ####

# Read in the cleaned data

clean_for_summary <- read.csv(here("inputs/data/summary_data.csv"))
clean_cut <- read.csv(here("inputs/data/bandwidth02.csv"))

##################################################################################
###### Subsection: Causal Inference, Internal Validity and External Validity #####
##################################################################################

#### Additional check with 0.1 GPA bandwidth ####

# Set students on academic probation as the baseline group
clean_cut$status <- as.factor(clean_cut$status)
clean_cut$status <- relevel(clean_cut$status, "On Academic Probation")

clean_cut1 <- clean_cut %>% 
  filter(dist_from_cut >= -0.1, dist_from_cut <= 0.1)

# summary statistics 
categorical1 <- clean_cut1 %>% 
  summary_factorlist("status", c("gender", "birthplace", "study_campus", "first_language"),
                     p = FALSE, 
                     digits = c(1,1,3,1),
                     add_dependent_label = TRUE, 
                     dependent_label_prefix = "",
                     add_col_totals = TRUE,
                     include_row_missing_col = FALSE,
                     col_totals_rowname = "",
                     col_totals_prefix = "N(%) = ")
continuous1 <- clean_cut1 %>% 
  summary_factorlist("status", c("hsgrade_pct", "totcredits_year1", "age_at_entry"),
                     p = FALSE,
                     digits = c(2,2,3,1),
                     add_dependent_label = TRUE, 
                     dependent_label_prefix = "",
                     add_col_totals = TRUE,
                     include_row_missing_col = FALSE,
                     col_totals_rowname = "",
                     col_totals_prefix = "N(%) = ")
combine1 <- rbind(categorical1, continuous1)
combine1 <- rbind(combine1, c("","","", ""))
combine1 <- rbind(combine1, c("","","Mean (SD)", "Mean (SD)"))
combine1 <- combine1[-c(12),]
combine1$` ` <- replace(combine1$` `, combine1$` ` == "Mean (SD)", "")
combine1$status <- NULL
combine1 <- combine1[c(1,3,2,5,4,6:11,15,16,12,13,14),] %>% 
  mutate(Description = c("","Gender","","Birth Place", "","","Study Campus","","", "First Language","","","","Highschool Grade Percentile","Credits Attempted in First Year", "Age at Entry"))

kable(combine1[,c(4,1,2,3)],caption = "Summary Statistics of Students Within 0.2 GPA of Probation Cutoff",
      booktabs = TRUE, linesep = "") %>% 
  kable_styling(latex_options = "hold_position")

# Model results

dropout_01 <- glm(left_school ~ status + dist_from_cut + gender + highHS + first_language ,family = "binomial", data = clean_cut1)
summary(dropout_01)

gpa_01 <- lm(GPA_year2 ~ status + dist_from_cut + gender + highHS + first_language, data = clean_cut1)
summary(gpa_01)

grad_01 <- glm(gradin4 ~ status + dist_from_cut + gender + highHS + first_language ,family = "binomial", data = clean_cut1)
summary(grad_01)


#### Check Campus Variation ####

# boxplot to show difference in highschool grade across campus
ggplot(clean_for_summary, aes(x = study_campus, y = hsgrade_pct, color = study_campus))+
  geom_boxplot()+
  theme_minimal()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
  labs(x = "Study Campus", y = "Highschool Grade Percentile",
       title = "Distribution of Student Highschool Grades by Campus")

# Add Campus variable into the model
gpa_camp2 <- lm(GPA_year2 ~ status + dist_from_cut + gender + first_language + highHS + study_campus, data = clean_cut)
#summary(gpa_camp2)
dropout_camp2 <- glm(left_school ~ status + dist_from_cut + gender + first_language + highHS + study_campus, family = "binomial", data = clean_cut)
#summary(dropout_camp2)
grad_camp2 <- glm(gradin4 ~ status + dist_from_cut + gender + first_language + highHS + study_campus  ,family = "binomial", data = clean_cut)
#summary(grad_camp2)

# Create modelsummary table
modelsummary(list("Logistic for Dropoout" = dropout_camp2, "Linear for Year 2 GPA" = gpa_camp2,"Logistic for Graudation" =  grad_camp2),
             statistic = "({p.value})",
             stars =TRUE,
             coef_map = c("(Intercept)" = "Constant","study_campusCampus 2" = "Campus 2" ,"study_campusCampus 3" = "Campus 3","statusIn Good Standing" = "In Good Standing", "dist_from_cut" = "f(Cutoff Distance)","genderMale" = "Gender Male", "highHS" = "Highschool Above Average", "first_languageOther" = "First Language Not English "),
             exponentiate = TRUE,
             title = "Regressions for Campus Variation towards Academic Probation within 0.2 GPA bandwidth",
             gof_omit = "BIC|R2 Adj.|F") %>% 
  add_footnote("Odds Ratio in Logistic Regression. Coefficient in Linear Regression. P-value in Parentheses", notation = "number") %>% 
  kable_styling(latex_options = "hold_position")

####################################
###### Subsection: Dirty Data ######
####################################

# Referenced the following to check number of NA values
# https://stackoverflow.com/questions/24027605/determine-the-number-of-na-values-in-a-column
na_count <-sapply(clean_for_summary, function(y) sum(length(which(is.na(y)))))
data.frame(na_count)

# GPA above cutoff but on probation, GPA below cutoff but in good standing total of 98 observations
clean_for_summary %>% 
  filter((dist_from_cut < 0 & probation_year1 !=1)| (dist_from_cut >=0 & probation_year1 !=0))

# graduation data conflict eg. graduation in Year 4 is 1 but graduation in Year 5 is 0 total of 775 observations
clean_for_summary %>% 
  filter((gradin4 == 1 & gradin5 ==0))

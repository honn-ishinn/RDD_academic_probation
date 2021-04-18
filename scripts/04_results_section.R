#### Preamble ####
# Purpose: Codes for analysis in Results Section
# Author: Hong Shi
# Date:  19 April 2021
# Contact: lancehong.shi@mail.utoronto.ca
# License: MIT
# Pre-requisites: Need to have downloaded the "summary_data.csv", "bandwidth02.csv","bandwidth06.csv", "bandwidth03.csv" 
#                 in "02_data_section.R" and saved it to inputs/data


#### Workspace set-up ####

library(tidyverse)
library(here)
library(kableExtra)
library(rdrobust)
library(broom)
library(modelsummary)


#### Import Data ####

# Read in the cleaned data

clean_data <- read.csv(here("inputs/data/cleaned_original.csv"))
clean_cut <- read.csv(here("inputs/data/bandwidth02.csv"))
clean_cut2 <- read.csv(here("inputs/data/bandwidth03.csv"))
clean_cut3<- read.csv(here("inputs/data/bandwidth06.csv"))

#############################
#### Subsection: Dropout ####
#############################

# Check the class of status variable and set "On Academic Probation"
class(clean_cut$status)
clean_cut$status <- as.factor(clean_cut$status)
levels(clean_cut$status)
# Set students on academic probation as the baseline group
clean_cut$status <- relevel(clean_cut$status, "On Academic Probation")
clean_cut2$status <- as.factor(clean_cut2$status)
clean_cut2$status <- relevel(clean_cut2$status, "On Academic Probation")
clean_cut3$status <- as.factor(clean_cut3$status)
clean_cut3$status <- relevel(clean_cut3$status, "On Academic Probation")

# Logistic Regression to estimate the impact of probation on student dropout

# Bandwidth 0.2
dropout_02 <- glm(left_school ~ status + dist_from_cut + gender + highHS + first_language ,family = "binomial", data = clean_cut)

#summary(dropout_02)

# Bandwidth 0.3
dropout_03 <- glm(left_school ~ status + dist_from_cut + gender + highHS + first_language ,family = "binomial", data = clean_cut2)

# Bandwidth 0.6
dropout_06 <- glm(left_school ~ status + dist_from_cut + gender + highHS + first_language ,family = "binomial", data = clean_cut3)

#### Summary Table ####

# Reference the following to customize modelsummary table: https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html
# Note: Broom package could not exponentiate the standard error right now
# Reference: https://github.com/tidymodels/broom/issues/422
modelsummary(list("0.2 GPA Bandwidth" = dropout_02, "0.3 GPA Bandwidth" = dropout_03,"0.6 GPA Bandwidth" =  dropout_06),
             statistic = "{std.error} ({p.value})",
             stars =TRUE,
             coef_map = c("(Intercept)" = "Constant", "statusIn Good Standing" = "In Good Standing", "dist_from_cut" = "f(Cutoff Distance)","genderMale" = "Gender Male", "highHS" = "Highschool Above Average", "first_languageOther" = "First Language Not English "),
             exponentiate = TRUE,
             title = "Logistic Regression for the Impact of Academic Probation on Student Dropout",
             gof_omit = "BIC") %>% 
  add_footnote("Odds Ratio in First Row. Standard Error of Log of Odds in Second Row. P-value in Parentheses", notation = "number") %>% 
  kable_styling(latex_options = "hold_position")

#### Figure ####

clean_cut$probation_year1 <- as.factor(clean_cut$probation_year1)
ggplot(data= clean_cut, aes(x = dist_from_cut, y = left_school, color = status))+
  geom_point(alpha = 0.05)+
  geom_smooth(method = "glm", method.args = list(family = 'binomial'))+
  theme_light()+
  theme(legend.title = element_blank(), legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  labs(title = "Academic Probation Impact on Dropout",
       x = "Year 1 GPA minus Probation Cutoff",
       y = "Left University")

###############################
#### Subsection: Year2 GPA ####
###############################

# Linear Regression to estimate the impact of probation on student Year 2 GPA

# Bandwidth 0.2
gpa_02 <- lm(GPA_year2 ~ status + dist_from_cut + gender + highHS + first_language, data = clean_cut)
#summary(gpa_02)
# Bandwidth 0.3
gpa_03 <- lm(GPA_year2 ~ status + dist_from_cut + gender + highHS + first_language, data = clean_cut2)

# Bandwidth 0.6
gpa_06 <- lm(GPA_year2 ~ status + dist_from_cut + gender + highHS + first_language, data = clean_cut3)

#### Summary Table ####

modelsummary(list("0.2 GPA Bandwidth" = gpa_02, "0.3 GPA Bandwidth" = gpa_03,"0.6 GPA Bandwidth" =  gpa_06),
             statistic = "{std.error} ({p.value})",
             stars =TRUE,
             coef_map = c("(Intercept)" = "Constant", "statusIn Good Standing" = "In Good Standing", "dist_from_cut" = "f(Cutoff Distance)","genderMale" = "Gender Male", "highHS" = "Highschool Above Average", "first_languageOther" = "First Language Not English "),
             title = "Linear Regression for the Impact of Academic Probation on Student Year 2 GPA",
             gof_omit = "BIC") %>% 
  add_footnote("Regression Coefficient in First Row. Standard Error in Second Row. P-value in Parentheses", notation = "number") %>% 
  kable_styling(latex_options = "hold_position")

#### Figure ####

ggplot(data= clean_cut, aes(x = dist_from_cut, y = GPA_year2, color = status))+
  geom_point(alpha = 0.05)+
  geom_smooth(method = "lm")+
  theme_light()+
  theme(legend.title = element_blank(), legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  labs(title = "Academic Probation Impact on Year 2 GPA",
       x = "Year 1 GPA minus Probation Cutoff",
       y = "Year 2 GPA")

################################
#### Subsection: Graduation ####
################################

# Logistic Regression to estimate the impact of probation on student dropout

# Bandwidth 0.2
grad_02 <- glm(gradin4 ~ status + dist_from_cut + gender + highHS + first_language ,family = "binomial", data = clean_cut)

#summary(grad_02)

# Bandwidth 0.3
grad_03 <- glm(gradin4 ~ status + dist_from_cut + gender + highHS + first_language ,family = "binomial", data = clean_cut2)

# Bandwidth 0.6
grad_06 <- glm(gradin4 ~ status + dist_from_cut + gender + highHS + first_language ,family = "binomial", data = clean_cut3)
#summary(grad_06)

#### Summary Table ####

modelsummary(list("0.2 GPA Bandwidth" = grad_02, "0.3 GPA Bandwidth" = grad_03,"0.6 GPA Bandwidth" =  grad_06),
             statistic = "{std.error} ({p.value})",
             stars =TRUE,
             coef_map = c("(Intercept)" = "Constant", "statusIn Good Standing" = "In Good Standing", "dist_from_cut" = "f(Cutoff Distance)","genderMale" = "Gender Male", "highHS" = "Highschool Above Average", "first_languageOther" = "First Language Not English "),
             exponentiate = TRUE,
             title = "Logistic Regression for the Impact of Academic Probation on Student Graduation by Year 4",
             gof_omit = "BIC") %>% 
  add_footnote("Odds Ratio in First Row. Standard Error of Log of Odds in Second Row. P-value in Parentheses", notation = "number") %>% 
  kable_styling(latex_options = "hold_position")

#### Figure  ####

clean_cut$probation_year1 <- as.factor(clean_cut$probation_year1)
ggplot(data= clean_cut, aes(x = dist_from_cut, y = gradin4, color = status))+
  geom_point(alpha = 0.05)+
  geom_smooth(method = "glm", method.args = list(family = 'binomial'))+
  theme_light()+
  theme(legend.title = element_blank(), legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  labs(title = "Academic Probation Impact on Graduation by Year 4",
       x = "Year 1 GPA minus Probation Cutoff",
       y = "Graduate by Year 4")

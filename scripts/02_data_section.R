#### Preamble ####
# Purpose: Codes for tables and plots in Data Section
# Author: Hong Shi
# Date:  11 April 2021
# Contact: lancehong.shi@mail.utoronto.ca
# License: MIT
# Pre-requisites: Need to have downloaded the "cleaned_original.csv" in "01_initial_clean.R" and saved it to inputs/data


#### Workspace set-up ####


# install.packages("haven")
library(tidyverse)
library(here)
library(haven)
library(finalfit)
library(kableExtra)
library(rdrobust)
library(broom)
#install.packages("rdpower")
library(rdpower)

#### Import Data ####

# Read in the cleaned data

clean_data <- read.csv(here("inputs/data/cleaned_original.csv"))

###################################
#### Subsection: Original Data ####
###################################

# Summary statistics for all students

# Referenced the following link to create this amazing summary table: https://finalfit.org/articles/all_tables_examples.html

# create column for summary statistics

clean_for_summary <-  clean_data %>% 
  mutate( gender = 
            case_when(sex == "M" ~ "Male",
                      sex == "F" ~ "Female"), 
          birthplace = case_when( bpl_north_america == 1 ~ "North America",
                                  bpl_asia == 1 ~ "Asia",
                                  bpl_other == 1 ~ "Other"),
          study_campus = case_when(loc_campus1 == 1 ~ "Campus 1",
                                   loc_campus2 == 1 ~ "Campus 2",
                                   loc_campus3 == 1 ~ "Campus 3"),
          first_language = case_when(mtongue == "English" ~ "English",
                                     mtongue == "Other" ~ "Other"),
          on_probation_after_1st_year = case_when( probation_year1 == 1 ~ "Yes",
                                                   probation_year1 == 0 ~ "No"),
          ever_on_probation = case_when( probation_ever == 1 ~ "Yes",
                                         probation_ever == 0 ~ "No"),
          left_university_after_1st_evaluation = case_when(left_school == 1 ~ "Yes",
                                                           left_school == 0 ~ "No"),
          ever_suspended = case_when(suspended_ever == 1 ~ "Yes",
                                     suspended_ever == 0 ~ "No"),
          graduated_by_year4 = case_when(gradin4 == 1 ~ "Yes",
                                         gradin4 == 0 ~ "No"),
          graduated_by_year5 = case_when(gradin5 == 1 ~ "Yes",
                                         gradin5 == 0 ~ "No"),
          graduated_by_year6 = case_when(gradin6 == 1 ~ "Yes",
                                         gradin6 == 0 ~ "No"),
          all_students = case_when(all == 1 ~ "All Students" ))

# Summary statistics for categorical variable
categorical1 <- clean_for_summary %>% 
  summary_factorlist("all_students", c("gender", "birthplace", "study_campus", "first_language", "on_probation_after_1st_year", "ever_on_probation", "left_university_after_1st_evaluation", "ever_suspended", "graduated_by_year4", "graduated_by_year5", "graduated_by_year6"),
                     p = FALSE, 
                     digits = c(1,1,3,1),
                     add_dependent_label = TRUE, 
                     dependent_label_prefix = "",
                     add_col_totals = TRUE,
                     include_row_missing_col = FALSE,
                     col_totals_rowname = "",
                     col_totals_prefix = "N(%) = ")

# Summary statistics for continuous variable

continuous1 <- clean_for_summary %>% 
  summary_factorlist("all_students", c("hsgrade_pct", "totcredits_year1", "age_at_entry", "dist_from_cut", "year2_dist_from_cut"),
                     p = FALSE,
                     digits = c(2,2,3,1),
                     add_dependent_label = TRUE, 
                     dependent_label_prefix = "",
                     add_col_totals = TRUE,
                     include_row_missing_col = FALSE,
                     col_totals_rowname = "",
                     col_totals_prefix = "N(%) = ")

# Add rows for visualization using kabel

combine1 <- rbind(categorical1, continuous1)
combine1 <- rbind(combine1, c("Characteristics", "",""))
combine1 <- rbind(combine1, c("Outcomes", "", ""))
combine1 <- rbind(combine1, c("","","Mean (SD)"))
combine1 <- rbind(combine1, c("","","Mean (SD)"))
combine1 <- rbind(combine1, c("","",""))
combine1 <- rbind(combine1, c("","",""))
combine1 <- rbind(combine1, c("","",""))
combine1$` ` <- replace(combine1$` `, combine1$` ` == "Mean (SD)", "")

# Manual Modification For Visualization

combine1$all_students <- NULL
combine1 <- combine1[c(32,1,3,2,5,4,6:11,36,34,27:29,37,33,26,13,12,15,14,17,16,19,18,21,20,23,22,25,24,38,35,30,31),] %>% 
  mutate(Description = c("Characteristics","","Gender","","Birth Place", "","","Study Campus","","", "First Language","","","","Highschool Grade Percentile","Credits Attempted in First Year", "Age at Entry","", "Outcomes","","On Probation After 1st Year","","Ever On Probation", "", "Left University After 1st Evaluation", "","Ever Suspended","","Graduated by Year 4","", "Graduated by Year 5","", "Graduated by Year 6","","","","Distance from Cutoff in 1st Year", "Distance from Cutoff in 2nd Year"))

# Check the number of samples in graduation and 2nd year GPA

graddata <- original_data %>% select(gradin4, gradin5, gradin6, year2_dist_from_cut)
grad_sample <- c(length(original_data$all), length(original_data$all), length(original_data$all), length(original_data$all)) - sapply(graddata,function(x) sum(is.na(x)))
grad_sample


# Generate summary table

kable(combine1[,c(3,1,2)],caption = "Summary Statistics of Observable Characteristics and Outcomoes of All Students",
      booktabs = TRUE, linesep = "") %>% 
  kable_styling() %>% 
  add_footnote("For all characteristics and outcomes except graduation rates and distance from cutoff in 2nd year. The entire dataset consists of 44362 students. Graduation rate samples are 30017 for Year 4, 24581 for Year 5, 19757 for Year 6.38576 students have GPA observed in 2nd Year", notation = "number")

########################################################
#### Subsection: Concerns about Bandwidth Selection ####
########################################################

##### Summary Statistics of Students Within 0.6 GPA of Probation Cutoff #####

# Lindo's bandwidth used the original paper
cut_data_lin <- clean_for_summary %>% 
  filter(dist_from_cut >= -0.6, dist_from_cut <= 0.6)

#Filter invalid data caused by university administrative error in data reporting

clean_cut2 <- cut_data_lin %>% 
  filter((dist_from_cut < 0 & probation_year1 ==1)| (dist_from_cut >=0 & probation_year1 ==0) )

clean_cut2 <- clean_cut2 %>% 
  mutate( status = case_when(probation_year1 == 0 ~ "In Good Standing",
                             probation_year1 == 1 ~ "On Academic Probation"))
categorical3 <- clean_cut2 %>% 
  summary_factorlist("status", c("gender", "birthplace", "study_campus", "first_language"),
                     p = FALSE, 
                     digits = c(1,1,3,1),
                     add_dependent_label = TRUE, 
                     dependent_label_prefix = "",
                     add_col_totals = TRUE,
                     include_row_missing_col = FALSE,
                     col_totals_rowname = "",
                     col_totals_prefix = "N(%) = ")
continuous3 <- clean_cut2 %>% 
  summary_factorlist("status", c("hsgrade_pct", "totcredits_year1", "age_at_entry"),
                     p = FALSE,
                     digits = c(2,2,3,1),
                     add_dependent_label = TRUE, 
                     dependent_label_prefix = "",
                     add_col_totals = TRUE,
                     include_row_missing_col = FALSE,
                     col_totals_rowname = "",
                     col_totals_prefix = "N(%) = ")

combine3 <- rbind(categorical3, continuous3)
combine3 <- rbind(combine3, c("","","", ""))
combine3 <- rbind(combine3, c("","","Mean (SD)", "Mean (SD)"))
combine3 <- combine3[-c(12),]
combine3$` ` <- replace(combine3$` `, combine3$` ` == "Mean (SD)", "")
combine3$status <- NULL
combine3 <- combine3[c(1,3,2,5,4,6:11,15,16,12,13,14),] %>% 
  mutate(Description = c("","Gender","","Birth Place", "","","Study Campus","","", "First Language","","","","Highschool Grade Percentile","Credits Attempted in First Year", "Age at Entry"))

kable(combine3[,c(4,1,2,3)],caption = "Summary Statistics of Students Within 0.6 GPA of Probation Cutoff",
      booktabs = TRUE, linesep = "") %>% 
  kable_styling(latex_options = "hold_position")


##### T test with 0.6 GPA bandwidth #####

# conduct the t test on highschool grade between treatment and comparison group within 0.6 GPA cutoff score
clean_cut_prob1 <- clean_cut2 %>% 
  filter(dist_from_cut < 0)
clean_cut_good1 <- clean_cut2 %>% 
  filter(dist_from_cut >= 0)
t.test(clean_cut_prob1$hsgrade_pct, clean_cut_good1$hsgrade_pct) %>% 
  tidy() %>%
  select(-c(estimate,statistic,parameter, alternative))%>%
  mutate_if(is.numeric, format, digits=4)%>%
  rename( `On Probation` = estimate1,
          `In Good Standing` = estimate2,
          `P-value` = p.value,
          `95% CI Low` = conf.low,
          `95% CI High` = conf.high,
          Method = method) %>% 
  kableExtra::kable(caption="Two Sided T-test on Student Highschool Grade Percentile within 0.6 GPA cutoff", align = "cccccc")%>%
  kable_styling(latex_options = "hold_position")

# conduct the t test on credit attempted between treatment and comparison group within 0.6 GPA cutoff score
clean_cut_prob1 <- clean_cut2 %>% 
  filter(dist_from_cut < 0)
clean_cut_good1 <- clean_cut2 %>% 
  filter(dist_from_cut >= 0)
t.test(clean_cut_prob1$totcredits_year1, clean_cut_good1$totcredits_year1) %>% 
  tidy() %>%
  select(-c(estimate, statistic,parameter, alternative))%>%
  mutate_if(is.numeric, format, digits=4)%>%
  rename( `On Probation` = estimate1,
          `In Good Standing` = estimate2,
          `P-value` = p.value,
          `95% CI Low` = conf.low,
          `95% CI High` = conf.high,
          Method = method) %>% 
  kable(caption="Two Sided T-test on Student Credits Attempted in Year 1 within 0.6 GPA cutoff",align = "cccccc")%>%
  kable_styling(latex_options = "hold_position")


##### Grading scheme of this Canadian university #####

# Grading scheme obtained from the university website
tibble(
  "Grade" = list("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D", "D-", "F"),
  "Grade Point Value" = list("4.0", "4.0", "3.7", "3.3", "3.0", "2.7", "2.3", "2.0", "1.7", "1.3", "1.0", "0.7", "0.0"),
  "Grade Percentage" = list("90-100", "85-89","80-84","77-79", "73-76","70-72","67-69","63-66","60-62","57-59","53-56","50-52","0-49"),
  "Definition" = list("Excellent","","","Good","","","Adequate","","","Marginal","","","Inadequate; no credit obtained")
) %>% 
  kable(caption = "Grading Scheme of the University",
        align = "cccc") %>% 
  kable_styling(latex_options = "hold_position")


#####  Summary Statistics of Students Within 0.6 GPA of Probation Cutoff #####

cut_data <- clean_for_summary %>% 
  filter(dist_from_cut >= -0.2, dist_from_cut <= 0.2)

#Filter invalid data caused by university administrative error in data reporting

clean_cut <- cut_data %>% 
  filter((dist_from_cut < 0 & probation_year1 ==1)| (dist_from_cut >=0 & probation_year1 ==0))

clean_cut <- clean_cut %>% 
  mutate( status = case_when(probation_year1 == 0 ~ "In Good Standing",
                             probation_year1 == 1 ~ "On Academic Probation"))
categorical2 <- clean_cut %>% 
  summary_factorlist("status", c("gender", "birthplace", "study_campus", "first_language"),
                     p = FALSE, 
                     digits = c(1,1,3,1),
                     add_dependent_label = TRUE, 
                     dependent_label_prefix = "",
                     add_col_totals = TRUE,
                     include_row_missing_col = FALSE,
                     col_totals_rowname = "",
                     col_totals_prefix = "N(%) = ")
continuous2 <- clean_cut %>% 
  summary_factorlist("status", c("hsgrade_pct", "totcredits_year1", "age_at_entry"),
                     p = FALSE,
                     digits = c(2,2,3,1),
                     add_dependent_label = TRUE, 
                     dependent_label_prefix = "",
                     add_col_totals = TRUE,
                     include_row_missing_col = FALSE,
                     col_totals_rowname = "",
                     col_totals_prefix = "N(%) = ")
combine2 <- rbind(categorical2, continuous2)
combine2 <- rbind(combine2, c("","","", ""))
combine2 <- rbind(combine2, c("","","Mean (SD)", "Mean (SD)"))
combine2 <- combine2[-c(12),]
combine2$` ` <- replace(combine2$` `, combine2$` ` == "Mean (SD)", "")
combine2$status <- NULL
combine2 <- combine2[c(1,3,2,5,4,6:11,15,16,12,13,14),] %>% 
  mutate(Description = c("","Gender","","Birth Place", "","","Study Campus","","", "First Language","","","","Highschool Grade Percentile","Credits Attempted in First Year", "Age at Entry"))

kable(combine2[,c(4,1,2,3)],caption = "Summary Statistics of Students Within 0.2 GPA of Probation Cutoff",
      booktabs = TRUE, linesep = "") %>% 
  kable_styling(latex_options = "hold_position")

###### T test with 0.2 bandwidth #####

# conduct the t test on highschool grade between treatment and comparison group within 0.2 GPA cutoff score
clean_cut_prob <- clean_cut %>% 
  filter(dist_from_cut < 0)
clean_cut_good <- clean_cut %>% 
  filter(dist_from_cut >= 0)
t.test(clean_cut_prob$hsgrade_pct, clean_cut_good$hsgrade_pct) %>% 
  tidy() %>%
  select(-c(estimate,statistic,parameter, alternative))%>%
  mutate_if(is.numeric, format, digits=4)%>%
  rename( `On Probation` = estimate1,
          `In Good Standing` = estimate2,
          `P-value` = p.value,
          `95% CI Low` = conf.low,
          `95% CI High` = conf.high,
          Method = method) %>% 
  kableExtra::kable(caption="Two Sided T-test on Student Highschool Grade Percentile within 0.2 GPA cutoff", align = "cccccc")%>%
kable_styling(latex_options = "hold_position")


# conduct the t test on credit attempted between treatment and comparison group within 0.2 GPA cutoff score
clean_cut_prob <- clean_cut %>% 
  filter(dist_from_cut < 0)
clean_cut_good <- clean_cut %>% 
  filter(dist_from_cut >= 0)
t.test(clean_cut_prob$totcredits_year1, clean_cut_good$totcredits_year1) %>% 
  tidy() %>%
  select(-c(estimate, statistic,parameter, alternative))%>%
  mutate_if(is.numeric, format, digits=4)%>%
  rename( `On Probation` = estimate1,
          `In Good Standing` = estimate2,
          `P-value` = p.value,
          `95% CI Low` = conf.low,
          `95% CI High` = conf.high,
          Method = method) %>% 
  kable(caption="Two Sided T-test on Student Credits Attempted in Year 1 within 0.2 GPA cutoff",align = "cccccc")%>%
  kable_styling(latex_options = "hold_position")

##### Effect size to raise sufficient power #####

# Calculate sample size required to raise sufficient power to estimate the probation effect on leaving school
# Referenced https://rdrr.io/cran/rdpower/man/rdpower-package.html to use the rdsampsi function

# Note: first order polynomial is used to calculated the effect size
power_data <- clean_for_summary
Y1 <- power_data$left_school
R <- power_data$dist_from_cut
Z1 <- power_data[c("left_school", "dist_from_cut")]
#test_power <- rdpower(data = Z)
left_sample <- rdsampsi(data = Z1, cutoff = 0)

# Calculate sample size required to raise sufficient power to estimate the probation effect on second year GPA
Y2 <- power_data$GPA_year2
R <- power_data$dist_from_cut
Z2 <- power_data[c("GPA_year2", "dist_from_cut")]
#test_power <- rdpower(data = Z)
GPA_sample <- rdsampsi(data = Z2, cutoff = 0)

# Calculate sample size required to raise sufficient power to estimate the probation effect on graduation by year 4
Y3 <- power_data$gradin4
R <- power_data$dist_from_cut
Z3 <- power_data[c("gradin4", "dist_from_cut")]
#test_power <- rdpower(data = Z)
grad4_sample <- rdsampsi(data = Z3, cutoff = 0)

tibble(
  Interest = list("Dropout", "GPA in Year 2", "Graduation by Year 4"),
  `Total Student` = list(left_sample$sampsi.h.tot,GPA_sample$sampsi.h.tot,grad4_sample$sampsi.h.tot),
  `On Probation` = list(left_sample$sampsi.h.r, GPA_sample$sampsi.h.r, grad4_sample$sampsi.h.r),
  `In Goodstanding` = list(left_sample$sampsi.h.l, GPA_sample$sampsi.h.l, grad4_sample$sampsi.h.l)
) %>% 
  kable(caption = "Sample Required to Raise Sufficient Power", align = "cccc") %>% 
  kable_styling(latex_options = "hold_position") %>% 
  add_footnote("The significance level is 0.05 and desired power is 0.8.", notation = "number")


####Also include data with bandwidth 0.3 for Result Section ####
cut_data3 <- clean_for_summary %>% 
  filter(dist_from_cut >= -0.3, dist_from_cut <= 0.3)

#Filter invalid data caused by university administrative error in data reporting

clean_cut3 <- cut_data3 %>% 
  filter((dist_from_cut < 0 & probation_year1 ==1)| (dist_from_cut >=0 & probation_year1 ==0) )

clean_cut3 <- clean_cut3 %>% 
  mutate( status = case_when(probation_year1 == 0 ~ "In Good Standing",
                             probation_year1 == 1 ~ "On Academic Probation"))

##### Write the csv file for further analysis ####

write_csv(clean_for_summary,here::here("inputs/data/summary_data.csv"))
write_csv(clean_cut,here::here("inputs/data/bandwidth02.csv"))
write_csv(clean_cut2,here::here("inputs/data/bandwidth06.csv"))
write_csv(clean_cut3,here::here("inputs/data/bandwidth03.csv"))

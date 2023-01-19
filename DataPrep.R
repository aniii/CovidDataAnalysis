library("tidyverse")
library("ggplot2")
library("DT")
library("corrplot")
library(GGally)
library(clValid)
library(dbscan)
library(caret)
.jinit()
library(rJava)
library(FSelector)
#setwd("Documents/SMU_DataMining")
#read data
df <- read_csv("dm1.csv")
df_trends <- read_csv("WOW_COVIDdf.csv")

df <- df[order(df$county_name, decreasing = FALSE), ]
cases <- df %>% mutate_if(is.character, factor)
cases <- cases %>% filter(confirmed_cases > 0) 

cases <- cases %>% 
  arrange(desc(confirmed_cases)) #%>%    
#select(county_name, state, confirmed_cases, deaths, total_pop, median_income, median_age)

cases <- cases %>% mutate(
  cases_per_10000 = confirmed_cases/total_pop*10000, 
  deaths_per_10000 = deaths/total_pop*10000, 
  death_per_case = deaths/confirmed_cases,
  family_households_per_10000 = family_households/total_pop*10000,
  rent_under_15_percent_per_10000=(rent_under_10_percent +rent_10_to_15_percent+rent_15_to_20_percent)/total_pop*10000,
  black_pop_per_10000=black_pop/total_pop*10000,
  asian_pop_per_10000=asian_pop/total_pop*10000 ,
  hispanic_pop_per_10000=hispanic_pop/total_pop*10000 ,
  other_race_pop_per_10000=other_race_pop/total_pop*10000,
  commuters_by_public_transportation_per_10000=commuters_by_public_transportation/total_pop*10000,
  families_with_young_children_per_10000=families_with_young_children/total_pop*10000,
  one_parent_families_with_young_children=one_parent_families_with_young_children/total_pop*10000,
  income_less_50000_per_10000 = (income_less_10000+income_10000_14999+income_15000_19999+
                                   income_20000_24999+income_25000_29999+income_30000_34999+
                                   income_35000_39999+income_40000_44999+income_45000_49999)/total_pop*10000,
  income_50000_100000_per_10000 = (income_50000_59999+ income_60000_74999+income_75000_99999)/total_pop*10000,
  income_100000_or_more_per_10000 = (income_100000_124999+income_125000_149999+income_150000_199999+
                                       income_200000_or_more)/total_pop*10000,
  #/total_pop*10000,
  commute_less_10_mins_per_10000=commute_less_10_mins/total_pop*10000,
  
  commute_more_10_mins_per_10000 = (commute_10_14_mins+commute_35_44_mins+
                                      commute_15_19_mins + commute_20_24_mins+commute_25_29_mins+commute_30_34_mins+
                                      commute_45_59_mins+commute_35_39_mins+commute_40_44_mins+commute_60_89_mins+
                                      commute_90_more_mins)/total_pop*10000,
  
  dwellings_less_10_units_attached_per_10000 = (dwellings_1_units_attached+dwellings_2_units+
                                                  dwellings_3_to_4_units+dwellings_5_to_9_units)/total_pop*10000,
  dwellings_more_10_units_per_10000 = (dwellings_10_to_19_units+dwellings_20_to_49_units+
                                         dwellings_50_or_more_units)/total_pop*10000,
  mobile_homes_per_10000=mobile_homes/total_pop*10000,
  male_under_20_per_10000 = (male_under_5+male_5_to_9+male_10_to_14+male_15_to_17+male_18_to_19)/total_pop*10000,
  male_20_to_59_per_10000 = (male_20+male_21+male_22_to_24+male_25_to_29+male_30_to_34+male_35_to_39+
                               male_40_to_44+male_45_to_49+male_50_to_54+male_55_to_59)/total_pop*10000,
  male_over_59_per_10000 = (male_60_61+male_62_64+male_65_to_66+male_67_to_69+male_70_to_74+
                              male_75_to_79+male_80_to_84+male_85_and_over)/total_pop*10000,
  female_under_20_per_10000 =(female_under_5+female_5_to_9+female_10_to_14+female_15_to_17+female_18_to_19)/total_pop*10000,
  female_20_to_59_per_10000 = (female_20+female_21+female_22_to_24+female_25_to_29+female_30_to_34+
                                 female_35_to_39+female_40_to_44+female_45_to_49+female_50_to_54+female_55_to_59)/total_pop*10000,
  female_over_59_per_10000 = (female_60_to_61+female_62_to_64+female_65_to_66+female_67_to_69+
                                female_70_to_74+female_75_to_79+female_80_to_84+female_85_and_over)/total_pop*10000,
  employed_pop_per_10000=employed_pop/total_pop*10000,
  pop_in_labor_force_per_10000 =pop_in_labor_force/total_pop*10000,
  # asian_male_45_54=asian_male_45_54/total_pop*10000,
  asian_male_55_64_per_10000=asian_male_55_64/total_pop*10000,
  #black_male_45_54=black_male_45_54/total_pop*10000,
  black_male_55_64_per_10000=black_male_55_64/total_pop*10000,
  # hispanic_male_45_54=hispanic_male_45_54/total_pop*10000,
  hispanic_male_55_64_per_10000=hispanic_male_55_64/total_pop*10000,
  # white_male_45_54=white_male_45_54/total_pop*10000,
  white_male_55_64_per_10000=white_male_55_64/total_pop*10000
)
cases <- cases %>% 
  left_join(df_trends, by = c('county_name', 'state') )
names(df_trends)
############family_households_per_10000
hist( cases$family_households_per_10000, main="Histogram for commuters", xlab="commuters")
boxplot(cases$family_households_per_10000)
temp_median = median(cases$family_households_per_10000,na.rm=TRUE)
temp_median
Q <- quantile(cases$family_households_per_10000 ,probs=c(.25,0.5 ,.75), na.rm = FALSE)
Q
iqr <- IQR(cases$family_households_per_10000)
temp_median = median(cases$family_households_per_10000,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
low
#cases_county$outlier <- (cases_county$commuters < (Q[1] - 1.5*iqr) & cases_county$commuters > (Q[2]+1.5*iqr))
cases$outlier <- (cases$family_households_per_10000 < low)
cases$family_households_per_10000[cases$outlier] = low
boxplot(cases$family_households_per_10000)
cases$outlier <- (cases$family_households_per_10000 > up)
cases$family_households_per_10000[cases$outlier] = up
boxplot(cases$family_households_per_10000, xlab="family_households_per_10000")
hist( cases$family_households_per_10000, main="Histogram for family_households_per_10000", xlab="family_households_per_10000")

############rent_under_15_percent_per_10000
hist( cases$rent_under_15_percent_per_10000, main="Histogram for commuters", xlab="commuters")
boxplot(cases$rent_under_15_percent_per_10000)
temp_median = median(cases$rent_under_15_percent_per_10000,na.rm=TRUE)
temp_median
Q <- quantile(cases$rent_under_15_percent_per_10000 ,probs=c(.25,0.5 ,.75), na.rm = FALSE)
Q
iqr <- IQR(cases$rent_under_15_percent_per_10000)
temp_median = median(cases$rent_under_15_percent_per_10000,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
low
cases$outlier <- (cases$rent_under_15_percent_per_10000 > up)
cases$rent_under_15_percent_per_10000[cases$outlier] = up
boxplot(cases$rent_under_15_percent_per_10000, xlab="rent_under_15_percent_per_10000")
hist( cases$rent_under_15_percent_per_10000, main="Histogram for rent_under_15_percent_per_10000", xlab="rent_under_15_percent_per_10000")

############income_less_50000_per_10000
hist( cases$income_less_50000_per_10000, main="Histogram for commuters", xlab="commuters")
boxplot(cases$income_less_50000_per_10000)
temp_median = median(cases$income_less_50000_per_10000,na.rm=TRUE)
temp_median
Q <- quantile(cases$income_less_50000_per_10000 ,probs=c(.25,0.5 ,.75), na.rm = FALSE)
Q
iqr <- IQR(cases$income_less_50000_per_10000)
temp_median = median(cases$income_less_50000_per_10000,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
low
cases$outlier <- (cases$income_less_50000_per_10000 < low)
cases$income_less_50000_per_10000[cases$outlier] = low
cases$outlier <- (cases$income_less_50000_per_10000 > up)
cases$income_less_50000_per_10000[cases$outlier] = up
boxplot(cases$income_less_50000_per_10000, xlab="income_less_50000_per_10000")
hist( cases$income_less_50000_per_10000, main="Histogram for income_less_50000_per_10000", xlab="income_less_50000_per_10000")

############black_pop_per_10000
hist( cases$black_pop_per_10000, main="Histogram for commuters", xlab="commuters")
boxplot(cases$black_pop_per_10000)
temp_median = median(cases$black_pop_per_10000,na.rm=TRUE)
temp_median
Q <- quantile(cases$black_pop_per_10000 ,probs=c(.25,0.5 ,.75), na.rm = FALSE)
Q
iqr <- IQR(cases$black_pop_per_10000)
temp_median = median(cases$black_pop_per_10000,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
#cases$outlier <- (cases$income_less_50000_per_10000 < low)
#cases$income_less_50000_per_10000[cases$outlier] = low
cases$outlier <- (cases$black_pop_per_10000 > up)
cases$black_pop_per_10000[cases$outlier] = up
boxplot(cases$black_pop_per_10000, xlab="black_pop_per_10000")
hist( cases$black_pop_per_10000, main="Histogram for black_pop_per_10000", xlab="black_pop_per_10000")

############asian_pop_per_10000
hist( cases$asian_pop_per_10000, main="Histogram for commuters", xlab="commuters")
boxplot(cases$asian_pop_per_10000)
temp_median = median(cases$asian_pop_per_10000,na.rm=TRUE)
temp_median
Q <- quantile(cases$asian_pop_per_10000 ,probs=c(.25,0.5 ,.75), na.rm = FALSE)
Q
iqr <- IQR(cases$asian_pop_per_10000)
temp_median = median(cases$asian_pop_per_10000,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
#cases$outlier <- (cases$income_less_50000_per_10000 < low)
#cases$income_less_50000_per_10000[cases$outlier] = low
cases$outlier <- (cases$asian_pop_per_10000 > up)
cases$asian_pop_per_10000[cases$outlier] = up
boxplot(cases$asian_pop_per_10000, xlab="asian_pop_per_10000")
hist( cases$asian_pop_per_10000, main="Histogram for asian_pop_per_10000", xlab="asian_pop_per_10000")

############hispanic_pop_per_10000
hist( cases$hispanic_pop_per_10000, main="Histogram for commuters", xlab="commuters")
boxplot(cases$hispanic_pop_per_10000)
temp_median = median(cases$hispanic_pop_per_10000,na.rm=TRUE)
temp_median
Q <- quantile(cases$hispanic_pop_per_10000 ,probs=c(.25,0.5 ,.75), na.rm = FALSE)
Q
iqr <- IQR(cases$hispanic_pop_per_10000)
temp_median = median(cases$hispanic_pop_per_10000,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
#cases$outlier <- (cases$income_less_50000_per_10000 < low)
#cases$income_less_50000_per_10000[cases$outlier] = low
cases$outlier <- (cases$hispanic_pop_per_10000 > up)
cases$hispanic_pop_per_10000[cases$outlier] = up
boxplot(cases$hispanic_pop_per_10000, xlab="hispanic_pop_per_10000")
hist( cases$hispanic_pop_per_10000, main="Histogram for hispanic_pop_per_10000", xlab="hispanic_pop_per_10000")

############other_race_pop_per_10000
hist( cases$other_race_pop_per_10000, main="Histogram for commuters", xlab="commuters")
boxplot(cases$other_race_pop_per_10000)
temp_median = median(cases$other_race_pop_per_10000,na.rm=TRUE)
temp_median
Q <- quantile(cases$other_race_pop_per_10000 ,probs=c(.25,0.5 ,.75), na.rm = FALSE)
Q
iqr <- IQR(cases$other_race_pop_per_10000)
temp_median = median(cases$other_race_pop_per_10000,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
#cases$outlier <- (cases$income_less_50000_per_10000 < low)
#cases$income_less_50000_per_10000[cases$outlier] = low
cases$outlier <- (cases$other_race_pop_per_10000 > up)
cases$other_race_pop_per_10000[cases$outlier] = up
boxplot(cases$other_race_pop_per_10000, xlab="other_race_pop_per_10000")
hist( cases$other_race_pop_per_10000, main="Histogram for other_race_pop_per_10000", xlab="other_race_pop_per_10000")

############commuters_by_public_transportation_per_10000
hist( cases$commuters_by_public_transportation_per_10000, main="Histogram for commuters", xlab="commuters")
boxplot(cases$commuters_by_public_transportation_per_10000)
temp_median = median(cases$commuters_by_public_transportation_per_10000,na.rm=TRUE)
temp_median
Q <- quantile(cases$commuters_by_public_transportation_per_10000 ,probs=c(.25,0.5 ,.75), na.rm = FALSE)
Q
iqr <- IQR(cases$commuters_by_public_transportation_per_10000)
temp_median = median(cases$commuters_by_public_transportation_per_10000,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
#cases$outlier <- (cases$income_less_50000_per_10000 < low)
#cases$income_less_50000_per_10000[cases$outlier] = low
cases$outlier <- (cases$commuters_by_public_transportation_per_10000 > up)
cases$commuters_by_public_transportation_per_10000[cases$outlier] = up
boxplot(cases$commuters_by_public_transportation_per_10000 , xlab="commuters_by_public_transportation_per_10000")
hist( cases$commuters_by_public_transportation_per_10000, main="Histogram for commuters_by_public_transportation_per_10000", xlab="commuters")

############median_income
hist( cases$median_income, main="Histogram for commuters", xlab="commuters")
boxplot(cases$median_income)
temp_median = median(cases$median_income,na.rm=TRUE)
temp_median
Q <- quantile(cases$median_income ,probs=c(.25,0.5 ,.75), na.rm = FALSE)
Q
iqr <- IQR(cases$median_income)
temp_median = median(cases$median_income,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
#cases$outlier <- (cases$income_less_50000_per_10000 < low)
#cases$income_less_50000_per_10000[cases$outlier] = low
cases$outlier <- (cases$median_income > up)
cases$median_income[cases$outlier] = up
boxplot(cases$median_income, xlab="median_income")
hist( cases$median_income, main="Histogram for commuters", xlab="commuters")

############income_per_capita
hist( cases$income_per_capita, main="Histogram for commuters", xlab="commuters")
boxplot(cases$income_per_capita)
temp_median = median(cases$income_per_capita,na.rm=TRUE)
temp_median
Q <- quantile(cases$income_per_capita ,probs=c(.25,0.5 ,.75), na.rm = FALSE)
Q
iqr <- IQR(cases$income_per_capita)
temp_median = median(cases$income_per_capita,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
#cases$outlier <- (cases$income_less_50000_per_10000 < low)
#cases$income_less_50000_per_10000[cases$outlier] = low
cases$outlier <- (cases$income_per_capita > up)
cases$income_per_capita[cases$outlier] = up
boxplot(cases$income_per_capita)
hist( cases$income_per_capita, main="Histogram for commuters", xlab="commuters")

############percent_income_spent_on_rent
#hist( cases$percent_income_spent_on_rent, main="Histogram for commuters", xlab="commuters")
#boxplot(cases$percent_income_spent_on_rent)
#temp_median = median(cases$percent_income_spent_on_rent,na.rm=TRUE)
#temp_median
#Q <- quantile(cases$percent_income_spent_on_rent ,probs=c(.25,0.5 ,.75), na.rm = TRUE)
#Q
#iqr <- IQR(cases$percent_income_spent_on_rent, na.rm = TRUE)
#temp_median = median(cases$percent_income_spent_on_rent,na.rm=TRUE)
#up <-  Q[3]+1.5*iqr # Upper Range  
#low<- Q[1]- 1.5*iqr # Lower Range
#cases$outlier <- (cases$percent_income_spent_on_rent < low)
#cases$percent_income_spent_on_rent[cases$outlier] = low
#cases$outlier <- (cases$percent_income_spent_on_rent > up)
#cases$percent_income_spent_on_rent[cases$outlier] = up
#boxplot(cases$percent_income_spent_on_rent)
#hist( cases$percent_income_spent_on_rent, main="Histogram for commuters", xlab="commuters")

############million_dollar_housing_units
hist( cases$million_dollar_housing_units, main="Histogram for commuters", xlab="commuters")
boxplot(cases$million_dollar_housing_units)
temp_median = median(cases$million_dollar_housing_units,na.rm=TRUE)
temp_median
Q <- quantile(cases$million_dollar_housing_units ,probs=c(.25,0.5 ,.75), na.rm = TRUE)
Q
iqr <- IQR(cases$million_dollar_housing_units, na.rm = TRUE)
temp_median = median(cases$million_dollar_housing_units,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
#cases$outlier <- (cases$million_dollar_housing_units < low)
#cases$million_dollar_housing_units[cases$outlier] = low
cases$outlier <- (cases$million_dollar_housing_units > up)
cases$million_dollar_housing_units[cases$outlier] = up
boxplot(cases$million_dollar_housing_units, xlab="million_dollar_housing_units")
hist( cases$million_dollar_housing_units, main="Histogram for commuters", xlab="commuters")

############families_with_young_children_per_10000
hist( cases$families_with_young_children_per_10000, main="Histogram for commuters", xlab="commuters")
boxplot(cases$families_with_young_children_per_10000)
temp_median = median(cases$families_with_young_children_per_10000,na.rm=TRUE)
temp_median
Q <- quantile(cases$families_with_young_children_per_10000 ,probs=c(.25,0.5 ,.75), na.rm = TRUE)
Q
iqr <- IQR(cases$families_with_young_children_per_10000, na.rm = TRUE)
temp_median = median(cases$families_with_young_children_per_10000,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
cases$outlier <- (cases$families_with_young_children_per_10000 < low)
cases$families_with_young_children_per_10000[cases$outlier] = low
cases$outlier <- (cases$families_with_young_children_per_10000 > up)
cases$families_with_young_children_per_10000[cases$outlier] = up
boxplot(cases$families_with_young_children_per_10000 , xlab="families_with_young_children_per_10000")
hist( cases$families_with_young_children_per_10000, main="Histogram for commuters", xlab="commuters")

############income_50000_100000_per_10000
hist( cases$income_50000_100000_per_10000, main="Histogram for commuters", xlab="commuters")
boxplot(cases$income_50000_100000_per_10000)
temp_median = median(cases$income_50000_100000_per_10000,na.rm=TRUE)
temp_median
Q <- quantile(cases$income_50000_100000_per_10000 ,probs=c(.25,0.5 ,.75), na.rm = TRUE)
Q
iqr <- IQR(cases$income_50000_100000_per_10000, na.rm = TRUE)
temp_median = median(cases$income_50000_100000_per_10000,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
cases$outlier <- (cases$income_50000_100000_per_10000 < low)
cases$income_50000_100000_per_10000[cases$outlier] = low
cases$outlier <- (cases$income_50000_100000_per_10000 > up)
cases$income_50000_100000_per_10000[cases$outlier] = up
boxplot(cases$income_50000_100000_per_10000, xlab="income_50000_100000_per_10000")
hist( cases$income_50000_100000_per_10000, main="Histogram for commuters", xlab="income_50000_100000_per_10000")

############income_100000_or_more_per_10000
hist( cases$income_100000_or_more_per_10000, main="Histogram for commuters", xlab="commuters")
boxplot(cases$income_100000_or_more_per_10000)
temp_median = median(cases$income_100000_or_more_per_10000,na.rm=TRUE)
temp_median
Q <- quantile(cases$income_100000_or_more_per_10000 ,probs=c(.25,0.5 ,.75), na.rm = TRUE)
Q
iqr <- IQR(cases$income_100000_or_more_per_10000, na.rm = TRUE)
temp_median = median(cases$income_100000_or_more_per_10000,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
#cases$outlier <- (cases$income_50000_100000_per_10000 < low)
#cases$income_50000_100000_per_10000[cases$outlier] = low
cases$outlier <- (cases$income_100000_or_more_per_10000 > up)
cases$income_100000_or_more_per_10000[cases$outlier] = up
boxplot(cases$income_100000_or_more_per_10000, xlab="income_100000_or_more_per_10000")
hist( cases$income_50000_100000_per_10000, main="Histogram for commuters", xlab="income_100000_or_more_per_10000")


############commute_less_10_mins_per_10000
hist( cases$commute_less_10_mins_per_10000, main="Histogram for commuters", xlab="commuters")
boxplot(cases$commute_less_10_mins_per_10000)
temp_median = median(cases$commute_less_10_mins_per_10000,na.rm=TRUE)
temp_median
Q <- quantile(cases$commute_less_10_mins_per_10000 ,probs=c(.25,0.5 ,.75), na.rm = TRUE)
Q
iqr <- IQR(cases$commute_less_10_mins_per_10000, na.rm = TRUE)
temp_median = median(cases$commute_less_10_mins_per_10000,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
#cases$outlier <- (cases$income_50000_100000_per_10000 < low)
#cases$income_50000_100000_per_10000[cases$outlier] = low
cases$outlier <- (cases$commute_less_10_mins_per_10000 > up)
cases$commute_less_10_mins_per_10000[cases$outlier] = up
boxplot(cases$commute_less_10_mins_per_10000, xlab="commute_less_10_mins_per_10000")
hist( cases$commute_less_10_mins_per_10000, main="Histogram for commuters", xlab="commuters")

############dwellings_less_10_units_attached_per_10000
hist( cases$dwellings_less_10_units_attached_per_10000, main="Histogram for commuters", xlab="commuters")
boxplot(cases$dwellings_less_10_units_attached_per_10000)
temp_median = median(cases$dwellings_less_10_units_attached_per_10000,na.rm=TRUE)
temp_median
Q <- quantile(cases$dwellings_less_10_units_attached_per_10000 ,probs=c(.25,0.5 ,.75), na.rm = TRUE)
Q
iqr <- IQR(cases$dwellings_less_10_units_attached_per_10000, na.rm = TRUE)
temp_median = median(cases$dwellings_less_10_units_attached_per_10000,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
#cases$outlier <- (cases$income_50000_100000_per_10000 < low)
#cases$income_50000_100000_per_10000[cases$outlier] = low
cases$outlier <- (cases$dwellings_less_10_units_attached_per_10000 > up)
cases$dwellings_less_10_units_attached_per_10000[cases$outlier] = up
boxplot(cases$dwellings_less_10_units_attached_per_10000, xlab="dwellings_less_10_units_attached_per_10000")

hist( cases$dwellings_less_10_units_attached_per_10000, main="Histogram for commuters", xlab="commuters")

############dwellings_more_10_units_per_10000
hist( cases$dwellings_more_10_units_per_10000, main="Histogram for commuters", xlab="commuters")
boxplot(cases$dwellings_more_10_units_per_10000)
temp_median = median(cases$dwellings_more_10_units_per_10000,na.rm=TRUE)
temp_median
Q <- quantile(cases$dwellings_more_10_units_per_10000 ,probs=c(.25,0.5 ,.75), na.rm = TRUE)
Q
iqr <- IQR(cases$dwellings_more_10_units_per_10000, na.rm = TRUE)
temp_median = median(cases$dwellings_more_10_units_per_10000,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
#cases$outlier <- (cases$income_50000_100000_per_10000 < low)
#cases$income_50000_100000_per_10000[cases$outlier] = low
cases$outlier <- (cases$dwellings_more_10_units_per_10000 > up)
cases$dwellings_more_10_units_per_10000[cases$outlier] = up
boxplot(cases$dwellings_more_10_units_per_10000, xlab="dwellings_more_10_units_per_10000")
hist( cases$dwellings_more_10_units_per_10000, main="Histogram for commuters", xlab="commuters")

############male_under_20_per_10000
hist( cases$male_under_20_per_10000, main="Histogram for commuters", xlab="commuters")
boxplot(cases$male_under_20_per_10000)
temp_median = median(cases$male_under_20_per_10000,na.rm=TRUE)
temp_median
Q <- quantile(cases$male_under_20_per_10000 ,probs=c(.25,0.5 ,.75), na.rm = TRUE)
Q
iqr <- IQR(cases$male_under_20_per_10000, na.rm = TRUE)
temp_median = median(cases$male_under_20_per_10000,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
cases$outlier <- (cases$male_under_20_per_10000 < low)
cases$male_under_20_per_10000[cases$outlier] = low
cases$outlier <- (cases$male_under_20_per_10000 > up)
cases$male_under_20_per_10000[cases$outlier] = up
boxplot(cases$male_under_20_per_10000, xlab="male_under_20_per_10000")
hist( cases$male_under_20_per_10000, main="Histogram for commuters", xlab="commuters")

############male_20_to_59_per_10000
hist( cases$male_20_to_59_per_10000, main="Histogram for commuters", xlab="commuters")
boxplot(cases$male_20_to_59_per_10000)
temp_median = median(cases$male_20_to_59_per_10000,na.rm=TRUE)
temp_median
Q <- quantile(cases$male_20_to_59_per_10000 ,probs=c(.25,0.5 ,.75), na.rm = TRUE)
Q
iqr <- IQR(cases$male_20_to_59_per_10000, na.rm = TRUE)
temp_median = median(cases$male_20_to_59_per_10000,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
cases$outlier <- (cases$male_20_to_59_per_10000 < low)
cases$male_20_to_59_per_10000[cases$outlier] = low
cases$outlier <- (cases$male_20_to_59_per_10000 > up)
cases$male_20_to_59_per_10000[cases$outlier] = up
boxplot(cases$male_20_to_59_per_10000, xlab="male_20_to_59_per_10000")
hist( cases$male_20_to_59_per_10000, main="Histogram for commuters", xlab="commuters")

############male_over_59_per_10000
hist( cases$male_over_59_per_10000, main="Histogram for commuters", xlab="commuters")
boxplot(cases$male_over_59_per_10000)
temp_median = median(cases$male_over_59_per_10000,na.rm=TRUE)
temp_median
Q <- quantile(cases$male_over_59_per_10000 ,probs=c(.25,0.5 ,.75), na.rm = TRUE)
Q
iqr <- IQR(cases$male_over_59_per_10000, na.rm = TRUE)
temp_median = median(cases$male_over_59_per_10000,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
cases$outlier <- (cases$male_over_59_per_10000 < low)
cases$male_over_59_per_10000[cases$outlier] = low
cases$outlier <- (cases$male_over_59_per_10000 > up)
cases$male_over_59_per_10000[cases$outlier] = up
boxplot(cases$male_over_59_per_10000, xlab="male_over_59_per_10000")
hist( cases$male_over_59_per_10000, main="Histogram for commuters", xlab="commuters")

############female_under_20_per_10000
hist( cases$female_under_20_per_10000, main="Histogram for commuters", xlab="commuters")
boxplot(cases$female_under_20_per_10000)
temp_median = median(cases$female_under_20_per_10000,na.rm=TRUE)
temp_median
Q <- quantile(cases$female_under_20_per_10000 ,probs=c(.25,0.5 ,.75), na.rm = TRUE)
Q
iqr <- IQR(cases$female_under_20_per_10000, na.rm = TRUE)
temp_median = median(cases$female_under_20_per_10000,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
cases$outlier <- (cases$female_under_20_per_10000 < low)
cases$female_under_20_per_10000[cases$outlier] = low
cases$outlier <- (cases$female_under_20_per_10000 > up)
cases$female_under_20_per_10000[cases$outlier] = up
boxplot(cases$female_under_20_per_10000, xlab="female_under_20_per_10000")
hist( cases$female_under_20_per_10000, main="Histogram for commuters", xlab="commuters")

############female_20_to_59_per_10000
hist( cases$female_20_to_59_per_10000, main="Histogram for commuters", xlab="commuters")
boxplot(cases$female_20_to_59_per_10000)
temp_median = median(cases$female_20_to_59_per_10000,na.rm=TRUE)
temp_median
Q <- quantile(cases$female_20_to_59_per_10000 ,probs=c(.25,0.5 ,.75), na.rm = TRUE)
Q
iqr <- IQR(cases$female_20_to_59_per_10000, na.rm = TRUE)
temp_median = median(cases$female_20_to_59_per_10000,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
cases$outlier <- (cases$female_20_to_59_per_10000 < low)
cases$female_20_to_59_per_10000[cases$outlier] = low
cases$outlier <- (cases$female_20_to_59_per_10000 > up)
cases$female_20_to_59_per_10000[cases$outlier] = up
boxplot(cases$female_20_to_59_per_10000, xlab="female_20_to_59_per_10000")
hist( cases$female_20_to_59_per_10000, main="Histogram for commuters", xlab="commuters")

############female_over_59_per_10000
hist( cases$female_over_59_per_10000, main="Histogram for commuters", xlab="commuters")
boxplot(cases$female_over_59_per_10000)
temp_median = median(cases$female_over_59_per_10000,na.rm=TRUE)
temp_median
Q <- quantile(cases$female_over_59_per_10000 ,probs=c(.25,0.5 ,.75), na.rm = TRUE)
Q
iqr <- IQR(cases$female_over_59_per_10000, na.rm = TRUE)
temp_median = median(cases$female_over_59_per_10000,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
cases$outlier <- (cases$female_over_59_per_10000 < low)
cases$female_over_59_per_10000[cases$outlier] = low
cases$outlier <- (cases$female_over_59_per_10000 > up)
cases$female_over_59_per_10000[cases$outlier] = up
boxplot(cases$female_over_59_per_10000, xlab="female_over_59_per_10000")
hist( cases$female_over_59_per_10000, main="Histogram for commuters", xlab="commuters")

############asian_male_55_64_per_10000
hist( cases$asian_male_55_64_per_10000, main="Histogram for commuters", xlab="commuters")
boxplot(cases$asian_male_55_64_per_10000)
temp_median = median(cases$asian_male_55_64_per_10000,na.rm=TRUE)
temp_median
Q <- quantile(cases$asian_male_55_64_per_10000 ,probs=c(.25,0.5 ,.75), na.rm = TRUE)
Q
iqr <- IQR(cases$asian_male_55_64_per_10000, na.rm = TRUE)
temp_median = median(cases$asian_male_55_64_per_10000,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
cases$outlier <- (cases$asian_male_55_64_per_10000 < low)
cases$asian_male_55_64_per_10000[cases$outlier] = low
cases$outlier <- (cases$asian_male_55_64_per_10000 > up)
cases$asian_male_55_64_per_10000[cases$outlier] = up
boxplot(cases$asian_male_55_64_per_10000, xlab="asian_male_55_64_per_10000")
hist( cases$asian_male_55_64_per_10000, main="Histogram for commuters", xlab="commuters")

############black_male_55_64_per_10000
hist( cases$black_male_55_64_per_10000, main="Histogram for commuters", xlab="commuters")
boxplot(cases$black_male_55_64_per_10000)
temp_median = median(cases$black_male_55_64_per_10000,na.rm=TRUE)
temp_median
Q <- quantile(cases$black_male_55_64_per_10000 ,probs=c(.25,0.5 ,.75), na.rm = TRUE)
Q
iqr <- IQR(cases$black_male_55_64_per_10000, na.rm = TRUE)
temp_median = median(cases$black_male_55_64_per_10000,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
cases$outlier <- (cases$black_male_55_64_per_10000 < low)
cases$black_male_55_64_per_10000[cases$outlier] = low
cases$outlier <- (cases$black_male_55_64_per_10000 > up)
cases$black_male_55_64_per_10000[cases$outlier] = up
boxplot(cases$black_male_55_64_per_10000, xlab="black_male_55_64_per_10000")
hist( cases$black_male_55_64_per_10000, main="Histogram for commuters", xlab="commuters")

############hispanic_male_55_64_per_10000
hist( cases$hispanic_male_55_64_per_10000, main="Histogram for commuters", xlab="commuters")
boxplot(cases$hispanic_male_55_64_per_10000)
temp_median = median(cases$hispanic_male_55_64_per_10000,na.rm=TRUE)
temp_median
Q <- quantile(cases$hispanic_male_55_64_per_10000 ,probs=c(.25,0.5 ,.75), na.rm = TRUE)
Q
iqr <- IQR(cases$hispanic_male_55_64_per_10000, na.rm = TRUE)
temp_median = median(cases$hispanic_male_55_64_per_10000,na.rm=TRUE)
up <-  Q[3]+1.5*iqr # Upper Range  
low<- Q[1]- 1.5*iqr # Lower Range
cases$outlier <- (cases$hispanic_male_55_64_per_10000 < low)
cases$hispanic_male_55_64_per_10000[cases$outlier] = low
cases$outlier <- (cases$hispanic_male_55_64_per_10000 > up)
cases$hispanic_male_55_64_per_10000[cases$outlier] = up
boxplot(cases$hispanic_male_55_64_per_10000, xlab="hispanic_male_55_64_per_10000")
hist( cases$hispanic_male_55_64_per_10000, main="Histogram for commuters", xlab="hispanic_male_55_64_per_10000")

# 
# 
# ############WOW1_Cases
# hist( cases$WOW1_Cases, main="Histogram for commuters", xlab="commuters")
# boxplot(cases$WOW1_Cases)
# temp_median = median(cases$WOW1_Cases,na.rm=TRUE)
# temp_median
# Q <- quantile(cases$WOW1_Cases ,probs=c(.25,0.5 ,.75), na.rm = TRUE)
# Q
# iqr <- IQR(cases$WOW1_Cases, na.rm = TRUE)
# temp_median = median(cases$WOW1_Cases,na.rm=TRUE)
# up <-  Q[3]+1.5*iqr # Upper Range  
# low<- Q[1]- 1.5*iqr # Lower Range
# cases$outlier <- (cases$WOW1_Cases < low)
# cases$WOW1_Cases[cases$outlier] = low
# cases$outlier <- (cases$WOW1_Cases > up)
# cases$WOW1_Cases[cases$outlier] = up
# boxplot(cases$WOW1_Cases)
# hist( cases$WOW1_Cases, main="Histogram for commuters", xlab="commuters")
# 
# ############WOW2_Cases
# hist( cases$WOW2_Cases, main="Histogram for commuters", xlab="commuters")
# boxplot(cases$WOW2_Cases)
# temp_median = median(cases$WOW2_Cases,na.rm=TRUE)
# temp_median
# Q <- quantile(cases$WOW2_Cases ,probs=c(.25,0.5 ,.75), na.rm = TRUE)
# Q
# iqr <- IQR(cases$WOW2_Cases, na.rm = TRUE)
# temp_median = median(cases$WOW2_Cases,na.rm=TRUE)
# up <-  Q[3]+1.5*iqr # Upper Range  
# low<- Q[1]- 1.5*iqr # Lower Range
# cases$outlier <- (cases$WOW2_Cases < low)
# cases$WOW2_Cases[cases$outlier] = low
# cases$outlier <- (cases$WOW2_Cases > up)
# cases$WOW2_Cases[cases$outlier] = up
# boxplot(cases$WOW2_Cases)
# hist( cases$WOW2_Cases, main="Histogram for commuters", xlab="commuters")
# 
# ############WOW3_Cases
# hist( cases$WOW3_Cases, main="Histogram for commuters", xlab="commuters")
# boxplot(cases$WOW3_Cases)
# temp_median = median(cases$WOW3_Cases,na.rm=TRUE)
# temp_median
# Q <- quantile(cases$WOW3_Cases ,probs=c(.25,0.5 ,.75), na.rm = TRUE)
# Q
# iqr <- IQR(cases$WOW3_Cases, na.rm = TRUE)
# temp_median = median(cases$WOW3_Cases,na.rm=TRUE)
# up <-  Q[3]+1.5*iqr # Upper Range  
# low<- Q[1]- 1.5*iqr # Lower Range
# cases$outlier <- (cases$WOW3_Cases < low)
# cases$WOW3_Cases[cases$outlier] = low
# cases$outlier <- (cases$WOW3_Cases > up)
# cases$WOW3_Cases[cases$outlier] = up
# boxplot(cases$WOW3_Cases)
# hist( cases$WOW3_Cases, main="Histogram for commuters", xlab="commuters")
# 
# names(cases)
# ############WOW1_Deaths
# hist( cases$WOW1_Deaths, main="Histogram for commuters", xlab="commuters")
# boxplot(cases$WOW1_Deaths)
# temp_median = median(cases$WOW1_Deaths,na.rm=TRUE)
# temp_median
# Q <- quantile(cases$WOW1_Deaths ,probs=c(.25,0.5 ,.75), na.rm = TRUE)
# Q
# iqr <- IQR(cases$WOW1_Deaths, na.rm = TRUE)
# temp_median = median(cases$WOW1_Deaths,na.rm=TRUE)
# up <-  Q[3]+1.5*iqr # Upper Range  
# low<- Q[1]- 1.5*iqr # Lower Range
# cases$outlier <- (cases$WOW1_Deaths < low)
# cases$WOW1_Deaths[cases$outlier] = low
# cases$outlier <- (cases$WOW1_Deaths > up)
# cases$WOW1_Deaths[cases$outlier] = up
# boxplot(cases$WOW1_Deaths)
# hist( cases$WOW1_Deaths, main="Histogram for commuters", xlab="commuters")
# 
# ############WOW2_Cases
# hist( cases$WOW2_Cases, main="Histogram for commuters", xlab="commuters")
# boxplot(cases$WOW2_Cases)
# temp_median = median(cases$WOW2_Cases,na.rm=TRUE)
# temp_median
# Q <- quantile(cases$WOW2_Cases ,probs=c(.25,0.5 ,.75), na.rm = TRUE)
# Q
# iqr <- IQR(cases$WOW2_Cases, na.rm = TRUE)
# temp_median = median(cases$WOW2_Cases,na.rm=TRUE)
# up <-  Q[3]+1.5*iqr # Upper Range  
# low<- Q[1]- 1.5*iqr # Lower Range
# cases$outlier <- (cases$WOW2_Cases < low)
# cases$WOW2_Cases[cases$outlier] = low
# cases$outlier <- (cases$WOW2_Cases > up)
# cases$WOW2_Cases[cases$outlier] = up
# boxplot(cases$WOW2_Cases)
# hist( cases$WOW2_Cases, main="Histogram for commuters", xlab="commuters")
# 
# ############WOW3_Cases
# hist( cases$WOW3_Cases, main="Histogram for commuters", xlab="commuters")
# boxplot(cases$WOW3_Cases)
# temp_median = median(cases$WOW3_Cases,na.rm=TRUE)
# temp_median
# Q <- quantile(cases$WOW3_Cases ,probs=c(.25,0.5 ,.75), na.rm = TRUE)
# Q
# iqr <- IQR(cases$WOW3_Cases, na.rm = TRUE)
# temp_median = median(cases$WOW3_Cases,na.rm=TRUE)
# up <-  Q[3]+1.5*iqr # Upper Range  
# low<- Q[1]- 1.5*iqr # Lower Range
# cases$outlier <- (cases$WOW3_Cases < low)
# cases$WOW3_Cases[cases$outlier] = low
# cases$outlier <- (cases$WOW3_Cases > up)
# cases$WOW3_Cases[cases$outlier] = up
# boxplot(cases$WOW3_Cases)
# hist( cases$WOW3_Cases, main="Histogram for commuters", xlab="commuters")
cases <- cases %>% select(county_name, state,total_pop,
                          cases_per_10000,deaths_per_10000,death_per_case,family_households_per_10000,
                          rent_under_15_percent_per_10000,
                          median_age,
                          #  median_rent,
                          income_less_50000_per_10000,
                          #white_pop,
                          black_pop_per_10000,
                          asian_pop_per_10000,
                          hispanic_pop_per_10000,
                          other_race_pop_per_10000,
                          commuters_by_public_transportation_per_10000,
                          median_income,income_per_capita,
                          #  percent_income_spent_on_rent,
                          million_dollar_housing_units,
                          families_with_young_children_per_10000,
                          income_50000_100000_per_10000,income_100000_or_more_per_10000, commute_less_10_mins_per_10000,
                          commute_more_10_mins_per_10000,
                          dwellings_less_10_units_attached_per_10000,dwellings_more_10_units_per_10000,
                          male_under_20_per_10000,male_20_to_59_per_10000,male_over_59_per_10000,
                          female_under_20_per_10000,female_20_to_59_per_10000,female_over_59_per_10000,
                          # one_parent_families_with_young_children,
                          #asian_male_45_54,
                          asian_male_55_64_per_10000,
                          #black_male_45_54,
                          black_male_55_64_per_10000,
                          #hispanic_male_45_54,
                          hispanic_male_55_64_per_10000,
                           WOW1_Cases,WOW2_Cases,WOW3_Cases,
                          WOW1_Deaths,WOW2_Deaths,WOW3_Deaths
                          # white_male_45_54,white_male_55_6
)
pairs(cases)
summary(cases)
write.csv(cases,"cases.csv")
# creation of classification data
final_cases <- cases %>% mutate(bad = as.factor(deaths_per_10000 > 38))
final_cases %>% pull(bad) %>% table()

# normalizing data
j <- sapply(final_cases, is.numeric)
final_cases[j] <- scale(final_cases[j])   
hist( final_cases$WOW3_Cases, main="Histogram for commuters", xlab="commuters")
abline(v = mean(final_cases$WOW3_Cases),                       # Add line for mean
       col = "red",
       lwd = 3)

a <- final_cases %>% select(-death_per_case, -cases_per_10000, -deaths_per_10000, - county_name ,- state,-bad)
corrplot(cor(a))
write.csv(final_cases,"final_cases.csv")

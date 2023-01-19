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
df <- read_csv("final_cases.csv")
df <- df[order(df$county_name, decreasing = FALSE), ]
cases <- df %>% mutate_if( is.character, factor)
cases <- cases %>% mutate_if(is.logical, factor)

cases <- cases %>% select(county_name, state,death_per_case, cases_per_10000, deaths_per_10000,bad,
                          million_dollar_housing_units,
                          asian_pop_per_10000 ,
                          commuters_by_public_transportation_per_10000,
                          income_100000_or_more_per_10000 ,
                          commute_more_10_mins_per_10000 ,
                          WOW1_Cases,
                          commute_less_10_mins_per_10000 ,
                          median_income ,
                          total_pop  ,
                          income_per_capita ,
                        #  female_20_to_59_per_10000,
                       #   hispanic_male_55_64_per_10000 ,
                        #  dwellings_less_10_units_attached_per_10000 ,
                       #   dwellings_more_10_units_per_10000 ,
                        #  other_race_pop_per_10000,
                       #   hispanic_pop_per_10000,
                       #   WOW2_Cases,
                       #   WOW3_Cases,
                        #  asian_male_55_64_per_10000,
                       #   male_20_to_59_per_10000
)
cases_train <- cases %>% filter(state %in% c("TX", "CA", "FL", "NY","IL","PA","NC"))
cases_train %>% pull(bad) %>% table()

a <- cases_train %>% select(-death_per_case, -cases_per_10000, -deaths_per_10000,-bad, - county_name ,- state)
corrplot(cor(a))


cases_test <-  cases %>% filter(!(state %in% c("TX", "CA", "FL", "NY","IL","PA","NC")))
cases_test %>% pull(bad) %>% table()

counties <- as_tibble(map_data("county"))
counties <- counties %>% 
  rename(c(county = subregion, state = region)) %>%
  mutate(state = state.abb[match(state, tolower(state.name))]) %>%
  select(state, county, long, lat, group)
counties 

counties_all <- counties %>% left_join(cases_train %>% 
                                         mutate(county = county_name %>% str_to_lower() %>% 
                                                  str_replace('\\s+county\\s*$', '')))
ggplot(counties_all, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))



cases_train <- cases_train %>% select(-death_per_case, -cases_per_10000, -deaths_per_10000)
dim(cases_train)
cases_train %>%  chi.squared(bad ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head(n = 22)

library(caret)

fit <- cases_train %>%
  train(bad ~ . - county_name - state,
        data = . ,
        #method = "rpart",
        method = "rfRules",
        #method = "nb",
        tuneLength = 5,
        trControl = trainControl(method = "cv", number = 10, repeats =3)
  )
fit
varImp(fit)
cases_test <- cases_test %>% na.omit
cases_test$bad_predicted <- predict(fit, cases_test)
counties_test <- counties %>% left_join(cases_test %>% 
                                          mutate(county = county_name %>% str_to_lower() %>% 
                                                   str_replace('\\s+county\\s*$', '')))
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))
confusionMatrix(data = cases_test$bad_predicted, ref = cases_test$bad)

# ######################################################################
#increasing dataset
#wait
cases_train <- cases %>% filter(state %in% c("TX", "CA", "FL", "NY","IL","PA","NC","OH","GA",
                                             "MH","NJ","TS","AZ","VA","MA","IN","WI","WA","SC",
                                             "MN","CO","MO","KY","AL"))
cases_train %>% pull(bad) %>% table()

a <- cases_train %>% select(-death_per_case, -cases_per_10000, -deaths_per_10000,-bad, - county_name ,- state)
corrplot(cor(a))


cases_test <-  cases %>% filter(!(state %in% c("TX", "CA", "FL", "NY","IL","PA","NC","OH","GA",
                                               "MH","NJ","TS","AZ","VA","MA","IN","WI","WA","SC",
                                               "MN","CO","MO","KY","AL")))
cases_test %>% pull(bad) %>% table()

counties <- as_tibble(map_data("county"))
counties <- counties %>% 
  rename(c(county = subregion, state = region)) %>%
  mutate(state = state.abb[match(state, tolower(state.name))]) %>%
  select(state, county, long, lat, group)
counties 

counties_all <- counties %>% left_join(cases_train %>% 
                                         mutate(county = county_name %>% str_to_lower() %>% 
                                                  str_replace('\\s+county\\s*$', '')))
ggplot(counties_all, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))



cases_train <- cases_train %>% select(-death_per_case, -cases_per_10000, -deaths_per_10000)
dim(cases_train)
cases_train %>%  chi.squared(bad ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head(n = 10)

library(caret)

fit <- cases_train %>%
  train(bad ~ . - county_name - state,
        data = . ,
        #method = "rpart",
        method = "rfRules",
        #method = "nb",
        tuneLength = 5,
        trControl = trainControl(method = "cv", number = 10, repeats =3)
  )
fit
varImp(fit)
cases_test <- cases_test %>% na.omit
cases_test$bad_predicted <- predict(fit, cases_test)
counties_test <- counties %>% left_join(cases_test %>% 
                                          mutate(county = county_name %>% str_to_lower() %>% 
                                                   str_replace('\\s+county\\s*$', '')))
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))
ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))
confusionMatrix(data = cases_test$bad_predicted, ref = cases_test$bad)



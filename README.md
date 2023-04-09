# CovidDataAnalysis

Analysis of Covid data available in google BigQuery. 
Collected the aggregated data until the November 2022 at county level information. For this study, we have considered all the conties except for Texas state(Project requirement). The target variable is Deaths. We created a new variable, deaths per 10000 cases. If this value was above 38 (mean) then the impact is high else the impact is low.
During the data preparation, we treated the missing values and outliers, we dropped the data that had more than 90% data missing and the highly correlated variables. 
The classification models used are:
1) CART
2) KNN
3) Naive-Bayes
4) Random-Forest

We observed that, the best model obtained was with Random-forest, with the best accuracy, precision and recall.

Language: R

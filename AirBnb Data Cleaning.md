# Airbnb Data Cleaning and Analysis 

## Introduction
This data analytics project was completed as part of the Oasis Infobyte internship program (December Batch 2025). The team provided the data through an open-source data platform, Kaggle. 
It contains a hosting record of various Airbnb hosts in five neighborhood groups in New York namely: Queens, Staten Island, Manhattan, Brooklyn, and Bronx. 
The data contained key information such as booking prices, minimum nights spent, hostid, reviews etc, with which the analysis will be guided. However, the objective of this data is to
clean, structure, detect outliers, and analyse. 

## Project Objective
1. Data Integrity: Ensuring the accuracy, consistency, and reliability of data throughout the
cleaning process.
2. Missing Data Handling: Dealing with missing values by either imputing them or making
informed decisions on how to handle gaps in the dataset.
3. Duplicate Removal: Identifying and eliminating duplicate records to maintain data
uniqueness.
4. Standardization: Consistent formatting and units across the dataset for accurate analysis.
5. Outlier Detection: Identifying and addressing outliers that may skew analysis or model
performance.

## My Role
On this project, I assume the role of the data analyst to uncover these insights, provide recommendations that influence decision-making, and apply also segment the reviews effectively.
As a Junior Data Analyst, the interquartile range (IQR) will be used to detect and cap skewed data (outliers).

### Tool
Google Sheet - Data Preview
R Studio - Data Analysis
Tableau - Visualization

### Data Preparation and Cleaning

Using R Studio, the necessary libraries were loaded and 
```R
library(tidyverse)
library(dplyr)
library(lubridate)
library(skimr)
library(janitor)
library(tibble)
library(ggplot2)
```
The data AB_NYC_2019 with 16 fields and 48895 records was imported into R Studio and reviewed for missing data. The sum of missing data in each row.
```R
str(AB_NYC_2019)
is.na(AB_NYC_2019)
colSums(is.na(AB_NYC_2019))
```
### Data Cleaning
Following the detection of missing values, an imputation method was applied, and depending on the datatype, some were replaced with "Unknown," 0, and "Nan." 
```R
AB_NYC <-AB_NYC_2019 %>% 
  mutate(host_name =ifelse(is.na(host_name), "Unknown", host_name))
```
The date contained in the dataset was formatted to extract the Day of the week, Month, Day, and Year
```R
AB_NYC <- AB_NYC %>%
  mutate(last_review = ymd(last_review))

AB_NYC <- AB_NYC %>%
  mutate(Month = month(last_review, label = TRUE, abbr = FALSE))

AB_NYC <- AB_NYC %>%
  mutate(Day = day(last_review))

AB_NYC <- AB_NYC %>%
  mutate(Weekday = wday(last_review, label = TRUE, abbr = FALSE))

AB_NYC <- AB_NYC %>% 
  mutate(Year = year(last_review))
```
After which,the dataset was expanded from 16 column to 20 columns. Missing values were imputed as such: 
```R
AB_NYC <- AB_NYC %>%
  mutate(name = ifelse(is.na(name), "Unknown", name))
AB_NYC <- AB_NYC %>% 
  mutate(reviews_per_month = ifelse(is.na(reviews_per_month), 0.0, reviews_per_month))

AB_NYC <- AB_NYC %>% 
  mutate(Day = ifelse(is.na(Day), NA, Day))

AB_NYC <- AB_NYC %>% 
  mutate(Year = ifelse(is.na(Year), NA, Year))
```

The data was further evaluated to identify unique names relevant for data filtering, grouping, and analysis. This revealed that there are  
```
unique(AB_NYC$neighbourhood)
```
Result: 220 unique Neighbourhoods

```
unique(AB_NYC$neighbourhood_group)
```
Result: "Brooklyn"      "Manhattan"     "Queens"        "Staten Island"  "Bronx" 
```R
unique(AB_NYC$id)
```
Results: 48,895

```R
unique(AB_NYC$name)
```
Results: 47,470

```R
unique(AB_NYC$host_name)
```
Results: 11,428

```R
unique(AB_NYC$host_id)
```
Results: 37,450

```R
unique(AB_NYC$room_type)
```



Using the distinct() function, we checked for duplicate values.
```R
AB_NYC <- AB_NYC %>% 
  distinct()
```
  
Check for Outliers

boxplot(AB_NYC$price , main = "Boxplot of Price", ylab = "Total Prices", col = "Red")

<img width="533" height="294" alt="Box plot with Outliers" src="https://github.com/user-attachments/assets/fc481f3b-fb6c-452b-bbdc-090ef3b01fa3" />


## Fix outliers in prices

```R
AB_NYC <-AB_NYC %>% 
  mutate(price = as.numeric(price))
```
Drop rows where prices = 0 so as to easily apply IQR to yield clear result and remove necessary outliers. This was done because some of the prices had 0 which is invalid when it comes to the currency and the prices were not specified and also, did not match the minimum per night.
```R
AB_NYC <- AB_NYC %>%
  filter(price > 0)
```
This further dropped the rows from 48895 to 45912
### Detecting and Fixing Outliers. 1. Calculate IQR for Price

quantile(AB_NYC$price)
   Result: 0%   25%   50%   75%  100% 
           10    69   106   175 10000 

Q1 <- quantile(AB_NYC$price, 0.25)
Q3 <- quantile(AB_NYC$price, 0.75)
IQR_value <- Q3 - Q1

View(IQR_value)

_Define Bounds_
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

summary(AB_NYC$price)
Result:  
  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  10.0    69.0   106.0   152.7   175.0 10000.0 

# Create a cleaned dataframe by filtering out outliers
AB_NYC_Final <- AB_NYC %>%
  filter(price >= lower_bound & price <= upper_bound)

summary(AB_NYC_Final$price)
Result: Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
10      65     100     120     159     334 

## plot
```R
library(ggplot2)

boxplot(AB_NYC_Final$price, main = "Boxplot of Price", ylab = "Total Prices", col = "Red")
```
<img width="533" height="376" alt="Airbnb Price Wout Outliers" src="https://github.com/user-attachments/assets/057f3995-5d6e-46eb-be7b-0a58fe5c15b5" />

## Segment the reviews

AB_NYC_Final <- AB_NYC_Final %>%
  mutate(review_segment  = case_when(
    reviews_per_month  >= 0   & reviews_per_month  <= 2.99 ~ "Poor",
    reviews_per_month  >= 3.00 & reviews_per_month  <= 5.99 ~ "Good",
    reviews_per_month  >= 6.00 & reviews_per_month  <= 7.99 ~ "High",
    reviews_per_month  >= 8.00 & reviews_per_month  <= 9.99 ~ "Excellent",
     reviews_per_month >= 10.00 & reviews_per_month <= 80.99 ~ "Outbound", TRUE ~ NA_character_ ))

After cleaning and removing invalid and extreme pricing values, Airbnb listings showed a strong concentration between $65–$175 per night. Listings priced above $334 were identified as statistical outliers and removed to prevent skewed summaries. Review segmentation revealed that most listings fall within the “Poor” to “Good” engagement range, indicating opportunities for hosts to improve visibility and guest interaction.


unique(AB_NYC_Final$reviews_per_month)

summary(AB_NYC_Final$reviews_per_month)

sum(AB_NYC_Final$number_of_reviews)

AB_NYC_Final <- AB_NYC_Final %>% 
 mutate(calculated_host_listings_count = as.numeric(calculated_host_listings_count))

Reviews <- AB_NYC_Final %>% 
  group_by(room_type, neighbourhood_group) %>% 
   summarise(Total_reviews = sum(number_of_reviews), average_reviews = mean(number_of_reviews),
            Hosting = sum(calculated_host_listings_count), average_hosting = mean(calculated_host_listings_count), Room_type_count = n(), .groups = "drop")

summary(AB_NYC$reviews_per_month)

AB_NYC_Final <-AB_NYC_Final %>% 
  mutate(reviews_per_month = as.numeric(reviews_per_month))


library(readr)

write.csv(AB_NYC_Final, "AB_NYC_Analysis_Final.csv", row.names = FALSE)

getwd()



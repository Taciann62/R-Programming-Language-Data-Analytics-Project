# Retail Sales Data 2023 

## Introduction

### This data analytics project is performed as an assigned task in fulfilment of the Oasis Infobyte internship program (December Batch 2025).
### The data was provided by the team through an open-source data platform, Kaggle. It contains a sales record of a retail store from 2023, alongside key fields such as Customer Gender, Product Name, Age, Product type, and the amount and Quantity purchased by each customer.

## Project Overview
### The Retail store collected a well-rounded dataset of its customers to get insight into their customer behaviour towards the products sold and, in that way, make a more informed business decision. The goal is to perform exploratory data analysis (EDA) to uncover patterns, trends, and insights that can help the retail business make informed decisions.

### Through the analysis of this data, insights will also be provided into customer purchasing behaviour over the year 2023, with each month clearly represented.

## My Role:

### On this project, I assume the role of the data analyst to uncover these insights, provide recommendations that influence decision-making.

## Data Analytics Objective:

 - Data Loading and Cleaning: Load the retail sales dataset.
 - Descriptive Statistics: Calculate basic statistics (mean, median, mode, standard deviation).
 - Time Series Analysis: Analyse sales trends over time using time series techniques.
 - In Customer and Product Analysis: Analyse customer demographics and purchasing behaviour.
 - Visualisation: Present insights through bar charts, line plots, and heatmaps.
 - Recommendations: Provide actionable recommendations based on the EDA.


## Business Question:
### Sales performance
   - What product generates the most revenue
   - What days are the busiest?
   - What month has the busiest sales?
   - What is the product revenue trend in the year 2023?
   - How did the customers behave towards the products(Gender & Age segment)

## Data Analytics steps taken:
- Data pulling and wrangling: Following the link provided, the dataset was accessible on Kaggle, downloaded from the source as a zip folder. After which, it was unzipped, and a preliminary review of the fields and records on Google Sheet to get a clear understanding of the dataset and its content.
- I proceeded to work on the date, on Google sheet, and formatted the date column to consist of Day-of-week, Day, Month, and Year.
- The data was filtered through the rows to ensure consistency of records. All these were done before proceeding to R studio. You may decide to move straight to R Studio. However, I often prefer to have a direct overview of datasets on a spreadsheet before analysing on other platforms such as BigQuery or R studio.


### Tools used in this Analysis
- Google Sheet - Data set preview and partial formatting
- R Studio - Data Analysis
- Tableau - Data Visualisation

## Data Analysis and Wrangling Procedures on R Studio

#### Load the library of the installed packages.

```R
library(tidyverse)
library(dplyr)
library(lubridate)
library(skimr)
library(janitor)
library(readr)
library(tibble)
```

#### View the retail sales data to see the rows and columns it contains. Recall, I have formatted the date in the spreadsheet.

```R
View(Retail_sales)
```

#### To locate duplicate rows
```R
unique(Retail_Sales_Data_retail_sales_dataset)
```

#### To remove duplicates and have only a unique set
```R
Retail_Sales<-distinct(Retail_Sales_Data_retail_sales_dataset) 
```
*<- renames the dataset*

```R
head(Retail_sales)` 
names(Retail_sales)` 
```
*<-shows the first 10 rows of the dataset*
*<-shows the column names*

- Transaction_ID
- Day_of_Week
- Day
- Month
- Year
- Customer_ID
- Gender
- Age
- Product_Category
- Quantity
- Price_per_unit
- Total_Amount

#### Note: The product _category are 3: *Beauty, Clothing, Electronics*
  ```R
  dim(Retail_sales) 
 str_length(Retail_sales$Age)
```

## Find duplicates and inconsistencies, missing values 

```R
   is.na(Retail_sales) *<- checks for mssing values*

unique(Retail_sales)
```

## Exploratory Data Analysis

### Calculate the Total Amount to Ensure Accuracy.

```R
Retail_sales$Total_Amount<-(Retail_sales$Quantity*Retail_sales$Price_per_Unit)
```

#### 1. How much was generated overall in 2023? 
```R
Total_rev_2023<-sum(Retail_sales$Total_Amount)
```
##### Result =  456,000

#### 2. How much is the average amount generated in 2023?
```R
Avg_rev_2023<-mean(Retail_sales$Total_Amount)
```
##### Result = 456

### Create Table of Revenue Generated
```R
Retail_Revenue<-Retail_sales %>%
summarise(Average_Revenue= mean(Total_Amount), Total_Revenue_2023=sum(Total_Amount),
  Median_purchase=median(Total_Amount), Standard_DV= sd(Total_Amount), .groups = "drops")
  ```

### Calculate the revenue for each product to get the Average, Average Quantity, Total Amount, Total Quantity, and Total Frequency by Product.

```R
Total_spent_on_product<-Retail_sales %>%
group_by(Product_Category) %>%
 summarise(Average_amount=round(mean(Total_Amount),2), average_purchase=round(mean(Quantity), 2),
  total_amount=sum(Total_Amount), total__Quantity_sold=sum(Quantity),transaction_count=n(),.groups = "drop")
```


### Calculate the revenue generated by each month to gain insight into product performance.
##### This answers the business question: How much was generated, total quantity purchased & total count per month?

```R
Total_Purchases_Per_Month<-Retail_sales %>% 
  group_by(Month) %>%
  summarise(total_amount=sum(Total_Amount), transaction_count=n(), total_quantity=sum(Quantity))
```

### Calculate the revenue generated by each month to gain insight into product performance grouped by product category.
```R
Total_Per_month_Product<-Retail_sales %>% 
  group_by(Month, Product_Category) %>%
  summarise(total_amount=sum(Total_Amount), average_purchase=round(mean(Total_Amount),2), total_quantity=sum(Quantity), transaction_count=n(), .groups =  "drop") %>% 
  arrange(desc(total_amount))
```

## Calculate the revenue generated by each day of week to gain insight into product performance.

```R
Total_Purchases_DOW<-Retail_sales %>% 
  group_by(Day_of_Week) %>%
  summarise(total_amount=sum(Total_Amount), average_purchase=round(mean(Quantity), 2), transaction_count=n(), total_quantity=sum(Quantity))
``` 

## Calculate the revenue generated by each month to gain insight into product performance grouped by product category.

```R
Total_Purchases_DOW_Product<-Retail_sales %>% 
  group_by(Day_of_Week, Product_Category) %>%
  summarise(total_amount=sum(Total_Amount),average_purchase=round(mean(Total_Amount),2), transaction_count=n(), total_quantity=sum(Quantity), average_qty=round(mean(Quantity),2), .groups = "drop")
```

## Customer Age group categorisation or segmentation.
### Having reviewed the dataset, I observed that the age ranges from 18-64. To analyse customer behaviour by age group, I performed an age segmentation grouping the ages thus
- Age 18 - 24 = Young Adult
- Age 25 - 34 = Middle Adult
- Age 35 - 44 = Established Consumer
- Age 45 - 54 = Late Career
- Age 55 - 64 = Pre_retirement

### R code for adequate age segmentation
```R
Retail_sales_data_updated<-Retail_sales %>% 
  mutate(age_segment= case_when(Age>=18 & Age <=24 ~"1. Young Adult(18-24)", Age>=25 & Age <=34 ~"2. Middle Adult(25-34)", 
    Age>=35 & Age<=44 ~"3. Established Consumer(35-44)", Age>=45 & Age<=54~"4. Late Career(45-54)", Age>=55 & Age<=64 ~"5. Pre_retirement(55-64)", TRUE ~ "00. Unknown"))
```

## Customer Purchasing Behaviour Based on Age Segment
```R
Customer_Behaviour<- Retail_sales_data_updated %>% 
  group_by(age_segment) %>% 
  summarise(total_spent=sum(Total_Amount), average_purchase=round(mean(Total_Amount, na.rm=TRUE),2), 
   transaction_count=n(), avg_quantity=mean(Quantity), total_quantity=sum(Quantity),.groups = "drop")
```

## Customer Purchases per Month Grouped by Age Segment
```R
Agegroup_purchase_per_month<-Retail_sales_data_updated %>% 
  group_by(Month, age_segment) %>% 
  summarise(total_spent_per_month = sum(Total_Amount), average_amount=round(mean(Total_Amount),2), total_quantity_customer=sum(Quantity), .groups = "drop")
```

## Customer Purchases per age group 
```R
Agegroup_purchase_product<-Retail_sales_data_updated %>% 
  group_by(Product_Category, age_segment) %>% 
  summarise(total_AMOUNT = sum(Total_Amount), average_amount=round(mean(Total_Amount),2), total_quantity_customer=sum(Quantity), .groups = "drop")
```

## Gender 
```R
Gender_behaviour<- Retail_sales_data_updated %>% 
  group_by(Gender) %>% 
  summarise(total_spent=sum(Total_Amount), average_purchase=round(mean(Total_Amount),2), 
            transaction_count=n(), total_quantity=sum(Quantity), .groups = "drop")
```

```R
Gender_product<- Retail_sales_data_updated %>% 
  group_by(Gender, Product_Category) %>% 
  summarise(total_spent=sum(Total_Amount), average_purchase=round(mean(Total_Amount),2), 
            transaction_count=n(), total_quantity=sum(Quantity),.groups = "drop")
```


## Exporting required tables in CSV for Visualisation

```R
write.csv(Agegroup_purchase_product, "Agegroup_Product_Purchase.csv", row.names = FALSE)

write.csv(Agegroup_purchase_per_month, "Monthly_Agegroup_Trend.csv", row.names =  FALSE)

write.csv(Customer_Behaviour, "Customer_Behaviour.csv", row.names = FALSE)

write.csv(Total_Per_month_Product, "Monthly_Product_Purchase.csv", row.names = FALSE)

write.csv(Gender_product, "Gender_Product_Purchase.csv", row.names = FALSE)

write.csv(Total_spent_on_product, "Retail_Sales_revenue.csv", row.names = FALSE)

write.csv(Total_Purchases_DOW_Product, "Sales_Trend_DOW.csv", row.names = FALSE)

write.csv(Retail_Revenue, "KPI Cards.csv", row.names = FALSE)
```

## Descriptive Analysis:

- The following insights were obtained:
- Revenue Generation:
- What was the most sold item in 2023 = Electronics 
- How much was generated by the most sold item: 156,905
- What was the least sold product category? = Beauty
- How much in total was generated by the least sold item = 143,515

### Demand Drivers by frequency of demand:
- Product category with the Highest Frequency: Clothing, with a total of 351 count
- Product category with the least frequency: Beauty, with a total of 307 counts

### Product Sales driven by Quantity:
- What product was sold the most? = Clothing with a total of 894 qty
- What product was sold the least? = Beauty with a total of 771 qty

## Sales Trends in 2023 by Month and Day of Week
- Month with highest revenue generation: May (23,245)
- Month with the least revenue generation: March (3,380)
- Busiest month in 2023: May (53,150)
- Least busiest month: September (23,620)
- Busiest Days/Peak sales day: Tuesday (161 - count of purchases in 2023)
- Least busiest day: Thursday (123 - count of purchase in 2023)

### Overall Monthly Sales Trend:
- May recorded the highest revenue and transaction volume, indicating a seasonal or promotional sales peak.
- March and September experienced the lowest sales, suggesting off-peak periods where demand stimulation strategies may be required.
- Electronics showed high volatility, while Clothing maintained a more consistent trend across months.


### Customer Analysis
##### The groups provided insights into customer behaviour toward the products and also their spending ability.
- Young Adult(18-24) = 74,650 in revenue 366 qty purchase
- Middle Adult(25-34) = 97,090 in revenue, 522 qty purchased
- Established Consumer(35-44) = 96,835 in revenue, 533 qty purchased
- Late Career(45-54) = 97,235 in revenue, 578 qty purchased
- Pre_retirement(55-64) = 90,190 in revenue 515 qty purchased

##### Late Career (45–54) customers generated the highest revenue (97,235) and recorded the highest quantity purchased (578 units), indicating strong purchasing power and consistent demand.

##### Middle Adults (25–34) and Established Consumers (35–44) also contributed significantly to total revenue, suggesting these age groups form the core customer base.

##### Young Adults (18–24) generated the lowest revenue (74,650) and recorded fewer purchases overall, indicating lower spending capacity.

#### Customer behaviour towards the products.
##### The Young Adults group happens to be the least revenue-demand driver throughout 2023. Factors that may have influenced these metrics could stem from finances, product style (including beauty, electronics, and clothing). However, their highest demand for each product relates to their age group as they had a high spend on Beauty products, with an average amount spent of 545.4 and an average quantity purchase of 2.57. While the customer age group with the highest drive is the Late Career group, as they generated the most revenue throughout 2023. 

##### Despite lower overall revenue contribution, Young Adults demonstrated a strong preference for Beauty products, with:
- Higher average spend per transaction
- Higher average quantity purchased per transaction

##### This suggests that while Young Adults are not the highest revenue drivers, targeted promotions and product positioning within the Beauty category could unlock growth potential for this segment, and in the same way, a look into current trends for clothes and beauty products could drive sales to the Young Adults. 
However, to limit waste, the retail store should maintain quantity and also check for specifically demanded beauty products requested by this age group and push along other products with them, to ensure availability and also pricing review to provide affordable and quality products for the age group.


#### Gender-based Purchasing Behaviour
##### Gender-based analysis reveals differences in product preference and spending patterns across categories.
- Female customers contributed a slightly higher share of total revenue, driven largely by Clothing and Electronics purchases, but high Beauty purchases compared to Male customers.
- Male customers showed stronger spending in Electronics, contributing disproportionately to revenue despite lower transaction counts.
- Average transaction value was higher among male customers in Electronics, indicating fewer but higher-value purchases.
- These patterns suggest that gender-informed product positioning and promotions could enhance category performance without requiring separate marketing campaigns.


### Recommendations:

#### Below are the recommendations for the Retail store: 
- Increase inventory and promotional focus on Electronics during peak months such as May, where demand and revenue peak.
- Introduce targeted Beauty promotions for Young Adult customers, who show high average spend despite lower overall purchase volume.
- Schedule and ensure there are more hands to support sales and product promotions around peak sales days like Tuesdays to optimise operational efficiency.
- Develop gender-informed marketing strategies that particularly promote Electronics to male customers and Beauty/Clothing bundles to female customers.
- Implement demand stimulation strategies during low-performing months (March and September) using discounts or bundled offers.


## Data Visualization 
### For the visualisation of the exported tables to gain graphical insights into the Retail sales data. The dataset tables were uploaded individually to Tableau Public.






<img width="1366" height="768" alt="Screenshot (69)" src="https://github.com/user-attachments/assets/d6a59c43-f7a2-428d-a77d-e1c1135250c4" />







###



<img width="1366" height="768" alt="Screenshot (70)" src="https://github.com/user-attachments/assets/3a5e6413-4c1d-4426-9934-7ba9b97df4a0" />





###





<img width="1366" height="768" alt="Screenshot (71)" src="https://github.com/user-attachments/assets/d96a6b80-2d46-44ad-8af4-96bae3c88b58" />





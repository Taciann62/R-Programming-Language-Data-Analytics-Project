## ================================
## DATA PREVIEW
## ================================
library(tidyverse)
library(skimr)
library(janitor)
library(ggcorrplot)
library(car)

View(Housing)
colSums(is.na(Housing))

Housing <- Housing %>% distinct()
str(Housing)
summary(Housing)

## ================================
## OUTLIER HANDLING (IQR)
## ================================

# Price
PQ1 <- quantile(Housing$price, 0.25)
PQ3 <- quantile(Housing$price, 0.75)
PIQR <- PQ3 - PQ1

Housing_Q <- Housing %>%
  filter(price >= (PQ1 - 1.5 * PIQR),
         price <= (PQ3 + 1.5 * PIQR))

# Area
AQ1 <- quantile(Housing_Q$area, 0.25)
AQ3 <- quantile(Housing_Q$area, 0.75)
AIQR <- AQ3 - AQ1

Housing_Q <- Housing_Q %>%
  filter(area >= (AQ1 - 1.5 * AIQR),
         area <= (AQ3 + 1.5 * AIQR))

summary(Housing_Q)

## ================================
## FEATURE SELECTION
## ================================

Housing_Refit <- Housing_Q %>%
  select(price, area, bathrooms, stories,
         airconditioning,
         parking, prefarea)

## ================================
## TRAIN / TEST SPLIT
## ================================

set.seed(123)
index_exp <- sample(seq_len(nrow(Housing_Refit)), 0.8 * nrow(Housing_Refit))

train_exp <- Housing_Refit[index_exp, ]
test_exp  <- Housing_Refit[-index_exp, ]

## ================================
## CORRELATION (NUMERIC + ENCODED CATEGORICAL)
## ================================

Housing_Num <- Housing_Refit %>%
  mutate(across(where(is.character), factor)) %>%
  mutate(across(where(is.factor), ~ as.numeric(.) - 1))

cor_matrix <- cor(Housing_Num, use = "complete.obs")

ggcorrplot(
  cor_matrix,
  hc.order = TRUE,
  type = "lower",
  lab = FALSE,
  colors = c("red", "white", "blue"),
  title = "Correlation Heatmap (No Outliers)"
)

## ================================
## LINEAR REGRESSION MODEL
## ================================

model_exp <- lm(price ~ ., data = train_exp)
summary(model_exp)

Result: Call:
  lm(formula = price ~ ., data = train_exp)

Residuals:
  Min       1Q   Median       3Q      Max 
-2985547  -607792   -64389   536059  4584866 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)        634311.1   195318.2   3.248  0.00126 ** 
  area                  285.7       30.2   9.461  < 2e-16 ***
  bathrooms          965543.3   118878.9   8.122 5.47e-15 ***
  stories            457225.7    59454.8   7.690 1.11e-13 ***
  airconditioningyes 735477.7   115719.7   6.356 5.54e-10 ***
  parking            208227.8    60888.5   3.420  0.00069 ***
  prefareayes        632081.3   120064.1   5.265 2.28e-07 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 989500 on 409 degrees of freedom
Multiple R-squared:  0.6077,	Adjusted R-squared:  0.6019 
F-statistic: 105.6 on 6 and 409 DF,  p-value: < 2.2e-16

## ================================
## MODEL DIAGNOSTICS
## ================================

par(mfrow = c(2, 2))
plot(model_exp, col = "blue")

vif(model_exp)

shapiro.test(resid(model_exp))
#Result Shapiro-Wilk normality test

data:  resid(model_exp)
W = 0.96933, p-value = 1.174e-07



anova(model_exp)

Result: 
  Analysis of Variance Table

Response: price
Df     Sum Sq    Mean Sq F value    Pr(>F)    
area              1 2.9148e+14 2.9148e+14 297.714 < 2.2e-16 ***
  bathrooms         1 1.6278e+14 1.6278e+14 166.262 < 2.2e-16 ***
  stories           1 8.5660e+13 8.5660e+13  87.491 < 2.2e-16 ***
  airconditioning   1 4.2466e+13 4.2466e+13  43.374 1.392e-10 ***
  parking           1 1.0719e+13 1.0719e+13  10.948   0.00102 ** 
  prefarea          1 2.7135e+13 2.7135e+13  27.715 2.275e-07 ***
  Residuals       409 4.0044e+14 9.7906e+11                      
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## ================================
## TEST SET EVALUATION
## ================================

exp_predictions <- predict(model_exp, newdata = test_exp)

exp_actuals <- test_exp$price

exp_mse <- mean((exp_predictions - exp_actuals)^2)
exp_rmse <- sqrt(exp_mse)

exp_rss <- sum((exp_predictions - exp_actuals)^2)
exp_tss <- sum((exp_actuals - mean(exp_actuals))^2)
exp_r_squared <- 1 - (exp_rss / exp_tss)

cat(
  "Evaluation Metrics:\n",
  "MSE:", exp_mse, "\n",
  "RMSE:", exp_rmse, "\n",
  "R-squared:", exp_r_squared
)
Result:
Evaluation Metrics:
 MSE: 1.12903e+12 
 RMSE: 1062558 
 R-squared: 0.6060161
## ================================
## ACTUAL vs PREDICTED
## ================================

ggplot(data = NULL, aes(x = exp_actuals, y = exp_predictions)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed", color = "red") +
  labs(
    title = "Actual vs Predicted Housing Prices",
    subtitle = paste("Test R-squared:", round(exp_r_squared, 3)),
    x = "Actual Price",
    y = "Predicted Price"
  ) +
  theme_minimal()









# --- 1. Load Data ---
Housing <- read.csv("Housing.csv")

# --- 2. Data Cleaning (Outlier Treatment) ---
# Filter Price Outliers
Q1_p <- quantile(Housing$price, 0.25)
Q3_p <- quantile(Housing$price, 0.75)
IQR_p <- Q3_p - Q1_p
Housing <- subset(Housing, price >= (Q1_p - 1.5 * IQR_p) & price <= (Q3_p + 1.5 * IQR_p))

# Filter Area Outliers
Q1_a <- quantile(Housing$area, 0.25)
Q3_a <- quantile(Housing$area, 0.75)
IQR_a <- Q3_a - Q1_a
Housing <- subset(Housing, area >= (Q1_a - 1.5 * IQR_a) & area <= (Q3_a + 1.5 * IQR_a))

# --- 3. Data Preparation (Manual Mapping & Dummies) ---
# Map Binary Variables (Yes/No to 1/0)
varlist <- c('mainroad', 'guestroom', 'basement', 'hotwaterheating', 'airconditioning', 'prefarea')
Housing[varlist] <- lapply(Housing[varlist], function(x) ifelse(x == "yes", 1, 0))

# Create Dummy Variables for Furnishing Status (Dropping first level to avoid dummy trap)
# This creates 'furnishingstatussemi-furnished' and 'furnishingstatusunfurnished'
dummies <- as.data.frame(model.matrix(~ furnishingstatus - 1, data = Housing))
Housing <- cbind(Housing, dummies[, -1]) 
Housing$furnishingstatus <- NULL # Remove original text column

# --- 4. Feature Scaling (Min-Max Scaling) ---
num_vars <- c('area', 'bedrooms', 'bathrooms', 'stories', 'parking', 'price')
Housing[num_vars] <- lapply(Housing[num_vars], function(x) (x - min(x)) / (max(x) - min(x)))

# --- 5. Split Data (70% Train, 30% Test) ---
set.seed(100)
train_idx <- sample(1:nrow(Housing), 0.7 * nrow(Housing))
train_data <- Housing[train_idx, ]
test_data  <- Housing[-train_idx, ]

# --- 6. Model Building (Using features identified in Python study) ---
model_final <- lm(price ~ area + bathrooms + stories + airconditioning + parking + prefarea, 
                  data = train_data)

# Display Summary
summary(model_final)
Result: lm(formula = price ~ area + bathrooms + stories + airconditioning + 
             parking + prefarea, data = train_data)

Residuals:
  Min       1Q   Median       3Q      Max 
-0.36754 -0.08279 -0.00996  0.07484  0.61773 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)      0.08354    0.01644   5.082 6.07e-07 ***
  area             0.36268    0.04159   8.721  < 2e-16 ***
  bathrooms        0.25228    0.03385   7.453 7.05e-13 ***
  stories          0.17513    0.02730   6.414 4.54e-10 ***
  airconditioning  0.10415    0.01723   6.046 3.77e-09 ***
  parking          0.12233    0.02640   4.634 5.05e-06 ***
  prefarea         0.09357    0.01823   5.132 4.74e-07 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1365 on 354 degrees of freedom
Multiple R-squared:  0.6214,	Adjusted R-squared:  0.6149 
F-statistic: 96.82 on 6 and 354 DF,  p-value: < 2.2e-16



# --- 7. Model Diagnostics (The "Health Check") ---
par(mfrow = c(2, 2)) # Create a 2x2 grid for plots
plot(model_final)

# --- 8. Model Evaluation (Test Set R-Squared) ---
predictions <- predict(model_final, newdata = test_data)
# Calculate R-Squared manually: 1 - (RSS / TSS)
rss <- sum((test_data$price - predictions)^2)
tss <- sum((test_data$price - mean(test_data$price))^2)
test_r2 <- 1 - (rss / tss)

# Final Result
cat("\n--- Final Evaluation ---\n")
cat("Test R-squared:", round(test_r2, 4), "\n")
view: Test R-squared: 0.5596 

# Turtle Games Data Analysis Project

###############################################################################

## Turtle Games’s sales department has historically preferred to use R when performing 
## sales analyses due to existing workflow systems. As you’re able to perform data analysis 
## in R, you will perform exploratory data analysis and present your findings by utilising 
## basic statistics and plots. You'll explore and prepare the data set to analyse sales per 
## product. The sales department is hoping to use the findings of this exploratory analysis 
## to inform changes and improvements in the team. (Note that you will use basic summary 

###############################################################################

# Approach

## Part 1. Load, clean and wrangle data using R
## Load cleaned data from work before. Use summary statistics and groupings if required to sense-check
## and gain insights into the data. Make sure to use different visualisations such as scatterplots, 
## histograms, and boxplots to learn more about the data set. Explore the data and comment on the 
## insights gained from your exploratory data analysis. For example, outliers, missing values, 
## and distribution of data. Also make sure to comment on initial patterns and distributions or 
## behaviour that may be of interest to the business.

## Part 2. Making recommendations to the business.
## Investigate customer behaviour and the effectiveness of the current loyalty program based 
## on the precedent work completed as well as the statistical analysis and modelling efforts.
##  - Can we predict loyalty points given the existing features using a relatively simple MLR model?
##  - Do you have confidence in the model results (Goodness of fit evaluation)
##  - Where should the business focus their marketing efforts?
##  - How could the loyalty program be improved?
##  - How could the analysis be improved?

###############################################################################

## Load, clean and wrangle data using R

# 1. Prepare workstation
## Import libraries
library(tidyverse)
library(ggplot2)
library(stringr)
library(DataExplorer)

## Get working directory
getwd()

## Import data (Turtle_review.csv)
reviews <- read.csv(file.choose(), header=TRUE)
# reviews <- read.csv("D:\\Users\\wilson.huang\\Downloads\\R\\turtle_reviews.csv", header= T)
## View the data
View(reviews)
dim(reviews)
as_tibble(reviews)

# 2. Data Wrangling & Exploring
## Remove language and platform columns (irrelevant)
new_reviews <- select(reviews, -c('language','platform'))

## Sense-check the new data frame
dim(new_reviews)
as_tibble(reviews)

## Determine if there is any missing values in the new data frame.
sum(is.na(new_reviews))

## Rename columns
colnames(new_reviews)
new_reviews <- new_reviews %>%
  rename(
    remuneration = "remuneration..k..",
    spending_score = "spending_score..1.100."
  )

## View renamed columns
colnames(new_reviews)

## Summarise the new data frame.
head(new_reviews)
summary(new_reviews)
DataExplorer::create_report(new_reviews)

# 3. Perform EDA by creating tables and visualisations
##    - Comment on distributions, patterns or outliers based on the visual exploration of the data?
## Boxplot
boxplot(new_reviews$age)
boxplot(new_reviews$remuneration)
boxplot(new_reviews$spending_score)
boxplot(new_reviews$loyalty_points) # potential outliers
boxplot(new_reviews$product)

## Histogram
hist(new_reviews$age)
hist(new_reviews$remuneration)
hist(new_reviews$spending_score)
hist(new_reviews$loyalty_points) # potential outliers
hist(new_reviews$product)

## - Insights based on the basic observations that may require further investigation?
unique(new_reviews$product)

## Spending_score & Product code
spend_product <- aggregate(spending_score ~ product,new_reviews,mean)
spend_product

## Customer & loyalty points
client <- new_reviews %>% group_by(gender, age, remuneration,education) %>% summarise(loyalty_points = mean(loyalty_points),.groups = "drop")
client

## Sold Product Number
barplot(table(new_reviews$product),
        xlab = "product code",
        ylab = "frequency")

##  - Are there any groupings that may be useful in gaining deeper insights into customer behaviour?
##  - Are there any specific patterns that you want to investigate?


# 4. Visualisation
## Create scatterplots, histograms, and boxplots to visually explore the loyalty_points data.
## Loyalty Points Distribution (Histogram)
ggplot(data = new_reviews,
       mapping = aes(x = loyalty_points))+
  geom_histogram()+
  labs(title = "Loyalty_points Distribution")

## Loyalty Points Distribution (Boxplot)
boxplot(new_reviews$loyalty_points)

# Select appropriate visualisations to communicate relevant findings and insights to the business. (add titles and labels!)

## A. Remuneration & Loyalty points
ggplot(data = new_reviews,
      mapping = aes(x = remuneration, y = loyalty_points))+
  geom_point()+
  labs(title = "Remuneration & Loyalty Points Correlation",
       x = "Remuneration",
       y = "Loyalty Points")
## Observation: positive correlation (as indicated before in Python as well)

## Remuneration & Loyalty points (with education?)
ggplot(data = new_reviews,
       mapping = aes(x = remuneration, y = loyalty_points, colour=education))+
  geom_point()+
  labs(title = "Remuneration & Loyalty Points Correlation (by Education)",
       x = "Remuneration",
       y = "Loyalty Points")
## Observation: no clear difference by education

## Remuneration & Loyalty points (with gender?)
ggplot(data = new_reviews,
       mapping = aes(x = remuneration, y = loyalty_points, colour=gender))+
  geom_point()+
  labs(title = "Remuneration & Loyalty Points Correlation (by Age)",
       x = "Remuneration",
       y = "Loyalty Points")
## Observation: no clear difference by gender

## B. Remuneration & Spending score
ggplot(data = new_reviews,
       mapping = aes(x = remuneration, y = spending_score))+
  geom_point()+
  labs(title = "Remuneration & Spending Score Correlation")
## Observation: needs clustering (done in Python)

## C. Spending score & Loyalty points
ggplot(data = new_reviews,
       mapping = aes(x = spending_score, y = loyalty_points))+
  geom_point() +
  labs(title = "Loyalty Points with Spending Score Correlation",
       x = "Spending Score",
       y = "Loyalty Points")
## Observations: positive correlation (as indicated before in Python as well)

## Spending score & Loyalty points (by education)
ggplot(data = new_reviews,
       mapping = aes(x = spending_score, y = loyalty_points, colour = education))+
  geom_point() +
  labs(title = "Loyalty Points with Spending Score Correlation (by Education)",
       x = "Spending Score",
       y = "Loyalty Points")
## Observations: no clear difference by education

## Spending score & Loyalty Points (by gender)
ggplot(data = new_reviews,
       mapping = aes(x = spending_score, y = loyalty_points, colour = gender))+
  geom_point() +
  labs(title = "Loyalty Points with Spending Score Correlation (by Gender)",
       x = "Spending Score",
       y = "Loyalty Points")
## Observations: no clear difference by gender

## D. Age & Loyalty Points
ggplot(data = new_reviews,
       mapping = aes(x = age, y = loyalty_points, ))+
  geom_point() +
  labs(title = "Age and Loyalty Points Correlation",
       x = "Age",
       y = "Loyalty Points")
## Observations: no clear correlation.

## E. Product & Loyalty Points
ggplot(data = new_reviews,
       mapping = aes(x = product, y = loyalty_points))+
  geom_point() +
  labs(title = "Product and Loyalty Points Correlation",
       x = "Product Code",
       y = "Loyalty Points")
## Observations: no clear correlation.

## Product & Loyalty Points (by gender)
ggplot(data = new_reviews,
       mapping = aes(x = product, y = loyalty_points, colour = gender))+
  geom_point() +
  labs(title = "Product and Loyalty Points Correlation (by Gender)",
       x = "Product Code",
       y = "Loyalty Points")
## Observations: no clear trends.


## Product & Loyalty Points (by Education)
ggplot(data = new_reviews,
       mapping = aes(x = product, y = loyalty_points, colour = education))+
  geom_point() +
  labs(title = "Product and Loyalty Points Correlation (by Education)",
       x = "Product Code",
       y = "Loyalty Points")
## Observations: no clear trends.

################################################################################
################################################################################

## Making recommendations to the business.

# 1. View the data 
head(new_reviews)
dim(new_reviews)
colnames(new_reviews)
as_tibble(new_reviews)

# 2. Statistical Analysis for all Numeric Columns.
## Import library
library(moments)

## A. Age column
qqnorm(new_reviews$age)
qqline(new_reviews$age, main = "Age Distribution")
shapiro.test(new_reviews$age)
skewness(new_reviews$age)
kurtosis(new_reviews$age)
summary(new_reviews$age)
## Conclusion: age is not normally distributed.
## Conclusion: The distribution of age is moderately right-skewed, 
## meaning that there are some older individuals whose age values extend the tail of the distribution to the right.
## Conclusion: The distribution of age has normal-like tail behavior, 
## with no extreme flatness or peakness.

## B. Remuneration column
qqnorm(new_reviews$remuneration)
qqline(new_reviews$remuneration, main = "Remuneration Distribution")
shapiro.test(new_reviews$remuneration)
skewness(new_reviews$remuneration)
kurtosis(new_reviews$remuneration)
summary(new_reviews$remuneration)
## Conclusion: remuneration is not normally distributed.
## Conclusion: The distribution is slightly skewed to the right, 
## meaning the majority of data points are concentrated on the left, but there are some higher values.
## Conclusion: The distribution has slightly flatter tails than a normal distribution,
## meaning fewer extreme values (outliers).

## C. Spending_score column
qqnorm(new_reviews$spending_score)
qqline(new_reviews$spending_score)
shapiro.test(new_reviews$spending_score)
skewness(new_reviews$spending_score)
kurtosis(new_reviews$spending_score)
summary(new_reviews$spending_score)
## Conclusion: spending_score is not normally distributed.
## Conclusion: The distribution of spending_score is approximately symmetric.
## Conclusion: The distribution of spending_score has flatter tails and is less peaked than a normal distribution.

## D. Loytal_points column
qqnorm(new_reviews$loyalty_points)
qqline(new_reviews$loyalty_points)
shapiro.test(new_reviews$loyalty_points)
skewness(new_reviews$loyalty_points)
kurtosis(new_reviews$loyalty_points)
summary(new_reviews$loyalty_points)
## Conclusion: The loyalty_points variable is not normally distributed.
## Conclusion: The distribution of loyalty_points is right-skewed, meaning most values are concentrated on the left, but there are some higher loyalty point values extending the right tail.
## Conclusion: The distribution has heavier tails and a more pronounced peak,
## indicating the presence of outliers and extreme values in the loyalty_points data.

# Log-transform age column
new_reviews <- new_reviews %>% 
  mutate(log_loyalty = log(loyalty_points))


# E. Product (code) column
qqnorm(new_reviews$product)
qqline(new_reviews$product)
shapiro.test(new_reviews$product)
skewness(new_reviews$product)
kurtosis(new_reviews$product)
summary(new_reviews$product)
# Conclusion: The distribution of product is not normally distributed.
# Conclusion: The distribution of product is slightly right-skewed, 
# meaning that there are more high values extending the distribution to the right.
# Conclusion: The distribution of product has flatter tails and is less peaked, with fewer extreme values or outliers.

# 3. Correlation tests
## Remove non-numeric columns
reviews_num <- select(new_reviews, -c(gender, education, review, summary))

## Check new data frame
dim(reviews_num)
head(reviews_num)
col(reviews_num)

## Determine the corrlation 
cor(reviews_num)

## Visualise the correlation
## Import psych packages
library(psych)

## Correlation table
corPlot(reviews_num, cex=2)

## Generate a report:
DataExplorer::create_report(reviews_num)

## Key insights from the correlation table:

## Remuneration & Loyalty Points (0.62): Strong positive correlation suggests higher earners tend to accumulate more loyalty points.
## Spending Score & Loyalty Points (0.67): Strong correlation indicates higher spenders earn more loyalty points.
## Spending Score & Remuneration (0.62): Higher earners also have higher spending scores.
## Product & Remuneration (0.31): Moderate correlation, suggesting wealthier customers may prefer certain products.
## Age & Spending Score (-0.22): Weak negative correlation; younger customers may spend more than older ones.
## Overall, remuneration, spending, and loyalty points are closely linked, while age has minimal impact.

## Possible Further Investigations:
## High Loyalty and Spending Behavior: Investigating the strong relationships between loyalty points, spending score, and remuneration might yield insights into customer segmentation and loyalty program effectiveness.
## Product Preferences: The moderate correlations between remuneration and product engagement might suggest that specific products are more attractive to wealthier customers.

# 4. Create different regression models
## Model a (all)
modela <- lm(loyalty_points ~ age + remuneration + spending_score + product, data = reviews_num)

## Check accuracy of modela
summary(modela)

## Conclusion Modela
## The model shows that age, remuneration, and spending score are all strong predictors of loyalty points, 
## while product does not significantly contribute. The high R-squared value suggests the model is a good fit for the data, 
## explaining most of the variation in loyalty points.

## Model b (removing product as variables)
modelb <- lm(loyalty_points~ age + remuneration + spending_score, data = reviews_num)

## Check accuracy of modelb
summary(modelb)


## Conclusion Modelb
## Modelb performs just as well as modela, but with one fewer variable (product). 
## This implies that the product variable was unnecessary for predicting loyalty points. 
## Both models explain around 84% of the variance, but modelb is more parsimonious, 
## making it preferable since it simplifies the model without sacrificing performance.

## Model c (remunuration and spending score only)
modelc <- lm(loyalty_points~remuneration + spending_score, data = reviews_num)

## Check accuracy
summary(modelc)

## Conclusions:
## Modelc performs worse than modela and modelb because it omits age,
## leading to a lower R-squared and higher residual error. Including age in the model improves prediction of loyalty points.


## Model d (model b but suing logged loyalty points)
modeld <- lm(log_loyalty~ age + remuneration + spending_score, data = reviews_num)

## Check accuracy of modelb
summary(modeld)

## Conclusion: lower R-sqaured than model b.
## We will use Model b to make predictions.

## Import package
library(car)

## Check for multicollinearity 
vif(modelb)

## All VIF values are less than 5, 
## indicating a low level of multicollinearity among the predictors.

## Residuals Analysis
## Extract residuals
residuals <- residuals(modelb)

## Plot residuals
ggplot(data.frame(residuals = residuals), aes(x = 1:length(residuals), y = residuals)) +
  geom_point(color = 'darkred') +
  geom_hline(yintercept = 0, color = 'black', lwd = 1 ) +
  labs(title = "Residual Plot", x = "Observation", y = "Residuals")
## Observations from the residual plot
## **Random Scatter:** 
## The residuals appear to be randomly scattered around the horizontal line at zero. 
## This is a good sign as it suggests that the model's errors are not systematically related to the fitted values.
## **No Clear Pattern:** There's no obvious pattern or trend in the residuals, 
## indicating that the linear regression model is appropriate for the data.

# 5. Predicted values using Model b
## Create an object to store prediction values
predicted_loyalty_point <- predict(modelb)

## Actual vs Predicted Plot
plot(reviews_num$loyalty_points, predicted_loyalty_point,
     xlab = "Actual Loyalty Points in Given Data",
     ylab = "Predicted Loyalty Points using Modelb",
     main = "Actual vs Predicted Loyalty Points",
     col = "darkblue", pch = 19)
  abline(a=0, b=1, col="darkred", lwd=2)  # Line for perfect prediction

## Observation:
## Model Performance: The positive correlation indicates that the model has some predictive power. 
## However, the scatter and underestimation at higher levels suggest room for improvement.

# 6. Create new scenarios
virtual_customers <- data.frame(
  index = paste0("Virtual Customer ", 1:9),  # Naming the index column
  age = c(25, 30, 35, 45, 50, 55, 60, 65, 70),  # Varying ages
  remuneration = c(35, 42.5, 65, 80, 100, 120, 100, 80, 82),  # Different salaries
  spending_score = c(20, 30, 40, 50, 60, 65, 80, 50, 30)  # Spending scores
)

## Check the data frame
virtual_customers

## Predict loyalty points for these new customers
predicted_loyalty_virtual <- predict(modelb, newdata = virtual_customers)
predicted_loyalty_virtual

## Visual the results
plot(predicted_loyalty_virtual,
     type = "b",  # Adds lines between the points
     main = "Predicted Loyalty Points based on Virtual Scenarios",
     xlab = "Virtual Customer",
     ylab = "Predicted Loyalty Points (Virtual)")

## Conclusion:
## Predictions based on specific customer scenarios (persona) can guide business decisions, 
## helping to anticipate loyalty levels based on demographic and financial profiles.

###############################################################################
###############################################################################


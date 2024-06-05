### Thesis 
# The reference price effect on product returns 
# Clear workspace ----
rm(list = ls())

# set working directory ----
setwd("C:/Users/gtsac/OneDrive") 

install.packages("corrplot")
install.packages("tidyverse")
install.packages("DescTools")
install.packages("MASS")
install.packages("DescTools")
install.packages("ggplot2")
install.packages("caTools")    
install.packages('car')
install.packages("ROCR")
install.packages("frm")

# load the required libraries
library(corrplot)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(DescTools)
library(MASS)
library(car)
library(caTools)
library(ROCR)
library(lmtest)
library(lubridate)
library(foreign)
library(VGAM)
library(frm)
library(sandwich)
library(lmtest)

# Open returns data ----
returns <- read.csv("returns_sample_MT.csv")

# first touch
summary(returns)
str(returns)

# Keep only rows that a purchase was done or a return occurred in the dataset
returns <- filter(returns, purchase_amount !=0.00 | return_dummy !=0)
# The dataset is reduced by 20266 rows. 
sum(returns$return_value != 0) #Rows where a return occurred are 139895 (almost 43,5 % of total cases)

# factors 
returns$ID <- as.factor(returns$ID) 
returns$ID_article <- as.factor(returns$ID_article)
returns$categories <- as.factor(returns$categories)
returns$day_of_week <- as.factor(returns$day_of_week)
returns$week_of_year <- as.factor(returns$week_of_year)
returns$year <- as.factor(returns$year)
returns$return_dummy <- as.factor(returns$return_dummy)
summary(returns)

### DATA CLEANING ----
# Positive values on returns, coupon, and sale
returns$return_value <- abs(returns$return_value)
returns$sale <- abs(returns$sale)
returns$coupon <- abs(returns$coupon)
# Clean dates
class(returns$date)
returns$date <- format(as.Date(returns$date, '%m/%d/%Y'), '%d/%m/%Y')
returns$date <- as.Date(returns$date, '%d/%m/%Y')
class(returns$date) #accepted form
# Quarter
returns$Quarter <- as.factor(quarter(returns$date))

aggregated_returns <- returns %>%
  group_by(ID, date) %>%
  summarise(purchase_sum = sum(purchase_amount),
            return_sum = sum(return_value),
            return_dummy = ifelse(return_sum > 0, 1, 0))

# Change category names due to space put at the end of the names
levels(unique(returns$categories))
levels(returns$categories)[levels(returns$categories) == "kitchen applicances "] <- "kitchen appliances"
levels(returns$categories)[levels(returns$categories) == "ladies "] <- "ladies"
levels(returns$categories)[levels(returns$categories) == "household "] <- "household"
levels(unique(returns$categories))

# Clean price
summary(returns$price_n) #assuming that the negative prices have been mistyped, transform them to positives
returns$price_n <- abs(returns$price_n) #absolute values used to avoid the negative values
summary(returns$price_n)
# Prices per category. Outliers
boxplot(returns$price_n[returns$price_n>0]~returns$categories[returns$price_n>0],col="goldenrod",ylab = "Prices (\u20AC)", main = "Prices per category", xlab = NULL, las = 2)

# Time pattern : returns affected by quarter of year
anova_out <- aov (return_value ~ Quarter, data = returns)
summary(anova_out)
rm(anova_out)

# identification of missing values
sum(is.na(returns)) #no missings
# identification of duplicates - bit expected
sum(duplicated(returns)) #number is 20 
duplicate_rows <- duplicated(returns) | duplicated(returns, fromLast = TRUE)
duplicate_returns <- subset(returns, duplicate_rows)
# can be explained, on the basis that same returns and products can be bought on the same day. 
## Also the number of duplicates is too small, compared to the dataset, to affect the  analysis
rm(duplicate_returns, duplicate_rows)

# New variable, called return ratio (share), equals returned products percentage to the purchased value 
returns$return_ratio <- returns$return_value / returns$purchase_amount 
summary(returns$return_ratio) 
# One case causes inf value. That is where return <>0 but a purchase didn't occur.
# Fix this by changing to zero.
returns$return_ratio[!is.finite(returns$return_ratio)] <- 0
summary(returns$return_ratio)
# max return ratio is over 1 (not explained as share). This happens, but in countable cases (less than 10).
## change the return ratios of over than 1 to 1.
returns$return_ratio <- ifelse(returns$return_ratio > 1, 1, returns$return_ratio)
summary(returns$return_ratio)
hist(returns$return_ratio, labels = TRUE)

# variances 
sd(returns$purchase_amount)
sd(returns$price_n)
sd(returns$amount)
sd(returns$return_value)
sd(returns$coupon)
sd(returns$sale)
sd(returns$return_ratio)
# another "refresh"
str(returns)
summary(returns)


# Price_n times the amount DOES NOT always equal the purchase_amount in the dataset. Here it's checked: ----
condition <- returns %>%
  mutate(condition_of_correction = ifelse(price_n * amount != purchase_amount, 'wrong', 'correct')) %>%
  group_by(condition_of_correction) %>%
  summarise(Count = n())
condition
rm(condition)

# Correlation between numerics ----
# pearson correlation matrix and heatmap
numeric_returns <- returns[, c("purchase_amount", "price_n", 'amount', "return_value", "coupon", "sale", "return_ratio")]
cor_matrix <- cor(numeric_returns, method = "pearson")
corrplot(cor_matrix, method = "color", col = colorRampPalette(c("#FF0000", "#FFFFFF", "#0000FF"))(100))
#shows extreme correlation between purchase amount and price_n, which makes sense as purchase amount is almost price_n times the amount
rm(numeric_returns)

# Create a dataset of sole occurrence of returns, in case it is needed 
returns1 <- filter(returns, return_dummy !=0) 

# Operationalizations of reference price ----
## Subcategory level is chosen to calculate that. Results will be more concrete than on a category level
# 1st operationalization : reference price is the average of highest, lowest and mean price in each subcategory.
REFERENCE_PRICE1 <- returns %>% group_by(id_subcategory) %>% summarize(reference_price1 = mean(price_n))
REFERENCE_PRICE2 <- returns %>% group_by(id_subcategory) %>% summarize(reference_price2 = min(price_n))
REFERENCE_PRICE3 <- returns %>% group_by(id_subcategory) %>% summarize(reference_price2 = max(price_n))
ref_price <- full_join(REFERENCE_PRICE1, REFERENCE_PRICE2)
ref_price1 <- full_join(ref_price, REFERENCE_PRICE3, by = 'id_subcategory')
ref_price1$reference_price <- (ref_price1$reference_price1 + ref_price1$reference_price2.x + ref_price1$reference_price2.y) / 3
ref_price1 <- ref_price1[, -c(2:4)]
rm(ref_price, REFERENCE_PRICE1, REFERENCE_PRICE2, REFERENCE_PRICE3)
returns2 <- full_join(returns, ref_price1, by = 'id_subcategory')

# additional new variables:
# taking the price difference (difference of actual price from the reference price)
returns2$price_dif <- returns2$price_n - returns2$reference_price
hist(returns2$price_dif)
returns2$price_diff_percentage <- returns2$price_dif / returns2$price_n
returns2$price_diff_percentage <- abs(returns2$price_diff_percentage)
returns2$perception <- ifelse(returns2$price_dif >= 0, "loss", "gain")
returns2 %>% group_by(perception) %>% summarise(Count = n())
returns2$perception <- as.factor(returns2$perception)
summary(returns2)
str(returns2)
sd(returns2$reference_price)
sd(returns2$price_dif)
sd(returns2$price_diff_percentage)


anova_1 <- aov(return_value ~ reference_price : perception, data = returns2)
summary(anova_1)
rm(anova_1)

anova_2 <- aov(return_ratio ~ reference_price : perception, data = returns2)
summary(anova_2)
rm(anova_2)

# correlation matrix for returns2
numeric_returns2 <- returns2[, c("purchase_amount", "price_n", "amount", "return_value", "coupon", "sale", "return_ratio", "reference_price", "price_dif", "price_diff_percentage")]
cor_matrix2 <- cor(numeric_returns2, method = "pearson")
corrplot(cor_matrix2, method = "color", col = colorRampPalette(c("#FF0000", "#FFFFFF", "#0000FF"))(100))
rm(numeric_returns2)

# 2nd operationalization: reference price is the most frequent price charged in each subcategory
# Calculate the most frequent value for each level
result <- tapply(returns$price_n, returns$id_subcategory, FUN = function(x) {
  freq_table <- table(x)
  most_frequent <- names(freq_table)[which.max(freq_table)]
  return(most_frequent)
})
# Print the result
print(result)
ref_price2 <- data.frame(id_subcategory = names(result), reference_price = unlist(result))
str(ref_price2)
ref_price2$id_subcategory <- as.integer(ref_price2$id_subcategory)
ref_price2$reference_price <- as.numeric(ref_price2$reference_price)
rm(result)
# new dataset, containing the reference price (2)
returns3 <- full_join(returns, ref_price2)

# time pattern on reference price
anova_2 <- aov(reference_price ~ Quarter, data = returns3)
summary(anova_2)
rm(anova_2)

returns3$price_dif <- returns3$price_n - returns3$reference_price
returns3$price_diff_percentage <- returns3$price_dif / returns3$price_n
returns3$price_diff_percentage <- abs(returns3$price_diff_percentage)
hist(returns3$price_dif)
returns3$perception <- ifelse(returns3$price_dif >= 0, "loss", "gain")
str(returns3$perception)
returns3$perception <- as.factor(returns3$perception)
returns3 %>% group_by(perception) %>% summarise(Count = n())
summary(returns3)
sd(returns3$reference_price)
sd(returns3$price_dif)
sd(returns3$price_diff_percentage)

# correlation matrix for returns3
numeric_returns3 <- returns3[, c("purchase_amount", "price_n", "amount", "return_value", "coupon", "sale", "return_ratio", "reference_price", "price_dif", "price_diff_percentage")]
cor_matrix3 <- cor(numeric_returns3, method = "pearson")
corrplot(cor_matrix3, method = "color", col = colorRampPalette(c("#FF0000", "#FFFFFF", "#0000FF"))(100))
rm(numeric_returns3)

# DESCRIPTIVE ANALYSIS----
#customer ID
unique_customers <- length(unique(returns$ID)) #10000 unique customers
#Identify and plot top 10 customer IDs
top_customers <- names(sort(table(returns$ID), decreasing = TRUE))[1:10]
customer_subset <- returns[returns$ID %in% top_customers, ]
ggplot(customer_subset, aes(x = ID)) +
  geom_bar(fill = "goldenrod") +
  xlab("Customer ID") +
  ylab("Purchase Frequency") +
  ggtitle("Top 10 Customers (Event Frequency)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
rm(top_customers, customer_subset)

# Number of customers per year
t_purchasePerYear<- table(returns$year,returns$ID)
nCustPerYear <- apply(t_purchasePerYear,1,function(x){sum(x>0)})
nCustPerYear #4 years in a dataset

# number of new customers per year
nYear = length(nCustPerYear)
nNewCustPerYear <- nCustPerYear[2:nYear] - nCustPerYear[1:(nYear-1)]
nNewCustPerYear
# get the number of customers that bought this year and last
t_didBuyThisYearAndLast <- t_purchasePerYear[2:nYear,]>0 & t_purchasePerYear[1:(nYear-1),]>0
nBuyThisYearAndLast <- apply(t_didBuyThisYearAndLast,1,function(x){sum(x)})
nBuyThisYearAndLast
# Divide by the number of customers per year to get the percent
pcntBuyThisYearAndLast <- nBuyThisYearAndLast / nCustPerYear[2:nYear] *100
pcntBuyThisYearAndLast
rm(t_purchasePerYear, t_didBuyThisYearAndLast)

# total returns and purchases per customer ID
total_purchases_returns <- returns %>%
  group_by(ID) %>%
  summarize(total_returns = sum(return_value), total_purchases = sum(purchase_amount))
summary(total_purchases_returns)
hist(total_purchases_returns$total_returns, labels = TRUE)
hist(total_purchases_returns$total_purchases)

# Number of purchases per customer ID
purchases_cust <- returns %>% group_by(ID) %>% summarise(Purchases = n())
summary(purchases_cust)
hist(purchases_cust$Purchases, labels = TRUE)

# Returns that a customer ID has done
returns_cust <- returns1 %>% group_by(ID) %>% summarise(returns = n())
summary(returns_cust)
hist(returns_cust$returns, labels = TRUE)

# ID ARTICLE
# Count the number of unique article IDs
unique_articleids <- length(unique(returns$ID_article)) #123618

# Top 10 article IDs to be purchased
top_articleids <- names(sort(table(returns$ID_article), decreasing = TRUE))[1:10]
articleid_subset <- returns[returns$ID_article %in% top_articleids, ]
ggplot(articleid_subset, aes(x = ID_article)) +
  geom_bar(fill = "goldenrod") +
  xlab("Article ID") +
  ylab("Purchase Frequency") +
  ggtitle("Top 10 Article IDs (Purchase Frequency)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Top 10 returned article IDs
top_returnedarticleids <- names(sort(table(returns1$ID_article), decreasing = TRUE))[1:10]
articleid_subset <- returns1[returns1$ID_article %in% top_returnedarticleids, ]
ggplot(articleid_subset, aes(x = ID_article)) +
  geom_bar(fill = "goldenrod") +
  xlab("Article ID") +
  ylab("Return Frequency") +
  ggtitle("Top 10 Article IDs (Return Frequency)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
rm(articleid_subset, top_articleids, top_returnedarticleids)

# CATEGORIES
ggplot(returns, aes(x = categories)) +
  geom_bar(fill = "goldenrod") +
  xlab("Categories") +
  ylab("Purchase Frequency") +
  ggtitle("Purchase Frequency for each product category") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#mostly bought category is ladies products by far, then underwear and gents. Then beachwear.

#CONSEQUENTLY: top 10 returned categories
top_categories <- names(sort(table(returns1$categories), decreasing = TRUE))[1:10]
cat1_subset <- returns1[returns1$categories %in% top_categories, ]
ggplot(cat1_subset, aes(x = categories)) +
  geom_bar(fill = "goldenrod") +
  xlab("Categories") +
  ylab("Return Frequency") +
  ggtitle("Top 10 Categories (Return Frequency)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
rm(top_categories, cat1_subset)

#number of products per category
products_per_category <- returns %>% group_by(categories) %>% summarise(n_unique_products = n_distinct(ID_article))
barplot(products_per_category$n_unique_products, names.arg = products_per_category$categories, las = 2, col = colorRampPalette(c("#0000FF", "#FFB800"))(25))

#number of subcats per category
subcat_per_category <- returns %>% group_by(categories) %>% summarise(n_unique_subcat = n_distinct(id_subcategory))
barplot(subcat_per_category$n_unique_subcat, names.arg = subcat_per_category$categories, las = 2, col = colorRampPalette(c("#0000FF", "#FFB800"))(25))

#max return per category
max_category_return <- returns1 %>% group_by(categories) %>% summarize(max_return = max(return_value))
barplot(max_category_return$max_return, names.arg = max_category_return$categories, las = 2, col = colorRampPalette(c("#0000FF", "#FFB800"))(25))

#max purchase per category
max_category_purchase <- returns %>% group_by(categories) %>% summarize(max_purchase = max(purchase_amount))
barplot(max_category_purchase$max_purchase, names.arg = max_category_purchase$categories, las = 2, col = colorRampPalette(c("#0000FF", "#FFB800"))(25))

# DAY OF WEEK
levels(returns$day_of_week) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
plot(returns$day_of_week, col = "goldenrod")
#Conclusion: most purchases occur at the start of the week
# How about returns?
levels(returns1$day_of_week) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
plot(returns1$day_of_week, col = "deepskyblue")

# Group the data by 'day_of_week' and calculate the total return value for each day
return_summary <- returns %>%
  group_by(day_of_week) %>%
  summarise(total_return_value = sum(return_value))
purchase_summary <- returns %>%
  group_by(day_of_week) %>%
  summarise(total_purchase_value = sum(purchase_amount))
# If you want to see the summary, you can print the result
print(return_summary)
plot(return_summary)
print(purchase_summary)
plot(purchase_summary)

# ID SUBCATEGORY
# Count the number of unique subcategory IDs
str(returns$id_subcategory)
unique_subcatIDs <- length(unique(returns$id_subcategory))
summary(returns$id_subcategory) #number of purchases per subcategory
summary(returns1$id_subcategory) #number of returns per subcategory
returns$id_subcategory <- as.factor(returns$id_subcategory)
#AMOUNT 
# THIS VARIABLE is not categorical!! There are continuous values in it!!!
summary(returns$amount)
boxplot(returns$amount) # outliers
hist(returns$amount, labels = TRUE)

#PRICE_N : previously on the code, outlier were found.
summary(returns$price_n)
hist(returns$price_n, labels = TRUE)

#PURCHASE_AMOUNT
summary(returns$purchase_amount)
boxplot(returns$purchase_amount) # outliers
hist(returns$purchase_amount, labels = TRUE)

#COUPON
summary(returns$coupon)
boxplot(returns$coupon) # outliers
hist(returns$coupon, labels = TRUE)

#SALE
summary(returns$sale)
boxplot(returns$sale) # outliers
hist(returns$sale, labels = TRUE) 

#YEAR
length(unique(returns$year)) #4 years
ggplot(data = returns, aes(x = year)) + geom_bar(fill = "blue", color = "black") + labs(title = "Yearly Purchase Events", x = "Year", y = "Count")
ggplot(data = returns1, aes(x = year)) + geom_bar(fill = "gold", color = "black") + labs(title = "Yearly Return Events", x = "Year", y = "Count")

#WEEK_OF_YEAR
length(unique(returns$week_of_year)) #52 weeks

# Group the data by 'week_of_year' and calculate the return frequency for each week
return_subset <- returns1 %>%
  group_by(week_of_year) %>%
  summarise(return_frequency = n()) %>%
  top_n(10, return_frequency)  # Select top 10 weeks based on return frequency

# Sort the data by 'week_of_year' in ascending order
return_subset <- return_subset[order(return_subset$week_of_year), ]

# Plot the top 10 weeks based on return frequency
ggplot(return_subset, aes(x = week_of_year, y = return_frequency)) +
  geom_bar(stat = "identity", fill = "gold") +
  xlab("Week of Year") +
  ylab("Return Frequency") +
  ggtitle("Return Frequency per Week (Top 10)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Group the data by 'week_of_year' and calculate the total return value for each week
return_summary1 <- returns %>%
  group_by(week_of_year) %>%
  summarise(total_return_value = sum(return_value)) %>%
  top_n(10, total_return_value)  # Select top 10 weeks based on total return values

# Sort the data by 'week_of_year' in ascending order
return_summary1 <- return_summary1[order(return_summary1$week_of_year), ]

# Plot the top 10 weeks
ggplot(return_summary1, aes(x = week_of_year, y = total_return_value)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Week of Year", y = "Total Return Value") +
  ggtitle("Top 10 Weeks by Total Return Values") +
  theme_bw()

# RETURN_VALUE
summary(returns$return_value)
boxplot(returns$return_value) # outliers
hist(returns$return_value, labels = TRUE) 

# RETURN_DUMMY
summary(returns$return_dummy)
return_count <- table(returns$return_dummy)
barplot(return_count, main="Return vs No return count",
        xlab="Return / No return sessions", ylim = c(0,190000), col = "deepskyblue")

#max purchase per date
max_purchase_dates <- returns %>% group_by(date) %>% summarize(max_purchase = max(purchase_amount))
plot(max_purchase_dates$date, max_purchase_dates$max_purchase, type = "l", xlab = "Date", ylab = "Max Purchase", main = "Maximum Purchases over time", col = "deepskyblue")

#max return per date
max_return_dates <- returns1 %>% group_by(date) %>% summarize(max_return = max(return_value))
plot(max_return_dates$date, max_return_dates$max_return, type = "l", xlab = "Date", ylab = "Max Return", main = "Maximum Returns over time", col = "deepskyblue")


# REGRESSION ANALYSIS ----
# returns 2 (1st operationalization of RP as average price)
# Get a 75% estimation sample and 25% validation sample
set.seed(1234)
returns2$estimation_sample <-rbinom(nrow(returns2), 1, 0.75)
returns2$validation_sample <- 1 - returns2$estimation_sample

# model 1 (returns2) concerning the return value
# Didn't include categories , including subcategories would calculate 665 coefs
## which takes too long.
## Also included the amount. Omitted variables judging by the correlation tests
modelreturn_2.1 <- lm(return_value ~ Quarter + reference_price + amount + date + day_of_week + 
                       perception + price_diff_percentage + sale + coupon, data = returns2, subset=estimation_sample ==1 )
summary(modelreturn_2.1)
vif(modelreturn_2.1) #no multicollinearity

# model without reference price 
modelreturn_2.2 <- lm(return_value ~ Quarter + amount + date + day_of_week +
                      perception + sale + price_diff_percentage + coupon, data = returns2, subset=estimation_sample ==1 )
summary(modelreturn_2.2)
AIC(modelreturn_2.1, modelreturn_2.2) #including RP is better
BIC(modelreturn_2.1, modelreturn_2.2) #including RP is better
anova(modelreturn_2.1, modelreturn_2.2)
vif(modelreturn_2.2) #test for multicollinearity, again no multicollinearity

# null model - same in returns2 and returns3
modelreturn_null <- lm(return_value ~ 1, data = returns2, subset=estimation_sample ==1 )
summary(modelreturn_null)
AIC(modelreturn_null) #3264429
BIC(modelreturn_null) #3264450

# validate the model
modelreturnval_2 <- lm(return_value ~ Quarter + reference_price + amount + date + day_of_week + 
                        perception + price_diff_percentage + sale + coupon + reference_price: perception + reference_price: price_diff_percentage , data = returns2, subset=validation_sample ==1 )
summary(modelreturnval_2)
AIC(modelreturnval_2)
BIC(modelreturnval_2)
vif(modelreturnval_2, type = 'predictor') #no multicollinearity

# model 2 of return_dummy (testing the probability of a return affected by factors)
## with reference price
modeldummy_2.1 <- glm(return_dummy ~ Quarter  + reference_price + amount + date + 
                        day_of_week + perception + sale + price_diff_percentage + 
                       coupon, data = returns2, family = binomial, subset=estimation_sample ==1 )
summary(modeldummy_2.1)
exp(coef(modeldummy_2.1))
vif(modeldummy_2.1)

#not including reference price
modeldummy_2.2 <- glm(return_dummy ~ Quarter + amount + date + 
                        day_of_week + perception + sale + price_diff_percentage + 
                        coupon, data = returns2, family = binomial, subset=estimation_sample ==1 )
summary(modeldummy_2.2)
exp(coef(modeldummy_2.2))
vif(modeldummy_2.2)
AIC(modeldummy_2.1, modeldummy_2.2) 
BIC(modeldummy_2.1, modeldummy_2.2) 

#null model - same in returns2 and returns3
modeldummy_0 <- glm(return_dummy ~ 1, data = returns2, family = binomial, subset=estimation_sample ==1 )
summary(modeldummy_0)
exp(coef(modeldummy_0))
AIC(modeldummy_0) #329937.3
BIC(modeldummy_0) #329947.7

# validate the model
modeldummyval_2 <- glm(return_dummy ~ Quarter  + reference_price + amount + date + 
                        day_of_week + perception + sale + price_diff_percentage + 
                        coupon + reference_price: perception + reference_price: price_diff_percentage, data = returns2, family = binomial, subset=validation_sample ==1 )
summary(modeldummyval_2)
exp(coef(modeldummyval_2))
AIC(modeldummyval_2) #100741.2
BIC(modeldummyval_2) #100889.9
vif(modeldummyval_2)

# model 3 of return share (ratio) - fractional logistic regression
# Fit the fractional logistic regression model
modelratio_2.1 <- glm(return_ratio ~  reference_price + Quarter + amount + date + 
                        day_of_week + perception + sale + price_diff_percentage + 
                        coupon , data = returns2, family = quasibinomial("logit"), subset=estimation_sample ==1 )
coeftest(modelratio_2.1, vcov.=vcovHC(modelratio_2.1, type="HC0"))
vif(modelratio_2.1)

modelratio_2.2 <- glm(return_ratio ~ Quarter  + amount + date + 
                        day_of_week + perception + sale + price_diff_percentage + 
                        coupon, data = returns2, family = quasibinomial("logit"), subset=estimation_sample ==1 )

coeftest(modelratio_2.2, vcov.=vcovHC(modelratio_2.2, type="HC0"))
vif(modelratio_2.2)

modelratio_null <- glm(return_ratio ~ 1, data = returns2, family = quasibinomial("logit"), subset=estimation_sample ==1 )
coeftest(modelratio_null, vcov.=vcovHC(modelratio_null, type="HC0"))

#validate the model
modelratioval_2 <- glm(return_ratio ~  reference_price + Quarter + amount + date + 
                        day_of_week + perception + sale + price_diff_percentage + 
                        coupon + reference_price: perception + reference_price: price_diff_percentage, data = returns2, family = quasibinomial("logit"), subset=validation_sample ==1 )
coeftest(modelratioval_2, vcov.=vcovHC(modelratioval_2, type="HC0"))
vif(modelratioval_2, type = 'predictor')

# returns 3 (2nd operationalization of RP as most frequent price per subcategory) ----
# Get a 75% estimation sample and 25% validation sample
set.seed(1234)
returns3$estimation_sample <-rbinom(nrow(returns3), 1, 0.75)
returns3$validation_sample <- 1 - returns3$estimation_sample

# models (returns3) concerning the return value
# with reference price
# estimate the model
modelreturn_3.1 <- lm(return_value ~ Quarter + reference_price +  amount + date + day_of_week +
                        perception + sale + price_diff_percentage + coupon, data = returns3, subset = estimation_sample ==1)
summary(modelreturn_3.1)
vif(modelreturn_3.1, type = 'predictor') #no multicollinearity

#not including reference price
modelreturn_3.2 <- lm(return_value ~ Quarter + amount + date + day_of_week +
                        perception + sale + price_diff_percentage + coupon, data = returns3, subset = estimation_sample ==1)
summary(modelreturn_3.2)
AIC(modelreturn_3.1, modelreturn_3.2) #including RP is better
BIC(modelreturn_3.1, modelreturn_3.2) #including RP is better
anova(modelreturn_3.1, modelreturn_3.2)
vif(modelreturn_3.2) #test for multicollinearity

# null model - same in returns2 and returns3
modelreturn_null <- lm(return_value ~ 1, data = returns3, subset = estimation_sample ==1)
summary(modelreturn_null)
AIC(modelreturn_null) #3264429
BIC(modelreturn_null) #3264450

#validate the effects
modelreturnval_3 <- lm(return_value ~ Quarter + reference_price + amount + date + day_of_week +
                        perception + sale + price_diff_percentage + coupon + reference_price: perception + reference_price: price_diff_percentage, data = returns3, subset = validation_sample ==1)
summary(modelreturnval_3)
AIC(modelreturnval_3)
BIC(modelreturnval_3)
vif(modelreturnval_3) #no multicollinearity

# model of return_dummy (testing the probability of a return affected by factors)
## with reference price
modeldummy_3.1 <- glm(return_dummy ~ Quarter  + reference_price + amount + date + 
                        day_of_week + perception + sale + price_diff_percentage + 
                        coupon, data = returns3, family = binomial, subset = estimation_sample == 1)
summary(modeldummy_3.1)
exp(coef(modeldummy_3.1))
vif(modeldummy_3.1)

# not including reference price
modeldummy_3.2 <- glm(return_dummy ~ Quarter + amount + date + 
                        day_of_week + perception + sale + price_diff_percentage + 
                        coupon, data = returns3, family = binomial, subset = estimation_sample == 1)
summary(modeldummy_3.2)
exp(coef(modeldummy_3.2))
vif(modeldummy_3.2)
AIC(modeldummy_3.1, modeldummy_3.2) #AIC proves the inclusion of reference price improves the model
BIC(modeldummy_3.1, modeldummy_3.2) #The BIC proves the same

#null model - same in returns2 and returns3
modeldummy_0 <- glm(return_dummy ~ 1, data = returns3, family = binomial, subset = estimation_sample == 1)
summary(modeldummy_0)
exp(coef(modeldummy_0))
AIC(modeldummy_0) #329937.3
BIC(modeldummy_0) #329947.7

#validate
modeldummyval_3 <- glm(return_dummy ~ Quarter  + reference_price + amount + date + 
                        day_of_week + perception + sale + price_diff_percentage + 
                        coupon + reference_price: perception + reference_price: price_diff_percentage, data = returns3, family = binomial, subset = validation_sample == 1)
summary(modeldummyval_3)
exp(coef(modeldummyval_3))
AIC(modeldummyval_3)
BIC(modeldummyval_3)
vif(modeldummyval_3)

# model 3 of return share (ratio) - fractional logistic regression
# Fit the fractional logistic regression model
modelratio_3.1 <- glm(return_ratio ~  reference_price + Quarter + amount + date + 
                        day_of_week + perception + sale + price_diff_percentage + 
                        coupon , data = returns3, family = quasibinomial("logit"))
coeftest(modelratio_3.1, vcov.=vcovHC(modelratio_3.1, type="HC0"))
vif(modelratio_3.1)

modelratio_3.2 <- glm(return_ratio ~ Quarter  + amount + date + 
                        day_of_week + perception + sale + price_diff_percentage + 
                        coupon, data = returns3, family = quasibinomial("logit"))
coeftest(modelratio_3.2, vcov.=vcovHC(modelratio_3.2, type="HC0"))
vif(modelratio_3.2)

modelratio_null <- glm(return_ratio ~ 1, data = returns3, family = quasibinomial("logit"))
coeftest(modelratio_null, vcov.=vcovHC(modelratio_null, type="HC0"))
vif(modelratio_null)

#validate the model
modelratioval_3 <- glm(return_ratio ~  reference_price + Quarter + amount + date + 
                         day_of_week + perception + sale + price_diff_percentage + 
                         coupon + reference_price: perception + reference_price: price_diff_percentage, data = returns3, family = quasibinomial("logit"), subset=validation_sample ==1 )
coeftest(modelratioval_3, vcov.=vcovHC(modelratioval_3, type="HC0"))
vif(modelratioval_3)

#### CHECK CHECK ----
# Limitations
# Create a scatter plot of residuals vs. predicted values
plot(modelreturn_2.1$fitted.values, resid(modelreturn_2.1),
     xlab = "Predicted Values",
     ylab = "Residuals",
     main = "Residual Plot") 
dwtest(modelreturn_2.1) #autocorrelation existing

# Run the Breusch-Pagan test
bptest(modelreturn_2.1) # result is significant. Heteroscedasticity is existent.

# Run the Goldfeld-Quandt test
gqtest(modelreturn_2.1) # result is significant. Heteroscedasticity is existent.

# Create groups based on an independent variable (e.g., categories)
grouped_residuals <- split(resid(modelreturn_2.1), returns2$categories)
# Run Bartlett's test
bartlett.test(grouped_residuals)

# Estimate the weights based on residuals
weights <- 1 / residuals(modelreturn_2.1)^2

# Run WLS by specifying weights
wls_model <- lm(return_value ~ Quarter + reference_price + amount + date + day_of_week +
                  perception + sale + price_diff_percentage + coupon, 
                data = returns2, weights = weights)
summary(wls_model)

# Fit the model both directions
Logistic_regression_both <- stepAIC(modelreturn_2.1, direction="both", trace = TRUE)
summary(Logistic_regression_both)

# THESE DON'T WORK!!

# Get predictions for all observations
predictions_model2 <- predict(Logistic_regression2, type = "response", newdata= our_validation_dataset2)

### After this you can calculate the fit criteria on this validation sample
# Hit rate
predicted_model2 <- ifelse(predictions_model2>.5,1,0)
summary(predicted_model2)
hit_rate_model2 <- table(returns2$validation_sample, predicted_model2, dnn= c("Observed", "Predicted"))
hit_rate_model2
#Get the hit rate
(hit_rate_model2[1,1]+hit_rate_model2[2,2])/sum(hit_rate_model2)
makeLiftPlot(predicted_model2)

#TDL table
decile_predicted_model2 <- ntile(predictions_model2, 10)
decile_model2 <- table(returns$return_dummy, decile_predicted_model2, dnn= c("Observed", "Decile"))
decile_model2

pred_model2 <- prediction(predictions_model2, returns$return_dummy)
perf_model2 <- performance(pred_model2,"tpr","fpr")
plot(perf_model2,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1)
auc_model2 <- performance(pred_model2,"auc")

#The Gini is related to the “Area under the Curve” (AUC), namely by: Gini = AUC*2 – 1
#So to get the Gini we do:
as.numeric(auc_model2@y.values)*2-1
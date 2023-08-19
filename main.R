# loading libraries
library(tidyverse)
library(caret)
library(lubridate)
library(randomForest)
library(caTools)

# loading data
df <- read_csv('electric_bike_ratings_2212.csv', show_col_types = FALSE)

# loading data
df%>%head()
df%>%tail()

# shape of data
dim(df)
str(df)

# descriptive statistics
summary(df)

# converts columns into appropriate types

df$owned <- factor(df$owned)

df$make_model <- factor(df$make_model)

df$web_browser <- factor(df$web_browser)

df$primary_use <- factor(df$primary_use)

df$value_for_money <- factor(df$value_for_money)

df$overall_rating <- as.numeric(df$overall_rating)


# Using regular expression with gsub() to remove number and special sign for "review_month"
df$review_month <- gsub("[0-9-]", "", df$review_month)
df$review_month <- factor(ifelse(is.na(df$review_month), "unknown", df$review_month))
levels(df$review_month)

# Using regular expression with str_extract to remove "10" and special sign for "value_for_money"

df$value_for_money <-  as.integer(str_extract(df$value_for_money, "[0-9]+"))

# repalce "-" with NA's in "reviewer_age" column
df$reviewer_age <- gsub("-", NA, df$reviewer_age) 
df$reviewer_age <- as.integer(df$reviewer_age)

# is it enough?
summary(df)

# handling with missing values
map(df, ~sum(is.na(.)))

# how does it look the levels of our factor?
levels(df$web_browser)

# convert once again to replace NA's with "unknown"
df <- df%>%
	mutate(
		web_browser = factor(ifelse(is.na(web_browser), 
        "unknown", web_browser), 
        labels = c("Android", "Chrome", "Firefox", "IE", "Opera", "Safari", "unknown"))
	)

# convert once again into appropriate type and replace NA's with average age
df$reviewer_age = ifelse(is.na(df$reviewer_age), mean(df$reviewer_age, na.rm = TRUE), df$reviewer_age)
df$reviewer_age = as.integer(df$reviewer_age)

# checking for valid integers
df%>%tail()
glimpse(df)

any(is.na(df))
for (col in colnames(df)) {
  if (any(is.na(df[[col]]))) {
    print(paste("Column", col, "contains NA values"))
} else{
	  print('There is no NA values')
  }
}

# Adding labels
df <- df%>%
mutate(owned_labs = factor(owned, labels = c("Non-Owners", "Owners")))


# The most numbers of observations across categories
df%>%
select(owned_labs)%>%
count(owned_labs)%>%
rename('Quantity' ='n', 'Ownership' = 'owned_labs')%>%
arrange(desc(Quantity))

# There is significantly much more "Owners" than "Non-Owners"
df%>%
ggplot(aes(owned_labs))+
geom_bar(fill = "#800020", width = .55)+
labs(title = "Reviews by Ownership",
	 x = "Ownership",
	 y = "Quantity")+
scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, by = 100))+
theme_bw()+
theme(panel.grid.major.x = element_blank())

# Top 3 reviewers which are "Owners" or "Non-Owners"
df%>%
select(make_model, owned_labs)%>%
count(make_model, owned_labs)%>%
rename('quantity' ='n')%>%
top_n(3)%>%
arrange(desc(quantity))

### Distribution of "overall_rating" with Histogram

df%>%
ggplot(aes(overall_rating))+
geom_histogram(bins = 80, colour = "purple")+
labs(title = "Ditribution of the overall rating",
	x = "Overall rating",
	y = "Count")+
scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10))+
theme_bw()+
theme(panel.grid.major.x = element_blank())

### Distribution of "overall_rating" with Boxplot

df%>%
ggplot(aes(overall_rating))+
geom_boxplot(fill = 'gray')+
stat_boxplot(geom = "errorbar")+
labs(title = "Distribution of the Overall Rating",
	subtitle = "Median & Quartiles", x = "Overall rating")+
theme_bw()+
theme(panel.grid.major.x = element_blank())

### Difference across Overall Rating between `models.`

df%>%
ggplot(aes(overall_rating, make_model, fill = owned_labs))+
geom_boxplot()+
labs(title = "Difference across overall rating between models",
	x = "Overall rating",
	y = "Models")

### The relationship between `Ownership` and `overall_rating`

df%>%
ggplot(aes(overall_rating,fill = owned_labs))+
geom_bar() +
labs(title = "Relationship beetween Ownership and Overall rating", x = "Overall rating", y = "Counts") +
facet_wrap(~owned_labs)

### Bar charts displays the relationship beetween Ownership and Overall Rating

df%>%
ggplot(aes(owned_labs, overall_rating, fill = owned_labs))+
geom_boxplot()+
labs(title = "Boxplot 'Owners' vs 'Non-Owners'",
	x = "Ownership",
	y = "Overall rating")+
theme_bw()+
theme(panel.grid.major.x = element_blank())

df <- df%>%
select(-owned_labs)
df%>%head()

# Split the data into training and testing sets
set.seed(123)
sample <- sample.split(df$owned, SplitRatio = 0.7)
train <- subset(df, sample == TRUE)
test <- subset(df, sample == FALSE)

# Fit a logistic regression model
model_glm <- glm(owned ~ overall_rating + make_model + reviewer_age, data = train, family = "binomial")

summary(model_glm)

# Make predictions on the testing set
pred_glm <- predict(model_glm, newdata = test, type = "response")

# Convert probabilities to binary predictions
pred_glm <- ifelse(pred_glm >= 0.5, 1, 0)

# Evaluating model accuracy
# using confusion matrix
table(test$owned, pred_glm)

accuracy <- mean(pred_glm == test$owned)
print(paste('Accuracy =', accuracy))

# Split the data into training and testing sets
set.seed(123)
sample_rf <- sample.split(df$owned, SplitRatio = 0.7)
train_rf <- subset(df, sample_rf== TRUE)
test_rf <- subset(df, sample_rf == FALSE)

# Create the Random Forest model
model_rf <- randomForest(owned ~ overall_rating + make_model + reviewer_age, data = train_rf, ntree = 500, importance = TRUE)

summary(model_rf)

# Make predictions on the testing set
pred_rf <- predict(model_rf, newdata = test_rf, type = "response")

accuracy_rf <- mean(pred_rf == test_rf$owned)

# Evaluating model accuracy
# using confusion matrix
table(test_rf$owned, pred_rf)
accuracy_rf <- mean(pred_rf == test_rf$owned)

print(paste('Accuracy =', accuracy_rf))

# Logistic model
cm1 <- confusionMatrix(table(pred_glm, test$owned), positive = "1")
# RandomForest model
cm2 <- confusionMatrix(table(pred_rf, test$owned), positive = "1")

cm1
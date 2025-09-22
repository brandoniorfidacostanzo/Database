install.packages("randomForest")
install.packages("readr")
install.packages("ggplot2")
install.packages("imputeTS")
install.packages("gridExtra")
install.packages("randomForest")
install.packages("moments")

library(moments)
library(randomForest)
library(gridExtra)
library(imputeTS)
library(readr)
library(randomForest)
library(ggplot2)



#Klay Section
#-------------------------------------------------------------------------------------------------------------------
file1_path <- "C:/Users/user/Downloads/bank+marketing (1)/bank/bank-full.csv" 
#filePathKlay 

bankfull1 <- read_delim(
  file1_path,
  delim = ";",    # Tell it the delimiter is a semicolon
  quote = '"',      # Tell it that fields are enclosed in double quotes
  trim_ws = TRUE    # A good practice to trim any extra spaces
)


bankfull1 <- read.csv2("C:/Users/user/Downloads/bank+marketing (1)/bank/bank-full.csv")
bankfull2 <- read.csv2("C:/Users/user/Downloads/bank+marketing (1)/bank/bank.csv")
colnames(bankfull2) <- colnames(bankfull1)


bank_data <- rbind(bankfull1, bankfull2)

View(bank_data)
str(bank_data)
summary(bank_data)


#Klay
-----------------------------------------------------------------------------------------------------------------------

##A)Change non numberic category to numberic category

#Part1: Education
bank_data$education[bank_data$education == "primary"] <- 1
bank_data$education[bank_data$education == "secondary"] <- 2
bank_data$education[bank_data$education == "tertiary"] <- 3

#Part 2: Job
bank_data$job[bank_data$job == "admin."]=1
bank_data$job[bank_data$job == "blue-collar"]=2
bank_data$job[bank_data$job == "entrepreneur"]=3
bank_data$job[bank_data$job == "housemaid"]=4
bank_data$job[bank_data$job == "management"]=5
bank_data$job[bank_data$job == "retired"]=6
bank_data$job[bank_data$job == "self-employed"]=7
bank_data$job[bank_data$job == "services"]=8
bank_data$job[bank_data$job == "student"]=9
bank_data$job[bank_data$job == "technician"]=10
bank_data$job[bank_data$job == "unemployed"]=11

#Part 3:Marital
bank_data$marital[bank_data$marital == "divorced"]=1
bank_data$marital[bank_data$marital == "married"]=2
bank_data$marital[bank_data$marital == "single"]=3

#Part4:Default
bank_data$default[bank_data$default == "yes"]=1
bank_data$default[bank_data$default == "no"]=2

#Part5:Hoursing
bank_data$housing[bank_data$housing == 'yes'] = 1
bank_data$housing[bank_data$housing == 'no'] = 2

#Part6:Loan
bank_data$loan[bank_data$loan == 'yes'] = 1
bank_data$loan[bank_data$loan == 'no'] = 2

#Paer7: Contact
bank_data$contact[bank_data$contact == 'telephone'] = 1
bank_data$contact[bank_data$contact == 'cellular'] = 2

#Part8: Poutcome
bank_data$poutcome[bank_data$poutcome == 'success'] = 1
bank_data$poutcome[bank_data$poutcome == 'failure'] = 2
bank_data$poutcome[bank_data$poutcome == 'other'] = 3

#Part9: y
bank_data$y[bank_data$y == 'yes'] = 1
bank_data$y[bank_data$y == 'no'] = 2

#Part10: Remove day,month
bank_data <- subset(bank_data,select = -c(day,month))

#-----------------------------------------------------------------------------------------------------------------------
  ##B) Clean the data
# Convert 'unknown' to NA
bank_data$education[bankfull1$education == "unknown"] = NA
bank_data$job[bank_data$job == "unknown"] = NA
bank_data$poutcome[bank_data$poutcome == "unknown"] = NA
bank_data$contact[bank_data$contact == "unknown"] = NA

# Check for missing values in all variables
missing_values <- colSums(is.na(bank_data))

# Print the count of missing values for each variable
print(missing_values)

# Convert character to numeric
bank_data$education <- as.numeric(bank_data$education)
bank_data$job <- as.numeric(bank_data$job)
bank_data$marital <- as.numeric(bank_data$marital)
bank_data$default <- as.numeric(bank_data$default)
bank_data$housing <- as.numeric(bank_data$housing)
bank_data$loan <- as.numeric(bank_data$loan)
bank_data$contact <- as.numeric(bank_data$contact)
bank_data$poutcome <- as.numeric(bank_data$poutcome)
bank_data$y <- as.numeric(bank_data$y)
bank_data$y_fac <- as.factor(bank_data$y)

# Replace NA to mode
bank_data$education_impute_mode <- na_mean(bank_data$education, option = "mode")
bank_data$job_impute_mode <- na_mean(bank_data$job, option = "mode")
bank_data$poutcome_impute_mode <- na_mean(bank_data$poutcome, option = "mode")
bank_data$contact_impute_mode <- na_mean(bank_data$contact, option = "mode")

#-----------------------------------------------------------------------------------------------------------------------
##c)Exploring Central Tendency

# Part1:Duration
hist(bank_data$duration)
summary(bank_data$duration)
summary(log10(bank_data$duration + 1))
summary(sqrt(bank_data$duration + 1))
p1 <- qplot(x = duration, data = bank_data)
p2 <- qplot(x = log10(duration + 1), data = bank_data)
p3 <- qplot(x = sqrt(duration + 1), data = bank_data)
gridExtra::grid.arrange(p1, p2, p3, ncol = 1)

original_duration <- bank_data$duration
original_duration_skew <- skewness(original_duration)
print(paste("Original Duration Skewness: ", original_duration_skew))

log_transformed_duration <- log10(original_duration + 1)
log_duration_skew <- skewness(log_transformed_duration)
print(paste("Log-Transformed Data Skewness: ", log_duration_skew))

sqrt_transformed_duration <- sqrt(original_duration + 1)
sqrt_duration_skew <- skewness(sqrt_transformed_duration)
print(paste("Square Root-Transformed Duration Skewness: ", sqrt_duration_skew))

# Part2:Age
hist(bank_data$age)
summary(bank_data$age)
summary(log10(bank_data$age + 1))
summary(sqrt(bank_data$age + 1))
p4 <- qplot(x = age, data = bank_data)
p5 <- qplot(x = log10(age + 1), data = bank_data)
p6 <- qplot(x = sqrt(age + 1), data = bank_data)
gridExtra::grid.arrange(p4, p5, p6, ncol = 1)

original_age <- bank_data$age
original_age_skew <- skewness(original_age)
print(paste("Original Age Skewness: ", original_age_skew))

log_transformed_age <- log10(original_age + 1)
log_age_skew <- skewness(log_transformed_age)
print(paste("Log-Transformed Age Skewness: ", log_age_skew))

sqrt_transformed_age <- sqrt(original_age + 1)
sqrt_age_skew <- skewness(sqrt_transformed_age)
print(paste("Square Root-Transformed Age Skewness: ", sqrt_age_skew))

# Part3: Balance
hist(bank_data$balance)
summary(bank_data$balance)
summary(log10(bank_data$balance + 1))
summary(sqrt(bank_data$balance + 1))
p7 <- qplot(x = balance, data = bank_data)
p8 <- qplot(x = log10(balance + 1), data = bank_data)
p9 <- qplot(x = sqrt(balance + 1), data = bank_data)
gridExtra::grid.arrange(p7, p8, p9, ncol = 1)

original_balance <- bank_data$balance
original_balance_skew <- skewness(original_balance)
print(paste("Original Balance Skewness: ", original_balance_skew))

log_transformed_balance <- log10(original_balance + 1)
log_balance_skew <- skewness(log_transformed_balance)
print(paste("Log-Transformed Balance Skewness: ", log_balance_skew))

sqrt_transformed_balance <- sqrt(original_balance + 1 )
sqrt_balance_skew <- skewness(sqrt_transformed_balance)
print(paste("Square Root-Transformed Balance Skewness: ", sqrt_balance_skew))

# Part4: Previous
hist(bank_data$previous)
summary(bank_data$previous)
summary(log10(bank_data$previous + 1))
summary(sqrt(bank_data$previous + 1))
p10 <- qplot(x = previous, data = bank_data)
p11 <- qplot(x = log10(previous + 1), data = bank_data)
p12 <- qplot(x = sqrt(previous), data = bank_data)
gridExtra::grid.arrange(p10, p11, p12, ncol = 1)

original_previous <- bank_data$previous
original_previous_skew <- skewness(original_previous)
print(paste("Original Previous Skewness: ", original_previous_skew))

log_transformed_previous <- log10(original_previous + 1)
log_previous_skew <- skewness(log_transformed_previous)
print(paste("Log-Transformed Previous Skewness: ", log_previous_skew))

sqrt_transformed_previous <- sqrt(original_previous + 1)
sqrt_previous_skew <- skewness(sqrt_transformed_previous)
print(paste("Square Root-Transformed Previous Skewness: ", sqrt_previous_skew))

# Part5: pdays
hist(bank_data$pdays)
summary(bank_data$pdays)
summary(log10(bank_data$pdays + 1))
summary(sqrt(bank_data$pdays + 1))
p13 <- qplot(x = pdays, data = bank_data)
p14 <- qplot(x = log10(pdays + 1), data = bank_data)
p15 <- qplot(x = sqrt(pdays + 1), data = bank_data)
gridExtra::grid.arrange(p13, p14, p15, ncol = 1)

original_pdays <- bank_data$pdays
original_pdays_skew <- skewness(original_pdays)
print(paste("Original Pdays Skewness: ", original_pdays_skew))

log_transformed_pdays <- log10(original_pdays + 1)
log_pdays_skew <- skewness(log_transformed_pdays)
print(paste("Log-Transformed Pdays Skewness: ", log_pdays_skew))

sqrt_transformed_pdays <- sqrt(original_pdays + 1)
sqrt_pdays_skew <- skewness(sqrt_transformed_pdays)
print(paste("Square Root-Transformed Pdays Skewness: ", sqrt_pdays_skew))

# Part6: campaign
hist(bank_data$campaign)
summary(bank_data$campaign)
summary(log10(bank_data$campaign + 1))
summary(sqrt(bank_data$campaign + 1))
p16 <- qplot(x = campaign, data = bank_data)
p17 <- qplot(x = log10(campaign + 1), data = bank_data)
p18 <- qplot(x = sqrt(campaign + 1), data = bank_data)
gridExtra::grid.arrange(p16, p17, p18, ncol = 1)

original_campaign <- bank_data$campaign
original_campaign_skew <- skewness(original_campaign)
print(paste("Original Campaign Skewness: ", original_campaign_skew))

log_transformed_campaign <- log10(original_campaign + 1)
log_campaign_skew <- skewness(log_transformed_campaign)
print(paste("Log-Transformed Campaign Skewness: ", log_campaign_skew))

sqrt_transformed_campaign <- sqrt(original_campaign + 1)
sqrt_campaign_skew <- skewness(sqrt_transformed_campaign)
print(paste("Square Root-Transformed Campaign Skewness: ", sqrt_campaign_skew))

#-----------------------------------------------------------------------------------------------------------------------
  ##D)Adding Column for Log-Transformed Numeric Values
bank_data$duration_log <- log10(bank_data$duration + 1)
bank_data$age_log <- log10(bank_data$age + 1)
bank_data$previous_log <- log10(bank_data$previous + 1)
bank_data$campaign_log <- log10(bank_data$campaign + 1)
#-----------------------------------------------------------------------------------------------------------------------
  ##E) Min-max normalization for numerical values

#Part1: Age
min_1<-min(bank_data$age_log)
max_1<-max(bank_data$age_log)
Value_1<-bank_data$age_log
norm_value_1 <- (Value_1-min_1)/(max_1-min_1)
bank_data$age_norm <- norm_value_1

#Part2: Balance
min_2<-min(bank_data$balance)
max_2<-max(bank_data$balance)
Value_2<-bank_data$balance
norm_value_2 <- (Value_2-min_2)/(max_2-min_2)
bank_data$balance_norm <- norm_value_2

#Part3:Pdyas
min_3<-min(bank_data$pdays)
max_3<-max(bank_data$pdays)
Value_3<-bank_data$pdays
norm_value_3 <- (Value_3-min_3)/(max_3-min_3)
bank_data$pdays_norm <- norm_value_3

#Part4:Duration
min_4<-min(bank_data$duration_log)
max_4<-max(bank_data$duration_log)
Value_4<-bank_data$duration_log
norm_value_4 <- (Value_4-min_4)/(max_4-min_4)
bank_data$duration_norm <- norm_value_4

#Paer5: Campaign
min_5<-min(bank_data$campaign_log)
max_5<-max(bank_data$campaign_log)
Value_5<-bank_data$campaign_log
norm_value_5 <- (Value_5-min_5)/(max_5-min_5)
bank_data$campaign_norm <- norm_value_5

#Part6: Previous
min_6<-min(bank_data$previous_log)
max_6<-max(bank_data$previous_log)
Value_6<-bank_data$previous_log
norm_value_6 <- (Value_6-min_6)/(max_6-min_6)
bank_data$previous_norm <- norm_value_6

min_7<-min(bank_data$previous)
max_7<-max(bank_data$previous)
Value_7<-bank_data$previous
norm_value_7 <- (Value_7-min_7)/(max_7-min_7)
bank_data$previous_norm1 <- norm_value_7


#------------------------------------------------------------------------------
  ##E) Compare original, log/min-max and min-max distribution
  
# Part1: Duration
print(paste("Original Duration Skewness: ", original_duration_skew))
print(paste("Log-Transformed Duration Skewness: ", log_duration_skew))
norm_duration_skew <- skewness(bank_data$duration_norm)
print(paste("Log-Transformed Min-Max Normalized Duration Skewness: ",
            norm_duration_skew))
# Part2 age
print(paste("Original Age Skewness: ", original_age_skew))
print(paste("Log-Transformed Age Skewness: ", log_age_skew))
norm_age_skew <- skewness(bank_data$age_norm)
print(paste("Log-Transformed Min-Max Normalized Age Skewness: ", norm_age_skew))

# Part3:previous
print(paste("Original Previous Skewness: ", original_previous_skew))
print(paste("Log-Transformed Previous Skewness: ", log_previous_skew))
norm_previous_skew <- skewness(bank_data$previous_norm)
print(paste("Log-Transformed Min-Max Normalized Previous Skewness: ",
            norm_previous_skew))

norm_log_previous <- log10(bank_data$previous_norm1 + 1)
norm_log_previous_skew <- skewness(norm_log_previous)
print(paste("Min-Max Normalized Log-Transformed Previous Skewness: ",
            norm_log_previous_skew))

# Part4 campaign
print(paste("Original Campaign Skewness: ", original_campaign_skew))
print(paste("Log-Transformed Campaign Skewness: ", log_campaign_skew))
norm_campaign_skew <- skewness(bank_data$campaign_norm)
print(paste("Min-Max Normalized Campaign Skewness: ", norm_campaign_skew))

# Part5 pdays
print(paste("Original Pdays Skewness: ", original_pdays_skew))
norm_pdays_skew <- skewness(bank_data$pdays_norm)
print(paste("Log-Transformed Min-Max Normalized Pdays Skewness: ", norm_pdays_skew))

# Part6: Balance
print(paste("Original Balance Skewness: ", original_balance_skew))
norm_balance_skew <- skewness(bank_data$balance_norm)
print(paste("Log-Transformed Min-Max Normalized Balance Skewness: ", norm_balance_skew))


#Brandon
#-----------------------------------------------------------------------------------------------------------------------

# Create a histogram to see the distribution
hist(bankfull2$age, 
     main = "Customer age",
     xlab = "Age",
     ylab = "Frequencies",
     col = "lightblue",
     breaks = 10)

abline(v = mean(bankfull2$age), col = "red", lwd = 2)

# Bar chart for different job types
ggplot(bankfull2, aes(x = job)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Customer Job Types", x = "Job Type", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate labels for readability

ggplot(bankfull2, aes(x = education, fill = y)) +
  geom_bar(position = "fill", color = "black") + # "fill" shows proportions (percentages)
  labs(title = "Subscription Rate by Education Level",
       x = "Education Level",
       y = "Proportion",
       fill = "Subscribed?") +
  scale_fill_manual(values = c("no" = "salmon", "yes" = "lightgreen")) +
  theme_minimal()


# Box plot of Balance vs. Subscription Status
ggplot(bankfull2, aes(x = y, y = balance, fill = y)) +
  geom_boxplot() +
  labs(title = "Bank Balance vs. Subscription Outcome",
       x = "Did the Customer Subscribe?",
       y = "Bank Balance") +
  scale_y_continuous(labels = scales::comma) +
  theme_light()


# Stacked bar chart for Housing Loan vs. Subscription
ggplot(bankfull2, aes(x = housing, fill = y)) +
  geom_bar(position = "fill") +
  labs(title = "Subscription Rate by Housing Loan Status",
       x = "Has Housing Loan?",
       y = "Proportion",
       fill = "Subscribed?") +
  scale_fill_manual(values = c("no" = "#F8766D", "yes" = "#00BFC4")) +
  theme_minimal()

# Mosaic plots 
mosaicplot(table(bankfull2$poutcome, bankfull2$y),
           main = "Previous Campaign Outcome vs. Current Subscription",
           color = c("salmon", "lightgreen"),
           xlab = "Previous Outcome",
           ylab = "Current Subscription (y)")



# Is there a relationship between experience and salary?
plot(employees$years_experience, employees$salary,
     main = "Experience vs Salary",
     xlab = "Years of Experience",
     ylab = "Salary ($)",
     pch = 19, col = "darkblue")

# Calculate correlation
correlation <- cor(employees$years_experience, employees$salary)
cat("Correlation between experience and salary:", correlation)

install.packages("randomForest")
install.packages("readr")
install.packages("ggplot2")
install.packages("imputeTS")
install.packages("gridExtra")
install.packages("randomForest")
install.packages("moments")
install.packages("dplyr")
install.packages("scales")
install.packages("ggridges")

library(dplyr)
library(moments)
library(randomForest)
library(gridExtra)
library(imputeTS)
library(readr)
library(randomForest)
library(ggplot2)
library(scales)
library(ggridges)


#Brandon Section
#-------------------------------------------------------------------------------------------------------------------
bankfull1 <- read_delim(
  file1_path,
  delim = ";",    # Tell it the delimiter is a semicolon
  quote = '"',      # Tell it that fields are enclosed in double quotes
  trim_ws = TRUE    # A good practice to trim any extra spaces
)


bankfull1 <- read.csv2("/Users/brandoniorfida-costanzo/Desktop/University/INF30036 - Business Analytics/Assignment 2/bank/bank-full.csv")
bankfull2 <- read.csv2("/Users/brandoniorfida-costanzo/Desktop/University/INF30036 - Business Analytics/Assignment 2/bank-additional/bank-additional-full.csv")
bank_data <- bind_rows(bankfull1, bankfull2)
str(bank_data)
View(bank_data)
summary(bank_data)


#Klay
-----------------------------------------------------------------------------------------------------------------------
  
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


#Part: Poutcome
bank_data$poutcome[bank_data$poutcome == 'success'] = 1
bank_data$poutcome[bank_data$poutcome == 'failure'] = 2
bank_data$poutcome[bank_data$poutcome == 'other'] = 3

#Part8: y
bank_data$y[bank_data$y == 'yes'] = 1
bank_data$y[bank_data$y == 'no'] = 2

#Part9: Remove day,month
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
bank_data$poutcome <- as.numeric(bank_data$poutcome)
bank_data$y <- as.numeric(bank_data$y)
bank_data$y_fac <- as.factor(bank_data$y)


# Replace NA to mode
mode_education <- names(sort(table(bank_data$education), decreasing = TRUE))
mode_job <- names(sort(table(bank_data$job), decreasing = TRUE))
mode_poutcome <- names(sort(table(bank_data$poutcome), decreasing = TRUE))

bank_data$education<- ifelse(is.na(bank_data$education), mode_education, bank_data$education)
bank_data$job<- ifelse(is.na(bank_data$job), mode_job, bank_data$job)
bank_data$poutcome<- ifelse(is.na(bank_data$poutcome), mode_poutcome, bank_data$poutcome)

# Re-Check for missing values in all variables
missing_values <- colSums(is.na(bank_data))
# Print the count of missing values for each variable
print(missing_values)

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
bank_data <- bank_data %>%
  mutate(y_subscription = factor(y, levels = c(1, 2), labels = c("Yes", "No")))


ggplot(bank_data, aes(x = y_subscription, fill = y_subscription)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  labs(title = "Distribution of Term Deposit Subscriptions",
       x = "Did the Customer Subscribe?",
       y = "Count of Customers",
       fill = "Subscribed?") + 
  theme_minimal() +
  scale_fill_brewer(palette = "Paired")

job_labels <- c("1" = "Admin", "2" = "Blue-Collar", "3" = "Entrepreneur",
                "4" = "Housemaid", "5" = "Management", "6" = "Retired",
                "7" = "Self-Employed", "8" = "Services", "9" = "Student",
                "10" = "Technician", "11" = "Unemployed")

bank_data <- bank_data %>%
  mutate(job_factor = factor(job, levels = 1:11, labels = job_labels))

ggplot(bank_data, aes(x = job_factor, fill = y_subscription)) +
  geom_bar(position = "fill") +
  labs(title = "Subscription Rate by Job Type",
       x = "Job Type",
       y = "Proportion of Customers",
       fill = "Subscription") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() # Flip coordinates to make job labels easier to read

#Age Distribution graphs
ggplot(bank_data, aes(x = age, fill = y_subscription)) +
  geom_density(alpha = 0.6) + # alpha makes the plots semi-transparent
  labs(title = "Age Distribution by Subscription Outcome",
       x = "Age of Customer",
       y = "Density",
       fill = "Subscription") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

ggplot(bank_data, aes(x = y_subscription, y = duration_log, fill = y_subscription)) +
  geom_boxplot() +
  labs(title = "Call Duration vs. Subscription Outcome",
       x = "Did the Customer Subscribe?",
       y = "Log-Transformed Call Duration (seconds)") +
  theme_minimal() +
  guides(fill = "none") # Hide the legend as the x-axis is clear enough



marital_labels <- c("1" = "Divorced", "2" = "Married", "3" = "Single")
bank_data <- bank_data %>%
  mutate(marital_factor = factor(marital, levels = 1:3, labels = marital_labels))

# Faceted histogram
ggplot(bank_data, aes(x = age)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ marital_factor) +
  labs(title = "Age Distribution by Marital Status",
       x = "Age of Customer",
       y = "Count of Customers") +
  theme_bw()


bank_data <- bank_data %>%
  mutate(y_subscription = factor(y, levels = c(1, 2), labels = c("Yes", "No")))

ggplot(bank_data, aes(x = y_subscription, y = campaign_log, fill = y_subscription)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Number of Campaign Contacts vs. Subscription Outcome",
       x = "Did the Customer Subscribe?",
       y = "Log-Transformed Number of Contacts",
       fill = "Subscribed?") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")



economic_cols <- bank_data %>%
  select(emp.var.rate, cons.price.idx, cons.conf.idx, euribor3m, nr.employed) %>%
  na.omit()


cor_matrix_econ <- round(cor(economic_cols), 2)
melted_cor_econ <- melt(cor_matrix_econ)

ggplot(melted_cor_econ, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "#377EB8", high = "#E41A1C", mid = "white",
                       midpoint = 0, limit = c(-1, 1), name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10)) +
  labs(title = "Correlation Matrix of Economic Indicators", x = "", y = "")

# --- Previous Campaign Outcome ---

plot_data_poutcome <- bank_data %>%
  mutate(poutcome_plot_category = case_when(
    poutcome == 1 ~ "Success",
    poutcome == 2 ~ "Failure",
    poutcome == 3 ~ "Other",
    is.na(poutcome) ~ "No Previous Contact",
    TRUE ~ "Unknown" # A fallback just in case
  ))

ggplot(plot_data_poutcome, aes(x = poutcome_plot_category, fill = y_subscription)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Subscription Rate by Previous Campaign Outcome",
       x = "Outcome of Previous Campaign",
       y = "Proportion of Customers",
       fill = "Subscribed?") +
  theme_minimal() +
  coord_flip()


# --- Housing vs. Personal Loans ---
bank_data <- bank_data %>%
  mutate(
    housing_factor = factor(housing,
                            levels = c(1, 2),
                            labels = c("Has Housing Loan", "No Housing Loan")),
    loan_factor = factor(loan,
                         levels = c(1, 2),
                         labels = c("Has Personal Loan", "No Personal Loan"))
  )

# Faceted bar plot, filtering out any NA values that may exist
# (These would be the original 'unknown' values that became NA)
ggplot(bank_data %>% filter(!is.na(housing_factor) & !is.na(loan_factor)),
       aes(x = y_subscription, fill = y_subscription)) +
  geom_bar() +
  facet_grid(housing_factor ~ loan_factor) +
  labs(title = "Subscription Count by Housing and Personal Loan Status",
       x = "Did the Customer Subscribe?",
       y = "Count of Customers",
       fill = "Subscribed?") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ---Day of the Week ---

# Order the days of the week logically
day_levels <- c("mon", "tue", "wed", "thu", "fri")

# Create the proportional bar chart, filtering out the NAs from the first dataset
ggplot(bank_data %>% filter(!is.na(day_of_week)),
       aes(x = factor(day_of_week, levels = day_levels), fill = y_subscription)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Subscription Rate by Day of the Week",
       x = "Day of Contact",
       y = "Proportion of Subscriptions",
       fill = "Subscribed?") +
  theme_minimal()



# --- Balance by Education Level ---
# All other original categories (like 'high.school', 'basic.4y') became NA. - Needs to be fixed
bank_data <- bank_data %>%
  mutate(education_factor = case_when(
    education == 1    ~ "Primary",
    education == 2    ~ "Secondary",
    education == 3    ~ "Tertiary",
    is.na(education)  ~ "Other/Unknown" # This captures all other categories
  ))

# Filtered data to make the plot readable (remove NAs, outliers, and the 'Other' group)
plot_data_balance <- bank_data %>%
  filter(!is.na(balance) & balance > 0 & balance < 10000 & education_factor != "Other/Unknown")

#Violin plot
ggplot(plot_data_balance, aes(x = education_factor, y = balance, fill = education_factor)) +
  geom_violin() +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Distribution of Bank Balance by Education Level",
       subtitle = "For customers with balances between $0 and $10,000",
       x = "Education Level",
       y = "Bank Balance") +
  theme_light() +
  guides(fill = "none") + # Hide legend as x-axis is clear
  coord_flip()

# ---Age vs. Bank Balance ---(Requires scales)
# Removed NAs, people with a negative balance, and cap the balance at a reasonable level to avoid extreme outliers skewing the view
plot_data_scatter <- bank_data %>%
  filter(!is.na(balance) & balance > 0 & balance < 50000)

ggplot(plot_data_scatter, aes(x = age, y = balance, color = y_subscription)) +
  geom_point(alpha = 0.4, size = 1.5) + # Use alpha for transparency to see dense areas
  geom_smooth(method = "loess", se = FALSE, color = "black") + # Add a smoothed trendline
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Bank Balance Across Customer Age",
       subtitle = "Showing trendline for customers with balances under $50,000",
       x = "Customer Age",
       y = "Bank Balance",
       color = "Subscribed?") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

# ---Default Rate by Marital Status ---
bank_data <- bank_data %>%
  mutate(default_factor = factor(default,
                                 levels = c(1, 2),
                                 labels = c("Has Defaulted", "No Default")))

ggplot(bank_data %>% filter(!is.na(marital_factor)),
       aes(x = marital_factor, fill = default_factor)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Credit Default Rate by Marital Status",
       x = "Marital Status",
       y = "Proportion of Customers",
       fill = "Credit Status") +
  theme_light() +
  scale_fill_manual(values = c("Has Defaulted" = "#E41A1C", "No Default" = "#377EB8"))

# ---Age Distribution by Job ---
#Ridge plot
ggplot(bank_data %>% filter(!is.na(job_factor)),
       aes(x = age, y = job_factor, fill = job_factor)) +
  geom_density_ridges() +
  labs(title = "Age Distribution Across Different Professions",
       x = "Customer Age",
       y = "Job Type") +
  theme_ridges() +
  theme(legend.position = "none") # Hide legend as y-axis is clear

# Subscription by Education -- EDUCATION CLEANING NEEDS TO BE REWORKED
education_summary <- bank_data %>%
  filter(!is.na(education_factor) & education_factor != "Other/Unknown") %>%
  group_by(education_factor) %>%
  summarise(
    subscription_rate = mean(y_subscription == "Yes", na.rm = TRUE),
    count = n()
  ) %>%
  arrange(subscription_rate) # Arrange by rate for a cleaner look

#Lollipop chart
ggplot(education_summary, aes(x = subscription_rate, y = reorder(education_factor, subscription_rate))) +
  geom_segment(aes(x = 0, yend = education_factor, xend = subscription_rate), color = "grey") +
  geom_point(color = "dodgerblue", size = 4) +
  scale_x_continuous(labels = percent_format()) +
  labs(title = "Term Deposit Subscription Rate by Education Level",
       x = "Subscription Rate",
       y = "Education Level") +
  theme_minimal()


# ---Balance vs. Duration ---
# Filter data for a clearer plot (positive balance, under $50k)
plot_data_bal_dur <- bank_data %>%
  filter(!is.na(balance) & balance > 0 & balance < 50000)

ggplot(plot_data_bal_dur, aes(x = duration_log, y = balance, color = y_subscription)) +
  geom_point(alpha = 0.3) + # Use transparency to see dense areas
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Subscription by Call Duration and Bank Balance",
       subtitle = "For customers with balances under $50,000",
       x = "Log-Transformed Call Duration (seconds)",
       y = "Bank Balance",
       color = "Subscribed?") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

#Balance vs. Campaign Contacts
ggplot(plot_data_bal_dur, aes(x = campaign_log, y = balance, color = y_subscription)) +
  geom_point(alpha = 0.3) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Subscription by Campaign Contacts and Bank Balance",
       subtitle = "For customers with balances under $50,000",
       x = "Log-Transformed Number of Campaign Contacts",
       y = "Bank Balance",
       color = "Subscribed?") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

#Campaign Contacts by Job Type ---

#Faceted box plots, filtering out NA job values
ggplot(bank_data %>% filter(!is.na(job_factor)),
       aes(x = job_factor, y = campaign_log, fill = job_factor)) +
  geom_boxplot() +
  facet_wrap(~ y_subscription, ncol = 1) + # Create separate panels for "Yes" and "No"
  coord_flip() + # Flip axes to make job titles readable
  labs(title = "Campaign Contact Distribution by Job and Subscription Outcome",
       x = "Job Type",
       y = "Log-Transformed Number of Contacts") +
  theme_bw() +
  theme(legend.position = "none")

#Balance vs. Duration for Subscribers ---
#Filtered data for SUBSCRIBED customers with a clear balance range
plot_data_subscribers <- bank_data %>%
  filter(y_subscription == "Yes", # <-- This is the key change
         !is.na(balance),
         balance > 0,
         balance < 50000)

ggplot(plot_data_subscribers, aes(x = duration_log, y = balance)) +
  geom_point(alpha = 0.5, color = "darkgreen") + # Color is now static
  geom_smooth(method = "loess", se = FALSE, color = "black") + # Optional trendline
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Balance vs. Call Duration for Subscribed Customers",
       subtitle = "For customers with balances under $50,000",
       x = "Log-Transformed Call Duration (seconds)",
       y = "Bank Balance") +
  theme_minimal()


# --- Balance vs. Campaign Contacts for Subscribers ---

# We can use the same 'plot_data_subscribers' data frame created above
ggplot(plot_data_subscribers, aes(x = campaign_log, y = balance)) +
  geom_point(alpha = 0.5, color = "darkcyan") + # Color is now static
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Balance vs. Campaign Contacts for Subscribed Customers",
       subtitle = "For customers with balances under $50,000",
       x = "Log-Transformed Number of Campaign Contacts",
       y = "Bank Balance") +
  theme_minimal()


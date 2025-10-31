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
install.packages("writexl")
install.packages("tidyverse")
install.packages("fastDummies")
install.packages("moments")
install.packages("dplyr")
install.packages("caret")
install.packages("recipes")
install.packages("themis")
install.packages("janitor")
install.packages("corrplot")
install.packages("smotefamily")

library(smotefamily)
library(corrplot)
library(janitor)
library(themis) 
library(recipes)
library(caret)
library(writexl)
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
library(tidyverse)       
library(fastDummies)     
library(moments)         
library(dplyr)

# -----------------------------------------------------------------------------------------------------
#   IMPORT AND MERGE DATA
# -----------------------------------------------------------------------------------------------------

# Define file paths
file1_path <- "C:/Users/user/Downloads/bank+marketing (1)/bank/bank-full.csv"
file2_path <- "C:/Users/user/Downloads/bank+marketing (1)/bank/bank.csv"

# Read the datasets (semicolon-delimited format)
bankfull1 <- read.csv2(file1_path)
bankfull2 <- read.csv2(file2_path)

# Align column names (for consistent merge)
colnames(bankfull2) <- colnames(bankfull1)

# Merge both datasets and remove duplicate rows
bank_data <- distinct(rbind(bankfull1, bankfull2))

View(bank_data)
str(bank_data)
summary(bank_data)

# -----------------------------------------------------------------------------------------------------
#  Section 1: Data Exploration and Analysis
# ----------------------------------------------------------------------------------------------------

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




# -----------------------------------------------------------------------------------------------------
#  Section 2: Data Preparation and Feature Engineering
# ----------------------------------------------------------------------------------------------------


# -----------------------------------------------------------------------------------------------------
#  STEP 1: INITIAL DATA CLEANING
# -----------------------------------------------------------------------------------------------------

# Replace "unknown" with NA to mark missing entries
bank_data[bank_data == "unknown"] <- NA

# Check missing values
missing_values <- colSums(is.na(bank_data))
cat("\n🔍 Missing values per variable:\n")
print(missing_values)

get_mode <- function(x) {
  ux <- na.omit(unique(x))                # unique non-NA values
  ux[which.max(tabulate(match(x, ux)))]   # returns the most frequent value
}

# Loop through each column and replace NA with mode (if any)
for (col in names(bank_data)) {
  if (any(is.na(bank_data[[col]]))) {
    mode_value <- get_mode(bank_data[[col]])
    bank_data[[col]][is.na(bank_data[[col]])] <- mode_value
    cat("✅ Missing values in", col, "replaced with mode:", mode_value, "\n")
  }
}

# Verify that there are no missing values left
cat("\n🔍 Checking for remaining missing values:\n")
print(colSums(is.na(bank_data)))

# -----------------------------------------------------------------------------------------------------
#  STEP 2: HANDLE CATEGORICAL VARIABLES
# -----------------------------------------------------------------------------------------------------

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


# 2.4 Convert ordinal factor to numeric (preserves order)
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

# -----------------------------------------------------------------------------------------------------
#  STEP 3: HANDLE TEMPORAL AND SEASONAL VARIABLES
# -----------------------------------------------------------------------------------------------------

# 3.1 Month: Convert to numeric and assign seasons
month_levels <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
bank_data$month <- factor(bank_data$month, levels = month_levels)

# Convert month to numeric order (1–12)
bank_data$month_num <- as.numeric(bank_data$month)

# Assign season based on marketing patterns
bank_data$season <- case_when(
  bank_data$month %in% c("mar","apr","may") ~ "Spring",
  bank_data$month %in% c("jun","jul","aug") ~ "Summer",
  bank_data$month %in% c("sep","oct","nov") ~ "Autumn",
  TRUE ~ "Winter"
)

# Drop 'day' variable as it doesn’t carry useful predictive info
bank_data <- subset(bank_data, select = -c(day))



# -----------------------------------------------------------------------------------------------------
#  STEP 4: FEATURE ENGINEERING — Deriving New Insights
# -----------------------------------------------------------------------------------------------------

# 4.1 Age Group
bank_data$age_group <- cut(bank_data$age,
                           breaks = c(0, 25, 35, 50, 65, 100),
                           labels = c("Youth", "YoungAdult", "MiddleAge", "Senior", "Elder"))

# 4.2 Balance Category
bank_data$balance_category <- cut(bank_data$balance,
                                  breaks = c(-Inf, 0, 1000, 5000, Inf),
                                  labels = c("Debt", "Low", "Medium", "High"))

# 4.3 Customer Risk Score
# Clients with either default or loan = high risk
bank_data$risk_score <- ifelse(bank_data$default == 1 | bank_data$loan == 1, "High", "Low")

# 4.4 Campaign Intensity
# Ratio of contacts in this campaign vs previous attempts
bank_data$campaign_intensity <- bank_data$campaign / (bank_data$previous + 1)

# 4.5 Recently Contacted Flag (pdays < 30)
bank_data$recently_contacted <- ifelse(bank_data$pdays < 30, 1, 0)

# -----------------------------------------------------------------------------------------------------
#  STEP 5: TRANSFORMATION & NORMALIZATION
# -----------------------------------------------------------------------------------------------------

# Identify continuous numeric variables
numeric_vars <- c("age", "balance", "duration", "pdays", "campaign", "previous")

# 5.1 Safe Log Transformation
for (v in numeric_vars) {
  # Replace negative or NA values with 0 before log
  safe_values <- pmax(bank_data[[v]], 0)
  safe_values[is.na(safe_values)] <- 0
  bank_data[[paste0(v, "_log")]] <- log10(safe_values + 1)
}

# 5.2 Min-Max Normalization
normalize <- function(x) {
  x <- ifelse(is.na(x), 0, x)  # Replace NA with 0
  if (max(x) != min(x)) {
    (x - min(x)) / (max(x) - min(x))
  } else {
    rep(0, length(x))  # Avoid division by zero
  }
}

for (v in numeric_vars) {
  log_col <- paste0(v, "_log")
  norm_col <- paste0(v, "_norm")
  bank_data[[norm_col]] <- normalize(bank_data[[log_col]])
}


# -----------------------------------------------------------------------------------------------------
#  STEP 6: FINAL INSPECTION 
# -----------------------------------------------------------------------------------------------------

# Check structure and summary statistics
str(bank_data)
summary(bank_data)


# -----------------------------------------------------------------------------------------------------
#  Section 3: Data Sampling and Validation Strategy 
# -----------------------------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------------------------
#  STEP 1: STRATIFIED SAMPLING
# -----------------------------------------------------------------------------------------------------

set.seed(123)
train_proportion <- 0.7
train_index <- sample(1:nrow(bank_data), 
                      size = floor(train_proportion * nrow(bank_data)))
train_data <- bank_data[train_index,]
test_data <- bank_data[-train_index,]

# Ensure target is a factor
train_data$y <- as.factor(train_data$y)
test_data$y  <- as.factor(test_data$y)


# -----------------------------------------------------------------------------------------------------
#  STEP 2: BASE RECIPE
# -----------------------------------------------------------------------------------------------------

base_recipe <- function(bank_data) {
  recipe(y ~ ., data = bank_data ) %>%
    step_impute_mode(all_nominal_predictors()) %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors())
}

# -----------------------------------------------------------------------------------------------------
#  STEP 3: Sampling
# -----------------------------------------------------------------------------------------------------

#3.1 SMOTE SAMPLING

# Check the class distribution in the target variable

cat("ORIGINAL DATA:\n")

cat("Total transactions:", nrow(train_data), "\n\n")
table(train_data$y)

# Calculate percentages for each class
cat("\nPercentages:\n")
prop.table(table(train_data$y)) * 100

# Simple bar chart to visualize class imbalance
barplot(table(bank_data$y),
        main = "BEFORE SMOTE: Very Imbalanced!",
        col = c("green", "red"),
        ylab = "Number of Transactions")

library(smotefamily)

# Separate features (X) and target (y) for SMOTE
X <- train_data[, c("age", "balance")]  # All columns except 'y'
y <- train_data$y  # Target variable: whether the customer subscribed

# Apply SMOTE to generate synthetic samples for the minority class
smote_result <- SMOTE(X, y, K = 5)

# Get the balanced data
train_SMOTE_Balanced_Data <- smote_result$data
names(balanced_data)[ncol(balanced_data)] <- "y"  # Ensure column name for target is 'y'


cat("BALANCED DATA:\n")
cat("Total transactions:", nrow(balanced_data), "\n\n")
table(balanced_data$y)

# Calculate percentages for each class in the balanced data
cat("\nPercentages:\n")
prop.table(table(balanced_data$y)) * 100

# Visualize the class distribution after SMOTE
barplot(table(balanced_data$y),
        main = "AFTER SMOTE: Balanced!",
        col = c("green", "red"),
        ylab = "Number of Transactions")

#Stratified Cross-Validation
library(ggplot2)
library(lattice)
library(caret)
train_control_smote <- trainControl(method = "cv", number = 5, sampling = "smote", classProbs = TRUE)

#3.2 Random Oversampling

library(ROSE)

# Show the imbalance in the initial dataset
cat("Initial Class Distribution in Training Data:\n")
table(train_data$y)  # Show class distribution

# Plot the class distribution before balancing
barplot(table(train_data$y),
        main = "BEFORE Random OverSampling: Very Imbalanced!",
        col = c("green", "red"),
        ylab = "Number of Transactions")

# Apply random oversampling using ROSE
oversampled_data <- ovun.sample(y ~ ., data = train_data, method = "over", N = max(table(train_data$y)) * 2)$data

# Show the class distribution after oversampling
cat("Class Distribution After Oversampling:\n")
table(oversampled_data$y)  # Show class distribution

# Plot the class distribution after oversampling
barplot(table(oversampled_data$y),
        main = "After Random OverSampling: Balanced!",
        col = c("green", "red"),
        ylab = "Number of Transactions")
table(oversampled_data$y)

#Stratified Cross-Validation
library(ggplot2)
library(lattice)
library(caret)
train_control_oversampling <- trainControl(method = "cv", number = 5, sampling = "over", classProbs = TRUE)

#3.3 Undersampling

# Load necessary libraries
library(caret)  # For downsampling
library(ggplot2)  # For visualization
library(randomForest)  # For model fitting

# Step 1: Show the imbalance in the initial dataset
cat("Initial Class Distribution in Training Data:\n")
print(table(train_data$y))  # Show class distribution

# Plot the class distribution before balancing
barplot(table(train_data$y),
        main = "BEFORE Random Undersampling: Very Imbalanced!",
        col = c("green", "red"),
        ylab = "Number of Transactions")

# Step 2: Apply random undersampling on Class 2 (Majority class) to match the size of Class 1
# Class 1 has 3723 instances, so reduce Class 2 to 3723 instances
class_1 <- train_data[train_data$y == 1, ]  # Subset for Class 1 (minority class)
class_2 <- train_data[train_data$y == 2, ]  # Subset for Class 2 (majority class)

# Randomly sample 3723 instances from Class 2 (to match Class 1)
set.seed(123)  # For reproducibility
class_2_undersampled <- class_2[sample(1:nrow(class_2), size = 3723), ]

# Combine the undersampled Class 2 with Class 1
undersampled_data <- rbind(class_1, class_2_undersampled)

# Step 3: Show the class distribution after undersampling
cat("Class Distribution After Undersampling:\n")
print(table(undersampled_data$y))  # Show class distribution

# Plot the class distribution after undersampling
barplot(table(undersampled_data$y),
        main = "After Random Undersampling: Balanced!",
        col = c("green", "red"),
        ylab = "Number of Transactions")

#Stratified Cross-Validation
library(ggplot2)
library(lattice)
library(caret)
train_control_undersampling <- trainControl(method = "cv", number = 5, sampling = "under", classProbs = TRUE)

#3.4 Cost-sensitive sampling

class_counts <- table(train_data$y)
cat("Class distribution in training data:\n")
print(class_counts)
class_weights <- 1 / class_counts
class_weights <- class_weights / sum(class_weights)  # normalize so weights sum to 1
cat("\nCalculated class weights:\n")
print(class_weights)

train_data$sample_weight <- ifelse(train_data$y == 1,
                                   class_weights["1"],
                                   class_weights["0"])

cost_factor <- 5  # can adjust this value if needed
train_data$sample_weight <- ifelse(train_data$y == 1,
                                   train_data$sample_weight * cost_factor,
                                   train_data$sample_weight)

train_cost_sensitive <- train_data
test_cost_sensitive  <- test_data

# Display the dimensions of the resulting data frames to verify the split
cat("Dimensions of training data:\n")
print(dim(train_cost_sensitive))
cat("\nDimensions of testing data:\n")
print(dim(test_cost_sensitive))

# Check class distribution in training and testing sets
cat("\nClass distribution in training set:\n")
print(table(train_cost_sensitive$y))
print(prop.table(table(train_cost_sensitive$y)))

cat("\nClass distribution in testing set:\n")
print(table(test_cost_sensitive$y))
print(prop.table(table(test_cost_sensitive$y)))




# -----------------------------------------------------------------------------------------------------
#  Section 4: Model Development and Training
# -----------------------------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------------------------
#  Model 1:Logistic Regression Model
# -----------------------------------------------------------------------------------------------------

cat("--- Training Logistic Regression Model ---\n\n")

# Train a Logistic Regression model
# The formula 'y ~ .' means predict 'y' using all other variables in the dataframe
# The family = binomial() specifies logistic regression for a binary outcome
logistic_model <- glm(y ~ ., data = train_data, family = binomial())

# Print the summary of the logistic regression model
cat("Logistic Regression Model Summary:\n")
summary(logistic_model)

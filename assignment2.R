install.packages("randomForest")
install.packages("readr")
install.packages("ggplot2")
library("readr")
library(randomForest)
library(ggplot2)

#Klay Section
#-------------------------------------------------------------------------------------------------------------------
file1_path <- "/Users/brandoniorfida-costanzo/Desktop/University/INF30036 - Business Analytics/Assignment 2/bank/bank-full.csv" #filePathKlay 
bankfull1 <- read_delim(
  file1_path,
  delim = ";",    # Tell it the delimiter is a semicolon
  quote = '"',      # Tell it that fields are enclosed in double quotes
  trim_ws = TRUE    # A good practice to trim any extra spaces
)


#bankfull1 <- read.csv2("/Users/brandoniorfida-costanzo/Desktop/University/INF30036 - Business Analytics/Assignment 2/bank/bank-full.csv")
bankfull2 <- read.csv2("/Users/brandoniorfida-costanzo/Desktop/University/INF30036 - Business Analytics/Assignment 2/bank-additional/bank-additional-full.csv")
#colnames(bankfull2) <- colnames(bankfull1)

bank_data <- rbind(bankfull1, bankfull2)








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

#Klay
#-----------------------------------------------------------------------------------------------------------------------
# Assuming levels like: unknown < primary < secondary < tertiary
bankfull2$education <- factor(bankfull2$education, 
                              levels = c("unknown", "primary", "secondary", "tertiary"),
                              ordered = TRUE)


# One-hot encoding for nominal variables
nominal_vars <- c("job", "marital", "contact", "poutcome")
bankfull2 <- bankfull2 %>%
  mutate(across(all_of(nominal_vars), as.factor)) %>%
  dummy_cols(select_columns = nominal_vars, remove_first_dummy = TRUE, remove_selected_columns = TRUE)


# Convert month names to factor with correct order
month_levels <- c("jan", "feb", "mar", "apr", "may", "jun", 
                  "jul", "aug", "sep", "oct", "nov", "dec")
bankfull2$month <- factor(bankfull2$month, levels = month_levels, ordered = TRUE)

# Optional: Encode season
bankfull2 <- bankfull2 %>%
  mutate(season = case_when(
    month %in% c("dec", "jan", "feb") ~ "winter",
    month %in% c("mar", "apr", "may") ~ "spring",
    month %in% c("jun", "jul", "aug") ~ "summer",
    month %in% c("sep", "oct", "nov") ~ "autumn"
  ))

bankfull2$season <- as.factor(bankfull2$season)


## Age Groups
bankfull2 <- bankfull2 %>%
  mutate(age_group = case_when(
    age < 25 ~ "young",
    age < 45 ~ "adult",
    age < 65 ~ "middle_aged",
    TRUE ~ "senior"
  )) %>%
  mutate(age_group = factor(age_group, levels = c("young", "adult", "middle_aged", "senior"), ordered = TRUE))

## Balance Categories
bankfull2 <- bankfull2 %>%
  mutate(balance_category = case_when(
    balance < 0 ~ "debt",
    balance < 1000 ~ "low",
    balance < 5000 ~ "medium",
    TRUE ~ "high"
  )) %>%
  mutate(balance_category = factor(balance_category, levels = c("debt", "low", "medium", "high"), ordered = TRUE))

## Customer Risk Profile
bankfull2 <- bankfull2 %>%
  mutate(risk_profile = case_when(
    default == "yes" | loan == "yes" | housing == "yes" ~ "high_risk",
    default == "no" & (loan == "yes" | housing == "yes") ~ "medium_risk",
    TRUE ~ "low_risk"
  )) %>%
  mutate(risk_profile = factor(risk_profile, levels = c("low_risk", "medium_risk", "high_risk"), ordered = TRUE))


# More contacts = higher intensity; pdays shows if previously contacted
bankfull2 <- bankfull2 %>%
  mutate(
    contact_intensity = case_when(
      campaign <= 1 ~ "low",
      campaign <= 3 ~ "medium",
      TRUE ~ "high"
    ),
    prev_contacted = ifelse(pdays == -1, "no", "yes")
  ) %>%
  mutate(across(c(contact_intensity, prev_contacted), as.factor))

bankfull2$y <- as.factor(bankfull2$y)





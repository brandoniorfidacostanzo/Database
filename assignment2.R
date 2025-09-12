install.packages("randomForest")
install.packages("readr")
install.packages("ggplot2")
library("readr")
library(randomForest)
library(ggplot2)

#-------------------------------------------------------------------------------------------------------------------
file1_path <- "/Users/brandoniorfida-costanzo/Desktop/University/INF30036 - Business Analytics/Assignment 2/bank/bank-full.csv"
bankfull1 <- read_delim(
  file1_path,
  delim = ";",    # Tell it the delimiter is a semicolon
  quote = '"',      # Tell it that fields are enclosed in double quotes
  trim_ws = TRUE    # A good practice to trim any extra spaces
)


bankfull1 <- read.csv2("/Users/brandoniorfida-costanzo/Desktop/University/INF30036 - Business Analytics/Assignment 2/bank/bank-full.csv")
bankfull2 <- read.csv2("/Users/brandoniorfida-costanzo/Desktop/University/INF30036 - Business Analytics/Assignment 2/bank-additional/bank-additional-full.csv")
#colnames(bankfull2) <- colnames(bankfull1)

bank_data <- rbind(bankfull1, bankfull2)

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
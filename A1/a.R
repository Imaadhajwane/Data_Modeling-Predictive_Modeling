is.na() # to find null 
sum(is.na(Sales.data)) # to find sum na 
# replace any value with any value 
mean(Sales.data$Profit)
mean(sales.data$Profit,na.rm=True) # to remove null value from the mean 
# mean value at all null values 
Sales.data$Profit[is.na(Sales.data$Profit)]=mean(Sales.data$Profit) 
View(Sales.data)
# replace empty value with our need 
Sales.data$Profit[is.na(Sales.data$Profit)]=50
# drop
Sales.data$PaymentMode=NULL
# summary
summary(Sales.data)
# Plotting 
hist(Sales.data$Profit, 
     main = "Sales Histogram",        # Title of the plot
     xlab = "Sales",                  # X-axis label
     ylab = "Profti",              # Y-axis label
     col = "skyblue",                 # Color of bars
     border = "black",                # Border color of bars
     breaks = 20                      # Number of bins
)
# transform using log
log(Sales.data$Profit)
log(Sales.data$Profit+1)
#cor
res <- cor.test(Sales.data$Profit, Sales.data$Amount, 
                method = "pearson")
res




#############################################################
#############################################################

#IMPORT DATASET
Sales.data <- read.csv("C:/Users/iamim/OneDrive/Desktop/Sixth Semester/DMPM_LAB/A1/Sales data.csv")
View(Sales.data)

# Step 2: Overview of the dataset
str(Sales.data)    # Display the structure of the dataset
summary(Sales.data) # Display summary statistics of the dataset

# Step 3: Get variable names
variables <- colnames(Sales.data)
cat("Variables in the dataset:", variables, "\n")

# Step 4: Check for missing values
missing_values <- colSums(is.na(Sales.data))
cat("Missing values for each variable:\n", missing_values, "\n")

# Optional: Display percentage of missing values for each variable
percentage_missing <- (missing_values / nrow(Sales.data)) * 100
cat("Percentage of missing values for each variable:\n", percentage_missing, "\n")

mean(Sales.data$Profit)

# Select only numerical variables
numerical_variables <- Sales.data[sapply(Sales.data, is.numeric)]

# Obtain summary information for numerical variables
numerical_summary <- summary(numerical_variables)

# Print the summary
print(numerical_summary)

# Assuming 'Sales.data' is your dataset
# Replace 'YourNumericVariable1', 'YourNumericVariable2', etc. with the actual numerical variable names in your dataset

# Select only numerical variables
numerical_variables <- Sales.data[sapply(Sales.data, is.numeric)]

# Plot histograms for numerical variables
par(mfrow = c(2, 2))  # Setting up a 2x2 grid for subplots
for (variable in colnames(numerical_variables)) {
  hist(numerical_variables[[variable]], main = paste("Histogram of", variable), col = "lightblue", border = "black", xlab = variable)
}

# Plot density plots for numerical variables
par(mfrow = c(2, 2))  # Resetting the layout
for (variable in colnames(numerical_variables)) {
  plot(density(numerical_variables[[variable]]), main = paste("Density Plot of", variable), col = "blue", lwd = 2, xlab = variable)
}

# Assuming 'Sales.data' is your dataset
# Replace 'YourNumericVariable1', 'YourNumericVariable2', etc. with the actual numerical variable names in your dataset

# Select only numerical variables
numerical_variables <- Sales.data[sapply(Sales.data, is.numeric)]

# Log transformation
log_transformed_data <- log(numerical_variables + 1)  # Adding 1 to avoid log(0) issues

# Scaling to the range [0, 1]
scaled_data <- scale(log_transformed_data, center = FALSE, scale = apply(log_transformed_data, 2, max))

# Print the first few rows of the scaled data for verification
print(head(scaled_data))


# Assuming 'scaled_data' contains your transformed and scaled numeric variables
# Replace 'YourNumericVariable1', 'YourNumericVariable2', etc. with the actual variable names in your dataset

# Select only transformed and scaled numeric variables
scaled_numerical_variables <- scaled_data

# Shapiro-Wilk Test
shapiro_test_results <- sapply(scaled_numerical_variables, function(x) shapiro.test(x)$p.value)

# Q-Q Plots
par(mfrow = c(2, 2))  # Setting up a 2x2 grid for subplots
for (variable in colnames(scaled_numerical_variables)) {
  qqnorm(scaled_numerical_variables[[variable]], main = paste("Q-Q Plot of", variable))
  qqline(scaled_numerical_variables[[variable]], col = 2)
}

# Print the results of the Shapiro-Wilk Test
print(shapiro_test_results)



# Assuming 'Sales.data' is your dataset

# Select only numeric variables
numeric_variables <- Sales.data[sapply(Sales.data, is.numeric)]

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_variables, use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# Plot the graph of the correlation matrix using corrplot
# Install the corrplot package if not already installed
# install.packages("corrplot")
library(corrplot)

# Plotting the correlation matrix
corrplot(correlation_matrix, method = "circle", type = "full", tl.col = "black", tl.srt = 45)


boxplot(numeric_variables)
# Install the GGally package if not already installed
# install.packages("GGally")
library(GGally)
ggpairs(numeric_variables)
# Install the pheatmap package if not already installed
# install.packages("pheatmap")
library(pheatmap)
pheatmap(correlation_matrix, color = colorRampPalette(c("blue", "white", "red"))(50))


# Assuming 'Category' is a categorical variable
barplot(table(Sales.data$Category), col = "lightblue", main = "Bar Plot of Category")

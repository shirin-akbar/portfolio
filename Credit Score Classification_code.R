#######################################PREPPING THE ENVIRONMENT
# Load necessary libraries
rm(list=ls()); gc()
set.seed(1) 
library(class)
library(fastDummies)
library(dplyr)
library(ggplot2)
library(Amelia)
library(stringr)
library(caret)
library(MASS)
library(rpart)
library(rpart.plot)
########################################DATA LOADING################################################
setwd('/Users/shirinakbar/Documents/Fall23/ISDS574/Project')
dat = read.csv('train.csv', stringsAsFactors=T, head=T,na.strings='')
########################################DATA CLEANING###############################################
# Clean numeric columns from special characters and handle negatives:
# Remove non-numeric characters from specified columns and convert to numeric
columns_with_special_characters <- c('Age', 'Annual_Income', 'Num_of_Loan', 'Num_of_Delayed_Payment','Changed_Credit_Limit','Outstanding_Debt','Amount_invested_monthly','Monthly_Balance')
#Loop through each continuous column
for (col_name in columns_with_special_characters) {
  dat[[col_name]] <- as.numeric(gsub("[^0-9.-]", "", as.character(dat[[col_name]])))
}

# Remove negative values from specified columns and replace with NA
#Delay_from_due_date,Changed_Credit_Limit will be kept negatives as it shows direction of value
dat[c("Age", "Num_Bank_Accounts", "Num_of_Loan", "Monthly_Balance")] <- lapply(dat[c("Age", "Num_Bank_Accounts", "Num_of_Loan", "Monthly_Balance")], function(x) ifelse(x < 0, NA, x))

# Transforming Credit History age to numeric format from years and months and replacing NAs by calculating lag/lead
dat$Credit_History_Age[dat$Credit_History_Age == "NA"] <- NA
age_parts <- strsplit(as.character(dat$Credit_History_Age), " Years and | Months")
age_parts <- lapply(age_parts, as.integer)
month_order <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
dat$Credit_History_Age<-round(sapply(age_parts, function(x) x[1] + x[2]/12),2)
# Store the original order of the data
original_order <- order(dat$ID)
# Perform the Credit_History_Age transformation
dat <- dat %>%
  arrange(Customer_ID, match(Month, month_order)) %>%
  mutate(Credit_History_Age = ifelse(is.na(Credit_History_Age), 
                                     ifelse(!is.na(lag(Credit_History_Age)), 
                                            lag(Credit_History_Age) + 1/12, 
                                            lead(Credit_History_Age) - 1/12),
                                     Credit_History_Age))
# Restore the original order of the data
dat <- dat[original_order, ]


#Cleaning Categorical variables 
dat$Occupation <- as.factor(ifelse(dat$Occupation == "_______", NA, as.character(dat$Occupation)))
dat$Credit_Mix <- as.factor(ifelse(dat$Credit_Mix == "_", NA, as.character(dat$Credit_Mix)))
dat$Payment_of_Min_Amount <- as.factor(ifelse(dat$Payment_of_Min_Amount == "NM", NA, as.character(dat$Payment_of_Min_Amount)))
dat$Payment_Behaviour <- as.factor(ifelse(dat$Payment_Behaviour == "!@9#%8", NA, as.character(dat$Payment_Behaviour)))

#Checking missing values using heatmap
matrix.na = is.na(dat)
pmiss = colMeans(matrix.na)
missmap(dat)
####################################HANDLING OUTLIERS##################################################

# Identify outliers in Age using boxplot & Histogram  and Remove outliers from 'Age'.
dat <- dat %>% mutate(Age = ifelse(Age < 0 | Age > 94, NA, as.numeric(Age)))
par(mfrow = c(1, 2)) 
boxplot(dat$Age, main = "Boxplot of Age", ylab = "Age", col = "blue", border = "black")
hist(dat$Age, main = "Histogram of Age", xlab = "Age", ylab = "Frequency", col = "blue", border = "black")


# List of categorical variables
categorical_columns <- c("Num_Bank_Accounts", "Num_Credit_Card", "Num_of_Loan", "Delay_from_due_date", "Num_Credit_Inquiries")

# Loop through categorical variables and print frequency table
for (var in categorical_columns) {
  print(paste("Frequency Table for", var))
  print(table(dat[[var]], useNA = "ifany"))
  cat("\n")  # Add a newline for better separation
}

# List of columns with outliers
columns_with_outliers <- c('Annual_Income', 'Monthly_Inhand_Salary', 'Num_Bank_Accounts','Num_Credit_Card','Interest_Rate','Num_of_Loan','Delay_from_due_date','Num_of_Delayed_Payment',
                           'Changed_Credit_Limit','Num_Credit_Inquiries','Credit_Utilization_Ratio','Credit_History_Age','Total_EMI_per_month','Amount_invested_monthly','Monthly_Balance')
# Identify and visualize outliers using boxplots for each column
par(mfrow = c(4, 4), mar = c(2, 2, 2, 2))# Adjust the layout based on the number of columns
for (col_name in columns_with_outliers) {
  boxplot(dat[[col_name]], main = col_name, col = "blue", border = "black", notch = TRUE)
}


#Function to replace outliers with NA for a specific column
replace_outliers_with_na <- function(data, column_name) {
  Q3 <- quantile(data[[column_name]], 0.75, na.rm = TRUE)
  Q1 <- quantile(data[[column_name]], 0.25, na.rm = TRUE)
  IQR <- Q3 - Q1
  upper_bound <- Q3 + 1.5 * IQR
  lower_bound <- Q1 - 1.5 * IQR
  data[[column_name]] <- ifelse(data[[column_name]] < lower_bound | data[[column_name]] > upper_bound, NA, data[[column_name]])
  return(data)
}
# Iterate through columns and replace outliers with NA
for (col_name in columns_with_outliers) {
  dat <- replace_outliers_with_na(dat, col_name)
}

# Function to replace the NAs in a column with mean or mode for each customer
replace_missing_values <- function(dat, column_name, method = "mean") {
  unique_ids <- unique(dat[, 'Customer_ID'])
  for (id in unique_ids) {
    values_for_id <- dat[dat[, 'Customer_ID'] == id, column_name]
    if (method == "mean") {
      value_to_fill <- round(mean(values_for_id, na.rm = TRUE), 2)
    } else if (method == "mode") {
      tab <- table(values_for_id)
      value_to_fill <- names(tab)[tab == max(tab)][1]
      if (length(value_to_fill) == 0) {
        value_to_fill <- NA
      }
    }
    dat[dat[, 'Customer_ID'] == id & is.na(dat[, column_name]), column_name] <- value_to_fill
  }
  return(dat)
}

# List of columns and methods to replace missing values
columns_to_replace <- c(
  'Name' = 'mode', 'Age' = 'mean', 'Monthly_Inhand_Salary' = 'mean', 
  'Occupation' = 'mode', 'Num_Bank_Accounts' = 'mode', 'Num_Credit_Card' = 'mode', 
  'Interest_Rate' = 'mean', 'Num_of_Loan' = 'mode', 'Type_of_Loan' = 'mode', 
  'Num_Credit_Inquiries' = 'mode', 'Credit_Mix' = 'mode', 'Outstanding_Debt' = 'mean',  
  'Amount_invested_monthly' = 'mean', 'Payment_of_Min_Amount' = 'mode', 
  'Total_EMI_per_month' = 'mode','Monthly_Balance'='mean'
)
# Iterate through columns and replace missing values
for (col_name in names(columns_to_replace)) {
  method <- columns_to_replace[col_name]
  dat <- replace_missing_values(dat, col_name, method)
}

#Checking missing values using heatmap
matrix.na = is.na(dat)
pmiss = colMeans(matrix.na)
missmap(dat)

# calculating corelation
cor(dat[, sapply(dat, is.numeric)], use = "complete.obs")
cor(dat$Monthly_Inhand_Salary, dat[, sapply(dat, is.numeric)], use = "complete.obs")
#creating heatmap
cor_matrix <- cor(dat[, sapply(dat, is.numeric)], use = "complete.obs")
heatmap(cor_matrix,main = "Correlation Heatmap")
#Removing IDs
#Monthly_Inhand_Salary has strong corelation to Annual Income; removing it along with the less important features.
dat =subset(dat, select = -c(ID, Customer_ID,Name,SSN,Type_of_Loan,Monthly_Inhand_Salary)) 

#omitting the remaining rows with NAs
dat <- na.omit(dat)
#####################################DATA TRANSFORMATION############################################
#combining three levels of outcome variable to two levels
dat$Credit_Score <- as.factor(ifelse(dat$Credit_Score %in% c('Good','Standard'), 'Good', 'Poor'))
dat$Credit_Score <- relevel(dat$Credit_Score, ref = 'Poor')

#changing Total_EMI_per_month to numeric
dat$Total_EMI_per_month <- as.numeric(dat$Total_EMI_per_month)

#Making the data balanced to contain equal number of Good and Poor instances
min_instances <- min(table(dat$Credit_Score))
balanced_data <- dat %>%
  group_by(Credit_Score) %>%
  slice_sample(n = min_instances) %>%
  ungroup()

#Retaining a copy of data for performing KNN before feature engineering into categorical columns
Knn_data<-balanced_data

# Transforming  Num_Bank_Accounts into three levels: Low, High, and Very High, based on frequency table
table(balanced_data$Num_Bank_Accounts)
balanced_data$Num_Bank_Accounts <- cut(as.numeric(as.character(balanced_data$Num_Bank_Accounts)), breaks = c(0, 3, 6, Inf), labels = c("Low", "High", "Very High"), include.lowest = TRUE)
#Num_Credit_Card
table(balanced_data$Num_Credit_Card)
balanced_data$Num_Credit_Card <- cut(as.numeric(as.character(balanced_data$Num_Credit_Card)), breaks = c(0, 2, 7, Inf), labels = c("0-2", "3-7", ">=8"), include.lowest = TRUE)
# Transforming Num_Credit_Inquiries into ordered factors after checking the distribution via frequency table & barplot=
table(balanced_data$Num_Credit_Inquiries)
# Create a bar plot to visualise Num_Credit_Inquiries
ggplot(balanced_data, aes(x = reorder(factor(Num_Credit_Inquiries), as.numeric(Num_Credit_Inquiries)), fill = Credit_Score)) +
   geom_bar(position = "dodge", alpha = 0.7) +
   labs(title = "Bar Plot of Num_Credit_Inquiries by Credit_Score")
# Combine factors into 3 levels
balanced_data$Num_Credit_Inquiries <- cut(as.numeric(as.character(balanced_data$Num_Credit_Inquiries)), breaks = c(0, 3, 7, Inf), labels = c("Low", "Moderate", "High"), include.lowest = TRUE)
# Convert Num_of_Loan to a factor with 'Low' and 'High' levels
table(balanced_data$Num_of_Loan)
# Create a bar plot to visualize Num_of_Loan
ggplot(balanced_data, aes(x = Num_of_Loan, fill = Credit_Score)) +
  geom_bar(position = "dodge", alpha = 0.7) +
  labs(title = "Bar Plot of Num_of_Loan by Credit_Score")
balanced_data$Num_of_Loan <- factor(ifelse(as.numeric(balanced_data$Num_of_Loan) <= 5, 'Low', 'High'),
                                levels = c('Low', 'High'))

####################################FUNCTIONS FOR CALUCULATING PERFORMANCE#########################
# Function to evaluate performance and setting cutoffs
performance = function(ytest, ypred, ct,method) {
  measures = c(
    Method=method,
    Cutoff = ct,
    ErrorRate = mean(ytest != ypred),
    Sensitivity = mean(ytest[ytest == 1] == ypred[ytest == 1]),
    Specificity = mean(ytest[ytest == 0] == ypred[ytest == 0]),
    Accuracy = mean(ytest == ypred)
  )
  return(measures)
}
#Setting threshold
cut_offs = c(0.6, 0.7, 0.8)

####################################LOGIISTIC CLASSIFIER############################################
# Splitting data to training and test
train = round(nrow(balanced_data) * 0.7, 0)
train = sample(nrow(balanced_data), train)
df_train = balanced_data[train, ]
df_test = balanced_data[-train, ]
result_log <- data.frame()

# Fit a generalized linear model with a binomial family to predict credit card default using all available predictors
fit_glm <- glm(Credit_Score ~ ., data = df_train, family = binomial())
# Loop over different variable selection methods
methods <- c("forward", "backward", "both")

for (method in methods) {
  # Perform variable selection based on the chosen method
  if (method == "forward") {
    model <- stepAIC(fit_glm, direction = method, scope = list(lower = ~1, upper = fit_glm$formula), trace = FALSE)
  } else {
    model <- stepAIC(fit_glm, direction = method, trace = FALSE)
  }
  
  # Loop over different cutoffs
  for (cutoff in cut_offs) {
    pred_probs <- predict.glm(model, newdata = df_test, type = "response")
    # Classify clients as defaulting or not defaulting based on the threshold
    pred <- ifelse(pred_probs < cutoff, 0, 1)
    # Calculate Performance
    performance_result = performance(as.numeric(df_test$Credit_Score) - 1, pred, cutoff,method)
    # Combine results into a data frame
    result_row <- data.frame(t(performance_result))
    result_log <- rbind(result_log, result_row)
    # Print the method, cutoff value, selected formula, and extracted metrics
    cat("Results for ",method, "at Cutoff:", cutoff, "\n")
    print(data.frame(t(performance_result)))
    print(confusionMatrix(table(pred,as.numeric(df_test$Credit_Score) - 1)))
    cat("Selected Formula:\n", as.character(formula(model)), "\n")
    cat("\n")
  }
}
# Print the final  result_log
print(result_log)

####################################KNN Classification###########################################################
#Plot showing month 
# Create a bar plot for Credit_Score and Month
ggplot(Knn_data, aes(x = Month, fill = Credit_Score)) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Plot: Credit_Score vs. Month",
       x = "Month",
       y = "Count") +
  scale_fill_manual(values = c("Good" = "lightblue", "Bad" = "lightgreen")) +
  theme_minimal()
# Create a bar plot for Credit_Score and Payment_of_Min_Amount
ggplot(Knn_data, aes(x = Payment_of_Min_Amount, fill = Credit_Score)) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Plot: Credit_Score vs. Payment_of_Min_Amount",
       x = "Payment_of_Min_Amount",
       y = "Count") +
  scale_fill_manual(values = c("Good" = "lightblue", "Bad" = "lightgreen")) +
  theme_minimal()

#Select significant variables 
Knn_data <- subset(Knn_data, select = -c(Occupation,Annual_Income,Num_of_Loan,Num_of_Delayed_Payment,Month))
Knn_data$Credit_Score=as.numeric((Knn_data$Credit_Score)) #convert pure strings to numeric to assign levels
Knn_data$Credit_Score=Knn_data$Credit_Score-1 
# One-hot encoding function
one_hot_encode <- function(data, col) {
  data <- cbind(data, model.matrix(~ . - 1, data = data[, col, drop = FALSE]))
  data <- data[, !(names(data) %in% col)]
  return(data)
}
# Apply one-hot encoding using the function
categorical_cols <- c("Credit_Mix","Payment_of_Min_Amount","Payment_Behaviour")
for (col in categorical_cols) {
  Knn_data <- one_hot_encode(Knn_data, col)
}
Knn_data[c("Num_Bank_Accounts", "Num_Credit_Card", "Num_Credit_Inquiries")] <- lapply(Knn_data[c("Num_Bank_Accounts", "Num_Credit_Card", "Num_Credit_Inquiries")], as.numeric)

# Move "Credit_Score" to the last column
Knn_data <- Knn_data[, c(setdiff(names(Knn_data), "Credit_Score"), "Credit_Score")]

#Splitting data for KNN
n.train = floor( nrow(Knn_data)*0.7 )
ind.train = sample(1:nrow(Knn_data), n.train)
ind.test = setdiff(1:nrow(Knn_data), ind.train)

Knn_data[,1:24] = scale(Knn_data[,1:24])
Xtrain = Knn_data[ind.train,1:24]
Xtest = Knn_data[ind.test,1:24]
ytrain = Knn_data[ind.train,25]
ytest = Knn_data[ind.test,25]
result_knn <- data.frame()

get.prob = function(x) {
  prob = attr(x, 'prob')
  ind=which(x==0)
  prob[ind] = 1 - prob[ind]
  return(prob)
}

#function to find best k for KNN
knn.bestK = function(train, test, y.train, y.test, k.grid = 1:100, ct) {
  # browser()
  fun.tmp = function(x) {
    y.tmp = knn(train, test, y.train, k = x, prob=T) # run knn for each k in k.grid
    prob = get.prob(y.tmp)
    y.hat = as.numeric( prob > ct ) + 1
    return( sum(y.hat != as.numeric(y.test)) )
  }
  ## create a temporary function (fun.tmp) that we want to apply to each value in k.grid
  error = unlist(lapply(k.grid, fun.tmp))
  names(error) = paste0('k=', k.grid)
  ## it will return a list so I need to unlist it to make it to be a vector
  out = list(k.optimal = k.grid[which.min(error)], 
             error.min = min(error)/length(y.test),
             error.all = error/length(y.test))
  return(out)
}

# Loop over cut-off values
for (cut_off in cut_offs) {
  # Find optimal k
  obj = knn.bestK(Xtrain, Xtest, ytrain, ytest, seq(1, 100, 2), cut_off)
  # Rerun with the best k
  ypred = knn(Xtrain, Xtest, ytrain, k = obj$k.optimal, prob = TRUE)
  # Evaluate performance
  performance_result = performance(ytest, ypred, cut_off,"KNN")
  result_row <- data.frame(t(performance_result))
  result_knn <- rbind(result_knn, result_row)
  print(obj$k.optimal)
}

# Print the results data frame
colnames(result_knn) <- c("Method","Cutoff", "ErrorRate", "Sensitivity", "Specificity", "Accuracy")
print(result_knn)

############################################CART################################################
#Function for  selecting CP
cartWithCPVal <- function(cp,cart.me = NULL, cart.bestpruned = NULL){
  K=10
  fit = rpart(Credit_Score ~ ., method="class", data=df_train, minsplit=5, xval=K, cp=cp)
  #Min-Error Tree
  cart.me = prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
  rpart.plot(cart.me, main = 'Min Error Tree')
  #Best Pruned Tree
  ind = which.min(fit$cptable[,"xerror"])
  se1 = fit$cptable[ind,"xstd"]/sqrt(K)
  xer1 = min(fit$cptable[,"xerror"]) + se1
  ind0 = which.min(abs(fit$cptable[1:ind,"xerror"] - xer1))
  cart.bestpruned = prune(fit, cp = fit$cptable[ind0,"CP"])
  rpart.plot(cart.bestpruned, main = 'Best Pruned Tree')
  printcp(x = cart.bestpruned)
  #Error on df_train Data
  yhat = predict(cart.bestpruned, df_train, type = "class")
  err.bp = mean(yhat != df_train$Credit_Score)
  #err.bp
  cat("\nAccuraccy:",1-err.bp)
  return(list(cart.me = cart.me, cart.bestpruned = cart.bestpruned))
}

#Comparing Different CP Values
cart0 = cartWithCPVal(0.00081)
cart1 = cartWithCPVal(0.00083)
cart2 = cartWithCPVal(0.00087)
cart3 = cartWithCPVal(0.001)

#Final Min Error Tree
cart.me = cart1$cart.me
rpart.plot(cart.me, main = 'Final Min Error Tree', tweak=1.3)
#Final Best Pruned Tree
cart.bestpruned = cart1$cart.bestpruned
rpart.plot(cart.bestpruned, main = 'Final Best Pruned Tree', tweak=1.3)
#Accuracy on Training Data
yhat = predict(cart.bestpruned, df_train, type = "class")
err.bp = mean(yhat != df_train$Credit_Score)
cat("Error:", err.bp)
cat("Accuraccy:",1-err.bp)
#Accuracy on Testing Data
yhat = predict(cart.bestpruned, df_test, type = "class")
err.df_test = mean(yhat != df_test$Credit_Score)
cat("Error on Validation Data:", err.df_test)
cat("Accuracy on Validation Data:", 1-err.df_test)

# Function to evaluate performance and setting cutoffs
performance = function(ytest, ypred, ct,method) {
  measures = c(
    Method=method,
    Cutoff = ct,
    ErrorRate = mean(ytest != ypred),
    Sensitivity = mean(ytest[ytest == 1] == ypred[ytest == 1]),
    Specificity = mean(ytest[ytest == 0] == ypred[ytest == 0]),
    Accuracy = mean(ytest == ypred)
  )
  
  return(measures)
}

performance_table_CART = function(cart_model ,cart_type){
  result_cart <- data.frame()
  for (cut_off in cut_offs) {
    prob1 = predict(cart_model, df_train, type = "prob")[,2]
    ypred = as.numeric(prob1 > cut_off)
    ytest = as.numeric(df_train$Credit_Score) - 1
    performance_result = performance(ytest, ypred, cut_off,cart_type)
    # Combine results into a data frame
    result_row <- data.frame(t(performance_result))
    result_cart <- rbind(result_cart, result_row)
  }
  colnames(result_cart) <- c("Method", "Cutoff", "ErrorRate", "Sensitivity", "Specificity", "Accuracy")
  return(result_cart)
}
result_cart_minimunerror = performance_table_CART(cart.me,"CART-ME Tree")
result_cart_bestpruned = performance_table_CART(cart.bestpruned,"CART-BP Tree")
#CART results
result_cart = rbind(result_cart_minimunerror, result_cart_bestpruned)
result_cart


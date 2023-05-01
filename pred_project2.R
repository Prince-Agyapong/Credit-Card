library(tidyverse)
# Load data
vintage <- read.csv("/Users/prince/Downloads/April Vintage 2013.csv") %>%
  group_by(ID) %>%
  mutate(lifetime_purchases = sum(TotalNetPurchaseAndCash),
         lifetime_payments = sum(TotalNetPayments)) 

 view(vintage) 
 

# Selecting MOB = 3
vintage1 <- subset(vintage, MOB == 3)
summary(vintage1)
#view(vintage1)

# Last statements
vintage2 <- vintage %>%
  group_by(ID) %>%
  slice_tail(n = 1)
#view(vintage2)

# Define bad
bad = if_else(vintage2$DaysDeliq >= 60 | !vintage2$ExtStatus %in% c(" ", "C" ),
              1, 0)

# Add bad to vintage2
vintage2$bad = bad

#view(vintage2)
summary(vintage2)
summary(bad)

# Create a frequency table of the "DaysDeliq" variable in "vintage2"
# This table shows the number of occurrences of each unique value, including any missing values
table(vintage2$DaysDeliq, useNA = "ifany")

# Create a frequency table of the "ExtStatus" variable in "vintage2"
# This table shows the number of occurrences of each unique value, including any missing values
table(vintage2$ExtStatus, useNA = "ifany")


 # Select ID and Bad
vintage3 = vintage2[, c(1, 27) ]
names(vintage2)

# Inner join on ID
model_vintage = inner_join(vintage1, vintage3, by = "ID") %>%
  mutate(charge_off_dollars = if_else(bad == 0, 0, EndingBalance))

view(model_vintage)
summary(model_vintage) 

# Bad rate
bad_rate <- sum(model_vintage$bad)/length(model_vintage$bad)
bad_rate

# Summary of lifetime_purchases
summary(model_vintage $lifetime_purchases)

# Summary of lifetime_payments
summary(model_vintage $lifetime_payments)


# Descriptive Statistics and Exploratory plots

# Plot 1: Bar plot of Outcome

ggplot(model_vintage , aes(x = factor(bad), fill = factor(bad))) +
  geom_bar(position = "dodge", color = "black") +
  ggtitle("Bar Plot of Bad") +
  xlab("bad") +
  ylab("Number of Customers") +
  scale_fill_manual(values = c("steelblue1", "lightcoral"), 
                    labels = c("NO", "YES")) +
  labs(fill = "bad") +
  theme_light()

# Plot 2: Boxplot of DaysDeliq 
ggplot(model_vintage , aes(x = DaysDeliq)) +
  geom_boxplot(fill = "steelblue1") +
  ggtitle("Box Plot of Days Delinquent") +
  ylab("Frequency") +
  xlab("Days Delinquent") +
  theme_light()

summary(model_vintage$DaysDeliq)

# Plot 3: Scatter plot of ActualMinPay vs. Opening Balance
ggplot(model_vintage , aes(x = ActualMinPay, y = OpeningBalance)) +
  geom_point(alpha = 0.7, color = "darkblue") +
  ggtitle("Actual Minimum Payment vs. Opening Balance") +
  xlab("Actual Minimum Payment") +
  ylab("Opening Balance") +
  theme_light()
cor(model_vintage$ActualMinPay, model_vintage$OpeningBalance)


# Plot 4: bar chart of TotalNetPurchaseAndCash and TotalFeesBilled by IntStatus
library(scales)

ggplot(model_vintage , aes(x = IntStatus)) +
  geom_bar(aes(y = TotalNetPurchaseAndCash, fill = "Net Purchase and Cash"),
           stat = "identity") +
  geom_bar(aes(y = TotalFeesBilled, 
               fill = "Fees Billed"), 
           stat = "identity",
           position = "stack") +
  ggtitle("Total Net Purchase and Cash and Fees Billed by Internal Status") +
  xlab("Internal Status") +
  ylab("Amount") +
  scale_fill_manual(values = c("navyblue", "lightblue")) +
  scale_y_continuous(labels = comma) +
  theme_economist_white()




# Plot 5: Line of CreditLimit by State

ggplot(model_vintage , aes(x = State, y = CreditLimit, group = 1)) +
  geom_line(stat = "summary", fun = "mean", color = "steelblue1", size = 1) +
  ggtitle("Average Credit Limit by State") +
  theme_light()

# Plot 6: Number of customers by state
ggplot(model_vintage , aes(x = State)) +
  geom_bar(fill = "navyblue") +
  ggtitle("Count of Customers by State") +
  xlab("State") +
  ylab("Count") +
  theme_light()


#  Plot 7: Histogram of OpeningBalance
ggplot(model_vintage , aes(x = OpeningBalance)) +
  geom_histogram(bins = 20, color = "black", fill = "steelblue1") +
  ggtitle("Histogram of Opening Balance") +
  xlab("Opening Balance") +
  ylab("Frequency") +
  theme_light()

# Plot 8: barplot of Total Net Purchased and Fees billed by Bad
library(ggthemes)
ggplot(model_vintage , aes(x = bad)) +
  geom_bar(aes(y = TotalNetPurchaseAndCash, fill = "Net Purchase and Cash"),
           stat = "identity") +
  geom_bar(aes(y = TotalFeesBilled, 
               fill = "Fees Billed"), 
           stat = "identity",
           position = "stack") +
  ggtitle("Total Net Purchase and Cash and Fees Billed by Bad Variable") +
  xlab("Bad Variable") +
  ylab("Amount") +
  scale_fill_manual(values = c("red4", "yellow2")) +
  scale_y_continuous(labels = scales::comma_format(), limits = c(0, NA)) +
  theme_economist_white()


# Splitting the dataset into the Training set and Test set (60% / 40%)
set.seed(123)
n = dim(model_vintage)[1]
split = sample(1:n,size = n*0.6,replace = F)

# Train set
train_set = model_vintage[split,] 
view(train_set)

# Test set
test_set = model_vintage[-split,] 
view(test_set)

# Fitting Decision Tree Regression to the dataset
tree = rpart(bad ~ ExtStatus + DaysDeliq + IntStatus + ActualMinPay + OverlimitAmount +
               DisplayMinPay + OpeningBalance + BillLateCharge + EndingBalance + CreditLimit
             + TotalNetPayments + TotalNetPurchaseAndCash + TotalFeesBilled + Concessions +
               QuarterlyCreditScore + Bscore, 
             data = train_set,
             cp = 0.002,
             minbucket = 150)
summary(tree)

# Prune Tree
pruned = clip.rpart(tree,best = 7)
pruned

# Draw Tree
draw.tree (pruned, cex=1, 
           nodeinfo = TRUE, units = "BadRate",
           cases ="obs",
           digits = 3, print.levels = TRUE,
           new = TRUE)

# Predicting a new result with Decision Tree Regression
tree_pred = predict(pruned, newdata = test_set) 

# Gains for tree
library(gains)
treegains = gains(test_set$bad, tree_pred)
treegains 

# Extract columns from gains object into data frame
treegains_df <- data.frame(depth = treegains$depth,
                           mean.prediction = treegains$mean.prediction,
                           mean.response = treegains$mean.resp)

# Plot of Depth against mean responses
ggplot(treegains_df, aes(x = depth)) +
  geom_line(aes(y = mean.prediction, color = "Predicted"), linetype = "dashed") +
  geom_line(aes(y = mean.response, color = "Actual")) +
  labs(x = "Depth", y = "Mean Response",
       title = "Mean Response by Sampled Population for Tree Model") +
  scale_color_manual(name = "Response", values = c("Predicted" = "blue", 
                                                   "Actual" = "red")) +
  theme_minimal()

# ROC for tree
library(ROCR) 

# Data frame with actual and predicted values
tree_results = as.data.frame(cbind(test_set$bad,tree_pred))

# Prediction
tree_roc.pred = prediction(tree_pred, test_set$bad,
                           label.ordering = NULL)

# Performance
tree.perf = performance(tree_roc.pred, measure = "tpr", x.measure = "fpr")
plot(tree.perf)

# Create a data frame with the ROC curve data
roc_tree_df <- data.frame(fpr = tree.perf@x.values[[1]], 
                          tpr = tree.perf@y.values[[1]])

# Calculate AUC 
auc_tree <- performance(tree_roc.pred, measure = "auc")@y.values[[1]]

# Plot ROC curve with reference line
ggplot(data = roc_tree_df, aes(x = fpr, y = tpr)) +
  geom_line(colour = "black", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  ggtitle(paste0("ROC Curve For Tree Model (AUC = ", round(auc_tree, 2), ")")) +
  theme_minimal()


# KS for tree
test2 = data.frame(tree.perf@x.values,tree.perf@y.values)
names(test2) = c('FPR','TPR')
head(test2)

#now plot percentile on the x axis and TPR and FPR as lines
cutcount = unlist(tree_roc.pred@tp) +unlist(tree_roc.pred@fp)
cutcount


percentile = cutcount/3030
percentile


ksdif = test2$FPR - test2$TPR
ksdif
ks = max(abs(ksdif))

plot(percentile,test2$FPR,type='l',xlab ='Depth of File',
     ylab ='Cumulative Proportion',col='purple',
     main = 'Kolmogorov-Smirnov Plot for Tree Model',)
points(percentile,test2$TPR,type = 'l',pch=1,col='forestgreen', lwd=1.5)
abline(0,1)
text(.2,.9,paste('Kolmogorov-Smirnov statistic =',round(ks,3)),cex=0.8)
which(abs(ksdif)==ks)

# Add a legend to the plot
legend("bottomright", 
       legend = c("False Positive Rate", "True Positive Rate"),
       col = c("purple", "forestgreen"),
       lty = 1,
       lwd = 2,
       bty = "n")

fprks=test2$FPR[which(abs(ksdif)==ks)]
tprks=test2$TPR[which(abs(ksdif)==ks)]
x=percentile[which(abs(ksdif)==ks)]
segments(x0=x,y0=fprks,x1=x,y1=tprks,col='red',lwd=2,lty=2)


# Logistic model 

mylogit = glm(bad ~ ExtStatus + DaysDeliq + IntStatus + ActualMinPay + OverlimitAmount +
               DisplayMinPay + OpeningBalance + BillLateCharge + EndingBalance + CreditLimit
             + TotalNetPayments + TotalNetPurchaseAndCash + TotalFeesBilled + Concessions +
               QuarterlyCreditScore + Bscore, 
             data = train_set,
             family = 'binomial')

summary(mylogit)

# Predicting a new result with Logistic Regression
logit_pred = predict(mylogit, newdata = test_set, type = "response") 
logit_pred = round(logit_pred, 4)
logit_pred

# Data frame with actual and predicted values
logit_results <- data.frame(bad = test_set$bad, 
                            Predicted = logit_pred)
logit_results


# Gains for logit
logitgains = gains(test_set$bad, logit_pred, 10) 
logitgains 
names(logitgains)
plot(logitgains)

# Extract columns from gains object into data frame
logitgains_df <- data.frame(depth = logitgains$depth,
                            mean.prediction = logitgains$mean.prediction,
                            mean.response = logitgains$mean.resp)


# Plot of Depth against mean responses
ggplot(logitgains_df, aes(x = depth)) +
  geom_line(aes(y = mean.prediction, color = "Predicted"), linetype = "dashed") +
  geom_line(aes(y = mean.response, color = "Actual")) +
  labs(x = "Depth",
       y = "Mean Response",
       title = "Mean Response by Sampled Population for Logit Model") +
  scale_color_manual(name = "Response",
                     values = c("Predicted" = "red", 
                                "Actual" = "darkslategray")) +
  theme_minimal()


# Prediction
logit_roc.pred = prediction(logit_results$Predicted, logit_results$bad,
                            label.ordering = NULL)
# Performance
logit.perf = performance(logit_roc.pred, measure = "tpr", x.measure = "fpr")

# Create a data frame with the ROC curve data
roc_df <- data.frame(fpr = logit.perf@x.values[[1]], 
                     tpr = logit.perf@y.values[[1]])

# Calculate AUC
auc_logit <- performance(logit_roc.pred, measure = "auc")@y.values[[1]]

# Plot ROC curve with reference line
ggplot(data = roc_df, aes(x = fpr, y = tpr)) +
  geom_line(colour = "darkgreen", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  ggtitle(paste0("ROC Curve for Logistic Model (AUC = ", round(auc_logit, 2), ")")) +
  theme_minimal()   


# KS for logistic
test = data.frame(logit.perf@x.values,logit.perf@y.values)
names(test) = c('FPR','TPR')
head(test)

#now plot percentile on the x axis and TPR and FPR as lines
cutcount = unlist(logit_roc.pred @tp) +unlist(logit_roc.pred @fp)
cutcount


percentile = cutcount/3030
percentile


ksdif = test$FPR - test$TPR
ksdif
ks = max(abs(ksdif))

plot(percentile,test$FPR,type='l',xlab ='Depth of File',
     ylab ='Cumulative Proportion',col='blue',
     main = 'Kolmogorov-Smirnov Plot for Logistic Model',)
points(percentile,test$TPR,type = 'l',pch=1,col='forestgreen')
abline(0,1)
text(.2,.9,paste('Kolmogorov-Smirnov statistic =',round(ks,3)),cex=0.8)


# Add a legend to the plot
legend("bottomright", 
       legend = c("False Positive Rate", "True Positive Rate"),
       col = c("blue", "forestgreen"),
       lty = 1,
       lwd = 2,
       bty = "n")

fprks1=test$FPR[which(abs(ksdif)==ks)]
tprks1=test$TPR[which(abs(ksdif)==ks)]
x1=percentile[which(abs(ksdif)==ks)]
segments(x0=x1,y0=fprks1,x1=x1,y1=tprks1,col='red',lwd=2,lty=2)


# Both ROC CURVES on one graph
# Create a data frame with the ROC curve data
roc_df <- data.frame(fpr = logit.perf@x.values[[1]], 
                     tpr = logit.perf@y.values[[1]])


ggplot(data = roc_df, aes(x = fpr, y = tpr)) +
  geom_line(aes(colour = "Logistic Regression"), size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  ggtitle(paste0("ROC Curve - Logistic Regression (AUC = ", 
                 round(auc_logit, 2), ")")) +
  theme_minimal() +
  # Add tree model ROC curve
  geom_line(data = roc_tree_df, aes(x = fpr, y = tpr, colour = "Tree Model"), 
            size = 1) +
  ggtitle("ROC Curve - Comparison of Logistic Regression and Tree Models") +
  labs(subtitle = paste0("AUC - Logistic Regression: ", round(auc_logit, 2),
                         " | AUC - Tree Model: ", round(auc_tree, 2))) +
  scale_color_manual(values = c("red", "blue1"), 
                     name = "Model", 
                     labels = c("Logistic Regression", "Tree Model"))


# Scored Data
Predicted_Tree = round(predict(pruned, model_vintage),4)
Predicted_Logit = round(predict(mylowgit, model_vintage,
                                type = "response"), 4)

Scored_vintage <- data.frame(Customer_ID = model_vintage$ID,
                          bad = model_vintage$bad, 
                          Predicted_Logit,
                          Predicted_Tree)

write.csv(Scored_vintage, file = "Scored_vintage.csv", row.names = FALSE)


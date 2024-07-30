################################################################################
################################################################################

# Ground Zero ... The point which initiated everything, produced life.

library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyverse)
library(tidyr)
library(Hmisc)
library(reshape2)
library(scales)
library(ggExtra)
library(ggcorrplot)
library(GGally)
library(scatterplot3d)
library(pwr)
library(data.table)
library(ggfortify)
library(caTools)
library(Metrics)
library(olsrr)
library(rms)
library(caret)
library(lmvar)
library(pROC)
library(ROCR)

setwd("D:\\university\\semester 1\\statistical inference\\hw\\pj2")
#setwd("D:\\pj2")
df <- read.csv(file='Airbnb_Open_Data.csv')
head(df)

# check for duplicates in entire dataframe (before pre-processing):
sum(duplicated(df) | duplicated(df, fromLast=TRUE))

# pre-processing:

# first the NA conversion:

df[df == ""] <- NA
df[df == "NA"] <- NA
df[df == "NAN"] <- NA
df[df == "-"] <- NA
df[df == "NONE"] <- NA
df[df == "none"] <- NA
df[df == "None"] <- NA

# second, turn appropriate character columns to categorical ones:

character_columns <- names(df[, sapply(df, is.character)])
character_columns

character_columns <- character_columns[character_columns != "NAME"]
character_columns <- character_columns[character_columns != "host.name"]
character_columns

df$review.rate.number <- ordered(df$review.rate.number, levels=1:5)

str(df)

df[character_columns] <- lapply(df[character_columns], as.factor)

str(df)

# third, to hopefully solve the problem of missing values, we have:

numerical_columns_with_na <- c("Construction.year", "price", "service.fee", "minimum.nights",
                               "number.of.reviews", "reviews.per.month",
                               "calculated.host.listings.count", "availability.365")

na_median <- function(x) replace(x, is.na(x), median(x, na.rm=TRUE))
df <- replace(df, numerical_columns_with_na, lapply(df[numerical_columns_with_na], na_median))

df$country <- df$country %>% replace_na("United States")

df <- df %>%
    group_by(neighbourhood) %>% 
    mutate(neighbourhood.group = na_if(neighbourhood.group, "")) %>% 
    fill(neighbourhood.group)

na_count <- sapply(df, function(x) sum(is.na(x)))
na_count <- data.frame(na_count)
na_count

# fourth, to fix manhattan -> Manhattan:

df[df == "manhatan"] <- "Manhattan"

df$neighbourhood.group <- droplevels(df$neighbourhood.group)

levels(df$neighbourhood.group)

# check for duplicates in entire dataframe (after pre-processing):
sum(duplicated(df) | duplicated(df, fromLast=TRUE))

################################################################################
################################################################################


################################################################################
################################################################################

# question 1:

################################################################################

# question 1 - part (a):

n <- 250

sampled_df <- df[sample(nrow(df), n),]

sampled_df_table <- table(sampled_df[, c("instant_bookable", "cancellation_policy")])
sampled_df_table

phat_1 <- sampled_df_table["False", "flexible"] / sum(sampled_df_table["False",])
phat_1

phat_2 <- sampled_df_table["True", "flexible"] / sum(sampled_df_table["True",])
phat_2

point_estimate <- phat_2 - phat_1
point_estimate

se <- sqrt(phat_2 * (1 - phat_2) / sum(sampled_df_table["False",]) + phat_2 * (1 - phat_2) / sum(sampled_df_table["True",]) )
se

z_star <- pnorm(0.975, lower.tail=FALSE)
z_star

confidence_interval <- point_estimate + c(-1, 1) * z_star * se
confidence_interval

################################################################################

################################################################################

# question 1 - part (b):

# pooling method:

# null_value <- 0

#p_pool <- (sampled_df_table["False", "flexible"] + sampled_df_table["True", "flexible"]) / (sum(sampled_df_table["False",]) + sum(sampled_df_table["True",]))
#p_pool

#point_estimate <- phat_2 - phat_1
#point_estimate

#se <- sqrt(p_pool * (1 - p_pool) * (1 / (sum(sampled_df_table["False",])) + 1 / (sum(sampled_df_table["True",]))))
#se

#z_score <- (point_estimate - null_value) / se)
#z_score

#p_value <- pnorm(z_score, lower.tail=FALSE)
#p_value

# chi-squared test method:
chisq.test(sampled_df_table)

################################################################################

################################################################################
################################################################################


################################################################################
################################################################################

# question 2:

n <- 15
n_sim <- 1000

sampled_df <- df[sample(nrow(df), n),]$instant_bookable

p_hat <- length(which(sampled_df == 'True')) / n
p_hat

simulation <- data.frame(t(replicate(n=n_sim, sample(levels(df$instant_bookable), size=n, replace=TRUE))))
head(simulation)

success <- apply(simulation, 1, function(x) length(which(x == 'True')))
head(success)

p_value <- length(which(success >= 8)) / n_sim
p_value

success_rate <- success / n

success_rate_df <- data.frame(success_rate=success_rate)

ggplot(data=success_rate_df, aes(x=success_rate)) +
    geom_histogram(aes(y=..count..), binwidth=0.075, fill="yellow", color="black") +
    ggtitle("Histogram of Success Rate of Simulation Method") + 
    theme(plot.title=element_text(hjust=0.5))

#hist(success_rate)

################################################################################
################################################################################


################################################################################
################################################################################

# question 3:

################################################################################

# question 3 - part (a):

n <- 100

k <- length(levels(df$cancellation_policy))
k

df_new <- df[!is.na(df$cancellation_policy),]

table(df_new$cancellation_policy) / length(df_new$cancellation_policy)

original_prob <- c(prop.table(table(df_new$cancellation_policy)))
original_prob

# observed, type: unbiased, random:

sampled_df_unbiased <- sample(df_new$cancellation_policy, n, replace=FALSE)
sampled_df_unbiased <- table(sampled_df_unbiased)
sampled_df_unbiased

# observed, type: biased:

bias_prob <- ifelse(df_new$cancellation_policy == "flexible", 0.7, 0.3)
head(bias_prob)

sampled_df_biased <- sample(df_new$cancellation_policy, n, prob=bias_prob)
sampled_df_biased <- table(sampled_df_biased)
sampled_df_biased

# first, implementation from scratch:

dof <- k - 1
dof

chi_squared_test <- function(observed_tmp, expected_tmp, dof_tmp)
{
    chi_squared <- 0
    
    for(i in 1:length(observed_tmp))
    {
        chi_squared <- chi_squared + ((observed_tmp[i] - expected_tmp[i]) ^ 2) / expected_tmp[i]
    }

    p_value <- pchisq(chi_squared, dof_tmp, lower.tail=FALSE)
    
    return(p_value)
}

# chi-squared test for unbiased samples:

expected <- sum(sampled_df_unbiased) * original_prob
expected

p_value <- chi_squared_test(sampled_df_unbiased, expected, dof)
p_value

# second approach, use built-in function:
chisq.test(sampled_df_unbiased, p=original_prob)

# chi-squared test for biased samples:

expected <- sampled_df_biased * original_prob
expected

p_value <- chi_squared_test(sampled_df_biased, expected, dof)
p_value

# second approach, use built-in function:
chisq.test(sampled_df_biased, p=original_prob)

################################################################################

################################################################################

# question 3 - part (b):

sampled_df <- df_new[sample(nrow(df_new), n, replace=FALSE),]

sampled_df_table <- table(sampled_df[, c("cancellation_policy", "instant_bookable")])
sampled_df_table

# just so we can keep the format, but enter different valuesL
expected_values <- copy(sampled_df_table)

# 3 rows, 2 columns, degree of freedom is:

nr <- length(levels(df$cancellation_policy))
nc <- length(levels(df$instant_bookable))

dof <- (nr - 1) * (nc - 1)
dof

# approach 1, our implementation:

table_total <- sum(sampled_df_table)
table_total

for(i in 1:nrow(sampled_df_table))
{
    for(j in 1:ncol(sampled_df_table))
    {
        expected_values[i, j] <- sum(sampled_df_table[i,]) * sum(sampled_df_table[, j]) / table_total
    }
}

expected_values

p_value <- chi_squared_test(sampled_df_table, expected_values, dof)
p_value

# approach 2, using built-in function:
chisq.test(sampled_df_table)

################################################################################

################################################################################
################################################################################


################################################################################
################################################################################

# question 4:

################################################################################

# question 4 - part (a):

colnames(df)

cor(df[sapply(df, is.numeric)][-1], df$number.of.reviews)

################################################################################

################################################################################

# question 4 - part (b):


# (a) and (b) sub-parts:

# for number.of.reviews ~ price

lm_price_number.of.reviews <- lm(formula=number.of.reviews ~ price, data=df)

summary(lm_price_number.of.reviews)

lm_price_number.of.reviews

# this is specific to (a):

# approach 1:
autoplot(lm_price_number.of.reviews)

# approach 2:

# linearity of the data:
plot(lm_price_number.of.reviews, 1)

# Normality of residuals:
plot(lm_price_number.of.reviews, 2)

# Constant Variability:
plot(lm_price_number.of.reviews, 3)


# for number.of.reviews ~ availability.365

lm_availability.365_number.of.reviews <- lm(formula=number.of.reviews ~ availability.365, data=df)

summary(lm_availability.365_number.of.reviews)

lm_availability.365_number.of.reviews

# this is specific to (a):

# approach 1:
autoplot(lm_availability.365_number.of.reviews)

# approach 2:

# linearity of the data:
plot(lm_availability.365_number.of.reviews, 1)

# Normality of residuals:
plot(lm_availability.365_number.of.reviews, 2)

# Constant Variability:
plot(lm_availability.365_number.of.reviews, 3)



# sub-part (d):

ggplot(data=df, aes(x=price)) +
    geom_point(aes(y=number.of.reviews), size=2, colour="grey") + 
    stat_smooth(aes(x=price, y=number.of.reviews, linetype="Linear Fit"), method="lm", formula=y ~ x, se=FALSE, color="black") + 
    scale_linetype_manual(name="Fit Type", values=c(2, 2)) + 
    ggtitle("number.of.reviews ~ price") +
    theme(legend.position="none") +
    theme(plot.title=element_text(hjust=0.5))

ggplot(data=df, aes(x=availability.365)) +
    geom_point(aes(y=number.of.reviews), size=2, colour="grey") + 
    stat_smooth(aes(x=availability.365, y=number.of.reviews, linetype="Linear Fit"), method="lm", formula=y ~ x, se=FALSE, color="black") + 
    scale_linetype_manual(name="Fit Type", values=c(2, 2)) + 
    ggtitle("number.of.reviews ~ availability.365") +
    theme(legend.position="none") +
    theme(plot.title=element_text(hjust=0.5))

################################################################################

################################################################################

# question 4 - part (d):

n <- length(df)
k <- 2 # number of our predictors

calculate_adjusted_r_squared <- function(model_tmp, predictor)
{
    ss_reg <- sum((fitted(model_tmp) - mean(predictor)) ^ 2)
    ss_res <- sum((fitted(model_tmp) - predictor) ^ 2)
    
    r_squared <- ss_res / (ss_reg + ss_res)
    
    adjusted_r_squared <- 1- (r_squared * (n - 1) / (n - k - 1))
    
    return(adjusted_r_squared)
}

# for number.of.reviews and price:

model <- lm(number.of.reviews ~ price, data=df)

#calculate_adjusted_r_squared(model, df$price)

anova(model)

summary(model)$adj.r.squared

# for number.of.reviews and availability.365:

model <- lm(number.of.reviews ~ availability.365, data=df)

#calculate_adjusted_r_squared(model, df$availability.365)

anova(model)

summary(model)$adj.r.squared

################################################################################

################################################################################

# question 4 - part (f):



# sub-part (a):

n <- 100

sampled_df <- df[sample(nrow(df), n),]

sample <- sample.split(sampled_df$number.of.reviews, SplitRatio=9/10)

number.of.reviews_train <- subset(sampled_df, sample == TRUE)
number.of.reviews_test <- subset(sampled_df, sample == FALSE)

# for number.of.reviews and price:

lm_price_number.of.reviews <- lm(formula=number.of.reviews ~ price, data=number.of.reviews_train)

summary(lm_price_number.of.reviews)

p_value <- summary(lm_price_number.of.reviews)$coefficients[8]
p_value

# for number.of.reviews and availability.365:

lm_availability.365_number.of.reviews <- lm(formula=number.of.reviews ~ availability.365, data=number.of.reviews_train)

summary(lm_availability.365_number.of.reviews)

p_value <- summary(lm_availability.365_number.of.reviews)$coefficients[8]
p_value



# sub-part (b):

confidence_interval <- function(model, alpha=0.05)
{
    point_estimate <- summary(model)$coefficients[2]
    se <- summary(model)$coefficients[4]
    round(point_estimate + c(-1, 1) * qnorm(1 - alpha / 2) * se, 4)
}

confidence_interval(lm_price_number.of.reviews)

confidence_interval(lm_availability.365_number.of.reviews)



# sub-part (c):

predictions_price <- round(predict(lm_price_number.of.reviews, number.of.reviews_test, type="response"), 2)
predictions_availability.365 <- round(predict(lm_availability.365_number.of.reviews, number.of.reviews_test, type="response"), 2)

predictions_vs_actual <- data.frame(number.of.reviews_test$number.of.reviews, predictions_price, predictions_availability.365)
colnames(predictions_vs_actual) <- c("Actual", "Predictions by price", "Predictions by availability.365")

predictions_vs_actual



# sub-part (d):

# first approach:

rmse_results <- c()

# for price:

price_rmse_result <- rmse(predictions_price, number.of.reviews_test$number.of.reviews)
rmse_results <- c(rmse_results, price_rmse_result)

# for availability.365:

availability.365_rmse_result <- rmse(predictions_availability.365, number.of.reviews_test$number.of.reviews)
rmse_results <- c(rmse_results, availability.365_rmse_result)

succes_rate <- data.frame((t(rmse_results)))
colnames(succes_rate) <- c("Results for price", "Results for availability.365")
succes_rate

# approach 2, MAPE:

mape_results <- c()

# for price:

predictions_vs_actual <- data.frame(cbind(actual=number.of.reviews_test$number.of.reviews, predicted=predictions_price))
price_mape_result <- paste(round((mean(abs((predictions_vs_actual$predicted - predictions_vs_actual$actual)) / predictions_vs_actual$actual)) * 100, 2), "%")
mape_results <- c(mape_results, price_mape_result) 

# for availability.365:

predictions_vs_actual <- data.frame(cbind(actual=number.of.reviews_test$number.of.reviews, predicted=predictions_availability.365))
availability.365_mape_result <- paste(round((mean(abs((predictions_vs_actual$predicted - predictions_vs_actual$actual)) / predictions_vs_actual$actual)) * 100, 2), "%")
mape_results <- c(mape_results, availability.365_mape_result) 

succes_rate <- data.frame((t(mape_results)))
colnames(succes_rate) <- c("Results for price", "Results for availability.365")
succes_rate

################################################################################

################################################################################
################################################################################


################################################################################
################################################################################

# question 5:

################################################################################

# question 5 - part (a):

# ggpairs(data.frame(
#     "number.of.reviews"=df$number.of.reviews,
#     "price"=df$price,
#     "minimum.nights"=df$minimum.nights,
#     "reviews.per.month"=df$reviews.per.month,
#     "calculated.host.listings.count"=df$calculated.host.listings.count,
#     "availability.365"=df$availability.365))

columns <- c("number.of.reviews", "neighbourhood.group", "reviews.per.month",
             "calculated.host.listings.count", "availability.365",
             "host_identity_verified")

ggpairs(data=df[, columns], upper=list(continuous="points", combo="dot_no_facet"), title="Correlogram") +
    theme(plot.title=element_text(hjust=0.5))

cor(df$reviews.per.month, df$number.of.reviews)
cor(df$price, df$number.of.reviews)

################################################################################

################################################################################

# question 5 - part (b):

model <- lm(number.of.reviews ~ reviews.per.month + host_identity_verified + neighbourhood.group + calculated.host.listings.count + availability.365, data=df)

summary(model)

plot(model$residuals, pch=16, col="red")

################################################################################

################################################################################

# question 5 - part (d):

create_formula <- function(response_variable, explanatory_variable)
{
    as.formula(paste(response_variable, paste(explanatory_variable, collapse=" + "), sep=" ~ "))
}

stepwise_selection <- function(method, all_variables)
{
    selected <- rep(0, length(all_variables))

    max_total_parameter <- 0
    
    for(i in seq(1, length(all_variables)))
    {
        remaining_variables <- all_variables[selected == 0]
        
        if(length(remaining_variables) == 0)
        {
            break
        }
        
        max_step_parameter <- 0
        max_step_variable <- NULL
        
        for(j in seq(1, length(remaining_variables)))
        {
            if(method == "forward")
            {
                temp_variables <- c(all_variables[selected == 1], remaining_variables[j])
            }
            else if(method == "backward")
            {
                temp_variables <- setdiff(all_variables[selected == 0], remaining_variables[j])
            }
            
            formula <- create_formula("number.of.reviews", temp_variables)
            
            model_summary <- summary(lm(formula, data=df))
            
            if(model_summary$adj.r.squared > max_step_parameter)
            {
                max_step_variable <- remaining_variables[j]
                max_step_parameter <- model_summary$adj.r.squared
            }
        }
        
        selected[which(all_variables == max_step_variable)] <- 1
        
        if(max_total_parameter < max_step_parameter)
        {
            max_total_parameter <- max_step_parameter
        }
        else
        {
            break
        }
    }
    
    if(method == "forward")
        return(lm(create_formula("number.of.reviews", all_variables[selected == 1]), data=df, y=TRUE, x=TRUE))
    else if(method == "backward")
        return(lm(create_formula("number.of.reviews", all_variables[selected == 0]), data=df, y=TRUE, x=TRUE))
}

variables <- names(df)
variables <- variables[variables != "NAME"]
variables <- variables[variables != "host.name"]
variables <- variables[variables != "X"]
variables <- variables[variables != "id"]
variables <- variables[variables != "host.id"]
variables <- variables[variables != "host.name"]
variables <- variables[variables != "country"]
#variables <- variables[variables != "long"]
#variables <- variables[variables != "lat"]
# if i omit lat and long, 0.01 point will be decreased from adjusted r-squared
variables

best_forward <- stepwise_selection(method="forward", variables)
summary(best_forward)

best_backward <- stepwise_selection(method="backward", variables)
summary(best_backward)

# the chosen ones, based on best_backward model:
explanatory_variables <- c("calculated.host.listings.count", "host_identity_verified",
                           "neighbourhood", "availability.365",
                           "review.rate.number", "reviews.per.month", "room.type",
                           "instant_bookable", "long", "lat")

final_model_formula <- create_formula("number.of.reviews", explanatory_variables)

final_model <- lm(formula=final_model_formula, data=df)

summary(final_model)

#final_model$coefficients
#best_forward$coefficients
#best_backward$coefficients

################################################################################

################################################################################

# question 5 - part (e):

# for part (b):

# approach 1:

model <- lm(formula=number.of.reviews ~ reviews.per.month + host_identity_verified + neighbourhood.group + calculated.host.listings.count + availability.365, data=df, na.action=na.exclude, x=TRUE, y=TRUE)

summary(model)

cv.lm(model, k=5)

# approach 2:

train_control_model <- trainControl(method="cv", number=5)

model <- train(number.of.reviews ~ reviews.per.month + host_identity_verified + neighbourhood.group + calculated.host.listings.count + availability.365, data=df, na.action=na.exclude, trControl=train_control_model, method="lm") 
model

model$resample


# for part (d):

# the chosen ones, based on best_backward model:
explanatory_variables <- c("calculated.host.listings.count", "host_identity_verified",
                           "neighbourhood", "availability.365",
                           "review.rate.number", "reviews.per.month", "room.type",
                           "instant_bookable", "long", "lat")

final_model_formula <- create_formula("number.of.reviews", explanatory_variables)

# approach 1:

model <- lm(formula=final_model_formula, data=df, na.action=na.exclude, x=TRUE, y=TRUE)

summary(model)

cv.lm(model, k=5)

# approach 2:

train_control_model <- trainControl(method="cv", number=5)

model <- train(final_model_formula, data=df, trControl=train_control_model, na.action=na.exclude,  method="lm") 
model

model$resample

################################################################################

################################################################################

# question 5 - part (f):

# linearity:
plot(final_model, 1)

# nearly normal:

ggplot(data=final_model, aes(sample=final_model$residuals)) + 
    stat_qq(col="red", alpha=0.5) +
    stat_qq_line()

ggplot(data=final_model, aes(final_model$residuals)) + 
    geom_histogram(bins=25, col="black", fill="yellow", alpha=0.5)

# constant variability:

ggplot(data=final_model, aes(final_model$fitted, final_model$residuals)) + 
    geom_point(color="red", alpha=0.5) + 
    stat_smooth(method=lm, se=FALSE, color="black")

autoplot(final_model)

################################################################################

################################################################################
################################################################################


################################################################################
################################################################################

# question 6:

levels(df$host_identity_verified)

training_ratio <- sample.split(df$host_identity_verified, SplitRatio=0.7)

train_df <- subset(df, training_ratio == TRUE)
test_df  <- subset(df, training_ratio == FALSE)

################################################################################

# question 6 - part (a):

model <- glm(host_identity_verified ~ neighbourhood.group + instant_bookable + review.rate.number + number.of.reviews + Construction.year + price, family=binomial(link='logit'), data=train_df)

summary(model)

################################################################################

################################################################################

# question 6 - part (b):

predictions <-  predict(model, train_df, type="response")
roc(host_identity_verified ~ predictions, data=train_df, plot=TRUE, print.auc=TRUE, smooth=TRUE)

predictions <-  predict(model, test_df, type="response")
roc(host_identity_verified ~ predictions, data=test_df, plot=TRUE, print.auc=TRUE, smooth=TRUE)

################################################################################

################################################################################

# question 6 - part (c):

# we choose "instant_bookable" variable which has TRUE/FALSE levels:

levels(df$instant_bookable)

true_prob <- seq(0, 1.01, 0.01)

or_ratio <- abs(summary(model)$coefficients[6])

predict_ <- function(x)
{
    return((or_ratio * x / (1 - x)) / (1 + (or_ratio * x / (1 - x))))
}

false_prob <- sapply(true_prob, predict_)

plot(false_prob, true_prob, type="l", col="red", lwd=1.5) +
    abline(a=0, b=1)

################################################################################

################################################################################

# question 6 - part (d):

# approach1:
exp(summary(model)$coefficients[6, 1] + qnorm(c(0.025, 0.975)) * summary(model)$coefficients[6, 2])

# approach 2:
exp(confint(model))[6,]

################################################################################

################################################################################
################################################################################


################################################################################
################################################################################

# question 7:

################################################################################

# question 7 - part (b):

# if bigger than 50 -> 1, else -> 0
df$number.of.reviews_categorical <- ifelse(df$number.of.reviews < 50, 0, 1)

df$number.of.reviews_categorical <- as.factor(df$number.of.reviews_categorical)

levels(df$number.of.reviews_categorical)

true_prob <- seq(0, 1.01, 0.01)

or_ratio <- abs(summary(model)$coefficients[11])

predict_ <- function(x)
{
    return((or_ratio * x / (1 - x)) / (1 + (or_ratio * x / (1 - x))))
}

false_prob <- sapply(true_prob, predict_)

plot(false_prob, true_prob, type="l", col="red", lwd=1.5) +
    abline(a=0, b=1)

################################################################################

################################################################################

# question 7 - part (c):

model <- glm(number.of.reviews_categorical ~ host_identity_verified + neighbourhood.group + instant_bookable + review.rate.number + reviews.per.month + Construction.year + price, family=binomial(link='logit'), data=df)

summary(model)

################################################################################

################################################################################

# question 7 - part (d):

training_ratio <- sample.split(df$number.of.reviews_categorical, SplitRatio=0.7)

train_df <- subset(df, training_ratio == TRUE)
test_df  <- subset(df, training_ratio == FALSE)

confusion_matrix_custom <- function(threshold)
{
    predictions_prob <- predict(model, newdata=test_df, type="response")
    
    predictions <- ifelse(predictions_prob > threshold, "1", "0")
    
    predictions <- factor(predictions, levels=c("0", "1"))
    
    cm <- confusionMatrix(predictions, as.factor(test_df$number.of.reviews_categorical))
    
    return(cm)
}

threshold <- seq(0, 1, 0.1)

utility_vector <- c()

for(i in 1:length(threshold))
{
    cm <- confusion_matrix_custom(threshold[i])
    
    TP <- cm$table[1]
    FP <- cm$table[2]
    FN <- cm$table[3]
    TN <- cm$table[4]
    
    utility <- TP + TN - 80 * FP - 10 * FN
    utility_vector <- c(utility_vector, utility)
}

plot(threshold, utility_vector, type="o", col="red", lwd=1.5) +
    abline(v=threshold[which.max(utility_vector)], col="black", lwd=2, lty=2) 

################################################################################

################################################################################
################################################################################

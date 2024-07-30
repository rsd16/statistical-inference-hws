################################################################################
################################################################################

# question 8:

setwd("D:\\university\\semester 1\\statistical inference\\hw\\hw4")

df <- read.csv(file="Diet.csv")
head(df)

# we want to work with weight loss, not "weight6weeks" or "pre.weight" alone.
df$weight_loss <- df$weight6weeks - df$pre.weight

head(df)

library(ggplot2)
library(dplyr)

################################################################################

# question 8 - part (a):

ggplot(data=df, aes(x=factor(Diet), y=weight_loss, fill=factor(Diet)))+
    geom_boxplot() +
    ggtitle("Relationship between diet and weight loss after 6 weeks") +
    theme(legend.position="none") +
    xlab("Diet") +
    theme(plot.title=element_text(hjust=0.5))

################################################################################

################################################################################

# question 8 - part (b):

diet_one_way_anova <- aov(weight_loss ~ factor(Diet), data=df)
diet_one_way_anova

#summary(diet_one_way_anova)

################################################################################

################################################################################

# question 8 - part (c):

summary(diet_one_way_anova)

################################################################################

################################################################################

# question 8 - part (d):

TukeyHSD(diet_one_way_anova)

plot(TukeyHSD(diet_one_way_anova), las=1)

################################################################################

################################################################################
################################################################################


################################################################################
################################################################################

# question 9:

# first load the dataset:

setwd("D:\\university\\semester 1\\statistical inference\\hw\\hw4")

df <- read.csv(file='Houses.csv')
head(df)

# then, we will go to each section in depth.

################################################################################

# question 9 - part (a):

N <- 1000

#df1 <- df[df$City == "London", "Area..Meter."]
#df2 <- df[df$City == "Berlin", "Area..Meter."]

df1 <- subset(df, select=Area..Meter., subset=City == "London", drop=TRUE)
df2 <- subset(df, select=Area..Meter., subset=City == "Berlin", drop=TRUE)

n1 <- length(df1)
n2 <- length(df2)

bootstrap_samples = c()

for(i in (1:N))
{
    sample1 <- sample(df1, n1, replace=TRUE)
    sample2 <- sample(df2, n2, replace=TRUE)
    
    diff_of_means <- mean(sample1) - mean(sample2)
    
    bootstrap_samples <- c(bootstrap_samples, diff_of_means)
}

bootstrap_samples_df = data.frame(bootstrap_samples=bootstrap_samples)

ggplot(data=bootstrap_samples_df, aes(x=bootstrap_samples)) +
    geom_histogram(color='black', fill="yellow", binwidth=7) +
    ggtitle("Histogram of Difference in Means for Bootstrap Samples") + 
    theme(plot.title=element_text(hjust=0.5))

################################################################################

################################################################################

# question 9 - part (b):

# calculate the p-value from original samples:

n1 <- length(df1)
n2 <- length(df2)

null_value <- 0

observation <- mean(df1) - mean(df2)
se <- sqrt((sd(df1) ^ 2) / n1 + (sd(df2) ^ 2) / n2)

t <- (observation - null_value) / se

dof <- min(n1, n2)

p_value_original_sample <- (1 - pt(t, df=dof)) * 2
p_value_original_sample

# calculate the p-value from bootstrap samples:

null_value <- 0

observation <- mean(df1) - mean(df2)
se <- sd(bootstrap_samples) / sqrt(length(bootstrap_samples))

t <- (observation - null_value) / se

dof <- length(bootstrap_samples) - 1

p_value_bootstrap_samples <- (1 - pt(t, df=dof)) * 2
p_value_bootstrap_samples

################################################################################

################################################################################

# question 9 - part (c):

# confidence interval for original sample:

n1 <- length(df1)
n2 <- length(df2)

observation = mean(df1) - mean(df2)
se <- sqrt((sd(df1) ^ 2) / n1 + (sd(df2) ^ 2) / n2)

dof <- min(n1, n2)

t_star <- qt((1 - 0.95) / 2, df=dof) * (-1)

lower <- observation - t_star * se
upper <- observation + t_star * se

cat("Confidence Interval for Original Samples = ", c(lower, upper))

# confidence interval for bootstrap samples:

observation <- mean(df1) - mean(df2)
se <- sd(bootstrap_samples) / sqrt(length(bootstrap_samples))

dof <- length(bootstrap_samples) - 1

t_star <- qt((1 - 0.95) / 2, df=dof) * (-1)

lower <- observation - t_star * se
upper <- observation + t_star * se

cat("Confidence Interval for Bootstrapped Samples = ", c(lower, upper))

################################################################################

################################################################################
################################################################################

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

setwd("D:\\university\\semester 1\\statistical inference\\hw\\pj1")

df <- read.csv(file='Airbnb_Open_Data.csv')
head(df)

# pre-processing:

# first the NA conversion:

df[df == ""] <- NA
df[df == "NA"] <- NA
df[df == "NAN"] <- NA
df[df == "-"] <- NA
df[df == "NONE"] <- NA
df[df == "none"] <- NA
df[df == "None"] <- NA

# then, turn appropriate character columns to categorical ones:

character_columns <- names(df[, sapply(df, is.character)])
character_columns

character_columns <- character_columns[character_columns != "NAME"]
character_columns <- character_columns[character_columns != "host.name"]
character_columns

df$review.rate.number <- ordered(df$review.rate.number, levels=1:5)

str(df)

df[character_columns] <- lapply(df[character_columns], as.factor)

str(df)

################################################################################
################################################################################


################################################################################
################################################################################

# question 0:

################################################################################

# question 0 - part (b):

nrow(df)
#length(colnames(df))
ncol(df)

columns <- data.frame(colnames(df))
colnames(columns) <- c("Columns of DataFrame")
columns

nrow(columns)

################################################################################

################################################################################

# question 0 - part (c):

# first identify the variables/columns with missing values:

na_count <- sapply(df, function(x) sum(is.na(x)))
na_count <- data.frame(na_count)
na_count

na_proportion <- sapply(df, function(x) sum(is.na(x)) / nrow(df))
na_proportion <- data.frame(na_proportion)
na_proportion

na_percentage <- sapply(df, function(x) round(sum(is.na(x)) / nrow(df) * 100, 2))
na_percentage <- data.frame(na_percentage)
na_percentage

# to hopefully solve the problem of missing values, we have:

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

# library(mice)
# md.pattern(df)
# imputed_df <- mice(df, m=5, maxit=50, meth="cart")
# summary(imputed_df)
# imputed_df <- complete(imputed_df)
# df[numerical_columns_with_na] <- imputed_df[numerical_columns_with_na]

################################################################################

################################################################################
################################################################################


################################################################################
################################################################################

# question 1:

################################################################################

# question 1 - part (a):

binwidth <- 15

ggplot(data=df, aes(x=number.of.reviews)) +
    geom_histogram(aes(y=..count..), color='black', fill="yellow", binwidth=binwidth) +
    geom_density(aes(y=..density.. * (nrow(df) * binwidth)), lwd=1, color='red') +
    ggtitle("'number.of.reviews' variable, Bin size = 15") +
    theme(plot.title=element_text(hjust=0.5))

################################################################################

################################################################################

# question 1 - part (c):

ggplot(data=df, aes(x=number.of.reviews))+
    geom_boxplot() +
    ggtitle("number.of.reviews") +
    theme(legend.position="none") +
    theme(plot.title=element_text(hjust=0.5))

################################################################################

################################################################################

# question 1 - part (e):

mean_df <- mean(df$number.of.reviews)
mean_df

median_df <- median(df$number.of.reviews)
median_df

variance_df <- var(df$number.of.reviews)
variance_df

standard_variation_df <- sd(df$number.of.reviews)
standard_variation_df

################################################################################

################################################################################

# question 1 - part (f):

percentages = c(
    sum((mean(df$number.of.reviews) - mean(df$number.of.reviews) / 2) > df$number.of.reviews),
    sum(((mean(df$number.of.reviews) - mean(df$number.of.reviews) / 2) <= df$number.of.reviews) & (df$number.of.reviews < mean(df$number.of.reviews))),
    sum(((mean(df$number.of.reviews) + mean(df$number.of.reviews) / 2) > df$number.of.reviews) & (df$number.of.reviews >= mean(df$number.of.reviews))),
    sum((mean(df$number.of.reviews) + mean(df$number.of.reviews) / 2) <= df$number.of.reviews)
    )

category_names = c('A', 'B', 'C', 'D')

result = data.frame(percentages=percentages, category_names=category_names)

result <- result %>% 
    arrange(desc(category_names)) %>%
    mutate(percentage=percentages / sum(result$percentages)) %>%
    mutate(y=cumsum(percentage) - 0.5 * percentage)

ggplot(data=result, aes(x="", y=percentage, fill=category_names)) +
    geom_bar(width=1, stat="identity") +
    ggtitle("'number.of.reviews' Groups and Their Proportion") +
    coord_polar(theta="y", start=0) +
    geom_text(aes(y=y, label=percent(percentage)), color="white", size=6) +
    theme_void() +
    theme(plot.title=element_text(hjust=0.5))

################################################################################

################################################################################

# question 1 - part (g):

ggplot(data=df, aes(x=number.of.reviews)) +
    geom_density(lwd=1, color='red') +
    geom_vline(aes(xintercept=median(number.of.reviews)), color="black", size=1) +
    geom_vline(aes(xintercept=mean(number.of.reviews)), color="purple", size=1) +
    ggtitle("'number.of.revieiws' Mean (Purple), Median (Black) and Density (Red)") + 
    theme(plot.title=element_text(hjust=0.5))

################################################################################

################################################################################
################################################################################


################################################################################
################################################################################

# question 2:

################################################################################

# question 2 - part (a):

df[df == "manhatan"] <- "Manhattan"

df$neighbourhood.group <- droplevels(df$neighbourhood.group)

levels(df$neighbourhood.group)

table(df$neighbourhood.group, useNA="no")

################################################################################

################################################################################

# question 2 - part (b):

#data=subset(df, !is.na(df$neighbourhood.group))

ggplot(data=df, aes(x=reorder(neighbourhood.group, -table(neighbourhood.group)[neighbourhood.group]), fill=neighbourhood.group)) +
    geom_bar(stat="count") +
    #geom_text(stat='count', aes(label=..count..), vjust=-1) +
    xlab("df$neighbourhood.group") +
    ggtitle("Horizontal Bar Plot of 'neighbourhood.group'") +
    theme(plot.title=element_text(hjust=0.5)) +
    theme(legend.position="none") +
    coord_flip()

################################################################################

################################################################################

# question 2 - part (c):

# data=subset(df, !is.na(df$neighbourhood.group))

ggplot(data=df, aes(x=reorder(neighbourhood.group,-table(neighbourhood.group)[neighbourhood.group]), fill=neighbourhood.group)) +
    geom_bar(stat="count") +
    geom_text(stat="count", aes(label=scales::percent(prop.table(stat(count)))), vjust=-1) +
    xlab("df$neighbourhood.group") +
    ggtitle("Bar Plot of 'neighbourhood.group'") +
    theme(plot.title=element_text(hjust=0.5)) +
    theme(legend.position="none") +
    coord_flip()

################################################################################

################################################################################

# question 2 - part (d):

ggplot(data=df, aes(x=neighbourhood.group, y=price, fill=neighbourhood.group)) +
    geom_violin(scale="count") +
    ggtitle("Violin Plot of 'neighbourhood.group'") +
    theme(plot.title=element_text(hjust=0.5)) +
    theme(legend.position="none")

################################################################################

################################################################################
################################################################################


################################################################################
################################################################################

# question 3:

################################################################################

# question 3 - part (a):

ggplot(data=df, aes(x=number.of.reviews, y=price)) +
    geom_point() +
    ggtitle("'number.of.reviews' ~ 'price'") +
    theme(plot.title=element_text(hjust=0.5))

ggplot(data=df, aes(x=service.fee, y=price)) +
    geom_point() +
    ggtitle("'service.fee' ~ 'price'") +
    theme(plot.title=element_text(hjust=0.5))

################################################################################

################################################################################

# question 3 - part (b):

ggplot(data=subset(df, !is.na(df$instant_bookable)), aes(x=service.fee, y=price, color=instant_bookable, shape=instant_bookable)) +
    geom_point() +
    ggtitle("'service.fee' ~ 'price' with 'instant_bookable'") +
    theme(plot.title=element_text(hjust=0.5))

################################################################################

################################################################################

# question 3 - part (c):

cor.test(df$service.fee, df$price)

################################################################################

################################################################################

# question 3 - part (d):

plt = ggplot(data=df, aes(x=service.fee, y=price)) +
    geom_point() + 
    geom_hex(bins=15) + 
    geom_smooth() + 
    scale_fill_viridis_c() + 
    ggtitle("'service.fee' ~ 'price', Bin Size = 15") + 
    theme(plot.title=element_text(hjust=0.5))

ggMarginal(p=plt, type="histogram")

################################################################################

################################################################################

# question 3 - part (e):

ggplot(data=df, aes(x=service.fee, y=price)) +
    #geom_density_2d() +
    stat_density_2d(aes(fill=..level..), geom="polygon", colour="red") +
    ggtitle("'service.fee' ~ 'price' 2D Density Plot") + 
    theme(plot.title=element_text(hjust=0.5))

################################################################################

################################################################################
################################################################################


################################################################################
################################################################################

# question 4:

################################################################################

# question 4 - part (a):

get_upper_tri <- function(cormat)
{
    cormat[lower.tri(cormat)] <- NA
    return(cormat)
}

numerical_variables <- c("Construction.year", "price", "service.fee", "minimum.nights", "number.of.reviews",
                         "reviews.per.month", "calculated.host.listings.count", "availability.365")

pvalue_matrix <- round(rcorr(as.matrix(df[, numerical_variables]))$P, 2)
pvalue_matrix <- get_upper_tri(pvalue_matrix)
melted_pvalue_matrix <- melt(pvalue_matrix, na.rm=TRUE)

corr_matrix <- round(rcorr(as.matrix(df[, numerical_variables]))$r, 2)
corr_matrix <- get_upper_tri(corr_matrix)
melted_corr_matrix <- melt(corr_matrix, na.rm=TRUE)

colnames(melted_corr_matrix)[3] <- "corr"

ggheatmap <- ggplot(data=melted_corr_matrix[melted_corr_matrix$Var1 != melted_corr_matrix$Var2,], aes(x=Var2, y=Var1, fill=corr)) +
    geom_tile(color="white")+
    scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0,
                         limit=c(-1, 1), space="Lab", name="Pearson\nCorrelation") +
    theme(axis.text.x=element_text(angle=45, vjust=1, size=6, hjust=1)) +
    coord_fixed() +
    geom_text(aes(label=paste("corr =", corr, "\n", "p =", melted_pvalue_matrix$value)), color="black", size=3) +
    labs(title=paste("Heatmap Correlogram"), x="", y="")

ggheatmap +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major=element_blank(),
          panel.border=element_blank(),
          panel.background=element_blank(),
          axis.ticks=element_blank(),
          legend.justification=c(1, 0),
          legend.position=c(0.6, 0.7),
          legend.direction="horizontal") +
    guides(fill=guide_colorbar(barwidth=7, barheight=1, title.position="top", title.hjust=0.5))

################################################################################

################################################################################

# question 4 - part (b):

numerical_variables <- c("Construction.year", "price", "service.fee", "minimum.nights", "number.of.reviews",
                         "reviews.per.month", "calculated.host.listings.count", "availability.365")

ggpairs(data=df[, numerical_variables], upper=list(continuous="points", combo="dot_no_facet"), title="Correlogram") +
    theme(plot.title=element_text(hjust=0.5))

# i don't use these, because they include some irrelevant numerical variables,
# such as id, X, host.id and etc:

#ggpairs(dplyr::select_if(df, is.numeric))
#ggpairs(df[sapply(df, is.numeric)])

################################################################################

################################################################################

# question 4 - part (c):

colors <- c("orange", "magenta")
colors <- colors[as.numeric(df$instant_bookable)]

with(df, scatterplot3d(availability.365, price, number.of.reviews,
                       main="3D Scatter Plot", pch=16, color=colors))

legend("right", legend=levels(df$instant_bookable), col=colors, pch=16)

################################################################################

################################################################################
################################################################################


################################################################################
################################################################################

# question 5:

################################################################################

# question 5 - part (a):

table(df[,c("instant_bookable", "host_identity_verified")])

################################################################################

################################################################################

# question 5 - part (b):

ggplot(data=df[!is.na(df$instant_bookable) & !is.na(df$host_identity_verified),], aes(x=instant_bookable, fill=host_identity_verified)) +
    geom_bar(position=position_dodge(width=0.8), width=0.7) +
    geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.8), vjust=-0.5, size=4) +
    ggtitle("'instant_bookable' ~ 'host_identity_verified'") +
    theme(plot.title=element_text(hjust=0.5))

################################################################################

################################################################################

# question 5 - part (c):

ggplot(data=df[!is.na(df$instant_bookable) & !is.na(df$host_identity_verified),], aes(x=instant_bookable, fill=host_identity_verified)) +
    geom_bar(position="fill", stat="count")+
    geom_text(aes(label=after_stat(count)), stat='count', position=position_fill(vjust=0.5)) +
    ggtitle("'instant_bookable' ~ 'host_identity_verified'") +
    theme(plot.title=element_text(hjust=0.5))

################################################################################

################################################################################

# question 5 - part (d):

df_new <- df[!is.na(df$instant_bookable),]
df_new <- df_new[!is.na(df_new$host_identity_verified),]

df_new %>% group_by(instant_bookable, host_identity_verified) %>%
    summarise(count=n()) %>%
    mutate(cut.count=sum(count), prop=count / sum(count)) %>%
    ungroup() %>%
    ggplot(aes(x=instant_bookable, y=prop, width=cut.count, fill=host_identity_verified)) +
    geom_bar(stat="identity", position="fill", colour="black") +
    geom_text(aes(label=scales::percent(prop)), position=position_stack(vjust=0.5)) + # if labels are desired
    facet_grid(~instant_bookable, scales="free_x", space="free_x")+
    theme(strip.background=element_blank(), strip.text=element_blank())

################################################################################

################################################################################
################################################################################


################################################################################
################################################################################

# question 6:

################################################################################

# question 6 - part (b):

n <- 25

null_value <- 0

sampled_df <- df[sample(nrow(df), n),]
sampled_df

observation <- abs(mean(sampled_df$number.of.reviews) - mean(sampled_df$price))
observation

se <- sd(sampled_df$number.of.reviews - sampled_df$price) / sqrt(n)
se

t_score <- abs(observation - null_value) / se
t_score

dof <- n - 1
dof

p_value <- 2 * pt(t_score, df=dof, lower.tail=FALSE)
p_value

me <- t_score * se
me

lower_bound <- observation - me
upper_bound <- observation + me

confidence_interval <- c(lower_bound, upper_bound)
confidence_interval

################################################################################

################################################################################
################################################################################


################################################################################
################################################################################

# question 7:

################################################################################

# question 7 - part (a):

n <- 150

sampled_df <- df[sample(nrow(df), n),]

sample_mean <- mean(sampled_df$service.fee)
sample_mean

sample_sd <- sd(sampled_df$service.fee)
sample_sd

se <- sample_sd / sqrt(n)
se

z_score <- qnorm((1 - 0.98) / 2, lower.tail=FALSE)
z_score

me <- z_score * se
me

lower_bound <- sample_mean - me
upper_bound <- sample_mean + me

confidence_interval <- c(lower_bound, upper_bound)
confidence_interval

################################################################################

################################################################################

# question 7 - part (c):

sampled_df <- data.frame(service.fee=sampled_df$service.fee)

ggplot(data=sampled_df, aes(x=service.fee)) +
    geom_histogram(aes(y=..density..), binwidth=10, fill="yellow", color="black") +
    ggtitle("Histogram of Sample, for 98% Confidence Interval for Mean") + 
    theme(plot.title=element_text(hjust=0.5)) + 
    geom_vline(aes(xintercept=lower_bound), linetype="dashed", size=1, color="red") + 
    geom_vline(aes(xintercept=upper_bound), linetype="dashed", size=1, color="red") + 
    geom_vline(aes(xintercept=sample_mean), linetype="dashed", size=1)

ggplot(data=df, aes(x=service.fee)) +
    geom_histogram(aes(y=..density..), binwidth=10, fill="yellow", color="black") +
    ggtitle("Histogram of Population, for 98% Confidence Interval for Mean") + 
    theme(plot.title=element_text(hjust=0.5)) + 
    geom_vline(aes(xintercept=lower_bound), linetype="dashed", size=1, color="red") + 
    geom_vline(aes(xintercept=upper_bound), linetype="dashed", size=1, color="red") + 
    geom_vline(aes(xintercept=sample_mean), linetype="dashed", size=1)

################################################################################

################################################################################

# question 7 - part (d):

mean(df$service.fee)

#null_Value <- mean(df$service.fee)
null_value <- 135

sample_mean <- mean(sampled_df$service.fee)
sample_mean

sample_sd <- sd(sampled_df$service.fee)
sample_sd

se <- sample_sd / sqrt(n)
se

z_score <- (sample_mean - null_value) / se
z_score

p_value <- 2 * pnorm(abs(z_score), lower.tail=FALSE)
p_value

################################################################################

################################################################################

# question 7 - part (f):

null_value <- 135
actual_mean <- mean(df$service.fee)

sample_sd <- sd(sampled_df$service.fee)
sample_sd

se <- sample_sd / sqrt(n)
se

z_score <- qnorm((1 - 0.95) / 2, lower.tail=FALSE)
z_score

me <- z_score * se
me

type_ii_error <- pnorm(abs(null_value + me - actual_mean) / se, lower.tail=FALSE) + 
                 pnorm(abs(null_value - me - actual_mean) / se, lower.tail=FALSE)

100 * round(type_ii_error, 3)

################################################################################

################################################################################

# question 7 - part (g):

power <- 100 * round(1 - type_ii_error, 3)
power

# for effect size:

for(null_value in c(seq(from=125,to=136, by=1)))
{
    power <- power.t.test(n=150, delta =mean(df$service.fee) - null_value, sd =sd(sampled_df$service.fee), type="one.sample", alternative="two.sided")$power
    cat("null-value =", null_value, "-- actual mean =", actual_mean, "-- effect size =", round(abs(null_value - actual_mean), 3), "- power=", round(power, 3) * 100, '%\n')
}

################################################################################

################################################################################
################################################################################


################################################################################
################################################################################

# question 8:

################################################################################

# question 8 - part (a):

quantile(df$number.of.reviews, probs=c(0.025, 0.975))

q1 <- quantile(df$number.of.reviews, probs=0.025, names=FALSE)
q1

q3 <- quantile(df$number.of.reviews, probs=0.975, names=FALSE)
q3

ggplot(data=df, aes(x=number.of.reviews)) +
    geom_histogram(aes(y =..count..), binwidth=15, fill="yellow", color="black") +
    ggtitle("Histogram of 95% Confidence Interval for Mean") + 
    theme(plot.title=element_text(hjust=0.5)) + 
    geom_vline(aes(xintercept=q1), linetype="dashed", size=1, color="red") + 
    geom_vline(aes(xintercept=q3), linetype="dashed", size=1, color="red")

################################################################################

################################################################################

# question 8 - part (b):

n <- 20

sampled_df <- df[sample(nrow(df), n),]$number.of.reviews

n_bootstrap <- 1000

bootstrap_samples <- c()

for(i in 1:n_bootstrap)
{
    bootstrap_sample <- sample(sampled_df, n, replace=TRUE)
    bootstrap_mean <- mean(bootstrap_sample)
    bootstrap_samples <- c(bootstrap_samples, bootstrap_mean) 
}

se <- sd(bootstrap_samples) / sqrt(n_bootstrap)
se

t_score <- qt(0.975, df=n_bootstrap - 1)
t_score

me <- t_score * se
me

confidence_interval <- mean(bootstrap_samples) + c(-me, me)
confidence_interval

bootstrap_samples_df <- data.frame(bootstrap_samples=bootstrap_samples)

ggplot(data=bootstrap_samples_df, aes(x=bootstrap_samples)) +
    geom_dotplot(binwidth=1.5) +
    ggtitle("Dot Plot of Bootstrap Distribution") + 
    theme(plot.title=element_text(hjust=0.5))

################################################################################

################################################################################

# question 8 - part (c):

ggplot(data=bootstrap_samples_df, aes(x=bootstrap_samples)) +
    geom_histogram(aes(y =..count..), binwidth=2, fill="yellow", color="black") +
    ggtitle("Histogram of Bootstrap Samples") + 
    theme(plot.title=element_text(hjust=0.5))

################################################################################

################################################################################
################################################################################


################################################################################
################################################################################

# question 9:

################################################################################

# question 9 - part (a):

one_way_anova <- aov(service.fee ~ cancellation_policy, data=df[!is.na(df$cancellation_policy),])
one_way_anova

summary(one_way_anova)

# actual mean values of three groups of "cancellation_policy" variable, for "service.fee" variable:

levels(df$cancellation_policy)

df1 <- subset(df, select=service.fee, subset=cancellation_policy == "flexible", drop=TRUE)
df2 <- subset(df, select=service.fee, subset=cancellation_policy == "moderate", drop=TRUE)
df3 <- subset(df, select=service.fee, subset=cancellation_policy == "strict", drop=TRUE)

#df1
#df2
#df3

length(df1)
length(df2)
length(df3)

mean(df1)
mean(df2)
mean(df3)

ggplot(data=df[!is.na(df$cancellation_policy),], aes(x=cancellation_policy, y=service.fee, fill=cancellation_policy)) +
    geom_boxplot() + 
    ggtitle("'cancellation_policy' ~ 'service.fee'") + 
    theme(legend.position="none") +
    theme(plot.title=element_text(hjust=0.5))

################################################################################

################################################################################

# question 9 - part (b):

# part 1. show the resulting plot of TukeyHSD:

TukeyHSD(one_way_anova)
plot(TukeyHSD(one_way_anova),las=1)

# part2. now, onto the performing paired t-test and comparing it with the results ...
# ... of the above:

df1 <- subset(df, select=service.fee, subset=cancellation_policy == "flexible", drop=TRUE)
df2 <- subset(df, select=service.fee, subset=cancellation_policy == "moderate", drop=TRUE)

n <- 25

sampled_df1 <- sample(x=df1, size=n, replace=FALSE)
sampled_df2 <- sample(x=df2, size=n, replace=FALSE)

# approach 1:
t.test(sampled_df1, sampled_df2, paired=TRUE, conf.level=0.95)

# approach 2:

diff <- sampled_df1 - sampled_df2
diff

mean_diff <- mean(diff)
mean_diff

mu0 <- 0
mu0

sd_diff <- sd(diff)
sd_diff

se <- sd_diff / sqrt(n)
se

dof <- n - 1
dof

p_value <- 2 * pt(abs((mean_diff - mu0) / se), lower.tail=FALSE, df=dof)
p_value

################################################################################

################################################################################
################################################################################
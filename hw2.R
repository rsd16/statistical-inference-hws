################################################################################
################################################################################

# question 9

# first load the dataset:

setwd("D:\\university\\semester 1\\statistical inference\\hw\\hw2")

df <- read.csv(file='Heart.csv')
head(df)

# then, we import some package:

library(ggplot2)

# then, we will go to each section in depth.

################################################################################

# question 9 - part (a):

quantile1 <- quantile(df$age, probs=0.025)
quantile1

quantile2 <- quantile(df$age, probs=0.975)
quantile2

ggplot(data=df, aes(x=age)) +
    geom_histogram(aes(y=..count..), color='black', fill="yellow", binwidth=2) +
    ggtitle("Bin size = 2") +
    theme(plot.title=element_text(hjust=0.5)) +
    geom_density(aes(y=..density.. * (nrow(df) * 2)), lwd=2, color='red') +
    geom_vline(xintercept=quantile1, color="black", linetype="dashed", size=1) +
    geom_vline(xintercept=quantile2, color="black", linetype="dashed", size=1)

################################################################################

################################################################################

# question 9 - part (b):

ggplot(df, aes(sample=thalch, shape=sex, color=sex)) +
    stat_qq() +
    stat_qq_line() +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())

################################################################################

################################################################################

# question 9 - part (c):

df <- within(df, origin <- factor(origin,
                                  levels=names(sort(table(origin), decreasing=FALSE))))

ggplot(df, aes(x=origin,fill=origin)) +
    geom_bar(stat="count") +
    ggtitle("Horizontal BarPlot of Origin") +
    theme(plot.title=element_text(hjust=0.5)) +
    theme(legend.position="none") +
    coord_flip()

################################################################################

################################################################################

# question 9 - part (d):

ggplot(df, aes(x=cp, y=trestbps, fill=cp))+
    geom_boxplot() +
    ggtitle("Heart cp and trestbps") +
    theme(legend.position="none") +
    theme(plot.title=element_text(hjust=0.5)) +
    ylim(0, 250)

################################################################################

################################################################################

# question 9 - part (e):

df_new <- df[!is.na(df$restecg),]
df_new <- df_new[!is.na(df_new$exang),]

df_new %>% group_by(restecg, exang) %>%
    summarise(count=n()) %>%
    mutate(cut.count=sum(count), prop=count / sum(count)) %>%
    ungroup() %>%
    ggplot(aes(x=restecg, y=prop, width=cut.count, fill=exang)) +
    geom_bar(stat="identity", position="fill", colour="black") +
    geom_text(aes(label=scales::percent(prop)), position=position_stack(vjust=0.5)) + # if labels are desired
    facet_grid(~restecg, scales="free_x", space="free_x")+
    theme(strip.background=element_blank(), strip.text=element_blank())

################################################################################

################################################################################
################################################################################

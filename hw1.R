################################################################################
################################################################################

# question 7

################################################################################

# question 7 - part (a):

# we first calculate for day:

scores_day <- c(99, 56, 78, 55.5, 32, 90, 80, 81, 56, 59, 45, 77, 84.5, 84, 70, 72, 68, 32, 79, 90)
scores_day

min_scores_day <- min(scores_day)
min_scores_day

max_scores_day <- max(scores_day)
max_scores_day

median_scores_day <- median(scores_day)
median_scores_day

# "probs" parameter refers to which percentile of the data.
q1_scores_day <- quantile(scores_day, probs=0.25, names=FALSE)
q1_scores_day

q3_scores_day <- quantile(scores_day, probs=0.75, names=FALSE)
q3_scores_day

# now, we calculate for night:

scores_night <- c(98, 78, 68, 83, 81, 89, 88, 76, 65, 45, 98, 90, 80, 84.5, 85, 79, 78, 98, 90, 79, 81, 25.5)
scores_night

min_scores_night <- min(scores_night)
min_scores_night

max_scores_night <- max(scores_night)
max_scores_night

median_scores_night <- median(scores_night)
median_scores_night

# "probs" parameter refers to which percentile of the data.
q1_scores_night <- quantile(scores_night, probs=0.25, names=FALSE)
q1_scores_night

q3_scores_night <- quantile(scores_night, probs=0.75, names=FALSE)
q3_scores_night

################################################################################

################################################################################

# question 7 - part (b):

# first, we do for day scores:

# to calculate the outliers, we first need to see the IQR and calculate...
# thresholds for outliers based on IQR:

iqr_scores_day <- IQR(scores_day)
iqr_scores_day

threshold_min_scores_day <- q1_scores_day - (1.5 * iqr_scores_day)

# now we check whether minimum value of the vector is actually...
# the first value in the vector, and there is now value lower than that:

threshold_min_scores_day

if(threshold_min_scores_day < min_scores_day)
{
    threshold_min_scores_day <- min_scores_day
}

threshold_min_scores_day

# now for maximum threshold:

threshold_max_scores_day <- q3_scores_day + (1.5 * iqr_scores_day)

# now we check whether maximum value of the vector is actually...
# the last value in the vector, and there is now value higher than that:

threshold_max_scores_day

if(threshold_max_scores_day > max_scores_day)
{
    threshold_max_scores_day <- max_scores_day
}

threshold_max_scores_day

# now to check whether we have outliers or not, for day scores:

#outliers_lower_than_min <- any(scores_day < threshold_min_scores_day)
outliers_lower_than_min <- scores_day[which(scores_day < threshold_min_scores_day)]
outliers_lower_than_min

outliers_higher_than_max <- scores_day[which(scores_day > threshold_max_scores_day)]
outliers_higher_than_max

# as for night scores:

iqr_scores_night <- IQR(scores_night)
iqr_scores_night

threshold_min_scores_night <- q1_scores_night - (1.5 * iqr_scores_night)

threshold_min_scores_night

if(threshold_min_scores_night < min_scores_night)
{
    threshold_min_scores_night <- min_scores_night
}

threshold_min_scores_night

# now for maximum threshold:

threshold_max_scores_night <- q3_scores_night + (1.5 * iqr_scores_night)

threshold_max_scores_night

if(threshold_max_scores_night > max_scores_night)
{
    threshold_max_scores_night <- max_scores_night
}

threshold_max_scores_night

# now to check whether we have outliers or not, for day scores:

outliers_lower_than_min_night <- scores_night[which(scores_night < threshold_min_scores_night)]
outliers_lower_than_min_night

outliers_higher_than_max_night <- scores_night[which(scores_night > threshold_max_scores_night)]
outliers_higher_than_max_night

# there are only outliers in scores for night, (45.0 and 25.5) and these are...
# smaller than actual threshold for minimum value for test scores at night.
# nevertheless, full discussion is done in "hw1.pdf" file. please refer to that.

################################################################################

################################################################################

# question 7 - part (c):

# answered in "hw1.pdf" file. please refer to that.

################################################################################

################################################################################

# question 7 - part (d):

# here, we will only plot the boxplot. for full discussion on the analysis...
# please refer to the "hw1.pdf" file.

boxplot(scores_day, scores_night,
        main="Test Scores for Day and Night",
        xlab="Day (Left Box) and Night (Right Box)",
        ylab="Test Scores")

################################################################################

################################################################################
################################################################################



################################################################################
################################################################################

# question 8

################################################################################

# question 8 - part (a):

library(ggplot2)

colnames(diamonds)

str(diamonds)

# column : type
# carat  : num
# cut    : Ord
# color  : Ord
# clarity: Ord
# depth  : num
# table  : num
# price  : int
# x      : num
# y      : num
# z      : num

################################################################################

################################################################################

# question 8 - part (b):

table(diamonds$clarity)

barplot(table(diamonds$clarity),
        ylim=c(0, 15000),
        main="Frequency of \"Clarity\" Variable in Diamonds Dataset",
        xlab="Categorical Values of \"Clarity\" Variable",
        ylab="Value Counts")

################################################################################

################################################################################

# question 8 - part (c):

hist(diamonds$price,
     xlim=c(0, 20000),
     ylim=c(0, 16000))

# for full discussion on the skewness, please refer to "hw1.pdf".

################################################################################

################################################################################

# question 8 - part (d):

boxplot(diamonds$price ~ diamonds$clarity,
        main="Relationship between Diamond Clarity and Price")

# for further explanations please refer to "hw1.pdf".

################################################################################

################################################################################

# question 8 - part (e):

table(diamonds$color)

## sorry for the large number of codes.
## i am a total noob in R, with a Python and C mindset.
## since, i am not allowed to use built-in packages,
## i wrote the codes this way.

## roses are red
## violets are blue
## coding efficiently in R
## i've got no clue

# i first want to do sum over all columns.
# unfortunately, i couldn't get "colSums" function to work.

total_frequency <- 0

for(value in table(diamonds$color))
{
    total_frequency <- total_frequency + value
}

total_frequency

# now, i will simply calculate the percentage of each value/column...
# and store it in an array.

percentages <- c()

counter <- 1

for(value in table(diamonds$color))
{
    percentages[counter] <- round((value / total_frequency) * 100)
    
    counter <- counter + 1
}

percentages

# i will concatenate labels (meaning, "A", "B" and etc.) with each of their...
# respective percentage.
# the result will look like: "A 30%".

labels <- paste(c("D,", "E,", "F,", "G,", "H,", "I,", "J,"), percentages)
labels <- paste(labels, "%", sep="")

pie(table(diamonds$color),
    main="Pie Chart of Colors in Diamonds Dataset",
    labels = labels,
    col=c("yellow", "blue", "red", "green", "purple", "lightblue", "mistyrose"))

legend("topleft",
       legend=c("D", "E", "F", "G", "H", "I", "J"),
       fill=c("yellow", "blue", "red", "green", "purple", "lightblue", "mistyrose"))

################################################################################

################################################################################

# question 8 - part (f):

plot(diamonds$depth, sqrt(diamonds$price),
     main="Relationship between Depth and Price",
     ylim=c(0, 160))

# for further explanations please refer to "hw1.pdf".

################################################################################

################################################################################
################################################################################

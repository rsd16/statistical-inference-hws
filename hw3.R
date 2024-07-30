################################################################################
################################################################################

# question 5 - part (b):

X <- seq(0, 1., by=.01)
curve(1 - (pnorm(60, mean=100 * x, sd=sqrt(100 * x * (1 - x))) - 
        pnorm(40, mean=100 * x, sd=sqrt(100 * x * (1 - x))) ),
        ylab="Power")

################################################################################
################################################################################


################################################################################
################################################################################

# question 7:

################################################################################

# question 7 - part (a):

a <- 0.05
n <- 50

plot(c(22, 23, 24, 25, 26, 27),
     pnorm(qnorm(a, 0, 1, lower.tail=TRUE) - (c(22, 23, 24, 25, 26, 27) - 28) / (5.6 / (sqrt(n)))),
     col="blue",type="l", xlab= "Means", ylab="alpha=0.05", main="Question 7, Part (a)")

################################################################################

################################################################################

# question 7 - part (b):

a <- 0.05
n <- 50

plot(c(22, 23, 24, 25, 26, 27),
     pnorm(qnorm(a, 0, 1, lower.tail=TRUE) - (c(22, 23, 24, 25, 26, 27) - 28) / (5.6 / (sqrt(n)))),
     col="blue", type="l", xlab= "Means", ylab="(alpha=0.05)=blue && (alpha=0.01)=red", main="Question 7, Part (b)")

a <- 0.01
n <- 50

lines(c(22, 23, 24, 25, 26, 27),
      pnorm(qnorm(a, 0, 1, lower.tail=TRUE) - (c(22, 23, 24, 25, 26, 27) - 28) / (5.6 / (sqrt(n)))),
      col="red",type="l")

################################################################################

################################################################################

# question 7 - part (c):

a <- 0.05
n <- 50

plot(c(22, 23, 24, 25, 26, 27),
     pnorm(qnorm(a, 0, 1, lower.tail=TRUE) - (c(22, 23, 24, 25, 26, 27) - 28) / (5.6 / (sqrt(n)))),
     col="blue", type="l", xlab= "Means", ylab="(n=50)=blue && (n=20)=red", main="Question 7, Part (c)")

a <- 0.01
n <- 20

lines(c(22, 23, 24, 25, 26, 27),
      pnorm(qnorm(a, 0, 1, lower.tail=TRUE) - (c(22, 23, 24, 25, 26, 27) - 28) / (5.6 / (sqrt(n)))),
      col="red",type="l")

################################################################################

################################################################################
################################################################################


################################################################################
################################################################################

# question 8:

# first load the dataset:

setwd("D:\\university\\semester 1\\statistical inference\\hw\\hw3")

df <- read.csv(file='Galton.csv')
head(df)

# then, we will go to each section in depth.

################################################################################

# question 8 - part (a):

original_mean_value <- mean(df$child)

it_was_in_interval <- 0

for(i in 1:20000) {
    # sample in size of 60, without replacement:
    x <- sample(df$child, size=60, replace=FALSE, prob=NULL)
    
    # calculate the mean of the sample data:
    mean_value <- mean(x)
    
    # compute the size:
    n <- length(x)
    
    # find the standard deviation:
    standard_deviation <- sd(x)
    
    # find the standard error:
    standard_error <- standard_deviation / sqrt(n)
    
    alpha = 0.05

    # degree of freedom for the t-test:
    degrees_of_freedom = n - 1

    # calculate the t-score using the information we computed:
    t_score = qt(p=alpha/2, df=degrees_of_freedom, lower.tail=FALSE)

    # margin of error:
    margin_error <- t_score * standard_error
    
    # calculate lower bound and upper bound:
    lower_bound <- mean_value - margin_error
    upper_bound <- mean_value + margin_error

    # check the condition that the original population mean value is inside the range:
    if(original_mean_value >= lower_bound && original_mean_value <= upper_bound) {
        it_was_in_interval <- it_was_in_interval + 1
    }
    
    #print(i)
}

it_was_in_interval

it_was_not_in_interval <- 20000 - it_was_in_interval
it_was_not_in_interval

percentage_in_interval <- it_was_in_interval / 20000 * 100
percentage_in_interval

percentage_not_in_interval <- it_was_not_in_interval / 20000 * 100
percentage_not_in_interval

################################################################################

################################################################################

# question 8 - part (b):

original_mean_value <- mean(df$child)

it_was_in_interval <- 0

for(i in 1:10000) {
    # sample in size of 10, without replacement:
    x <- sample(df$child, size=10, replace=FALSE, prob=NULL)
    
    # calculate the mean of the sample data:
    mean_value <- mean(x)
    
    # compute the size:
    n <- length(x)
    
    # find the standard deviation:
    standard_deviation <- sd(x)
    
    # find the standard error:
    standard_error <- standard_deviation / sqrt(n)
    
    alpha = 0.1

    # degree of freedom for the t-test:
    degrees_of_freedom = n - 1

    # calculate the t-score using the information we computed:
    t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=FALSE)

    # margin of error:
    margin_error <- t_score * standard_error
    
    # calculate lower bound and upper bound:
    lower_bound <- mean_value - margin_error
    upper_bound <- mean_value + margin_error

    # check the condition that the original population mean value is inside the range:
    if(original_mean_value >= lower_bound && original_mean_value <= upper_bound) {
        it_was_in_interval <- it_was_in_interval + 1
    }
    
    #print(i)
}

it_was_in_interval

it_was_not_in_interval <- 10000 - it_was_in_interval
it_was_not_in_interval

percentage_in_interval <- it_was_in_interval / 10000 * 100
percentage_in_interval

percentage_not_in_interval <- it_was_not_in_interval / 10000 * 100
percentage_not_in_interval

################################################################################

################################################################################

# question 8 - part (c):

x <- sample(df$child, size=70, replace=FALSE, prob=NULL)

n <- 70
s <- sd(x)
mu0 <- 60
mu <- mean(x)

# approach 1. t-test:

error <- qt(0.975, df=n-1) * s / sqrt(n)

confidence_interval_left <- mu0 - error
confidence_interval_right <- mu0 + error

t_score_left <- (confidence_interval_left - mu) / (s / sqrt(n))
t_score_right <- (confidence_interval_right - mu) / (s / sqrt(n))

p <- pt(t_score_right, df=n-1) - pt(t_score_left, df=n-1)
p

power <- 1 - p
power

# approach 2. z-test:

error <- qnorm(0.975) * s / sqrt(n)

confidence_interval_left <- mu0 - error
confidence_interval_right <- mu0 + error

z_score_left <- (confidence_interval_left - mu) / (s / sqrt(n))
z_score_right <- (confidence_interval_right - mu) / (s / sqrt(n))

p <- pnorm(z_score_right) - pnorm(z_score_left)
p

power <- 1 - p
power

# approach 3. using non-centrality parameter (ncp):

ncp <- (mu - mu0) / (s / sqrt(n))
t <- qt(0.975, df=n-1)

p <- pt(t, df=n-1, ncp=ncp) - pt(-t, df=n-1, ncp=ncp)
p

power <- 1 - (pt(t, df=n-1, ncp=ncp) - pt(-t, df=n-1, ncp=ncp))
power

# approach 4. using "pwr" library:

library("pwr")

diff <- (mu - mu0) / (s / sqrt(n))

pwr.t.test(d=diff, n=n, sig.level=0.05, type="one.sample", alternative="two.sided")

################################################################################

################################################################################

# question 8 - part (d):

x <- sample(df$child, size=10, replace=FALSE, prob=NULL)

n <- 10
s <- sd(x)
mu0 <- 60
mu <- mean(x)

# approach 1. t-test:

error <- qt(0.975, df=n-1) * s / sqrt(n)

confidence_interval_left <- mu0 - error
confidence_interval_right <- mu0 + error

t_score_left <- (confidence_interval_left - mu) / (s / sqrt(n))
t_score_right <- (confidence_interval_right - mu) / (s / sqrt(n))

p <- pt(t_score_right, df=n-1) - pt(t_score_left, df=n-1)
p

power <- 1 - p
power

# approach 2. z-test:

error <- qnorm(0.975) * s / sqrt(n)

confidence_interval_left <- mu0 - error
confidence_interval_right <- mu0 + error

z_score_left <- (confidence_interval_left - mu) / (s / sqrt(n))
z_score_right <- (confidence_interval_right - mu) / (s / sqrt(n))

p <- pnorm(z_score_right) - pnorm(z_score_left)
p

power <- 1 - p
power

# approach 3. using non-centrality parameter (ncp):

ncp <- (mu - mu0) / (s / sqrt(n))
t <- qt(0.975, df=n-1)

p <- pt(t, df=n-1, ncp=ncp) - pt(-t, df=n-1, ncp=ncp)
p

power <- 1 - (pt(t, df=n-1, ncp=ncp) - pt(-t, df=n-1, ncp=ncp))
power

# approach 4. using "pwr" library:

library("pwr")

diff <- (mu - mu0) / (s / sqrt(n))

pwr.t.test(d=diff, n=n, sig.level=0.05, type="one.sample", alternative="two.sided")

################################################################################

################################################################################
################################################################################

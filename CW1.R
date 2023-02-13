# Importing the dataframe file as csv

df = read.csv("filesize.csv", header = TRUE)

# Installing Pareto library

library(Pareto)

# Fetching the column from the dataframe

file_size = df$x

# Calculating the numerical summaries

mean = mean(file_size)
summary = summary(file_size)
standard_deviation = sd(file_size)
variance = var(file_size)

# Printing the summaries

print(summary)
cat("Standard Deviation: ", standard_deviation)
cat("Variance: ", variance)

# Creating histograms

hist(file_size, main = "Histogram of Dataframe", 
     xlab='Sizes', xlim=c(1000,80000), ylim=c(0,1000),breaks = 100)

# MLE

x_m=1000 # Given
alpha_hat = length (file_size) / (sum(log(file_size)) 
                                  - length(file_size)*(log(x_m)))
alpha_hat

# Estimated standard error

ese_alpha = alpha_hat / sqrt(length(file_size))
ese_alpha

# Confindence Interval

z_025 <- qnorm(p = 0.025, lower.tail = FALSE)
alpha_CI_L = alpha_hat - (z_025*ese_alpha)
alpha_CI_U = alpha_hat + (z_025*ese_alpha)

cat("Confidence Interval: [", alpha_CI_L, ",", alpha_CI_U, "] ")

# Simulations

y_prime=0
for(i in 1:10000)
{ y_prime[i]=mean(Pareto::rPareto(length(file_size),x_m,alpha_hat))}
hist(y_prime,main = "Histogram of Average File Size",
     xlab='Average File Sizes in kB',xlim=c(1400,1900),ylim=c(0,700),breaks = 100)

# Calculating the summaries for Y'

mean_avg = mean(y_prime)
summary_avg = summary(y_prime)
standard_deviation_avg = sd(y_prime)
variance_avg = var(y_prime)
print(summary_avg)
cat("Standard Deviation: ", standard_deviation_avg)
cat("Variance: ", variance_avg)

# Calculating the maximal possible limit

max_possible_limit = qPareto(0.99, length(file_size),alpha_hat)
print(max_possible_limit)
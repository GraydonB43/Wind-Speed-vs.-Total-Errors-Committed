### Term Project ###

getwd()
setwd("/Users/graydon/Desktop/Baseball DataSet")
data <- read.csv("baseball_reference_2016_clean.csv")
data
data_orig <- data
data$total_errors<- data$away_team_errors + data$home_team_errors
length(data$wind_speed)
sample_data <-data[sample(nrow(data), 500),]
sample_data$X <- NULL
sample_data$attendance <- NULL
sample_data$away_team <- NULL
sample_data$away_team_errors <- NULL
sample_data$away_team_hits <- NULL
sample_data$away_team_runs <- NULL
sample_data$date <- NULL
sample_data$field_type <- NULL
sample_data$game_type <- NULL
sample_data$home_team <- NULL
sample_data$home_team_errors <- NULL
sample_data$home_team_hits <- NULL
sample_data$home_team_runs <- NULL
sample_data$start_time <- NULL
sample_data$venue <- NULL
sample_data$day_of_week <- NULL
sample_data$temperature <- NULL
sample_data$wind_direction <- NULL
sample_data$sky <- NULL
sample_data$total_runs <- NULL
sample_data$game_hours_dec <- NULL
sample_data$season <- NULL
sample_data$home_team_win <- NULL
sample_data$home_team_loss <- NULL
sample_data$home_team_outcome <- NULL
sample_data
write.csv(sample_data, "C:\\Users\\graydon\\Desktop\\Baseball DataSet\\term project.csv")

hist(sample_data$wind_speed)
hist(sample_data$total_errors)
typeof(sample_data$wind_speed)
max(sample_data$total_errors)
max(sample_data$wind_speed)
length(sample_data$wind_speed)
length(sample_data$total_errors)

hist(sample_data$total_errors[sample_data$wind_speed %in% c(1:10)])
hist(sample_data$total_errors[sample_data$wind_speed %in% c(11:25)])

# Both histograms for total errors at wind speeds of 0-9 mph and at a wind speeds of 10-25 mph are right skewed, so two-sided t-test would be best method.

### Part 1 - Two-Sided T-test Testing Whether single digit wind speeds (0-9 mph) lead to the same mean # of errors committed as double digit wind speeds (10-25 mph) ####

# Step 1: h0: mu1=mu2, meaning that the mean number of errors committed between wind speeds of 0-10 mph is the same as the mean number 
# of errors committed for wind speeds of 11 mph or greater.
# h1: mu1 < mu2, meaning that the mean number of errors committed when the wind speed is 11 mph or greater is greater than the mean number 
# of errors committed when wind speeds are between 0 and 10 mph.
alpha <- 0.05 # Chosen because this is a very commonly used alpha metric

# Step 2: 

# Data gathering:
xbar1 <- mean(sample_data$total_errors[sample_data$wind_speed %in% c(1:10)])
s1 <- sd(sample_data$total_errors[sample_data$wind_speed %in% c(1:10)])

xbar2 <- mean(sample_data$total_errors[sample_data$wind_speed %in% c(11:25)])
s2 <- sd(sample_data$total_errors[sample_data$wind_speed %in% c(11:25)])

n1 <- 500
n2 <- 500
df <- n1-1 # Conservative estimate of df listed in Lecture 4

# The appropriate test statistic to use in this case would be the two-sided t-test because the data for total errors for both wind speed 
# groups is right-skewed (not normally distributed) and because the population sd is unknown even though n is over 30.

# Step 3:
qt(0.95, df = df)

# The appropriate critical value associated with a right hand tail of alpha = 0.05 is 1.645.
# Therefore, we will reject h0 if the p-value is <= alpha or if the t-statistic is >= |1.645|. Otherwise,
# we do not reject h0.

# Step 4: 
# Computing test statistic and associated p-value:
t.test(sample_data$total_errors[sample_data$wind_speed %in% c(1:10)], sample_data$total_errors[sample_data$wind_speed %in% c(11:25)],
       alternative = "less", conf.level = 0.95)
# t-statistic = -0.1
# df = 289.3
# p-value = 0.4602

# Visualizations of effect of wind speed on total number of errors committed:
# Boxplot:
boxplot(sample_data$wind_speed~sample_data$total_errors, xlab="Total Errors Committed", ylab="Wind Speed (in MPH)", main= "Total Errors Committed vs Recorded Wind Speed", col=topo.colors(7))



# Step 5: 
# Based on this information, we would fail to reject h0 because since the t-statistic is not greater than 1.645 and 
# because the p-value is greater than alpha. This means we do not have significant evidence at the alpha = 0.05 level to
# conclude that the number of total errors committed by both teams for games with wind speeds between 0-10 mph is less than 
# the number of total errors committed by teams when wind speeds are between 11-25 mph. The above boxplot detailing the reported 
# wind speeds for each additional error committed by both teams during our 500 game sample supports our conclusion as the mean wind
# speeds for each increase in the number of errors committed between 0 and 5 generally remains consistent throughout. The one exception to 
# this is games where 6 total errors were committed, as the mean wind speed at this level is closer to 17 mph. However, I think this was more a
# result of the limited number of games that occurred where 6 errors were recorded rather than wind speed having a direct effect on these errors
# being committed.




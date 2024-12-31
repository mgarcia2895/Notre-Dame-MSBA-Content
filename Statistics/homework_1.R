##################
### Homework 1 ###
##################

# For this homework, you will be using the 
# level_up_data.csv file in the data folder.
# There are 1000000 observations of 8 variables.
# That is a sizable amount of data. If you find
# your computer really struggling through anything,
# you can draw a random sample of your data:

test_data <- read.csv("C:/Users/miguel.garcia/Downloads/level_up_data.csv")
#sample_rows <- sample(1:nrow(test_data), .25 * nrow(test_data))
#sampled_data <- test_data[sample_rows, ]

## Step 1 -- Data Import ##

# For this step, all you need to do is import your data
# and run some type of summary on your data. You've
# seen summary(), but you should try
# Hmisc::describe() or psych::describe() for some
# expanded summary information.
summary(test_data)
install.packages("psych")
library(Hmisc)
Hmisc::describe(test_data)

## Step 2 -- Identify Variables ##

# Now that you've got a feel for your data, select
# some numeric variables and examine the correlations
# between them. Provide a brief explanation for the 
# correlations you see and what that might mean
# for those relationships. 
cor_matrix <- cor(test_data)
upper_triangle <- upper.tri(cor_matrix)
cor_values <- cor_matrix[upper_triangle]
variable_pairs <- which(upper_triangle, arr.ind = TRUE)
print(cor_matrix)
#




## Step 3 -- Hypotheses ##

# Now that you have a solid understanding of your
# data, generate two distinct hypotheses for
# the currentSalary variable. 

## Step 4 -- Visualize ##

# Now that you have your hypothesis, 
# generate visualizations to depict those relationships.
# Does it seem like your visualizations offer
# support for your hypotheses?

## Step 5 -- Model ##

# Use a linear regression model to test those 
# hypotheses. Once you have your models ran, 
# tell me about the coefficients. Did your
# model support your hypothesis or not?

## Step 6 -- Compile ##

# Once you have your work done, go up to 
# File and then click Compile Report...
# You might be prompted to install a
# few packages and that is okay. This
# will save your output as an html file and
# it will save in the exact same location
# as your R file. Submit the html file!
# Breathe a sigh of relief and remember
# this quote from Bob Ross: 
# If you have light on light, you have nothing. 
# If you have dark on dark, you basically have nothing. 
# Just like in life. You gotta have a little sadness 
# once in a while so you know when the good times come.
# Submitting your homework is a good time!

aggregate(test_data$currentSalary, 
          by = list(test_data$department), 
          mean)

library(ggplot2)

ggplot(test_data, 
       aes(department, currentSalary, group = department)) +
  geom_boxplot()

summary(lm(currentSalary ~ as.factor(department), data = test_data))

sampled_data$department <- as.factor(sampled_data$department)

performance <- lm(currentSalary ~ department * performance, data = sampled_data)

factorTest2 <- lm(currentSalary ~ relevel(test_data$department, 
                                      ref = "2"), 
                  data = test_data)

sjPlot::plot_model(performance, type = "int", 
                   title = "") +
  ggplot2::theme_minimal()

library(effects)

modEffects <- effect("department*performance", performance)

plot(modEffects)

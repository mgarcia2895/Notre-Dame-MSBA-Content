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
View (test_data)
#sample_rows <- sample(1:nrow(test_data), .25 * nrow(test_data))
#sampled_data <- test_data[sample_rows, ]

## Step 1 -- Data Import ##

# For this step, all you need to do is import your data
# and run some type of summary on your data. You've
# seen summary(), but you should try
# Hmisc::describe() or psych::describe() for some
# expanded summary information.
summary(test_data)


str(test_data)



## Step 2 -- Identify Variables ##

# Now that you've got a feel for your data, select
# some numeric variables and examine the correlations
# between them. Provide a brief explanation for the 
# correlations you see and what that might mean
# for those relationships. 

cor(test_data$workDistance,
    test_data$performance,
    use = "pairwise.complete.obs")
###MG - This produced a correlation of 0.0008289411, the correlation b/w 
###MG - workDistance and performance is positive as distance increases,
###MG - the performance increases. However it is a very weak correlation.
###MG - There may not be a clear linear relationship between these variables.

cor(test_data$numberPriorJobs,
    test_data$performance,
    use = "pairwise.complete.obs")
###MG - This produced a correlation of0.001639447, the correlation b/w 
###MG - number of prior jobs and performance is positive as salary increases,
###MG - the months to separate increases. However it is a very weak correlation.
###MG - There may not be a clear linear relationship between these variables.



## Step 3 -- Hypotheses ##

# Now that you have a solid understanding of your
# data, generate two distinct hypotheses for
# the currentSalary variable.


###MG Null Hypothesis:There is no significant relationship between number of 
###MG prior jobs and current salary.

###MG Alternative hypothesis: There is a significant positive/negative 
###MG relationship between number of prior jobs and current salary.


## Step 4 -- Visualize ##

# Now that you have your hypothesis, 
# generate visualizations to depict those relationships.
# Does it seem like your visualizations offer
# support for your hypotheses?

plot(test_data$numberPriorJobs, test_data$currentSalary, 
     xlab = "Number of Prior Jobs", 
     ylab = "Current Salary",
     main = "Scatterplot of Number of Prior Jobs vs Current Salary")

boxplot(currentSalary ~ numberPriorJobs, data = test_data,
        xlab = "Number of Prior Jobs", 
        ylab = "Current Salary",
        main = "Boxplot of Current Salary by Number of Prior Jobs")





## Step 5 -- Model ##

# Use a linear regression model to test those 
# hypotheses. Once you have your models ran, 
# tell me about the coefficients. Did your
# model support your hypothesis or not?

linear_model <- lm(currentSalary ~ numberPriorJobs, data = test_data)

summary(linear_model)

summary(linear_model$residuals)


###MG The coefficient (slope) is 621.907, which indicates the salary value
###MG increases by 621.9 for every number of prior jobs someone has. The t-score
###MG for this model is 3795, which indicates this relationship is significant.
###MG The residuals also indicate a normal distribution. However, a low R^2
###MG value of 0.05359 indicates A low R² indicates that the model does not 
###MG explain much of the variability in the response variable.
###MG The majority of the variability in the dependent variable is not accounted 
###MG for by the linear relationship with the predictors included in the model.


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

test_data$department <- as.factor(test_data$department)

performance <- lm(currentSalary ~ department * performance, data = test_data)

factorTest2 <- lm(currentSalary ~ relevel(test_data$department, 
                                      ref = "2"), 
                  data = test_data)

library(sjPlot)
sjPlot::plot_model(performance, type = "int", 
                   title = "") +
  ggplot2::theme_minimal()

library(effects)

modEffects <- effect("department*performance", performance)

plot(modEffects)

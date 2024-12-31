##################
### Homework 2 ###
##################


## Task 1 ##

# You will be using the teamPerc.RData for this assignment. 
# It is an RData file, so you will use the load function to bring it in:
# load("/Users/sethberry/Downloads/teamPerc.RData")
# Notice that I don't assign it a name, just run it (with your own path) 
# and you'll see an object called "teamPerc" in your environment. 
load("/Users/miguel.garcia/Downloads/teamPerc.RData")
View(teamPerc)



## Task 2 ##

# The first column, "Rater", denotes if a leader or subordinate is
# offering the ratings. You can probably figure out which value
# is the "boss" and which represents the "subordinate". Perform
# a basic model to see if leaders assess effectiveness ("effect") 
# differently than subordinates. Tell me what you found. 
###different effect rating between raters


model <- lm(effect ~ Rater, data = teamPerc)

summary(model)


library(ggplot2)


ggplot(teamPerc, aes(x = Rater, y = effect)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot of EffectRating vs. Rater",
       x = "Rater",
       y = "EffectRating")



###MG Both the Intercept and Rater coefficients have a indication of
###MG high significance. Approximately 2.89% of the variability in the effect
###MG is explained by the model. Assuming Rater 3 are the subordinates and Rater
###MG 0 are the bosses, the subordinates are rating the bosses' effectiveness
###MG greater than the bosses do of their own by the difference of means.


## Task 3 ##

# What do subordinates want out of their leaders? Variables 73 
# through 88 (takesCharge to operational) all deal with characteristics
# of leaders (you can probably imagine what they mean). 
# Use those variables as predictors for effectiveness. 

selected_columns <- names(teamPerc)[73:88]
print(selected_columns)

model <- lm(effect ~ ., data = teamPerc[, c("effect", selected_columns)])
summary(model)

## Task 4 ##

# Pick one of those variables that look "strong" and interact 
# it with either "leader_tenure", "leader_age", "leader_female", 
# or "leader_experience". Plot your results and tell me what you
# think it means. 

model <- lm(leader_experience ~ listens, data = teamPerc)
summary(model)

###MG There is a statistically significant relationship between the listens 
###MG variable and leader experience. However, the low 
###MG R-squared values indicate that the model does not explain much of the 
###MG variability in leader experience, suggesting that other factors not 
###MG included in the model might influence leader experience.I believe people
###MG may naturally trust individuals who are older or have more years of 
###MG service to assess performance, regardless of the actual outcomes.

## Task 5 ##

## Solve this problem

# Maximize: 40_oil + 7.5_flower
# Subject to: 
# requirement = 30_oil + 3_flower <= 8960
# oil, flower >= 0


library(linprog)


##objectiveFunction 

c_vec <- c(40, 7.5)
b_vec <- c(8960)


##constraintValues


constr_type <- c("<=")


##constraintMatrix
A_mat <- matrix(c(30, 3), nrow = 1, byrow = TRUE)

lp_result <- linprog::solveLP(c_vec, 
                              b_vec, 
                              A_mat, 
                              maximum = TRUE,
                              const.dir = constr_type)
print(lp_result)

# What is the projected revenue for this solution and 
###MG 22,400


# how much of each product needs to be made?
###MG The optimal value would be 0 units of oil and 2986.67 units of flower.





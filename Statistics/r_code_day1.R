stats_survey <- read.csv(
  "C:/Users/miguel.garcia/Downloads/msba_stats_survey.csv"
)
str(stats_survey)

summary(stats_survey)

table(stats_survey$favorite_food)

table(stats_survey$r_feeling)

library(stringr)

stats_survey$pizza <- str_extract(
  stats_survey$pizza,
  "[0-9]{1,}"
)


stats_survey$pizza <- 
  as.numeric(stats_survey$pizza)

mean_pizza <- mean(stats_survey$pizza,
                   na.rm = TRUE)

greater_than_meant <- stats_survey

library('ggplot2')
  
  bold_claims <- ggplot(stats_survey, 
           aes(x = pizza, 
               y = vertical_jump,
               color = major)) + 
        geom_point() + geom_smooth (method = "lm") +
        theme_minimal()
  
plotly::ggplotly(bold_claims)


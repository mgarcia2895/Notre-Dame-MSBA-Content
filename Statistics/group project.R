marketing <- read.csv("C:/Users/miguel.garcia/Downloads/customer_ID_marketing.csv")

View(marketing)

marketing$id <- 1:2205

View(marketing)

Total <- marketing$MntWines + marketing$MntFishProducts + marketing$MntFruits + 
  marketing$MntGoldProds + marketing$mnt


library(corrplot)

corr_data <- marketing[, 1:15]
corrs <- cor(corr_data, use = "pairwise.complete.obs")
corrplot::corrplot.mixed(corrs)


library(dplyr)
library(ggplot2)



# Focus columns for relationship b/w demographics and campaign acceptance
selected_columns <- c("Income", "Kidhome", "Teenhome", "Age", "marital_Divorced", 
                      "marital_Married", "marital_Single", "marital_Together", 
                      "marital_Widow", "education_2n.Cycle", "education_Basic", 
                      "education_Graduation", "education_Master", "education_PhD", 
                      "AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5", "AcceptedCmp1", 
                      "AcceptedCmp2")


subset_data <- marketing[selected_columns]

cor_matrix <- cor(subset_data, use = "pairwise.complete.obs")

# Heatmap
heatmap(cor_matrix, main = "Correlation Heatmap", col = colorRampPalette(c("blue", "white", "red"))(100))


glm_model <- glm(AcceptedCmp1 ~ Income + Kidhome + Teenhome + Age + 
                   marital_Divorced + marital_Married + marital_Single + 
                   marital_Together + marital_Widow + education_2n.Cycle + 
                   education_Basic + education_Graduation + education_Master + 
                   education_PhD, 
                 data = marketing, 
                 family = "binomial")
summary(glm_model)

#####INSIGHT - Income appears to be statistically significant, with a positive
#####         coefficient. This suggests that higher income is associated with a 
#####         higher probability of accepting Campaign 1.Teenhome is statistically 
#####         significant with a negative coefficient, suggesting that having 
#####         teenagers at home is associated with a lower probability of acceptance.


cor_matrix <- cor(marketing[, c("AcceptedCmp4", "Income", "Kidhome", "Teenhome", "Age")])
cor_matrix


ggplot(marketing, aes(x = factor(AcceptedCmp4), y = Income)) +
  geom_boxplot() +
  labs(title = "Income vs. Campaign Acceptance", x = "Campaign Acceptance", y = "Income")

#####INSIGHT - Weak correlations, but the acceptance of campaign 4 had a higher mean than the non acceptance. 

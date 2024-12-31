## Stats Project Code ##

library(dplyr)
library(ggplot2)
library(corrplot)
library(tidyverse)

#______________________________________________________________________________________________#

## Patrick

df <- read.csv("C:/Users/patri/Downloads/Customer_ID_marketing.csv")
summary(df)

#For which purchase method are shoppers most receptive to campaigns? Is there a positive correlation between 
#number of campaigns a shopper accepts that differs for different shopping channels?

#condense dataframe into relevant columns to answer question above
condensed_df <- select(df, NumWebPurchases:AcceptedCmp2, AcceptedCmpOverall)

#create correlation matrix
cor_matrix <- cor(condensed_df, use = "complete.obs")

#visualize correlation matrix
corrplot(cor_matrix, method = "circle")
#based on the correlation matrix, purchases who by via catalog appear to be most likely to be receptive
#to campaigns overall (at least, they are the most correlated). There are also positive correlations
#between campaigns accepted and number of web purchases and store purchases, so shoppers who use 
#all mediums are responding to campaigns to some extent. Based on this information, the linear
#regression model will use all shoppers as independent variables. 

#Write file to CSV to evaluate scatterplots in tableau (note - methodology to write to CSV from ChatGPT)
write.csv(condensed_df, "C:/Users/patri/Downloads/Customer_ID_marketing_condensed.csv", row.names = FALSE)

model <- lm(AcceptedCmpOverall ~ NumWebPurchases + NumCatalogPurchases + NumStorePurchases, data = condensed_df)
summary(model)

#based on the linear regression model, there is a statistically significant (by measure of p value and t value)
#relationship between the number of campaigns that a customer responds to and the number of catalog purchases
#they have. This suggests that, all other things being equal, it makes sense for the marketing team to prioritize
#campaigns to the catalog purchasers. 

ggplot(condensed_df, aes(x = NumCatalogPurchases, y = AcceptedCmpOverall)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#based on the graph, there is a clear outlier in this dataset. 

#as learned in Data Management, using DPLYR to eliminate this record from the dataset to evalute 
#whether is has an impact on regression line. 

df_cleaned <- condensed_df %>%
  filter(NumCatalogPurchases != 28)

#re-perform ggplot to ensure the desired outcome of eliminating outlier point was obtained. 

ggplot(df_cleaned, aes(x = NumCatalogPurchases, y = AcceptedCmpOverall)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

model_v2 <- lm(AcceptedCmpOverall ~ NumWebPurchases + NumCatalogPurchases + NumStorePurchases, data = df_cleaned)
summary(model_v2)

#The t value and p value are convincing in establishing the statistically significant relationship
#between the number of catalog purchases and the number of campaigns a customer will respond to. 

#However, despite the statistical significance of this finding, the coefficient associated with the number 
#of catalog purchases is very small. Additionally, as presented in the ggplot graphs, a linear model does not
#appear to be a commonsense fit to describe the relationship in campaign responses and number of catalog purchases - 
#the graphs make even less sense for the other purchase types. So, my takeaway and recommendation is that 
#there should be other metrics that determine the marketing budget rather than the method a customer uses to 
#purchase (store, catalog, web). 

#How does purchase channel impact receptiveness to individual campaigns?


#create linear regression models for each individual campaign:
cmp1_model <- lm(AcceptedCmp1 ~ NumWebPurchases + NumCatalogPurchases + NumStorePurchases, data = df)
cmp2_model <- lm(AcceptedCmp2 ~ NumWebPurchases + NumCatalogPurchases + NumStorePurchases, data = df)
cmp3_model <- lm(AcceptedCmp3 ~ NumWebPurchases + NumCatalogPurchases + NumStorePurchases, data = df)
cmp4_model <- lm(AcceptedCmp4 ~ NumWebPurchases + NumCatalogPurchases + NumStorePurchases, data = df)
cmp5_model <- lm(AcceptedCmp5 ~ NumWebPurchases + NumCatalogPurchases + NumStorePurchases, data = df)

#view summaries of each campaign:
summary(cmp1_model)
summary(cmp2_model)
summary(cmp3_model)
summary(cmp4_model)
summary(cmp5_model)

#evaluating this question on a campaign by campaign basis, the number of Catalog purchases was 
#the most commonly statistically significant predictive variable using t scores and p values. 
#Campaigns 3 and 4, however, seemed to also trigger a statistically significant relationship
#between the number of web and store purchase variables. That said, the coefficients are so 
#small that they are almost uninterpretable, specifically given the binary nature of the dependent
#variables. Again, my recommendation would be to focus consideration elsewhere to continue
#to optimize the store's marketing strategy, rather than to give too much concern to the channel
#via which shoppers purchased. 

#______________________________________________________________________________________________#

## Patty Kingery

## PROJECT HOMEWORK #####


project_data <- read.csv("~/Downloads/marketing.csv")

summary(project_data)

str(project_data)


names(project_data)

########

# campaign 1:
# I did a regression on campaign 1.  The only predictor of significance 
# is Age (younger) for those who would accept Campaign 1.  

accepted_campaign_1 <- project_data[
  project_data$AcceptedCmp1 == 1, c(1:36)]

summary(accepted_campaign_1)

accepted_model_1 <- lm(AcceptedCmp1 ~ ., data = accepted_campaign_1)


summary(accepted_model_1)


# campaign 1:
# I did a regression on campaign 1 for those who did not accept campaign 1
# There was nothing of significance to predict who would not accept

non_accepted_campaign_1 <- project_data[
  project_data$AcceptedCmp1 == 0, c(1:36)]

summary(non_accepted_campaign_1)

non_accepted_model_1 <- lm(AcceptedCmp1 ~ ., data = non_accepted_campaign_1)


summary(non_accepted_model_1)


####

# campaign 2:
# I did a regression on campaign 2 and got nearly all demographics were 
# predictors for accepting Campaign 2
# I got a warning that the summary is essentially a perfect fit -- need to 
# investigate further


names(project_data)

accepted_campaign_2 <- project_data[
  project_data$AcceptedCmp2 == 1, c(1:36)]

summary(accepted_campaign_2)

accepted_model_2 <- lm(AcceptedCmp2 ~ ., data = accepted_campaign_2)


summary(accepted_model_2)


### Only 30 people accepted campaign 2 vs over 100 in each of the other campaigns

# campaign 2:
# I did a regression on campaign 2 for those who did not accept
# and got nearly all demographics were 
# predictors for not accepting Campaign 2
# I got a warning that the summary is essentially a perfect fit -- need to 
# investigate further

#### WHAT IS GOING ON WITH CAMPAIGN 2? #########. 
# Campaign 2 has lowest Mean of only 1.36% acceptance as compared 
# 6.4% for #1, 7.3% for #3, 7.4% for #4, and 7.3% for campaign 5


names(project_data)

non_accepted_campaign_2 <- project_data[
  project_data$AcceptedCmp2 == 0, c(1:36)]

summary(non_accepted_campaign_2)

non_accepted_model_2 <- lm(AcceptedCmp2 ~ ., data = non_accepted_campaign_2)


summary(non_accepted_model_2)


####

# campaign 3:
# I did a regression on campaign 3 and it looks like number of web visits
# per month is a very strong predictor of who will accept campaign 3.  Number of
# Customer days has a strong negative predictor on who will accept campaign 3.
# Those who bought more meat products, fewer number of deals purchased, fewer 
# number of web purchases, and accepted campaign 5 were all somewhat strong
# predictors of who would accept campaign 3

names(project_data)

accepted_campaign_3 <- project_data[
  project_data$AcceptedCmp3 == 1, c(1:36)]

summary(accepted_campaign_3)

accepted_model_3 <- lm(AcceptedCmp3 ~ ., data = accepted_campaign_3)


summary(accepted_model_3)


####

# campaign 4:
# I did a regression on campaign 4 and it looks like there are no 
# predictors for ncampaign 4 - we may need to investigate further

names(project_data)

accepted_campaign_4 <- project_data[
  project_data$AcceptedCmp4 == 1, c(1:36)]

summary(accepted_campaign_4)

accepted_model_4 <- lm(AcceptedCmp4 ~ ., data = accepted_campaign_4)


summary(accepted_model_4)


####

# campaign 5:
# I did a regression on campaign 5 and it looks like age (younger) is a 
# somewhat strong predictors for who accepted campaign 5

names(project_data)

accepted_campaign_5 <- project_data[
  project_data$AcceptedCmp5 == 1, c(1:36)]

summary(accepted_campaign_5)

accepted_model_5 <- lm(AcceptedCmp5 ~ ., data = accepted_campaign_5)


summary(accepted_model_5)


# Lets see if any of the campaigns predicted products purchased


names(project_data)

summary(project_data)

### Campaigns 1, 4, and 5 have were strong predictors for purchase of 
# wine with campaign 5 having the great effect on wine amounts purchased

MntWines_campaigns <- lm(MntWines ~ AcceptedCmp1 +
                           AcceptedCmp2 + AcceptedCmp3 +
                           AcceptedCmp4 + AcceptedCmp5,
                         data = project_data)

summary(MntWines_campaigns)

##

### Campaigns 1, and 5 were strong predictors for purchase of 
# fruits.  Campaign 4 actually had somewhat significant negative
# predictor on purchasing Fruit (maybe specials on veggies)  and 
# campaign 2 had a little significance on reduction of purchase of fruit

MntFruits_campaigns <- lm(MntFruits ~ AcceptedCmp1 +
                            AcceptedCmp2 + AcceptedCmp3 +
                            AcceptedCmp4 + AcceptedCmp5,
                          data = project_data)

summary(MntFruits_campaigns)



### Campaigns 1 and 5 were very significant predictors on purchase of 
# meats, with campaign 5 having the strongest impact.  Campaigns 2 and 4
# had a little significance on negative impact on purchasing meats.

MntMeatProducts_campaigns <- lm(MntMeatProducts ~ AcceptedCmp1 +
                                  AcceptedCmp2 + AcceptedCmp3 +
                                  AcceptedCmp4 + AcceptedCmp5,
                                data = project_data)

summary(MntMeatProducts_campaigns)


### Campaigns 1 and 5 were very significant predictors on purchase of 
# fish, with campaign 1 having the strongest impact.  Campaign 4
# had a significant negative impact on purchase of fish and 
# campaign 2 had a somewhat significant negative impact on 
# purchasing fish

MntFishProducts_campaigns <- lm(MntFishProducts ~ AcceptedCmp1 +
                                  AcceptedCmp2 + AcceptedCmp3 +
                                  AcceptedCmp4 + AcceptedCmp5,
                                data = project_data)

summary(MntFishProducts_campaigns)



### Campaigns 1, 4 and 5 were very significant predictors on purchase of 
# sweets, with campaign 5 having the strongest impact.  Campaign 4 had strong
# significant negative impact on purchase of sweets and 
# campaign 2 had a somewhat significant negative impact on 
# purchasing sweets

MntSweetProducts_campaigns <- lm(MntSweetProducts ~ AcceptedCmp1 +
                                   AcceptedCmp2 + AcceptedCmp3 +
                                   AcceptedCmp4 + AcceptedCmp5,
                                 data = project_data)

summary(MntSweetProducts_campaigns)



### Campaigns 1, 3 and 5 were very significant predictors on purchase of 
# gold products, with campaign 5 having the strongest impact. 

MntGoldProds_campaigns <- lm(MntGoldProds ~ AcceptedCmp1 +
                               AcceptedCmp2 + AcceptedCmp3 +
                               AcceptedCmp4 + AcceptedCmp5,
                             data = project_data)

summary(MntGoldProds_campaigns)


names(project_data)

summary(project_data)


#Lets look at kinds of products sell over the various channels
#

####
# Lets see if any campaigns explains where someone purchases
# 
# Web Purchases: Campaigns 1 and 4 were very significant to predicting Web Purchases

NumWebPurchases_campaigns <- lm(NumWebPurchases ~ AcceptedCmp1 +
                                  AcceptedCmp2 + AcceptedCmp3 +
                                  AcceptedCmp4 + AcceptedCmp5,
                                data = project_data)

summary(NumWebPurchases_campaigns)



## Looks like Income is strongest predictor on web purchases with Recency and 
# Teen at home strong predictors as well.  It looks like Buying gold products is a 
# strong negative predictor for purchasing on web

products_WebPurchases <- project_data[
  project_data$NumWebPurchases, c(1:36)]

summary(products_WebPurchases)

WebPurchases_model_Products <- lm(NumWebPurchases ~ ., data = products_WebPurchases)


summary(WebPurchases_model_Products)


# Catalog Purchases: campaigns 1, 3, and 5 appear to be predictors for catalog
# purchases


NumCatalogPurchases_campaigns <- lm(NumCatalogPurchases ~ AcceptedCmp1 +
                                      AcceptedCmp2 + AcceptedCmp3 +
                                      AcceptedCmp4 + AcceptedCmp5,
                                    data = project_data)

summary(NumCatalogPurchases_campaigns)

## What are the customer buying through catalog

products_CatalogPurchases <- project_data[
  project_data$NumWCatalogPurchases, c(1:36)]

summary(products_CatalogPurchases)

# There are a NAs throughout -- looks like products or demographics are
# not predictors for who will purchase through the catalog 


# Store Purchases: All campaigns but #2 appear to be predictors for 
# store purchases


NumStorePurchases_campaigns <- lm(NumStorePurchases ~ AcceptedCmp1 +
                                    AcceptedCmp2 + AcceptedCmp3 +
                                    AcceptedCmp4 + AcceptedCmp5,
                                  data = project_data)

summary(NumStorePurchases_campaigns)

## what are customers purchasing through the store -- it looks like buying 
## sweets and fish are strong predictors on who will buy 
## through the store.  Kids or Teen home indicates negative 
## predictor on purchasing at store

products_StorePurchases <- project_data[
  project_data$NumStorePurchases, c(1:36)]

summary(products_StorePurchases)

StorePurchases_model_Products <- lm(NumStorePurchases ~ ., data = products_StorePurchases)


summary(StorePurchases_model_Products)

#______________________________________________________________________________________________#

## Bryce

#Loading in Data set
Project_Data <- read.csv("C:/Users/Bryce/Documents/Stats File/Project/marketing.csv")

#Understanding the structure of the data
summary(Project_Data)
#View(Project_Data)
str(Project_Data)

#Creating a correlation table to see on a larger scale what data has a relationship
corr_data <- Project_Data[, 1:20]
corrs <- cor(corr_data, use = "pairwise.complete.obs")
corrplot::corrplot.mixed(corrs)

#Marketing Campaign success

#Campaign 1 had a 6.88% success rate
Project_Data%>%
  count(AcceptedCmp1)

#Campaign 2 had a 1.38% success rate
Project_Data%>%
  count(AcceptedCmp2)

#Campaign 3 had a 7.98% success rate
Project_Data%>%
  count(AcceptedCmp3)

#Campaign 4 had a 8.04% success rate
Project_Data%>%
  count(AcceptedCmp4)

#Campaign 5 had a 7.88% success rate
Project_Data%>%
  count(AcceptedCmp5)

summary(Project_Data)


cor(Project_Data$MntTotal, Project_Data$AcceptedCmp4, use = "pairwise.complete.obs")


#Marketing Campaign 5 acceptance rate vs Income level
Project_Data%>%
  ggplot()+geom_point(mapping = aes(x=Income, y=MntTotal,color=AcceptedCmp5))+
  theme_minimal()


#Customers with Children
Project_Data%>%
  ggplot()+geom_jitter(mapping = aes(x=Kidhome, y=MntTotal, color=AcceptedCmp5))+
  theme_minimal()

#Customers with Teens
Project_Data%>%
  ggplot()+geom_jitter(mapping = aes(x=Teenhome, y=MntTotal, color=AcceptedCmp5))+
  theme_minimal()


#Discounted Purchases
Project_Data%>%
  ggplot()+geom_jitter(mapping = aes(x=Income, y=MntTotal, color=NumDealsPurchases))+
  theme_minimal()


#______________________________________________________________________________________________#

## Andrea

marketing_data <- read.csv("C:/Users/Gugu/Documents/Notre Dame/Courses/Statistics for Managerial Statistics 1/Homework/Project/Customer_ID_marketing.csv")

names(marketing_data)
#View(marketing_data)
str(marketing_data)

##CmP - # of campaigns accepted they are doing
##Mnt - ammount


corr_data <- marketing_data[,1:15]

corrs <- cor(corr_data, use = "pairwise.complete.obs")

corrplot::corrplot.mixed(corrs)

## Corr
##income & MntWines: 0.73
##income & MeatProducts: 0.70
##income & catalogPurchases: 0.71
##MntWines & CatalogPurchases: 0.67
##MeatProducts & CatalogPurchases: 0.71
##income & NumStorePurchases: 0.69

##how does each marketing campaign affect revenue? What type of customers to sell to?
##corr of each campaign accepted by each of the demographics?
## row 11-15: which purchase types are impacted by campaigns and revenue?
##how does campaign acceptance affect revenue?
##My question - which product is selling regardless of the marketing campaign? By Sunday.
##look at where campaign


#% sales of products


marketing_NoCampaign <- marketing_data %>% 
  filter(AcceptedCmpOverall == 0)
##1747 rows

marketing_wCampaign <- marketing_data %>%
  filter(AcceptedCmpOverall != 0)
##458 rows

marketing_data$AcceptedCheck <- (marketing_data$AcceptedCmpOverall > 0) 

##check if there is a difference in the average amount of products sold between those who accepted a campaign and those who never have.
#Fish
t.test(marketing_data$MntFishProducts ~ marketing_data$AcceptedCheck)

#Wine
t.test(marketing_data$MntWines ~ marketing_data$AcceptedCheck)

#Fruit
t.test(marketing_data$MntFruits ~ marketing_data$AcceptedCheck)

#Meat
t.test(marketing_data$MntMeatProducts ~ marketing_data$AcceptedCheck)

#Sweets
t.test(marketing_data$MntSweetProducts ~ marketing_data$AcceptedCheck)

#Gold -- what are gold products?
t.test(marketing_data$MntGoldProds ~ marketing_data$AcceptedCheck)

#Regular 
t.test(marketing_data$MntRegularProds ~ marketing_data$AcceptedCheck)

##all products are different comparing overall accepted vs not accepted.

##just curious about income differences between those accepting a campaign vs not accepting.
t.test(marketing_data$Income ~ marketing_data$AcceptedCheck)

#Fish
t.test(marketing_data$MntFishProducts ~ marketing_data$AcceptedCmp1)
t.test(marketing_data$MntFishProducts ~ marketing_data$AcceptedCmp2) #not significant
t.test(marketing_data$MntFishProducts ~ marketing_data$AcceptedCmp3) #not significant
t.test(marketing_data$MntFishProducts ~ marketing_data$AcceptedCmp4) #not significant
t.test(marketing_data$MntFishProducts ~ marketing_data$AcceptedCmp5)

#Wine
t.test(marketing_data$MntWines ~ marketing_data$AcceptedCmp1)
t.test(marketing_data$MntWines ~ marketing_data$AcceptedCmp2)
t.test(marketing_data$MntWines ~ marketing_data$AcceptedCmp3)
t.test(marketing_data$MntWines ~ marketing_data$AcceptedCmp4)
t.test(marketing_data$MntWines ~ marketing_data$AcceptedCmp5)

#Fruit
t.test(marketing_data$MntFruits ~ marketing_data$AcceptedCmp1)
t.test(marketing_data$MntFruits ~ marketing_data$AcceptedCmp2) #not significant
t.test(marketing_data$MntFruits ~ marketing_data$AcceptedCmp3) #not significant
t.test(marketing_data$MntFruits ~ marketing_data$AcceptedCmp4) #not significant
t.test(marketing_data$MntFruits ~ marketing_data$AcceptedCmp5)

#Meat
t.test(marketing_data$MntMeatProducts ~ marketing_data$AcceptedCmp1)
t.test(marketing_data$MntMeatProducts ~ marketing_data$AcceptedCmp2) #not significant
t.test(marketing_data$MntMeatProducts ~ marketing_data$AcceptedCmp3) #not significant
t.test(marketing_data$MntMeatProducts ~ marketing_data$AcceptedCmp4)
t.test(marketing_data$MntMeatProducts ~ marketing_data$AcceptedCmp5)

#Sweets
t.test(marketing_data$MntSweetProducts ~ marketing_data$AcceptedCmp1)
t.test(marketing_data$MntSweetProducts ~ marketing_data$AcceptedCmp2) #not significant
t.test(marketing_data$MntSweetProducts ~ marketing_data$AcceptedCmp3) #not significant
t.test(marketing_data$MntSweetProducts ~ marketing_data$AcceptedCmp4) #not significant
t.test(marketing_data$MntSweetProducts ~ marketing_data$AcceptedCmp5)


#Gold -- what are gold products?
t.test(marketing_data$MntGoldProds ~ marketing_data$AcceptedCmp1)
t.test(marketing_data$MntGoldProds ~ marketing_data$AcceptedCmp2)
t.test(marketing_data$MntGoldProds ~ marketing_data$AcceptedCmp3)
t.test(marketing_data$MntGoldProds ~ marketing_data$AcceptedCmp4) #not significant
t.test(marketing_data$MntGoldProds ~ marketing_data$AcceptedCmp5)


#Regular 
t.test(marketing_data$MntRegularProds ~ marketing_data$AcceptedCmp1)
t.test(marketing_data$MntRegularProds ~ marketing_data$AcceptedCmp2)
t.test(marketing_data$MntRegularProds ~ marketing_data$AcceptedCmp3) #not significant
t.test(marketing_data$MntRegularProds ~ marketing_data$AcceptedCmp4)
t.test(marketing_data$MntRegularProds ~ marketing_data$AcceptedCmp5)


marketing_data %>%
  group_by(AcceptedCheck) %>%
  summarize(mean(MntFishProducts))

54.83624-33.27876
(27.97101+15.14395)/2

#most of my work was done in tableau

#______________________________________________________________________________________________#

## Miguel
marketing <- read.csv("C:/Users/miguel.garcia/Downloads/customer_ID_marketing.csv")

#View(marketing)

marketing$id <- 1:2205

#View(marketing)

Total <- marketing$MntWines + marketing$MntFishProducts + marketing$MntFruits + 
  marketing$MntGoldProds + marketing$mnt


corr_data <- marketing[, 1:15]
corrs <- cor(corr_data, use = "pairwise.complete.obs")
corrplot::corrplot.mixed(corrs)



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

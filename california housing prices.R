# import dataset
house.df = read.csv('D:/housing.csv')

# check details
str(house.df)
summary(house.df)
dim(house.df)

# preprocessing-----------------
# check missing values and remove rows containing any NAs
dim(house.df[!complete.cases(house.df),])

house.df = house.df[complete.cases(house.df),]
dim(house.df)

# relationships between pairs
cor(house.df[c(1:9)])

# feature engineering-----------
avg_rooms = house.df$total_rooms/house.df$households
avg_bedrooms = house.df$total_bedrooms/house.df$households
persons_per_house = house.df$population/house.df$households
house.df = cbind(house.df, avg_rooms, avg_bedrooms, persons_per_house)

house.df = house.df[c(1:8, 11:13, 9,10)] # rearrange columns
names(house.df)

# data visualization------------
options(scipen=999)
require(ggplot2)
qplot(longitude, latitude, data=house.df,
      color=median_house_value)+scale_color_gradient(low="blue", high="red")
qplot(median_income, median_house_value, data=house.df,
      geom = c("point", "smooth"))
qplot(ocean_proximity, median_house_value, data=house.df, geom = "boxplot", alpha = I(0.1))
qplot(house.df$median_house_value, geom = "histogram")

library(GGally)
ggpairs(house.df[c(1:8,12,13)])

library(corrplot)
corrplot.mixed(corr=cor(house.df[1:12], use="complete.obs"),
               upper="ellipse", tl.pos="lt")

# in sample explanation---------
house.m1 = lm(median_house_value ~ longitude + latitude + housing_median_age + 
              total_rooms + total_bedrooms + population + households +
              median_income + ocean_proximity, data = house.df)
summary(house.m1)

house.m2 = lm(median_house_value ~ longitude + latitude + housing_median_age + 
              median_income + avg_rooms + avg_bedrooms +¡@persons_per_house + 
              ocean_proximity, data = house.df)
summary(house.m2)

# validation--------------------
library(DAAG)
cv.lm(data=house.df, house.m1, m=5)
cv.lm(data=house.df, house.m2, m=5)

# prediction--------------------
set.seed(42)
train.index = sample(c(1:20433), 16000, replace=FALSE)  
house.train = house.df[train.index,]
house.test = house.df[-train.index,]
names(house.train)

predict.house = lm(median_house_value ~ longitude + latitude + housing_median_age + 
                  total_rooms + total_bedrooms + population + households + 
                  median_income + ocean_proximity, data = house.train)
summary(predict.house)

par(mfrow=c(2,2)) 
plot(predict.house)
coef(predict.house)

# confidence interval
confint(predict.house)
library(jtools)
plot_summs(predict.house, scale=TRUE)

library(forecast)
test.pred = predict(predict.house, house.test)
accuracy(test.pred, house.test$median_house_value)

# residuals
res.index = sample(c(1:4433), 20, replace=FALSE)
test.residuals = house.test$median_house_value[res.index] - test.pred[res.index] 
data.frame("Actual" = house.test$median_house_value[res.index],
           "Predicted" = test.pred[res.index], 
           "Residual" = test.residuals)

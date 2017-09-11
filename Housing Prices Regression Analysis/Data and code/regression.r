library(readxl)
library(car)
library(SparseM)

#setting working directory to desired location
setwd("F:/patee/spring 2017/527 - data analytics/week15/house/")

#read the data
data <- read_excel("kc_house_data.xlsx")
View(data)

# subset the data
data1 <- subset(data,select = c("price","sqft_living","sqft_lot","sqft_above","sqft_basement","yr_built"))
View(data1)

# find the correlation and summarize it
cor(data1)
c <- cor(data1)
summary(c)

#modelling the dataset
options(scipen = 999)
model1 <- lm(price~sqft_living,data = data1)
model2 <- lm(price~yr_built,data = data1)
model3 <- lm(price~sqft_living+sqft_basement+sqft_lot+yr_built, data = data1)
summary(model1)
summary(model2)
summary(model3)
#predict(model)

#plot the graphs
plot(data1, col = "orange")
p1 <- plot(sqft_living~sqft_lot, data =  data1, col = "purple",ylab = "Price ($)", xlab = "Living Area (sqft)", main = "Plot graph of price v/s living area")
abline(lm(price~sqft_living,data = data1))
lines(lowess(price,sqft_living), col = "blue")
p2 <- plot(price~sqft_basement, data = data1, col = "cyan", ylab = "Price ($)", xlab = "Basement Area (sqft)", main = "Plot Graph of price v/s basement area")
abline(lm(price~sqft_basement,data = data1))
p3 <- plot(price~sqft_living+sqft_basement+sqft_lot+yr_built, data = data1, col = "purple")
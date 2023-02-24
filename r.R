library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(corrgram) # Correlograms http://www.datavis.ca/papers/corrgram.pdf
library(car) #required for nearest neighbors
library(FNN) # nearest neighbors techniques
library(pROC) # to make ROC curve

wine_data <- read.csv("C:/Users/NP/Desktop/linear regression/redwine.csv", sep=";",header=T)
wine_data
head(wine_data)
summary(wine_data)
summary(wine_data$quality)
summary(wine_data$alcohol)

table(wine_data$quality)



#1

#sqrtal = sqrt(wine_data$alcohol)
linear_quality = lm(quality ~ alcohol  , data=wine_data)    # 1 linear model   ÇáÝ
summary(linear_quality) # 1 linear model    ÇáÝ
#È
plot(wine_data$alcohol,wine_data$quality)
#plot(c(wine_data$alcohol,c(0)),c(wine_data$quality,c(0)))
abline(linear_quality, col = "red")

#Ì

summary(linear_quality)











#2
#ÇáÝ      ÚÑÖ ÇÒ ãÈÏÇ ÑÏ äãíÔæÏ 
summary(linear_quality)

#È

linear_quality_om = lm(quality ~ alcohol-1  , data=wine_data)    # ÚÈæÑí ÇÒ ãÑ˜Ò  ÇáÝ
summary(linear_quality_om) # ÈÑÇÒÔ     ÇáÝ

anova(linear_quality_om)

#

plot(c(wine_data$alcohol,c(0)),c(wine_data$quality,c(0)),pch=20)
abline(linear_quality_om, col = "red")
abline(linear_quality, col = 3)





#3
wine_data_3 = wine_data
wine_data_3$alcohol = wine_data_3$alcohol-10.42

linear_quality_3 = lm(quality ~ alcohol , data=wine_data_3)    # 1 linear model   ÇáÝ
summary(linear_quality_3) # 1 linear model    ÇáÝ

plot(wine_data_3$alcohol,wine_data_3$quality)
plot(c(wine_data_3$alcohol,c(0,0)),c(wine_data_3$quality,c(0,5.634946 )))
abline(linear_quality_3, col = "red")

predict(linear_quality_3 ,newdata = list(alcohol = 0))





#6
linear_quality = lm(quality ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=wine_data)
summary(linear_quality)
anova(linear_quality)

#6 È
ssy = sum((wine_data$quality - mean(wine_data$quality))^2);"ssy";ssy
sse=anova(linear_quality)$"Sum Sq"[12];"sse";sse
ssr = ssy - sse;"ssr";ssr
#sum(anova(linear_quality)$"Sum Sq"[-12])

#6 
anova(linear_quality)

#6 t

linear_qualityt = lm(quality ~ citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=wine_data)
summary(linear_qualityt)
anova(linear_qualityt)
ssy = sum((wine_data$quality - mean(wine_data$quality))^2);"ssyt";ssy
sse=anova(linear_qualityt)$"Sum Sq"[10];"sset";sse
ssr = ssy - sse;"ssrt";ssr


#6 j

linear_quality = lm(quality ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=wine_data)

pred = predict(linear_quality,newdata=list(fixed.acidity= 1,volatile.acidity = 1,citric.acid = 1,residual.sugar = 1,chlorides = 1,free.sulfur.dioxide = 1,total.sulfur.dioxide = 1,density = 1,pH = 1,sulphates = 1,alcohol = 1),interval="prediction",level = 0.95);
pred






#7

linear_quality = lm(quality ~. , data=wine_data) 
#summary(linear_quality)
anova(linear_quality)
ssy = sum((wine_data$quality - mean(wine_data$quality))^2);"ssyt";ssy
sse=anova(linear_quality)$"Sum Sq"[12];"sset";sse
ssr = ssy - sse;"ssrt";ssr

linear_quality = lm(quality ~.-citric.acid-residual.sugar   , data=wine_data) 
#summary(linear_quality)
anova(linear_quality)
ssy = sum((wine_data$quality - mean(wine_data$quality))^2);"ssyt";ssy
sse=anova(linear_quality)$"Sum Sq"[10];"sset";sse
ssr = ssy - sse;"ssrt";ssr

anova(linear_quality)







corrgram(wine_data, lower.panel=panel.shade, upper.panel=panel.ellipse)

linear_quality_1 = lm(quality ~ alcohol, data = wine_data)
summary(linear_quality_1)
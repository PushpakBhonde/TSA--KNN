
################## Time series forecasting using tsfknn package ###################

install.packages("tsfknn")
library(tsfknn)

#One step ahead forecasting

timeS <- window(nottem, end = c(1930, 12))
pred = knn_forecasting(timeS, h = 1, lags = 1:12, k = 2,transform = "none")
head(knn_examples(pred))
nearest_neighbors(pred)
pred$prediction
ts.plot(nottem)
data("nottem")
attach(df)
df=data.frame(nottem)

       
  #### Verification ####
x1920=window(nottem, start=c(1920,1) ,end = c(1920, 12))
x1921=window(nottem, start=c(1921,1) ,end = c(1921, 12))
x1922=window(nottem, start=c(1922,1) ,end = c(1922, 12))
x1923=window(nottem, start=c(1923,1) ,end = c(1923, 12))
x1924=window(nottem, start=c(1924,1) ,end = c(1924, 12))
x1925=window(nottem, start=c(1925,1) ,end = c(1925, 12))
x1926=window(nottem, start=c(1926,1) ,end = c(1926, 12))
x1927=window(nottem, start=c(1927,1) ,end = c(1927, 12))
x1928=window(nottem, start=c(1928,1) ,end = c(1928, 12))
x1929=window(nottem, start=c(1929,1) ,end = c(1929, 12))
x1930=window(nottem, start=c(1930,1) ,end = c(1930, 12))
dist1={} 
dist2={} 
dist3={} 
dist4={} 
dist5={} 
dist6={} 
dist7={} 
dist8={} 
dist9={}
dist10={}
for( i in 1:12)
{
  dist1[i]=(x1920[i]-x1930[i])^2
  dist2[i]=(x1921[i]-x1930[i])^2
  dist3[i]=(x1922[i]-x1930[i])^2
  dist4[i]=(x1923[i]-x1930[i])^2
  dist5[i]=(x1924[i]-x1930[i])^2
  dist6[i]=(x1925[i]-x1930[i])^2
  dist7[i]=(x1926[i]-x1930[i])^2
  dist8[i]=(x1927[i]-x1930[i])^2
  dist9[i]=(x1928[i]-x1930[i])^2
  dist10[i]=(x1929[i]-x1930[i])^2
}
sqrt(sum(dist1))
sqrt(sum(dist2))
sqrt(sum(dist3))
sqrt(sum(dist4))
sqrt(sum(dist5))
sqrt(sum(dist6))
sqrt(sum(dist7))
sqrt(sum(dist8))
sqrt(sum(dist9))
sqrt(sum(dist10))

plot(pred)
library(ggplot2)
autoplot(pred, highlight = "neighbors", faceting = FALSE)

#Multi-step ahead strategies

#The MIMO strategy

timeS <- window(UKDriverDeaths, end = c(1979, 12))
pred1 <- knn_forecasting(timeS, h = 12, lags = 1:12, k = 2, msas = "MIMO")
pred1$prediction
nearest_neighbors(pred1)
autoplot(pred1, highlight = "neighbors", faceting = FALSE)

#The recursive strategy

pred2 <- knn_forecasting(timeS, h = 2, lags = 1:12, k = 2, msas = "recursive")
pred2$prediction
nearest_neighbors(pred2)
autoplot(pred2, highlight = "neighbors")

#Comparison between MIMO and recursive
nearest_neighbors(pred1)
timeS
nearest_neighbors(pred2)

#models with different k parameters

pred <- knn_forecasting(ldeaths, h = 12, lags = 1:12, k = c(2,4))
pred$prediction

pred1 <- knn_forecasting(ldeaths, h = 12, lags = 1:12, k = 2)
k_2 = pred1$prediction
k_2

pred2 <- knn_forecasting(ldeaths, h = 12, lags = 1:12, k = 4)
k_4 = pred2$prediction
k_4

k_2_4={}
for( i in 1:12){
  k_2_4[i]=mean(c(k_2[i],k_4[i]))
}
k_2_4

#Different combination functions

times = window(nottem, end = c(1930, 12)) #a subset of values from the data-set
pred1 = knn_forecasting(times, h = 1, lags = 1:12, k = 5, cf = "mean", transform = "none")
pred2 = knn_forecasting(times, h = 1, lags = 1:12, k = 5, cf="median", transform = "none")
pred3 = knn_forecasting(times, h = 1, lags = 1:12, k = 5, cf="weighted",transform = "none")

#Default parameters
nottem1=knn_forecasting(nottem,h=12)
nottem1

   #### Default k values ####
pred_3 <- knn_forecasting(nottem, h = 12, lags = 1:12, k = 3)
knn_3 = pred_3$prediction

pred_5 <- knn_forecasting(nottem, h = 12, lags = 1:12, k = 5)
knn_5 = pred_5$prediction

pred_7 <- knn_forecasting(nottem, h = 12, lags = 1:12, k = 7)
knn_7 = pred_7$prediction

k_3_5_7={}
for( i in 1:12){
  k_3_5_7[i]=mean(c(knn_3[i],knn_5[i],knn_7[i]))
}
k_3_5_7
nottem1$prediction

  #### Default lagged values ####
pred_qt <- knn_forecasting(UKgas, h = 12) #quarterly gas consumption of UK
pred_qt

#Evaluating forecast accuracy of tsfknn

     #when rolling is false
pred <- knn_forecasting(ldeaths, h = 12, lags = 1:12, k = 2, transform ="none")
ro <- rolling_origin(pred, h = 6, rolling = FALSE)
ro$global_accu
plot(ro)

     #when rolling is true
pred <- knn_forecasting(ldeaths, h = 12, lags = 1:12, k = 2, transform = "none")
ro <- rolling_origin(pred, h = 6)
print(ro$test_sets)
ro$global_accu


#Comparing with other time series forecasting packages
#Verifying Accuracy and Time   

#tsfknn
a=Sys.time()
timeS <- window(nottem, end = c(1930, 12))
pred <- knn_forecasting(timeS, h = 24, lags = 1:12, k = 2)
ro <- rolling_origin(pred,rolling = FALSE)
pred$prediction
ro$global_accu
b=Sys.time()
b-a

  ### verifying mape of tsfknn ###
x <- window(nottem, start=c(1929,1), end = c(1930, 12))
f=ro$predictions
mape={}
for( i in 1:length(f))
{
  mape[i]=(abs((x[i]-f[i])/x[i]))
}
mean(mape)*100

#ETS

library(forecast)

c=Sys.time()
timeS <- window(nottem, end = c(1930, 12))
fit = ets(timeS)
forecast(fit)
accuracy(fit)
d=Sys.time()
d-c

#Auto.arima

c=Sys.time()
timeS <- window(nottem, end = c(1930, 12))
fit=auto.arima(timeS)
plot(forecast(fit))
accuracy(fit)
d=Sys.time()
d-c

#nnetar
c=Sys.time()
timeS <- window(nottem, end = c(1930, 12))
fit=nnetar(timeS)
f=forecast(fit,h=24)
x <- window(nottem, start=c(1931,1), end = c(1932, 12))
data.class(x)
accuracy(f,x)
d=Sys.time()
d-c

#MLP
library(nnfor)
set.seed(100)
c=Sys.time()
timeS <- window(nottem, end = c(1930, 12))
fit=mlp(timeS,lags=1:frequency(timeS))
f=forecast(fit,h=24)
x <- window(nottem, start=c(1931,1), end = c(1932, 12))
accuracy(f,x)
d=Sys.time()
d-c

#ELM
set.seed(1)
c=Sys.time()
timeS <- window(nottem, end = c(1930, 12))
fit <- elm(timeS, lags=1:frequency(timeS),type="step",rep=24, comb="mean")
f1=forecast(fit,h=24)
x <- window(nottem, start=c(1931,1), end = c(1932, 12))
accuracy(f1,x)
d=Sys.time()
d-c













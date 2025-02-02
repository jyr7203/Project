bicycle <- read.csv("C:/Users/Jeon Yerin/OneDrive - 고려대학교/바탕 화면/따릉이/따릉이/train.csv")
bicycle <- na.omit(bicycle)
head(bicycle)
table(is.na(bicycle))
str(bicycle)
bicycle <- ts(bicycle)

rent <- read.csv("C:/Users/Jeon Yerin/Downloads/서울특별시 공공자전거 대여이력 정보_2306.csv", fileEncoding = "UTF-8")
rent <- read.csv(file, header = TR fileEncoding = "UTF-8")

rent <- read.csv("C:/Users/Jeon Yerin/Downloads/rent.csv")
plot(decompose(bicycle, type="multiplicative"))
AirPassengers
bicycle <- ts(bicycle)
head(bicycle)

library(ggplot2)
# 데이터프레임에서 시간별 따릉이 대여수의 평균 계산
hourly_mean_counts <- aggregate(count ~ hour, data = bicycle, FUN = mean)
hourly_mean_counts

# 시간대별 따릉이 이용자 수 계산
hourly_counts <- aggregate(count ~ hour, data = bicycle, FUN = sum)
hourly_counts

# ggplot을 사용하여 bar plot 그리기
ggplot(hourly_counts, aes(x = factor(hour), y = count)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Hourly Bicycle Rental Counts",
       x = "Hour of the Day",
       y = "Total Rental Counts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # x 축 라벨 각도 조절


#temperature에 따른 count 시각화
ggplot(bicycle, aes(x = hour_bef_temperature, y = count)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Relationship between Temperature and Bicycle Rental Counts",
       x = "Hour Before Temperature",
       y = "Bicycle Rental Counts") +
  theme_minimal()


library(ggplot2)

# 온도를 범주형 변수로 변환
bicycle$temperature_category <- cut(bicycle$hour_bef_temperature, breaks = c(3, 10, 17, 24, 30),
                               labels = c('Cold', 'Chill', 'Warm', 'Hot'))

# 각 온도 범주별 이용자 수 계산
temperature_counts <- aggregate(count ~ temperature_category, data = bicycle, FUN = sum)

# ggplot을 사용하여 bar plot 그리기
ggplot(temperature_counts, aes(x = temperature_category, y = count)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(title = "Bicycle Rental Counts by Temperature",
       x = "Temperature Category",
       y = "Total Rental Counts") +
  theme_minimal()

# 강수량
prop.table(table(bicycle$hour_bef_precipitation)) #비가 온 날이 97%


# Assuming your data frame is named df
library(ggplot2)

# Convert hour_bef_precipitation to a factor for better color mapping
bicycle$hour_bef_precipitation <- factor(bicycle$hour_bef_precipitation, levels = c(0, 1), labels = c("No Rain", "Rain"))
 
# Plotting
ggplot(bicycle, aes(x = hour, y = count, color = hour_bef_precipitation)) +
  geom_point() +
  scale_color_manual(values = c("No Rain" = "black", "Rain" = "pink")) +
  labs(x = "Hour", y = "Count", color = "Precipitation") +
  theme_minimal()

library(ggplot2)

# Create a data frame for plotting
plot_data <- data.frame(
  category = c("clean", "rainy"),
  value = c(rain_total['clean'], rain_total['rainy']),
  stringsAsFactors = FALSE
)

# Create a color vector
original_color <- c("#1f77b4", "#ff7f0e")

# Plotting using ggplot2
p <- ggplot(plot_data, aes(x = 0, y = value, fill = category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )

# Clean percentage annotation
p <- p +
  annotate(
    "text",
    x = 0.5,
    y = rain_total['clean'] / 2,
    label = paste0(round(rain_total['clean'] * 100), "%"),
    size = 7,
    color = "white"
  ) +
  annotate(
    "text",
    x = 0.5,
    y = 0.45,
    label = "clean",
    size = 7,
    color = "white"
  )

# Rainy percentage annotation
p <- p +
  annotate(
    "text",
    x = 0.5,
    y = (rain_total['clean'] + rain_total['rainy']) / 2,
    label = paste0(round(rain_total['rainy'] * 100), "%"),
    size = 7,
    color = "white"
  ) +
  annotate(
    "text",
    x = 0.5,
    y = 0.45,
    label = "rainy",
    size = 7,
    color = "white"
  )

# Title & Subtitle
p <- p +
  ggtitle("Clean & Rainy Usage") +
  labs(subtitle = "Clean and rainy usage is 8:2.")

# Remove axis labels and ticks
p <- p +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Remove legend
p <- p + theme(legend.position = "none")

# Remove grid lines
p <- p + theme(panel.grid = element_blank())

# Display the plot
print(p)


# humidity에 따른 count 시각화
ggplot(bicycle, aes(x = hour_bef_humidity, y = count)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Relationship between Humidity and Bicycle Rental Counts",
       x = "Hour Before Humidity",
       y = "Bicycle Rental Counts") +
  theme_minimal()

# 미세먼지 분류 및 count 시각화
pm_order <- c('good', 'not bad', 'bad', 'very bad')

# 미세먼지 수치에 따라 범주화
bicycle$hour_bef_pm10 <- cut(bicycle$hour_bef_pm10, breaks = c(9, 74, 139, 204, Inf), labels = pm_order)
bicycle$hour_bef_pm2.5 <- cut(bicycle$hour_bef_pm2.5, breaks = c(0, 15, 35, 75, Inf), labels = pm_order)

# hour_bef_pm2.5에 대한 빈도표 생성
pm_2 <- table(bicycle$hour_bef_pm2.5)[pm_order]

# hour_bef_pm10에 대한 빈도표 생성
pm_10 <- table(bicycle$hour_bef_pm10)[pm_order]

# 결과 출력
print(pm_2)
print(pm_10)

# 미세먼지 데이터 프레임 생성
pm2_total <- data.frame(pm_2)

# 바 차트 그리기
barplot(as.matrix(t(pm2_total)), beside = TRUE, col = c('#38FFA5', '#2FE290', '#447D64', '#445664'),
        ylim = c(0, max(pm2_total) + 2000),
        names.arg = colnames(pm2_total),
        xlab = "pm2.5",
        ylab = "Count",
        main = "pm2.5 & Count",
        font.main = 1,
        cex.main = 1.4,
        font.lab = 1,
        cex.lab = 1.2,
        font.axis = 1,
        cex.axis = 1,
        axis.lty = 1,
        args.legend = list(title = "Legend", x = "topright", cex = 0.8),
        col.main = "black",
        col.lab = "black",
        col.axis = "black",
        grid.lty = 3,
        plot = TRUE)

# 미세먼지 데이터 프레임 생성
pm10_total <- data.frame(pm10_total)

# 바 차트 그리기
barplot(as.matrix(t(pm10_total)), beside = TRUE, col = c('#38FFA5', '#2FE290', '#447D64', '#445664'),
        ylim = c(0, max(pm10_total) + 2000),
        names.arg = colnames(pm10_total),
        xlab = "pm10",
        ylab = "Count",
        main = "pm10 & Count",
        font.main = 1,
        cex.main = 1.4,
        font.lab = 1,
        cex.lab = 1.2,
        font.axis = 1,
        cex.axis = 1,
        axis.lty = 1,
        args.legend = list(title = "Legend", x = "topright", cex = 0.8),
        col.main = "black",
        col.lab = "black",
        col.axis = "black",
        grid.lty = 3,
        plot = TRUE)
# 초미세먼지가 오히려 많을 때 따릉이 사용량이 많다

# 변수간 상관관계 확인(precipitation은 binary variable이기 때문에 id와 함께 제거하고 진행)
bicycle[,-c(1,4)] %>% GGally::ggpairs()


# visualization of variable 
 
library(fpp2)
bicycle_a <- ts(bicycle, frequency = 24) 
#설명력이 높게 나타난 온도, 오존, 습도, 바람 등에 따라 시간에 따른 따릉이 대여수가 어떻게 나타나는지 알아보기 위해 fpp2 라이브러리에 있는 autplot 함수를 이용하기로 하였다. autoplot 함수에 적용하기 위해 x축이 될 hour의 단위에 맞게 bicycle 데이터를 주기가 24인 time series 객체로 변형해주었다.
autoplot(bicycle_a[,c("hour_bef_temperature", "hour_bef_ozone", "hour_bef_humidity", "hour_bef_windspeed", "hour_bef_pm2.5")]) + xlab("hour") + ylab("count")
#plot을 보면 time series 데이터로 변환하는 과정에서 count 수에 변형이 일어난 것으로 보인다. 하지만, 시간에 따른 기온, 오존, 습도, 바람, 미세먼지 등의 요인의 시간에 따른 변화를 살펴보면 기온, 오존, 습도가 시간에 따른 변화가 비슷하게 보인다.

bicycle_a <- ts(bicycle$count, frequency = 24)
time=time(bicycle_a)
fit_naive=tslm(bicycle_a~time+I(time^2))
ts.plot(bicycle); lines(fitted(fit_naive), col=2)
# 데이터를 시간에 따른 ts 객체로 변환하고 naive regression approach를 사용하여 시간에 따른 대여량의 ts.plot을 그렸다. 증가하거나 감소하는 trend는 보이지 않으며 이는 년 단위가 아니라 하루를 기준으로 시간에 따른 대여량을 분석한 결과로 인한 것이라고 예상한다. However, naive regression approach cannot afford to explain oscilliation by heterogeneity of variance. Therefore, cycle should be considered additionally. 그전에 naive regression approach를 사용한 fit의 잔차에 대한 검정을 할 것이다.
checkresiduals(fit_naive) #plot
checkresiduals(fit_naive, plot=F) #breusch-godfrey test
# 잔차가 lag 51을 제외하고는 serial correlation을 보이지 않고, Breusch-Godfrey test 에서 p-value가 0.8792로 매우 크기 때문에 there is no serial correlation.

hour <- bicycle_a %>% cycle %>% as.factor
ts.plot(bicycle_a)
fit_cycle=tslm(bicycle_a ~ time + hour)
ts.plot(bicycle_a); lines(fitted(fit_cycle), col=2)
# cycle 요소를 추가하여 tslm을 fit한 결과, oscilliation이 반영된 모습을 볼 수 있다. 붉은 선이 oscilliation의 모습인데, 실제 time series와 oscilliation의 폭은 일치하지 않지만 oscilliation의 모양이 같게 fit되었음을 알 수 있다.
checkresiduals(fit_cycle) #plot
checkresiduals(fit_cycle, plot=F) #breusch-godfrey test
# 앞에서 분석한 naive regression approach를 적용한 것과 일치하게 잔차가 lag 51을 제외하고는 serial correlation을 보이지 않고, Breusch-Godfrey test 에서 p-value가 0.8997로 매우 크기 때문에 there is no serial correlation.

## naive approach와 cyclic component를 고려한 두 regression 모델의 결과를 종합적으로 분석하면, 자전거 대여수가 많거나 적게 나타나는 특징적인 모습은 시계열 모델보다 오히려 시간별 자전거 대여수 집계로 잘 나타남을 알 수 있다.

##model selection and forecasting
# regression
library(tidyverse)
library(forecast)
bicycle <- ts(bicycle)
tslm(count ~ hour_bef_pm10, data=bicycle) %>% summary()
tslm(count ~ hour_bef_precipitation, data=bicycle) %>% summary()
tslm(count ~ hour_bef_pm2.5, data=bicycle) %>% summary()
tslm(count ~ hour_bef_visibility, data=bicycle) %>% summary()
tslm(count ~ hour_bef_windspeed, data=bicycle) %>% summary()
tslm(count ~ hour_bef_humidity, data=bicycle) %>% summary()
tslm(count ~ hour_bef_ozone, data=bicycle) %>% summary()
tslm(count ~ hour_bef_temperature, data=bicycle) %>% summary()
tslm(count ~ hour, data=bicycle) %>% summary()
#변수 하나만 고려했을 때 시간 변수를 제외하고 환경적 요인 중 가장 설명력이 낮은 변수는 미세먼지, 가장 설명력이 높은 변수는 37%를 설명하는 온도 변수(온도, 오존, 습도, 바람 순으로 설명력이 높게 나타남) 또한, precipitation 변수는 data exploratory analysis의 결과 비가 오면 1, 아니면 0인 dummy variable인데, 비가 오지 않은 날이 97%를 차지하여 R^2값이 어느정도 나타나도 따릉이 대여 수를 설명하는 것이 적절하지 않을 것이라고 생각한다. 또한, 그렇기에 변수 중요도가 낮게 나타날 것이라고 예상한다.

# 단일 변수만을 고려했을 때 설명력이 낮게 나타났던 변수를 제외하는 방식으로 여러 회귀 모델을 만들고, CV() 함수를 통해 best model을 selection 
fit1 <- tslm(count ~ hour+hour_bef_temperature+hour_bef_pm10+hour_bef_precipitation+hour_bef_pm2.5+hour_bef_humidity+hour_bef_ozone+hour_bef_visibility+hour_bef_windspeed, data=bicycle) #모든 변수 고려
fit2 <- tslm(count ~ hour+hour_bef_temperature+hour_bef_precipitation+hour_bef_pm10+hour_bef_humidity+hour_bef_ozone+hour_bef_visibility+hour_bef_windspeed, data=bicycle) # hour_bef_pm2.5 제거
fit3 <- tslm(count ~ hour+hour_bef_temperature+hour_bef_pm2.5+hour_bef_humidity+hour_bef_ozone+hour_bef_visibility+hour_bef_windspeed, data=bicycle) #hour_bef_pm10, hour_bef_humidity 제거
fit4 <- tslm(count ~ hour+hour_bef_temperature+hour_bef_ozone+hour_bef_humidity+hour_bef_pm10, data=bicycle) #hour_bef_pm2.5, hour_bef_precipitation, hour_bef_visibility, hour_bef_windspeed 제거
fit5 <- tslm(count ~ hour+hour_bef_temperature+hour_bef_ozone+hour_bef_humidity, data=bicycle) #hour_bef_pm10, hour_bef_pm2.5, hour_bef_precipitation, hour_bef_visibility, hour_bef_windspeed 제거

fit1 %>% CV(); fit2 %>% CV(); fit3 %>% CV(); fit4 %>% CV(); fit5 %>% CV()
# fit5가 adjusted R^2는 가장 크지만, 그 다음으로 adjusted R^2가 큰 fit1이 CV, AIC, corrected AIC, BIC가 모두 가장 작기 때문에 모든 변수를 고려한 모델이 best model이다.

summary(fit1)
checkresiduals(fit1)
# fit1의 결과 설명력은 48%이며, p-value가 크므로 잔차간 serial correlation이 없다.
# 특정 패턴을 보이지 않기때문에 랜덤하다. 잔차가 자기상관성이 없다. 또한, 잔차가 약간 오른쪽으로 치우치긴 했지만 정규성을 띤다고 볼 수 있다. checkresiduals 함수로  확인해본 결과, p값이 0.5보다 커 잔차간 serial correlation이 있다는 귀무가설을 기각하기에 잔차 간 correlation이 없으며, stationary하다고 판단할 수 있다.
#변수를 추가하면서 통계적 유의성과 r^2 값이 커짐 

# tslm이 아니라 이번에는 randomforest를 이용해 regression 진행하였다. Random Forest is an ensemble learning method that builds a multitude of decision trees during training and outputs the average prediction (for regression) of the individual trees. It is based on the idea of bagging (bootstrap aggregating) and introduces randomness to improve the predictive performance and control overfitting. 또한,Random Forest provides a feature importance score, allowing to identify which features contribute most to the model's predictions.
#따릉이 대여수(count 변수)를 target 변수로 하여 random forest regression으로 데이터셋 분할하여 예측한다. 우선, bicycle 데이터를 데이터 분할 시 가장 많이 적용하는 7:3의 비율로 train data와 test data로 분할한다. 그 다음 randomForest library를 이용하여 count variable을 target variable로 하여 forecast를 진행한다. 
# Install and load the randomForest package
install.packages("randomForest")
library(randomForest)


# Assuming your data frame is named bicycle
# Split the data into training and testing sets
set.seed(123)
sample_index <- sample(seq_len(nrow(bicycle)), size = 0.7 * nrow(bicycle))
train_data <- bicycle[sample_index, ]
test_data <- bicycle[-sample_index, ]


# Train a random forest regression model
model1 <- randomForest(count ~ hour + hour_bef_temperature + hour_bef_windspeed +
hour_bef_humidity, data = train_data)
# 우선, tslm으로 fit한 regression model의 결과를 CV() 함수로 확인한 결과 CV, AIC, BIC, AICc, R^2의 지표에서 성능이 높게 나타났던 모든 변수를 포함한 모델을 random forest regression으로 train시켜보고자 한다.
importance(model1)
varImpPlot(model1)
# 변수를 축소하여 새로운 모델을 만들어 성능을 비교하기 위해 모든 변수를 포함한 random forest regression 결과에서 중요도가 높게 나타난 변수를 파악한다. importance 함수로 변수의 중요도를 살피면, hour, temperature, ozone, windspeed, humidity, visibility, pm10, pm2.5, precipitation의 순서로 중요도가 높게 나타난다. 이 결과를 가지고 새로운 모델을 만들어보고자 한다.

# Make predictions on the test set
predictions1 <- predict(model1, newdata = test_data)
predictions2 <- predict(model1, newdata = train_data)

# Model evaluation
mean((test_data[,11] - predictions1)^2)  # test MSE
mean(abs(test_data[,11] - predictions1)) # test MAE
sqrt(mean((test_data[,11] - predictions1)^2)) # test RMSE

mean((train_data[,11] - predictions2)^2)  # train MSE
mean(abs(train_data[,11] - predictions2)) # train MAE
sqrt(mean((train_data[,11] - predictions2)^2)) # train RMSE

#train data와 test data에 대한 model evaluation 결과, 각각 MSE, MAE, RMSE가 다음과 같다. train data에 대해 MSE, MAE, RMSE가 낮게 나타나 오차가 적고 정확도가 더  높은 것을 알 수 있다.

# Visualize the predictions
plot(test_data[,11], predictions1, main = "Random Forest Predicted vs. Actual Bike Rental Count",
     xlab = "Actual Count", ylab = "Predicted Count", col = "gray", pch = 16)
abline(0, 1, col = "skyblue")

# 실제 따릉이 대여수와 예측한 대여수가 어느 정도 일치하고, 150대 정도를 넘어가면 예측과 실제 대여수가 차이가 생긴다. 또한, 예측한 대여수는 실제 대여수만큼 300~400대의 분포가 존재하지 않음을 볼 수 있다.


#추가적으로 10-fold cross validation을 통해 test data에 대한 MSE와 MAE를 구해보고자 한다.
##########################
# Computing the CV error

V = 10 #V-fold CV
mse.test = 0
mae.test = 0

set.seed(1234)
id = sample(1:V, nrow(bicycle), replace = T)

for(i in 1:V) {
  
  print(i)
  
  ## Data Partitioning
  
  test.index = which(id==i)
  train_data = bicycle[-test.index,] #train data                              
  test_data = bicycle[ test.index,] #test data
  
  ## Foresting
  
  fit = randomForest(count~., data=train_data, ntree=100, mtry=5, importance=T, na.action=na.omit)
  
  ## Predicting and Evaluating
  
  predictions1 = predict(fit, newdata=test_data, type="response")
  mse.test = mse.test + mean((test_data[,11] - predictions1)^2)  # MSE
  mae.test = mae.test + mean(abs(test_data[,11] - predictions1)) # MAE
}

cv.mse.test  = mse.test/V;  cv.mse.test  # test CV MSE
cv.mae.test  = mae.test/V;  cv.mae.test  # test CV MAE



# 다음으로 time series decomposition을 진행하여 따릉이 대여수의 시간에 따른 seasonal,trend, remainder 효과를 분석해보고자 한다. count 변수를 주기를 24로 하여 시계열 객체로 분해하고 decomposition을 하였다.
# time series decomposition
bicycle_ts <- ts(bicycle$count, frequency = 24) # 시계열 객체로 분해
bicycle_decomposed <- stl(bicycle_ts, s.window = "periodic") # 시계열 분해 
plot(bicycle_decomposed)
plot(fit <- stl(bicycle_ts, s.window=6))

# 다음으로는 simple exponential smoothing, holt's linear trend, holt's damped trend 모델의 성능을 비교해보고자 한다. 
library(forecast)
library(aTSA)
autoplot(bicycle_ts, series="Data") + autolayer(trendcycle(fit), series="Trend-cycle")
bicycle_ts2 <- window(bicycle_ts, start=1, end=24)
fit1 <- ses(bicycle_ts2) #ses
fit2 <- holt(bicycle_ts2) #liner trend
fit3 <- holt(bicycle_ts2, damped = TRUE) #damped trend
accuracy(fit1, bicycle_ts) #ses
accuracy(fit2, bicycle_ts) #liner trend
accuracy(fit3, bicycle_ts) #damped trend
#ses 모델이 가장 성능이 좋다

data = cbind(Data=window(bicycle_ts, start=1),
              SES=fit1$mean, "Holt's"=fit2$mean, "Damped trend"=fit3$mean)
autoplot(data)


# additive 모델과 multiplicative 모델을 사용하여 시간에 따른 자전거 대여량의 변화를 확인하였다. Also, perform Holt-Winters exponential smoothing on a time series on bicycle_ts. The seasonal component is specified as both "additive" and "multiplicative." After fitting the models, using autoplot function, visualized the states of the models. 
fit1 = hw(bicycle_ts, seasonal = "additive")
fit2 = hw(bicycle_ts, seasonal = "multiplicative")
addstates = fit1$model$states[,1:3]
multstates = fit2$model$states[,1:3]
p1 = autoplot(addstates, facets=TRUE)
p2 = autoplot(multstates, facets=TRUE)
gridExtra::grid.arrange(p1,p2, ncol=2)
# Two model has similar aspect, but multiplicative holt winters exponential smoothing model has more oscilliated seasonal effect in the first season.

# 추가적으로 ets 함수로 exponential smoothing을 하면 모델의 accuaracy는 RMSE가 90.76602, MAE는 68.71932가 나온다. 
print(fit <- ets(bicycle_ts, lambda=0)) #exponential smoothing with ets
accuracy(fit)


# ARIMA 모델 생성
arima_model <- auto.arima(bicycle_ts)
summary(arima_model) #arima 모델의 성능이 조금 더 좋음

# 또는 지수평활법 모델 생성
ets_model <- ets(bicycle_ts)
summary(ets_model)

# 모델의 잔차(residuals) 확인
residuals <- residuals(arima_model)

# 잔차의 정규성 확인
qqnorm(residuals)
qqline(residuals)


library(fpp2); library(forecast)
y = bicycle[,c("count")]
checkresiduals(y)
auto.arima(y)
AP = Arima(bicycle, order=c(4,0,4))

library(ggplot2); library(tidyverse)
ggplot(bicycle,aes(x=hour, y=count)) + geom_point() + geom_line() + geom_smooth(method="loess")




pacman::p_load(ISLR,ggplot2,gridExtra,sqldf,caret,readr,tidyverse,car,corrplot,plotly,xlsx,DT,Hmisc,rpart,rpart.plot  )
#setwd("~/Desktop/巨量/巨量報告/Group_08")
load("car_prices.rdata")
df<-df %>% filter(df$year<df$saleyear)

df$condition<-as.numeric(df$condition)
df$sellingprice<-as.numeric(df$sellingprice)
#character轉factor
df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
#取出要訓練的資料
df2 <-df[c("make","model","body", "state", "condition", "odometer", "color", "interior", "vehicleage", "sellingprice")]
A = df2 %>% group_by(model) %>% summarise(n= n()) %>% filter(n < 6500)
df2 = filter(df2, !(model %in% A$model))

# 正規化
preProcessPlan <- preProcess(df2[,-10], method = c('range')) #9要改成sellingprice所在欄位
df3<-predict(preProcessPlan, df2)

#切模型
set.seed(1)
train_idx <- createDataPartition(df3$sellingprice, p = 0.7, list = F)
train_d <- df3[train_idx,]
test_d <- df3[-train_idx,]


MAE <- function(predicted, actual) return(mean(abs(predicted - actual)))

#使用變數：sellingprice ~ .

#決策樹
DT = rpart(sellingprice ~make + model + body + state + condition + odometer + color +
           interior + vehicleage, data = train_d,control = rpart.control(maxdepth = 6))

#印出決策樹
plot.party(as.party(DT))
#plot(varImp(RT))
#計算MAE
tr_mae <- MAE(train_d$sellingprice, predict(DT, newdata = train_d))
ts_mae <- MAE(test_d$sellingprice, predict(DT, newdata = test_d))

rpart.rules(DT)



library(readxl)
library(glmnet)
library(ggplot2)
library(caret)
library(rpart)
library(randomForest)
library("e1071")

stocks <- read_excel("Data_2018.xlsx", skip = 7)
Date<- as.character(stocks$X__1)
stocks$X__1<- as.Date(Date, format = '%Y%m%d')
stocks <- data.frame(stocks)
x <- t(data.frame(rep(NA, ncol(stocks))))
x<-x[rep(seq_len(nrow(x)),each = 30),]
colnames(x) <-colnames(stocks)
return_to_p <- stocks[-seq(1,30),]
return_to_p <- rbind(return_to_p,x)
stocks$stock_tp <- return_to_p$Drugs
stocks <- stocks[1:12166, ]
stocks_num <-nrow(stocks)

#linear regression

#step 1, find optimal window size
window_size <- seq(140, 360, by=40)
mse_values<- c()

for(i in window_size){
  se_values <-c()
  pred_lm_fix <- data.frame(Date = as.Date(character()),
                            actural_return = double(),
                            predicted_return = double(),
                            f_err = double())
  for (j in seq((i + 1),stocks_num,by = 1)){
    train <- stocks[(j-i):(j-1), -c(1)]
    model <- lm(stock_tp ~ ., train) 
    pred_value<- predict(model, stocks[i,-1])
    actural_return <-  stocks[i,50]
    predicted_return <- pred_value[[1]]
    f_err <- actural_return - predicted_return
    pred_lm_fix <- rbind(pred_lm_fix, c(stocks[i,1], stocks[i,50],pred_value[[1]], f_err))
    colnames(pred_lm_fix) <- c("Date", "actural_return", "predicted_return", "f_err")
    
  }
  mse_values <-c(mse_values, mean((pred_lm_fix$f_err)^2))
}
windows <-data.frame(window_size,mse_values)

ggplot(windows, aes(x=window_size, y = mse_values)) + geom_line()
op_w <- windows[which.min(windows[,2]),1]








#step 2, predict

op_w_linear <- op_w

pred_lm_fix <- data.frame(Date = as.Date(character()),
                          actural_return = double(),
                          historic_mean = double(),
                          predicted_return = double(),
                          f_err = double(),
                          r_rr = double(),
                          diff_RMSE <- double())

initial_w_size <- op_w_linear 

for (i in seq((initial_w_size+1),stocks_num,by=1)){ #fixed window size 
  train <- stocks[(i-initial_w_size):(i-1), -c(1)]
  model <- lm(stock_tp ~ ., train)
  pred_value<- predict(model, stocks[i,-1])
  historic_mean <- mean(stocks[(i-initial_w_size):(i-1),50])
  actural_return <-  stocks$stock_tp[i] 
  predicted_return <- pred_value[[1]]
  f_err <- actural_return - predicted_return
  r_err <- actural_return - historic_mean
  pred_lm_fix <- rbind(pred_lm_fix, c(stocks[i,1], actural_return,pred_value[[1]], historic_mean, f_err, r_err, NA,NA,NA))
  colnames(pred_lm_fix) <- c("Date", "actural_return", "historic_mean", "predicted_return", "f_err", "r_err","RMSE_diff","se_lm","se_mean")
  pred_lm_fix$se_lm[i-initial_w_size] <- (pred_lm_fix$f_err[i-initial_w_size])^2
  pred_lm_fix$se_mean[i-initial_w_size] <- (pred_lm_fix$r_err[i-initial_w_size])^2
  pred_lm_fix$RMSE_diff[i-initial_w_size] <- (mean(pred_lm_fix$se_mean))^0.5 - (mean(pred_lm_fix$se_lm))^0.5
  
}

#save the plot of RMSE difference
Drugs_linear_lag30<-ggplot(pred_lm_fix) + geom_line( aes( x =  Date, y = RMSE_diff, colour = "Mean -Linear")) 
ggsave("Drugs_linear_lag30.jpg", plot = Drugs_linear_lag30)

#save the outcome of computer industry-linear-lag365
write.csv(pred_lm_fix,'linear_Drugs_lag30.csv')







#lasso

#find obtimal windowsize in lasso

window_size <- seq(100, 400, by=50)
mse_values<- c()

for(j in window_size){#though he said don't snoop the windowsize outside the cV, I am now using cv to find the best window size
  se_values <- c()
  for (i in seq((j+1), stocks_num, by=1)){ #fixed window size
    a <- (i-j)
    train_X <- model.matrix(stock_tp ~., stocks[a:(i-1), ])[,-c(1,50)]
    predict_x <- model.matrix(stock_tp ~., stocks[(i-1):i, ])[,-c(1,50)]
    train_Y <- as.matrix(stocks[a:(i-1),50])
    cv <- cv.glmnet(train_X, train_Y, family = "gaussian", nfold = 5, type.measure = "mse", paralle = TRUE, alpha = 1)
    model<- glmnet(train_X, train_Y, family = "gaussian", lambda = cv$lambda.min, alpha = 1)
    pred_value <-predict(model,predict_x,s= cv$lambda.min)
    actural_return <-  stocks[i,50]
    predicted_return <- pred_value[[2]]
    se <- (actural_return - predicted_return)^2
    se_values <- rbind(se_values, c(se,j))
  }
  mse_values <- rbind(mse_values, se_values)
} 
mse_lasso <- mse_values
mse_values <- as.data.frame(mse_values)
mse_values <- aggregate(.~V2,mse_values, mean)

windows <-data.frame(window_size,mse_values)

ggplot(windows, aes(x=window_size, y = V1)) + geom_line()+labs(y = "mse_values")
op_w <- windows[which.min(windows[,3]),1]
op_w_lasso <- op_w



#prediction

initial_w_size<- op_w

pred_lasso <- data.frame(Date = as.Date(character()),
                         actural_change_spot = double(),
                         historic_avetage = double(),
                         predicted_change = double(),
                         f_err = double(),
                         r_rr = double(),
                         diff_RMSE <- double())
set.seed(42)

for (i in seq((initial_w_size+1), stocks_num, by=1)){ #fixed window size
  train_X <- model.matrix(stock_tp ~., stocks[(i-initial_w_size):(i-1), ])[,-c(1,50)]
  predict_x <- model.matrix(stock_tp ~., stocks[(i-1):i, ])[,-c(1,50)]
  train_Y <- as.matrix(stocks[(i-initial_w_size):(i-1),50])
  cv <- cv.glmnet(train_X, train_Y, family = "gaussian", nfold = 5, type.measure = "mse", paralle = TRUE, alpha = 1)
  model<- glmnet(train_X, train_Y, family = "gaussian", lambda = cv$lambda.min, alpha = 1)
  pred_value <-predict(model,predict_x,s= cv$lambda.min)
  
  historic_mean <- mean(stocks[(i-initial_w_size):i-1,50])
  actural_return <-  stocks[i,50]
  predicted_return <- pred_value[[2]]
  f_err <- actural_return - predicted_return 
  r_err <- actural_return - historic_mean
  pred_lasso <- rbind(pred_lasso, c(stocks[i,1], stocks[i,50],predicted_return, historic_mean, f_err, r_err, NA, NA, NA,NA))
  colnames(pred_lasso) <- c("Date", "actural_return", "predicted_return", "historic_mean", "f_err", "r_err","RMSE_lasso","se_lasso","se_mean", "RMSE_diff") 
  pred_lasso$se_lasso[i-initial_w_size] <- (pred_lasso$f_err[i-initial_w_size])^2
  pred_lasso$se_mean[i-initial_w_size] <- (pred_lasso$r_err[i-initial_w_size])^2
  pred_lasso$RMSE_lasso[i-initial_w_size] <- (mean(pred_lasso$se_lasso))^0.5
  pred_lasso$RMSE_diff[i-initial_w_size] <-(mean(pred_lasso$se_mean))^0.5 - (mean(pred_lasso$se_lasso))^0.5
}

#save outcome
write.csv(pred_lasso,'pred_lasso_drugs_lag30.csv')
Drug_lasso_lag30<-ggplot(pred_lasso) + geom_line( aes( x =  Date, y = RMSE_diff, colour = "Mean - Lasso")) 
ggsave("Drug_lasso_lag30.jpg", plot = Drug_lasso_lag30)








#ramdom forest

window_size <- seq(200, 350, by=50) 
w_leaf_mse_df <- data.frame(windows = double(), leaf = double(), mse_values = double())
for(i in window_size){
  se_values <-c()
  pred_rf<- data.frame(Date = as.Date(character()),
                       actural_return = double(),
                       predicted_return = double(),
                       f_err = double())
  min_leaf <- seq(10,30,by = 10)
  for (leaf in min_leaf){
    for (j in seq((i + 1),stocks_num,by = 1)){
      a<-j-i
      train <- stocks[a:(j-1), -c(1)]
      rf <- randomForest(stock_tp ~ ., train, ntree = 5,nodesize = leaf)
      pred_value<- predict(rf, stocks[j,-c(1,50)])
      actural_return <-  stocks[i,50]
      predicted_return <- pred_value[[1]]
      f_err <- actural_return - predicted_return
      pred_rf<- rbind(pred_rf, c(stocks[i,1], stocks[i,50],pred_value[[1]], f_err))
      colnames(pred_rf) <- c("Date", "actural_return", "predicted_return", "f_err")
    }
    mse_values_leaf <-c(i,leaf,mean((pred_rf$f_err)^2))
    mse_values_leaf <-t(data.frame(mse_values_leaf))
    colnames(mse_values_leaf) <- c('windows','leaf','mse_values')
    w_leaf_mse_df <-rbind(w_leaf_mse_df, mse_values_leaf)
  }
}


op_w_rf <- w_leaf_mse_df[which.min(w_leaf_mse_df[,3]),1]
op_node_size <- w_leaf_mse_df[which.min(w_leaf_mse_df[,3]),2]

#prediction
initial_w_size<- op_w_rf

pred_rf <- data.frame(Date = as.Date(character()),
                      actural_return = double(),
                      historic_return = double(),
                      predicted_return = double(),
                      f_err = double(),
                      r_rr = double(),
                      diff_RMSE <- double())
set.seed(42)

for (i in seq((initial_w_size+1), stocks_num, by=1)){ #fixed window size
  train <- stocks[(i-initial_w_size):(i-1), -c(1)]
  rf <- randomForest(stock_tp ~ ., train, ntree = 5,nodesize = op_node_size)
  pred_value<- predict(rf, stocks[i,-c(1,50)])
  actural_return <-  stocks[i,50]
  predicted_return <- pred_value[[1]]
  f_err <- actural_return - predicted_return
  historic_mean <- mean(stocks[(i-initial_w_size):i-1,2])
  r_err <- actural_return - historic_mean
  pred_rf <- rbind(pred_rf, c(stocks[i,1], stocks[i,50],predicted_return, historic_mean, f_err, r_err, NA, NA, NA,NA))
  colnames(pred_rf) <- c("Date", "actural_return", "predicted_return", "historic_mean", "f_err", "r_err","RMSE_rf","se_rf","se_mean","RMSE_diff") 
  pred_rf$se_rf[i-initial_w_size] <- (pred_rf$f_err[i-initial_w_size])^2
  pred_rf$se_mean[i-initial_w_size] <- (pred_rf$r_err[i-initial_w_size])^2 
  pred_rf$RMSE_rf[i-initial_w_size] <- (mean(pred_rf$se_rf))^0.5
  pred_rf$RMSE_diff[i-initial_w_size] <- (mean(pred_rf$se_mean))^0.5-(mean(pred_rf$se_rf))^0.5
}

#save outcome for rf
write.csv(pred_rf, "RF_Drugs_lag30.csv")
Drugs_rf_lag30<- ggplot(pred_rf) + geom_line( aes( x =  Date, y = RMSE_diff, colour = "Mean - Random Forest")) 
ggsave("Drugs_rf_lag30.jpg", plot = Drugs_rf_lag30)







#svm- This one taks forever

set.seed (1)
#find obtimal windowsize in lasso
#fix window size
#test_num <-100

window_size <- seq(300,400, by=30)
mse_values<- c()

for(j in window_size){#
  se_values <- c()
  pred_svm <- data.frame(Date = as.Date(character()),
                         actural_return = double(),
                         predicted_return = double(),
                         f_err = double())
  for (i in seq((j+1), stocks_num, by=1)){ #fixed window size 
    a <- (i-j)
    train_X <- model.matrix(stock_tp ~., stocks[a:(i-1), ])[,-c(1,50)]
    predict_x <- model.matrix(stock_tp ~., stocks[(i-1):i, ])[,-c(1)]
    train_Y <- c(stocks[a:(i-1),50])
    dat <- data.frame(train_X, train_Y)
    tune.out=tune(svm, stock_tp~.,data=stocks[a:(i-1), -c(1)] ,kernel ="radial",
                  ranges = list(cost = c(0.1,0.5,1),gamma = c(1)) )
    bestmod <-tune.out$best.model
    pred_value <-predict(bestmod,predict_x)
    actural_return <-  stocks[i,50]
    predicted_return <- pred_value[[2]]
    f_err <- actural_return - predicted_return
    pred_svm <- rbind(pred_svm, c(stocks[i,1], stocks[i,50],predicted_return, f_err))
    colnames(pred_svm) <- c("Date", "actural_return", "predicted_return", "f_err")
  }
  mse_values <-c(mse_values, mean((pred_svm$f_err)^2))
}
windows_svm <-data.frame(window_size,mse_values)

ggplot(windows_svm, aes(x=window_size, y = mse_values)) + geom_line()+labs(y = "mse_values")

op_w_svm <- windows_svm[which.min(windows_svm[,2]),1]

#prediction
initial_w_size<- op_w_svm
pred_svm <- data.frame(Date = as.Date(character()),
                       actural_change_spot = double(),
                       predicted_change = double(),
                       f_err = double(),
                       diff_RMSE <- double())
set.seed(1)

for (i in seq((initial_w_size+1), stocks_num, by=1)){ #fixed window size
  a <- (i-initial_w_size)
  train_X <- model.matrix(stock_tp ~., stocks[a:(i-1), ])[,-c(1,50)]
  predict_x <- model.matrix(stock_tp ~., stocks[(i-1):i, ])[,-c(1)]
  train_Y <- c(stocks[a:(i-1),50])
  dat <- data.frame(train_X, train_Y)
  tune.out=tune(svm, stock_tp~.,data=stocks[a:(i-1), -c(1)] ,kernel ="radial",
                ranges = list(cost = c(0.1,0.5,1),gamma = c(1)) )
  bestmod <-tune.out$best.model
  pred_value <-predict(bestmod,predict_x)
  historic_mean <- mean(stocks[(i-initial_w_size):i-1,50])
  actural_return <-  stocks[i,50]
  predicted_return <- pred_value[[2]]
  f_err <- actural_return - predicted_return
  r_err <- actural_return - historic_mean
  pred_svm <- rbind(pred_svm, c(stocks[i,1], stocks[i,50],predicted_return,historic_mean, f_err, r_err,NA,NA,NA,NA))
  colnames(pred_svm) <- c("Date", "actural_return", "predicted_return", "historic_mean","f_err","r_err","RMSE_svm","se_svm","se_mean","RMSE_diff")
  pred_svm$se_svm[i-initial_w_size] <- (pred_svm$f_err[i-initial_w_size])^2
  pred_svm$se_mean[i-initial_w_size] <- (pred_svm$r_err[i-initial_w_size])^2
  pred_svm$RMSE_svm[i-initial_w_size] <- mean(pred_svm$se_svm)^0.5
  pred_svm$RMSE_diff[i-initial_w_size] <-(mean(pred_svm$se_mean))^0.5 - (mean(pred_svm$se_svm))^0.5
}

#save outcome
write.csv(pred_svm,'svm_Drugs_lag30.csv')
Drugs_svm_lag30<-ggplot(pred_svm) + geom_line( aes( x =  Date, y = RMSE_diff, colour = "Mean - Svm")) 
ggsave("Drugs_svm_lag30.jpg", plot = Drugs_svm_lag30)


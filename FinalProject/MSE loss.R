setwd("C:\\Users\\Billy Tian\\Desktop\\BIS557 project")
ledata <- read.csv("Life Expectancy Data.csv", header=T)
ledata <- ledata[complete.cases(ledata$Life.expectancy),]
set.seed(557)
test.id <- sample(1:2928, 700)

library(reticulate)
library(keras)
library(tensorflow)
library(fastDummies)

country <- ledata[,1]
cat.data <- dummy_cols(ledata, select_columns = 'Country')
cat.country <- cat.data[,-(1:22)]
cat.country <- cat.country[,-c(40, 50, 106, 111, 118, 125, 129, 143, 147, 179)]

#for (i in 1:ncol(cat.country)){
#  if (sum(cat.country[,i])<1){
#    print(i)
#    print(sum(cat.country[,i]))
#  }
#}

#Get train/test Y
test.ledata <- ledata[test.id,]
train.ledata <- ledata[-test.id,]
train.y <- train.ledata[,4]
test.y <- test.ledata[,4]

X.data <- ledata[,-c(1, 4)]
X.data$Year <- X.data$Year-2000
X.data <- cbind(X.data, cat.country)

X.data$infant.deaths[X.data$infant.deaths==0 | X.data$infant.deaths==1] <- NA
X.data$infant.deaths[is.na(X.data$infant.deaths)] <- mean(X.data$infant.deaths, na.rm=T)

X.data$percentage.expenditure[X.data$percentage.expenditure==0] <- NA
X.data$percentage.expenditure[is.na(X.data$percentage.expenditure)] <- mean(X.data$percentage.expenditure, na.rm=T)

X.data$Measles[X.data$Measles==0] <- NA
X.data$Measles[is.na(X.data$Measles)] <- mean(X.data$Measles, na.rm=T)

X.data$under.five.deaths[X.data$under.five.deaths==0 | X.data$under.five.deaths==1] <- NA
X.data$under.five.deaths[is.na(X.data$under.five.deaths)] <- mean(X.data$under.five.deaths, na.rm=T)

X.data$Income.composition.of.resources[X.data$Income.composition.of.resources==0] <- NA
X.data$Income.composition.of.resources[is.na(X.data$Income.composition.of.resources)] <- mean(X.data$Income.composition.of.resources, na.rm=T)

X.data$Schooling[X.data$Schooling==0] <- NA
X.data$Schooling[is.na(X.data$Schooling)] <- mean(X.data$Schooling, na.rm=T)

X.data$Status <- ifelse(X.data$Status=="Developing", 0, 1)
X.data$Alcohol[is.na(X.data$Alcohol)] <- mean(X.data$Alcohol, na.rm=T)
X.data$Hepatitis.B[is.na(X.data$Hepatitis.B)] <- mean(X.data$Hepatitis.B, na.rm=T)
X.data$Total.expenditure[is.na(X.data$Total.expenditure)] <- mean(X.data$Total.expenditure, na.rm=T)
X.data$GDP[is.na(X.data$GDP)] <- mean(X.data$GDP, na.rm=T)
X.data$Population[is.na(X.data$Population)] <- mean(X.data$Population, na.rm=T)
X.data$BMI[is.na(X.data$BMI)] <- mean(X.data$BMI, na.rm=T)
X.data$Polio[is.na(X.data$Polio)] <- mean(X.data$Polio, na.rm=T)
X.data$Diphtheria[is.na(X.data$Diphtheria)] <- mean(X.data$Diphtheria, na.rm=T)
X.data$thinness..1.19.years[is.na(X.data$thinness..1.19.years)] <- mean(X.data$thinness..1.19.years, na.rm=T)
X.data$thinness.5.9.years[is.na(X.data$thinness.5.9.years)] <- mean(X.data$thinness.5.9.years, na.rm=T)



#train.X <- train.X[,-2]
#test.X <- test.X[,-2]
#train.X <- train.X[complete.cases(train.X),]
#test.X <- train.X[complete.cases(test.X),]

#Normalize input data
#mean <- apply(train.X, 2, function(x) mean(x,na.rm=T))
#std <- apply(train.X, 2, function(x) sd(x,na.rm=T))
#train_data <- scale(train.X, center=mean, scale=sd) 
#test_data <- scale(test.X, center=mean, scale=sd) 

X.data$Year <- (X.data$Year-mean(X.data$Year))/sd(X.data$Year)
X.data$Adult.Mortality <- (X.data$Adult.Mortality-mean(X.data$Adult.Mortality))/sd(X.data$Adult.Mortality)
X.data$infant.deaths <- (X.data$infant.deaths-mean(X.data$infant.deaths))/sd(X.data$infant.deaths)
X.data$Alcohol <- (X.data$Alcohol-mean(X.data$Alcohol))/sd(X.data$Alcohol)
X.data$percentage.expenditure <- (X.data$percentage.expenditure-mean(X.data$percentage.expenditure))/sd(X.data$percentage.expenditure)
X.data$Hepatitis.B <- (X.data$Hepatitis.B-mean(X.data$Hepatitis.B))/sd(X.data$Hepatitis.B)
X.data$Measles <- (X.data$Measles-mean(X.data$Measles))/sd(X.data$Measles)
X.data$BMI <- (X.data$BMI-mean(X.data$BMI))/sd(X.data$BMI)
X.data$under.five.deaths <- (X.data$under.five.deaths-mean(X.data$under.five.deaths))/sd(X.data$under.five.deaths)
X.data$Polio <- (X.data$Polio-mean(X.data$Polio))/sd(X.data$Polio)
X.data$Total.expenditure <- (X.data$Total.expenditure-mean(X.data$Total.expenditure))/sd(X.data$Total.expenditure)
X.data$Diphtheria <- (X.data$Diphtheria -mean(X.data$Diphtheria ))/sd(X.data$Diphtheria)
X.data$HIV.AIDS <- (X.data$HIV.AIDS-mean(X.data$HIV.AIDS))/sd(X.data$HIV.AIDS)
X.data$GDP <- (X.data$GDP -mean(X.data$GDP))/sd(X.data$GDP)
X.data$Population <- (X.data$Population -mean(X.data$Population ))/sd(X.data$Population )
X.data$thinness..1.19.years <- (X.data$thinness..1.19.years -mean(X.data$thinness..1.19.years))/sd(X.data$thinness..1.19.years)
X.data$thinness.5.9.years <- (X.data$thinness.5.9.years-mean(X.data$thinness.5.9.years))/sd(X.data$thinness.5.9.years)
X.data$Schooling <- (X.data$Schooling -mean(X.data$Schooling))/sd(X.data$Schooling)
X.data$Income.composition.of.resources <- (X.data$Income.composition.of.resources-mean(X.data$Income.composition.of.resources))/sd(X.data$Income.composition.of.resources)

train.X <- X.data[-test.id,]
#train.Status <- train.X[,2]
test.X <- X.data[test.id,]
#test.Status <- test.X[,2]

X <- matrix(0, nrow=2228, ncol=203)
for (i in 1:203){
  X[,i] <- as.numeric(train.X[,i])
}

test.data <- matrix(0, nrow=700, ncol=203)
for (i in 1:203){
  test.data[,i] <- as.numeric(test.X[,i])
}


#train.y <- cbind(as.numeric(train.y<=63), 
#                 as.numeric(train.y>63 & train.y<=72), 
#                 as.numeric(train.y>72 & train.y<=75.5), 
#                 as.numeric(train.y>75.5))

#x1 <- as.numeric(train.X[,1])
#x2 <- as.numeric(train.X[,2])
#x3 <- as.numeric(train.X[,3])
#x4 <- as.numeric(train.X[,4])
#x5 <- as.numeric(train.X[,5])
#x6 <- as.numeric(train.X[,6])
#x7 <- as.numeric(train.X[,7])
#x8 <- as.numeric(train.X[,8])
#x9 <- as.numeric(train.X[,9])
#x10 <- as.numeric(train.X[,10])
#x11 <- as.numeric(train.X[,11])
#x12 <- as.numeric(train.X[,12])
#x1 <- c(rep(1, 1114), rep(0, 1114))
#x2 <- rnorm(2228, 0, 2)
#x3 <- runif(2228, 0, 1)
#x4 <- x3*x2
#x5 <- x1^2+x2^3
#X <- cbind(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)




model <- keras_model_sequential() %>%
  
  # Network architecture
  layer_dense(units = 64, activation = "relu", input_shape = ncol(X)) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 4, activation = "softmax") %>%
  
  # Backpropagation
  compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
  )

fit1 <- model %>%
  fit(
    x = X,
    y = train.y,
    epochs = 100,
    batch_size = 64,
    validation_split = 0.2,
    verbose = 1
  )


# Display output
fit1
plot(fit1)




model1 <- keras_model_sequential() %>%
  layer_dense(units = 256, activation = "relu",
              input_shape = dim(X)[[2]]) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1)
model1 %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = c("mae")
)

fit1 <- model1 %>% fit(
  x = X,
  y = train.y,
  epochs = 250,
  batch_size = 64,
  validation_split = 0.2,
  view_metrics=T,
  verbose=1
)

#loss: 2.2571 - mae: 1.0391 - val_loss: 8.9447 - val_mae: 2.1320

model1 <- keras_model_sequential() %>%
  layer_dense(units = 256, activation = "relu",
              input_shape = dim(X)[[2]],
              kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dense(units = 64, activation = "relu",
              kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dense(units = 1)
model1 %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = c("mae")
)
#0.001, loss: 2.5713 - mae: 1.0732 - val_loss: 10.1609 - val_mae: 2.3090
#0.01, loss: 3.5741 - mae: 0.9805 - val_loss: 12.5373 - val_mae: 2.3748

model1 <- keras_model_sequential() %>%
  layer_dense(units = 256, activation = "relu",
              input_shape = dim(X)[[2]],
              kernel_regularizer = regularizer_l1(0.001)) %>%
  layer_dense(units = 64, activation = "relu",
              kernel_regularizer = regularizer_l1(0.001)) %>%
  layer_dense(units = 1)
model1 %>% compile(
  optimizer = "rmsprop",
  loss = "mse",
  metrics = c("mae")
)
#loss: 5.4008 - mae: 1.5132 - val_loss: 16.0705 - val_mae: 2.9903

model1 <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu",
              input_shape = dim(X)[[2]],
              kernel_regularizer = regularizer_l1_l2(0.001)) %>%
  layer_dense(units = 64, activation = "relu",
              kernel_regularizer = regularizer_l1_l2(0.001)) %>%
  layer_dense(units = 1)
model1 %>% compile(
  optimizer = "rmsprop",
  loss = "mse",
  metrics = c("mae")
)

model1 <- keras_model_sequential() %>%
  layer_dense(units = 256, activation = "relu",
              input_shape = dim(X)[[2]]) %>%
  layer_dropout(rate=0.1) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(rate=0.1) %>%
  layer_dense(units = 1)
model1 %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = c("mae")
)

#0.1:loss: 24.7671 - mae: 3.9371 - val_loss: 10.6596 - val_mae: 2.3767

fit1 <- model1 %>% fit(
  x = X,
  y = train.y,
  epochs = 250,
  batch_size = 64,
  validation_split = 0.2,
  view_metrics=T,
  verbose=1
)


fit2.df <- data.frame(fit2)

# Display output
fit2$params
plot(fit2)




build_model0 <- function(){
  model <- keras_model_sequential() %>%
    layer_dense(units = 256, activation = "relu",
                input_shape = dim(X)[[2]]) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  model %>% compile(
    optimizer = "adam",
    loss = "mse",
    metrics = c("mae")
  )
}

build_model1 <- function(){
  model <- keras_model_sequential() %>%
    layer_dense(units = 256, activation = "relu",
                kernel_regularizer = regularizer_l1(0.001),
                input_shape = dim(X)[[2]]) %>%
    layer_dense(units = 64, activation = "relu",
                kernel_regularizer = regularizer_l1(0.001)) %>%
    layer_dense(units = 1)
  model %>% compile(
    optimizer = "adam",
    loss = "mse",
    metrics = c("mae")
  )
}

build_model2 <- function(){
  model <- keras_model_sequential() %>%
    layer_dense(units = 256, activation = "relu",
                kernel_regularizer = regularizer_l2(0.001),
                input_shape = dim(X)[[2]]) %>%
    layer_dense(units = 64, activation = "relu",
                kernel_regularizer = regularizer_l2(0.001)) %>%
    layer_dense(units = 1)
  model %>% compile(
    optimizer = "adam",
    loss = "mse",
    metrics = c("mae")
  )
}

build_model3 <- function(){
  model <- keras_model_sequential() %>%
    layer_dense(units = 256, activation = "relu",
                kernel_regularizer = regularizer_l1_l2(l1=0.001, l2=0.001),
                input_shape = dim(X)[[2]]) %>%
    layer_dense(units = 64, activation = "relu",
                kernel_regularizer = regularizer_l1_l2(l1=0.001, l2=0.001)) %>%
    layer_dense(units = 1)
  model %>% compile(
    optimizer = "adam",
    loss = "mse",
    metrics = c("mae")
  )
}

build_model4 <- function(){
  model <- keras_model_sequential() %>%
    layer_dense(units = 256, activation = "relu",
                input_shape = dim(X)[[2]]) %>%
    layer_dropout(rate=0.1) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dropout(rate=0.1) %>%
    layer_dense(units = 1)
  model %>% compile(
    optimizer = "adam",
    loss = "mse",
    metrics = c("mae")
  )
}

build_model5 <- function(){
  model <- keras_model_sequential() %>%
    layer_dense(units = 256, activation = "relu",
                input_shape = dim(X)[[2]]) %>%
    layer_dropout(rate=0.3) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dropout(rate=0.3) %>%
    layer_dense(units = 1)
  model %>% compile(
    optimizer = "adam",
    loss = "mse",
    metrics = c("mae")
  )
}

build_model6 <- function(){
  model <- keras_model_sequential() %>%
    layer_dense(units = 256, activation = "relu",
                input_shape = dim(X)[[2]], constraint_unitnorm(axis = 0)) %>%
    layer_dense(units = 64, activation = "relu", constraint_unitnorm(axis = 0)) %>%
    layer_dense(units = 1)
  model %>% compile(
    optimizer = "adam",
    loss = "mse",
    metrics = c("mae")
  )
}

build_model7 <- function(){
  model <- keras_model_sequential() %>%
    layer_dense(units = 256, activation = "relu",
                input_shape = dim(X)[[2]],
                kernel_regularizer = regularizer_l1_l2(l1=0.001, l2=0.001)) %>%
    layer_dropout(rate=0.1) %>%
    layer_dense(units = 64, activation = "relu",
                kernel_regularizer = regularizer_l1_l2(l1=0.001, l2=0.001)) %>%
    layer_dropout(rate=0.1) %>%
    layer_dense(units = 1)
  model %>% compile(
    optimizer = "adam",
    loss = "mse",
    metrics = c("mae")
  )
}


set.seed(557)
k <- 5
indices <- sample(1:nrow(X))
folds <- cut(indices, breaks = k, labels = FALSE)
num_epochs <- 500
# model 0
all_mae_histories <- NULL
set.seed(557)
for (i in 1:k) {
  cat("processing fold #", i, "\n")
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- X[val_indices,]
  val_targets <- train.y[val_indices]
  partial_train_data <- X[-val_indices,]
  partial_train_targets <- train.y[-val_indices]
  model <- build_model0()
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 64, verbose = 0
  )
  mae_history <- history$metrics$val_mae
  all_mae_histories <- rbind(all_mae_histories, mae_history)
}

avg_mae0 <- data.frame(epoch = seq(1:ncol(all_mae_histories)), val_mae = apply(all_mae_histories, 2, mean))

# model 1
all_mae_histories <- NULL
set.seed(557)
for (i in 1:k) {
  cat("processing fold #", i, "\n")
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- X[val_indices,]
  val_targets <- train.y[val_indices]
  partial_train_data <- X[-val_indices,]
  partial_train_targets <- train.y[-val_indices]
  model <- build_model1()
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 64, verbose = 0
  )
  mae_history <- history$metrics$val_mae
  all_mae_histories <- rbind(all_mae_histories, mae_history)
}

avg_mae1 <- data.frame(epoch = seq(1:ncol(all_mae_histories)), val_mae = apply(all_mae_histories, 2, mean))


# model 2
all_mae_histories <- NULL
set.seed(557)
for (i in 1:k) {
  cat("processing fold #", i, "\n")
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- X[val_indices,]
  val_targets <- train.y[val_indices]
  partial_train_data <- X[-val_indices,]
  partial_train_targets <- train.y[-val_indices]
  model <- build_model2()
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 64, verbose = 0
  )
  mae_history <- history$metrics$val_mae
  all_mae_histories <- rbind(all_mae_histories, mae_history)
}

avg_mae2 <- data.frame(epoch = seq(1:ncol(all_mae_histories)), val_mae = apply(all_mae_histories, 2, mean))


# model 3
all_mae_histories <- NULL
set.seed(557)
for (i in 1:k) {
  cat("processing fold #", i, "\n")
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- X[val_indices,]
  val_targets <- train.y[val_indices]
  partial_train_data <- X[-val_indices,]
  partial_train_targets <- train.y[-val_indices]
  model <- build_model3()
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 64, verbose = 0
  )
  mae_history <- history$metrics$val_mae
  all_mae_histories <- rbind(all_mae_histories, mae_history)
}

avg_mae3 <- data.frame(epoch = seq(1:ncol(all_mae_histories)), val_mae = apply(all_mae_histories, 2, mean))


# model 4
all_mae_histories <- NULL
set.seed(557)
for (i in 1:k) {
  cat("processing fold #", i, "\n")
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- X[val_indices,]
  val_targets <- train.y[val_indices]
  partial_train_data <- X[-val_indices,]
  partial_train_targets <- train.y[-val_indices]
  model <- build_model4()
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 64, verbose = 0
  )
  mae_history <- history$metrics$val_mae
  all_mae_histories <- rbind(all_mae_histories, mae_history)
}

avg_mae4 <- data.frame(epoch = seq(1:ncol(all_mae_histories)), val_mae = apply(all_mae_histories, 2, mean))


# model 5
all_mae_histories <- NULL
set.seed(557)
for (i in 1:k) {
  cat("processing fold #", i, "\n")
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- X[val_indices,]
  val_targets <- train.y[val_indices]
  partial_train_data <- X[-val_indices,]
  partial_train_targets <- train.y[-val_indices]
  model <- build_model5()
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 64, verbose = 0
  )
  mae_history <- history$metrics$val_mae
  all_mae_histories <- rbind(all_mae_histories, mae_history)
}

avg_mae5 <- data.frame(epoch = seq(1:ncol(all_mae_histories)), val_mae = apply(all_mae_histories, 2, mean))

# model 6
all_mae_histories <- NULL
set.seed(557)
for (i in 1:k) {
  cat("processing fold #", i, "\n")
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- X[val_indices,]
  val_targets <- train.y[val_indices]
  partial_train_data <- X[-val_indices,]
  partial_train_targets <- train.y[-val_indices]
  model <- build_model6()
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 64, verbose = 0
  )
  mae_history <- history$metrics$val_mae
  all_mae_histories <- rbind(all_mae_histories, mae_history)
}

avg_mae6 <- data.frame(epoch = seq(1:ncol(all_mae_histories)), val_mae = apply(all_mae_histories, 2, mean))


# model 7
all_mae_histories <- NULL
set.seed(557)
for (i in 1:k) {
  cat("processing fold #", i, "\n")
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- X[val_indices,]
  val_targets <- train.y[val_indices]
  partial_train_data <- X[-val_indices,]
  partial_train_targets <- train.y[-val_indices]
  model <- build_model7()
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 64, verbose = 0
  )
  mae_history <- history$metrics$val_mae
  all_mae_histories <- rbind(all_mae_histories, mae_history)
}

avg_mae7 <- data.frame(epoch = seq(1:ncol(all_mae_histories)), val_mae = apply(all_mae_histories, 2, mean))


#library(ggplot2)
#ggplot(average_mae_history, aes(x = epoch, y = validation_mae)) + geom_line()

#p0 <- ggplot(avg_mae3, aes(x = epoch, y = val_mae)) + geom_smooth()
#p0


x_epoch <- avg_mae0[,1]
val_mae0 <- avg_mae0[,2]
val_mae1 <- avg_mae1[,2]
val_mae2 <- avg_mae2[,2]
val_mae3 <- avg_mae3[,2]
val_mae4 <- avg_mae4[,2]
val_mae5 <- avg_mae5[,2]
val_mae6 <- avg_mae6[,2]
val_mae7 <- avg_mae7[,2]
#keep.ind <- seq(1,150,1)

plot(x_epoch, val_mae0, type="l", col="navy", lty=1, ylim=c(0,4), lwd=2)
lines(x_epoch, val_mae1, type="l", col="red", lty=1, lwd=2)
lines(x_epoch, val_mae2, type="l", col="green", lty=1, lwd=2)
lines(x_epoch, val_mae3, type="l", col="lightblue", lty=1, lwd=2)
lines(x_epoch, val_mae4, type="l", col="orange", lty=1, lwd=2)

legend("bottomleft", legend = c("Baseline", "L1-Regularization", "L2-Regularization", "Elastic Net", "Dropout"), 
       col=c("navy", "red", "green", "lightblue", "orange"), lty=c(1, 1, 1, 1, 1), cex=0.5, lwd=2, horiz = F)


#ggplot
plotdata <- data.frame(cbind(x_epoch, val_mae0, val_mae1, val_mae2, val_mae3, val_mae4, val_mae5, val_mae6, val_mae7))
ggplot(plotdata, aes(x=x_epoch)) + 
  geom_line(aes(y = val_mae0), color = "red", size=1) + 
  geom_line(aes(y = val_mae1), color = "navy", size=1) +
  geom_line(aes(y = val_mae2), color = "green", size=1) +
  geom_line(aes(y = val_mae3), color = "blue", size=1) +
  geom_line(aes(y = val_mae4), color = "pink", size=1) +
  geom_line(aes(y = val_mae5), color = "orange", size=1) +
  geom_line(aes(y = val_mae6), color = "purple", size=1) +
  geom_line(aes(y = val_mae7), color = "yellow", size=1) +
  ylim(1, 4) + xlim(0, 500) +
  labs(title="Validation MAE by Methods", x="Epoch", y="Mean Absolute Error")

ggplot(plotdata, aes(x = x_epoch, y = val_mae3)) + geom_smooth()


model <- build_model()
model %>% fit(X, train.y,
              epochs = 30, batch_size = 20, verbose = 1)
result <- model %>% evaluate(test_data, test_targets)
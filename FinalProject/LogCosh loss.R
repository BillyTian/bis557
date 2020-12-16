#TRY LOGCOSH LOSS

logcosh_model0 <- function(){
  model <- keras_model_sequential() %>%
    layer_dense(units = 256, activation = "relu",
                input_shape = dim(X)[[2]]) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  model %>% compile(
    optimizer = "adam",
    loss = "logcosh",
    metrics = c("mae")
  )
}

logcosh_model1 <- function(){
  model <- keras_model_sequential() %>%
    layer_dense(units = 256, activation = "relu",
                kernel_regularizer = regularizer_l1(0.001),
                input_shape = dim(X)[[2]]) %>%
    layer_dense(units = 64, activation = "relu",
                kernel_regularizer = regularizer_l1(0.001)) %>%
    layer_dense(units = 1)
  model %>% compile(
    optimizer = "adam",
    loss = "logcosh",
    metrics = c("mae")
  )
}

logcosh_model2 <- function(){
  model <- keras_model_sequential() %>%
    layer_dense(units = 256, activation = "relu",
                kernel_regularizer = regularizer_l2(0.001),
                input_shape = dim(X)[[2]]) %>%
    layer_dense(units = 64, activation = "relu",
                kernel_regularizer = regularizer_l2(0.001)) %>%
    layer_dense(units = 1)
  model %>% compile(
    optimizer = "adam",
    loss = "logcosh",
    metrics = c("mae")
  )
}

logcosh_model3 <- function(){
  model <- keras_model_sequential() %>%
    layer_dense(units = 256, activation = "relu",
                kernel_regularizer = regularizer_l1_l2(l1=0.001, l2=0.001),
                input_shape = dim(X)[[2]]) %>%
    layer_dense(units = 64, activation = "relu",
                kernel_regularizer = regularizer_l1_l2(l1=0.001, l2=0.001)) %>%
    layer_dense(units = 1)
  model %>% compile(
    optimizer = "adam",
    loss = "logcosh",
    metrics = c("mae")
  )
}

logcosh_model4 <- function(){
  model <- keras_model_sequential() %>%
    layer_dense(units = 256, activation = "relu",
                input_shape = dim(X)[[2]]) %>%
    layer_dropout(rate=0.1) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dropout(rate=0.1) %>%
    layer_dense(units = 1)
  model %>% compile(
    optimizer = "adam",
    loss = "logcosh",
    metrics = c("mae")
  )
}

logcosh_model5 <- function(){
  model <- keras_model_sequential() %>%
    layer_dense(units = 256, activation = "relu",
                input_shape = dim(X)[[2]]) %>%
    layer_dropout(rate=0.3) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dropout(rate=0.3) %>%
    layer_dense(units = 1)
  model %>% compile(
    optimizer = "adam",
    loss = "logcosh",
    metrics = c("mae")
  )
}

logcosh_model6 <- function(){
  model <- keras_model_sequential() %>%
    layer_dense(units = 256, activation = "relu",
                input_shape = dim(X)[[2]], constraint_unitnorm(axis = 0)) %>%
    layer_dense(units = 64, activation = "relu", constraint_unitnorm(axis = 0)) %>%
    layer_dense(units = 1)
  model %>% compile(
    optimizer = "adam",
    loss = "logcosh",
    metrics = c("mae")
  )
}

#L1L2+Dropout
logcosh_model7 <- function(){
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
    loss = "logcosh",
    metrics = c("mae")
  )
}

l1l2cons <- function(){
  model <- keras_model_sequential() %>%
    layer_dense(units = 256, activation = "relu",
                input_shape = dim(X)[[2]],
                kernel_regularizer = regularizer_l1_l2(l1=0.001, l2=0.001), 
                constraint_unitnorm(axis = 0)) %>%
    layer_dense(units = 64, activation = "relu",
                kernel_regularizer = regularizer_l1_l2(l1=0.001, l2=0.001), 
                constraint_unitnorm(axis = 0)) %>%
    layer_dense(units = 1)
  model %>% compile(
    optimizer = "adam",
    loss = "logcosh",
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
  model <- logcosh_model0()
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 64, verbose = 0
  )
  mae_history <- history$metrics$val_mae
  all_mae_histories <- rbind(all_mae_histories, mae_history)
}

logcosh_avg_mae0 <- data.frame(epoch = seq(1:ncol(all_mae_histories)), val_mae = apply(all_mae_histories, 2, mean))

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
  model <- logcosh_model1()
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 64, verbose = 0
  )
  mae_history <- history$metrics$val_mae
  all_mae_histories <- rbind(all_mae_histories, mae_history)
}

logcosh_avg_mae1 <- data.frame(epoch = seq(1:ncol(all_mae_histories)), val_mae = apply(all_mae_histories, 2, mean))


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
  model <- logcosh_model2()
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 64, verbose = 0
  )
  mae_history <- history$metrics$val_mae
  all_mae_histories <- rbind(all_mae_histories, mae_history)
}

logcosh_avg_mae2 <- data.frame(epoch = seq(1:ncol(all_mae_histories)), val_mae = apply(all_mae_histories, 2, mean))


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
  model <- logcosh_model3()
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 64, verbose = 0
  )
  mae_history <- history$metrics$val_mae
  all_mae_histories <- rbind(all_mae_histories, mae_history)
}

logcosh_avg_mae3 <- data.frame(epoch = seq(1:ncol(all_mae_histories)), val_mae = apply(all_mae_histories, 2, mean))


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
  model <- logcosh_model4()
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 64, verbose = 0
  )
  mae_history <- history$metrics$val_mae
  all_mae_histories <- rbind(all_mae_histories, mae_history)
}

logcosh_avg_mae4 <- data.frame(epoch = seq(1:ncol(all_mae_histories)), val_mae = apply(all_mae_histories, 2, mean))


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
  model <- logcosh_model5()
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 64, verbose = 0
  )
  mae_history <- history$metrics$val_mae
  all_mae_histories <- rbind(all_mae_histories, mae_history)
}

logcosh_avg_mae5 <- data.frame(epoch = seq(1:ncol(all_mae_histories)), val_mae = apply(all_mae_histories, 2, mean))

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
  model <- logcosh_model6()
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 64, verbose = 0
  )
  mae_history <- history$metrics$val_mae
  all_mae_histories <- rbind(all_mae_histories, mae_history)
}

logcosh_avg_mae6 <- data.frame(epoch = seq(1:ncol(all_mae_histories)), val_mae = apply(all_mae_histories, 2, mean))


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
  model <- logcosh_model7()
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 64, verbose = 0
  )
  mae_history <- history$metrics$val_mae
  all_mae_histories <- rbind(all_mae_histories, mae_history)
}

logcosh_avg_mae7 <- data.frame(epoch = seq(1:ncol(all_mae_histories)), val_mae = apply(all_mae_histories, 2, mean))


# composite: l1l2 cons
all_mae_histories <- NULL
set.seed(557)
for (i in 1:k) {
  cat("processing fold #", i, "\n")
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- X[val_indices,]
  val_targets <- train.y[val_indices]
  partial_train_data <- X[-val_indices,]
  partial_train_targets <- train.y[-val_indices]
  model <- l1l2cons()
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 64, verbose = 0
  )
  mae_history <- history$metrics$val_mae
  all_mae_histories <- rbind(all_mae_histories, mae_history)
}

avg_mae_l1l2cons <- data.frame(epoch = seq(1:ncol(all_mae_histories)), val_mae = apply(all_mae_histories, 2, mean))


x_epoch <- logcosh_avg_mae0[,1]
logcosh_val_mae0 <- logcosh_avg_mae0[,2]
logcosh_val_mae1 <- logcosh_avg_mae1[,2]
logcosh_val_mae2 <- logcosh_avg_mae2[,2]
logcosh_val_mae3 <- logcosh_avg_mae3[,2]
logcosh_val_mae4 <- logcosh_avg_mae4[,2]
logcosh_val_mae5 <- logcosh_avg_mae5[,2]
logcosh_val_mae6 <- logcosh_avg_mae6[,2]
logcosh_val_mae7 <- logcosh_avg_mae7[,2]
l1l2consval_mae <- avg_mae_l1l2cons[,2]


#ggplot
logcosh_plotdata <- data.frame(cbind(x_epoch, 
                                     logcosh_val_mae0, 
                                     logcosh_val_mae1, 
                                     logcosh_val_mae2, 
                                     logcosh_val_mae3, 
                                     logcosh_val_mae4, 
                                     logcosh_val_mae5, 
                                     logcosh_val_mae6,
                                     logcosh_val_mae7))
ggplot(logcosh_plotdata, aes(x=x_epoch)) + 
  geom_line(aes(y = logcosh_val_mae0), color = "red", size=1) + 
  geom_line(aes(y = logcosh_val_mae1), color = "navy", size=1) +
  geom_line(aes(y = logcosh_val_mae2), color = "green", size=1) +
  geom_line(aes(y = logcosh_val_mae3), color = "blue", size=1) +
  geom_line(aes(y = logcosh_val_mae4), color = "pink", size=1) +
  geom_line(aes(y = logcosh_val_mae5), color = "orange", size=1) +
  geom_line(aes(y = logcosh_val_mae6), color = "purple", size=1) +
  geom_line(aes(y = logcosh_val_mae7), color = "yellow", size=1) +
  ylim(1, 4) + xlim(0, 500) +
  labs(title="Validation MAE by Methods", x="Epoch", y="Mean Absolute Error")


set.seed(557)
model1 <- l1l2cons()
model1 %>% fit(X, train.y,
              epochs = 500, batch_size = 64, verbose = 1)
result1 <- model1 %>% evaluate(test.data, test.y)
result1

set.seed(557)
model2 <- build_model3()
model2 %>% fit(X, train.y,
               epochs = 500, batch_size = 64, verbose = 1)
result2 <- model2 %>% evaluate(test.data, test.y)
result2

set.seed(557)
model3 <- build_model7()
model3 %>% fit(X, train.y,
               epochs = 500, batch_size = 64, verbose = 1)
result3 <- model3 %>% evaluate(test.data, test.y)
result3

#Big graph method comparison (only dropout 0.1)
pdata <- rbind(avg_mae0, avg_mae1, avg_mae2, avg_mae3, avg_mae4, avg_mae6)
pdata <- cbind(pdata, Method)

library(ggplot2)
ggplot(pdata, aes(x=epoch, y=val_mae, color=Method)) +
  geom_line(size=0.8)+
  ylim(1, 4) + xlim(0, 500) +
  labs(title="Validation MAE by Methods (MSE Loss)", x="Epoch", y="Mean Absolute Error")


#Big graph method comparison (only dropout 0.1)
logcosh_pdata <- rbind(logcosh_avg_mae0, logcosh_avg_mae1, logcosh_avg_mae2, logcosh_avg_mae3, logcosh_avg_mae4, logcosh_avg_mae6)
Method <- c(rep("Baseline", 500), rep("L1-Penalty", 500), rep("L2-Penalty", 500), rep("Elastic Net", 500), rep("Dropout", 500), rep("Weight Constraint", 500))
logcosh_pdata <- cbind(logcosh_pdata, Method)

ggplot(logcosh_pdata, aes(x=epoch, y=val_mae, color=Method)) +
  geom_line(size=0.8)+
  ylim(1, 4) + xlim(0, 500) +
  labs(title="Validation MAE by Methods (Logcosh Loss)", x="Epoch", y="Mean Absolute Error")


#Composite vs. best(l1l2)
comp_pdata <- rbind(logcosh_avg_mae4, logcosh_avg_mae3, avg_mae_l1l2cons, logcosh_avg_mae7)
Method <- c(rep("Dropout", 500), rep("Elastic Net (EN)", 500), rep("EN + Weight Constraint", 500), rep("EN + Dropout", 500))
comp_pdata <- cbind(comp_pdata, Method)
ggplot(comp_pdata, aes(x=epoch, y=val_mae, color=Method)) +
  geom_line(size=0.8)+
  ylim(1, 4) + xlim(0, 500) +
  labs(title="Composite Methods Comparison", x="Epoch", y="Mean Absolute Error")


#Same method under different loss functions
en_pdata <- rbind(logcosh_avg_mae3, avg_mae3)
Method <- c(rep("Logcosh Loss", 500), rep("MSE Loss", 500))
en_pdata <- cbind(en_pdata, Method)
ggplot(en_pdata, aes(x=epoch, y=val_mae, color=Method)) +
  geom_line(size=0.8)+
  ylim(1, 4) + xlim(0, 500) +
  labs(title="Loss Comparison Example: Elastic Net", x="Epoch", y="Mean Absolute Error")

do_pdata <- rbind(logcosh_avg_mae4, avg_mae4)
Method <- c(rep("Logcosh Loss", 500), rep("MSE Loss", 500))
do_pdata <- cbind(do_pdata, Method)
ggplot(do_pdata, aes(x=epoch, y=val_mae, color=Method)) +
  geom_line(size=0.8)+
  ylim(1, 4) + xlim(0, 500) +
  labs(title="Loss Comparison Example: Dropout", x="Epoch", y="Mean Absolute Error")



reg.data <- data.frame(cbind(train.y, X))
form <- train.y ~.
reg.model <- lm(form, data=reg.data)
reg.test.data <- data.frame(test.data)
names(reg.test.data) <- paste0("V", 2:204)
pred.y <- predict(reg.model, reg.test.data, type="response")
reg.mae <- mean(abs(test.y-pred.y))
reg.mae

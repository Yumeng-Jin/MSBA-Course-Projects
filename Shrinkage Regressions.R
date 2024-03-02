# setwd("/Users/yumi/ucdavis/Winter Quarter/BAX-442/HW4")
rm(list = ls(all = TRUE)) #clear data
library(glmnet)

# Load the dataset
cars_data <- read.csv('Cars_Data.csv')

x <- as.matrix(cars_data[,2:16])
y <- as.matrix(cars_data[,17])
n = nrow(cars_data)

## Partition training and test data sets
m <- 0.8*n  # 80% training size ==> 20% holdout sample
ts <- sample(1:n,m)  # random draw 80 rows out of 100 rows
x.train <-  x[ts,]
y.train <-  y[ts]
x.test <- x[-ts,]
y.test <- y[-ts]

# Define a sequence of alpha values
alpha_seq <- seq(0, 1, by = 0.1)


### Elastic Net ###

elastic_alpha <- seq(0.1, 0.9, by = 0.1)

# Initialize a data frame to store MSE for each alpha value
results <- data.frame(alpha = elastic_alpha, mse = rep(NA, length(elastic_alpha)),  lambda.min = rep(NA, length(elastic_alpha)))

# Loop over alpha values

for(i in 1:length(elastic_alpha)) {
  # Fit the elastic net model using CV
  cv.elastic <- cv.glmnet(x, y, alpha = elastic_alpha[i], type.measure = "mse", nfolds = 10)
  # Extract the MSE at lambda.min for the current alpha
  lambda_min_elastic = cv.elastic$lambda.min
  mse_at_min_lambda_elastic <- cv.elastic$cvm[cv.elastic$lambda == cv.elastic$lambda.min]
  # Store the result
  results$lambda.min[i] <- mse_at_min_lambda_elastic
  results$mse[i] <- mse_at_min_lambda_elastic
}

# Find the alpha value with the minimum MSE
best_alpha <- results$alpha[which.min(results$mse)]
best_lambda_elastic <- results$lambda.min[which.min(results$mse)]
print(paste("The best alpha value for elastic net model is:", best_alpha))
print(paste("The corresponding lambda.min is:", best_lambda_elastic))

# find the top 3 most significant variables
ones <- as.matrix(rep(1, n), n, 1)  # vector of 1s for intercept
xx.s <- scale(x, center = TRUE, scale = TRUE)
xx <- cbind(ones, xx.s)  # xx matrix with intercept
k_elastic <- best_lambda_elastic  # k is best lambda from lasso regression
p = ncol(x)
xpx <- t(xx) %*% xx + k_elastic*diag(p+1)  
# X'X matrix with Lasso Penalty: X'X + kI. Also p is (p + 1) b/c of intercept
xpxi <- solve(xpx)  # Inverse matrix of (X'X + k I)

beta_elastic <- xpxi %*% t(xx) %*% y
# beta = (X'X + kI)^(-1)*X'*Y

yhat_elastic <- xx %*% beta_elastic  # yhat
sse_elastic <- t(y - yhat_elastic) %*% (y - yhat_elastic)  # variance of err = y - yhat
sig.sq_elastic <- sse_elastic/n
var.cov_elastic <- c(sig.sq_elastic) * xpxi %*% (t(xx) %*% xx) %*% xpxi  # Variance- Covariance Matrix
se_elastic <- sqrt(diag(var.cov_elastic))  # std err of estimates
tval_elastic <- beta_elastic/ se_elastic

out_elastic <- cbind(beta_elastic, se_elastic, tval_elastic)	
colnames(out_elastic) <- c("Elastic Net Estimate", "Elastic Net SEs", "Elastic Net Tvals")

variable_names <- rownames(out_elastic)[-1]
t_val_elastic <- out_elastic[-1, 3]
top3_sig_variables_elastic <- order(abs(t_val_elastic), decreasing = TRUE)[1:3]
top3_variables_names_elastic <- variable_names[top3_sig_variables_elastic]
formula_elastic <- as.formula(paste("y ~", paste(top3_variables_names_elastic, collapse = " + ")))
model_elastic <- lm(formula_elastic, data = as.data.frame(x[, top3_sig_variables_elastic]))
summary(model_elastic)


### Ridge ###
cv.ridge <-  cv.glmnet(x, y, alpha = 0, type.measure = "mse", nfolds = 10)  # 10-fold cross-validation

# Find the alpha value with the minimum MSE
best_lambdaridge <- cv.ridge$lambda.min
print(paste("The corresponding lambda.min is:", best_lambdaridge))

# find the top 3 most significant variables
ones <- as.matrix(rep(1, n), n, 1)  # vector of 1s for intercept
xx.s <- scale(x, center = TRUE, scale = TRUE)
xx <- cbind(ones, xx.s)  # xx matrix with intercept
kridge <- best_lambdaridge  # k is best lambda from ridge regression
p = ncol(x)
xpx <- t(xx) %*% xx + kridge*diag(p+1)  
# X'X matrix with Ridge Penalty: X'X + kI. Also p is (p + 1) b/c of intercept
xpxi <- solve(xpx)  # Inverse matrix of (X'X + k I)

betaridge <- xpxi %*% t(xx) %*% y
# beta = (X'X + kI)^(-1)*X'*Y

yhatridge <- xx %*% betaridge  # yhat
sseridge <- t(y - yhatridge) %*% (y - yhatridge)  # variance of err = y - yhat
sig.sqridge <- sseridge/n
var.covridge <- c(sig.sqridge) * xpxi %*% (t(xx) %*% xx) %*% xpxi  # Variance- Covariance Matrix
seridge <- sqrt(diag(var.covridge))  # std err of estimates
tvalridge <- betaridge/ seridge

outridge <- cbind(betaridge, seridge, tvalridge)	
colnames(outridge) <- c("Elastic Net Estimate", "Elastic Net SEs", "Elastic Net Tvals")

variable_names <- rownames(outridge)[-1]
t_valridge <- outridge[-1, 3]
top3_sig_variablesridge <- order(abs(t_valridge), decreasing = TRUE)[1:3]
top3_variables_namesridge <- variable_names[top3_sig_variablesridge]
formularidge <- as.formula(paste("y ~", paste(top3_variables_namesridge, collapse = " + ")))
modelridge <- lm(formularidge, data = as.data.frame(x[, top3_sig_variablesridge]))
summary(modelridge)


### Lasso ###
cv.lasso <-  cv.glmnet(x, y, alpha = 1, type.measure = "mse", nfolds = 10)  # 10-fold cross-validation

# Find the alpha value with the minimum MSE
best_lambda_lasso <- cv.lasso$lambda.min
print(paste("The corresponding lambda.min is:", best_lambda_lasso))

# find the top 3 most significant variables
ones <- as.matrix(rep(1, n), n, 1)  # vector of 1s for intercept
xx.s <- scale(x, center = TRUE, scale = TRUE)
xx <- cbind(ones, xx.s)  # xx matrix with intercept
k_lasso <- best_lambda_lasso  # k is best lambda from lasso regression
p = ncol(x)
xpx <- t(xx) %*% xx + k_lasso*diag(p+1)  
# X'X matrix with Lasso Penalty: X'X + kI. Also p is (p + 1) b/c of intercept
xpxi <- solve(xpx)  # Inverse matrix of (X'X + k I)

beta_lasso <- xpxi %*% t(xx) %*% y
# beta = (X'X + kI)^(-1)*X'*Y

yhat_lasso <- xx %*% beta_lasso  # yhat
sse_lasso <- t(y - yhat_lasso) %*% (y - yhat_lasso)  # variance of err = y - yhat
sig.sq_lasso <- sse_lasso/n
var.cov_lasso <- c(sig.sq_lasso) * xpxi %*% (t(xx) %*% xx) %*% xpxi  # Variance- Covariance Matrix
se_lasso <- sqrt(diag(var.cov_lasso))  # std err of estimates
tval_lasso <- beta_lasso/ se_lasso

out_lasso <- cbind(beta_lasso, se_lasso, tval_lasso)	
colnames(out_lasso) <- c("Elastic Net Estimate", "Elastic Net SEs", "Elastic Net Tvals")

variable_names <- rownames(out_lasso)[-1]
t_val_lasso <- out_lasso[-1, 3]
top3_sig_variables_lasso <- order(abs(t_val_lasso), decreasing = TRUE)[1:3]
top3_variables_names_lasso <- variable_names[top3_sig_variables_lasso]
formula_lasso <- as.formula(paste("y ~", paste(top3_variables_names_lasso, collapse = " + ")))
model_lasso <- lm(formula_lasso, data = as.data.frame(x[, top3_sig_variables_lasso]))
summary(model_lasso)
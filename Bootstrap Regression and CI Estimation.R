###### Part 1: Aggregate Team Members' Preference ######

test_Eddy <- c(15, 14, 5, 20, 23, 10, 11, 24, 3, 21, 22, 7, 6, 16, 18, 4, 12, 19, 8, 9, 13, 2, 17, 1)
test_Jessica <- c(24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
test_Yumi <- c(15, 17, 5, 23, 24, 10, 13, 14, 3, 21, 22, 9, 6, 8, 1, 18, 20, 12, 4, 7, 1, 16, 19, 11)
test_Yuxin <- c(14, 16, 10, 23, 24, 19, 13, 15, 8, 21, 22, 11, 3, 6, 2, 17, 20, 7, 4, 5, 1, 12, 18, 9)
test_Sara <- c(12, 9, 7, 22, 24, 20, 10, 19, 8, 21, 23, 11, 3, 6, 2, 16, 18, 14, 4, 5, 1, 15, 17, 13)



###### Part 2: Define the functions of willingness to pay ######

# Define the 24 product profiles
screen_75_inch <- c(1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0)
screen_85_inch <- c(0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0)
resolution_4k <- c(0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1)
sony <- c(1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0)
price <- c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1)
  
# Generate design matrix
design_matrix <- cbind(screen_75_inch, screen_85_inch, resolution_4k, sony, price)

# Define dependent and independent variables
X <- design_matrix
y <- test_Eddy
data <- cbind(y,X)

# Build the linear regression model
out <- lm(y ~ X)



###### Part 3: Residual Bootstrap ######

# Set up basic variables
nn <- nrow(data)
yhat <- predict(out)
rr <- out$resid					      	
bb <- 1000

# Define wtp matrix
wtp_75inch.out1 <- matrix(0, bb, 1)
wtp_85inch.out1 <- matrix(0, bb, 1)		
wtp_4k.out1 <- matrix(0, bb, 1)	
wtp_sony.out1 <- matrix(0, bb, 1)

# Do Residual Bootstrap 1000 times to get 95% CI
for(ii in 1:bb) {
  
  ystar <- yhat + rr[sample(nn, nn, replace = TRUE)]		
  out.star <- lm(ystar ~ X)
  
  # Calculate partworth and price per utility
  partworth <- matrix(out.star$coefficients)
  price_saving <- 2500-2000
  price_partworth<- abs(partworth[6])
  price_per_utility <- price_saving/price_partworth
  
  # Calculate willingness to pay for 75 inch
  wtp_75inch <- price_per_utility*partworth[2]
  wtp_75inch.out1[ii] <- wtp_75inch							

  # Calculate willingness to pay for 85 inch
  wtp_85inch <- price_per_utility*partworth[3]
  wtp_85inch.out1[ii] <- wtp_85inch
  
  # Calculate willingness to pay for 4K resolution
  wtp_4k <- price_per_utility*partworth[4]
  wtp_4k.out1[ii] <- wtp_4k	
  
  # Calculate willingness to pay for brand Sony
  wtp_sony <- price_per_utility*partworth[5]
  wtp_sony.out1[ii] <- wtp_sony

}

# Use quantile function to calculate CI and Mean
wtp_75inch.CI.resid.boot <- quantile(wtp_75inch.out1, probs = c(0.025, 0.975))
wtp_75inch.CI.resid.boot
wtp_75inch.avg.resid <- mean(wtp_75inch.out1)
wtp_75inch.avg.resid

wtp_85inch.CI.resid.boot <- quantile(wtp_85inch.out1, probs = c(0.025, 0.975))
wtp_85inch.CI.resid.boot
wtp_85inch.avg.resid <- mean(wtp_85inch.out1)
wtp_85inch.avg.resid

wtp_4k.CI.resid.boot <- quantile(wtp_4k.out1, probs = c(0.025, 0.975))
wtp_4k.CI.resid.boot
wtp_4k.avg.resid <- mean(wtp_4k.out1)
wtp_4k.avg.resid

wtp_sony.CI.resid.boot <- quantile(wtp_sony.out1, probs = c(0.025, 0.975))
wtp_sony.CI.resid.boot
wtp_sony.avg.resid <- mean(wtp_sony.out1)
wtp_sony.avg.resid



###### Part 4: Data Bootstrap ######

# Define wtp matrix
wtp_75inch.out2 <- matrix(0, bb, 1)				
wtp_85inch.out2 <- matrix(0, bb, 1)	
wtp_4k.out2 <- matrix(0, bb, 1)	
wtp_sony.out2 <- matrix(0, bb, 1)	

# Do Data Bootstrap 1000 times to get 95% CI
for(ii in 1:bb) {
  
  data.star <- data[sample(nn, nn, replace = TRUE),]
  ystar <- data.star[,1]
  xstar <- data.star[,2:6]
  out.star <- lm(ystar~xstar)
  
  partworth <- matrix(out.star$coefficients)
  price_saving <- 2500-2000
  price_partworth<- abs(partworth[6])
  price_per_utility <- price_saving/price_partworth
  
  # Calculate willingness to pay for 75 inch
  wtp_75inch <- price_per_utility*partworth[2]
  wtp_75inch.out2[ii] <- wtp_75inch	
  
  # Calculate willingness to pay for 85 inch
  wtp_85inch <- price_per_utility*partworth[3]
  wtp_85inch.out2[ii] <- wtp_85inch
  
  # Calculate willingness to pay for 4K resolution
  wtp_4k <- price_per_utility*partworth[4]
  wtp_4k.out2[ii] <- wtp_4k
  
  # Calculate willingness to pay for brand Sony
  wtp_sony <- price_per_utility*partworth[5]
  wtp_sony.out2[ii] <- wtp_sony

}

# Use quantile function to calculate CI and Mean
wtp_75inch.CI.data.boot <- quantile(wtp_75inch.out2, probs = c(0.025, 0.975))
wtp_75inch.CI.data.boot
wtp_75inch.avg.data <- mean(wtp_75inch.out2)
wtp_75inch.avg.data

wtp_85inch.CI.data.boot <- quantile(wtp_85inch.out2, probs = c(0.025, 0.975))
wtp_85inch.CI.data.boot
wtp_85inch.avg.data <- mean(wtp_85inch.out2)
wtp_85inch.avg.data

wtp_4k.CI.data.boot <- quantile(wtp_4k.out2, probs = c(0.025, 0.975))
wtp_4k.CI.data.boot
wtp_4k.avg.data <- mean(wtp_4k.out2)
wtp_4k.avg.data

wtp_sony.CI.data.boot <- quantile(wtp_sony.out2, probs = c(0.025, 0.975))
wtp_sony.CI.data.boot
wtp_sony.avg.data <- mean(wtp_sony.out2)
wtp_sony.avg.data


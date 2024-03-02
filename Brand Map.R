# 1. Build brand maps for car brands. The client's brand is Infinity

# 2. How many factors to retain? 

# 3. Assign names to the retained factors (you may need to flip the factors and then assign names)

# 4. Explain iso-preference line and its difference from the regression line

# 5. Explain what is an ideal vector and why it indicates the direction of increasing preferences 

# 6. Compute the angles of iso-preference line and ideal vector arrow

# 7. Find 95% confidence interval for the angle of the ideal vector. Use data bootstrap method to obtain CI

# 8. Recommend to Infinity's managers what they should do to improve their product design

library(glmnet)

# import the data
data <-  read.csv("Cars_Data.csv")
y <- data[,17]                  # overall performance
x <- as.matrix(data[,2:16])     # independent variables
cor_mat <- cor(x)               # correlation matrix for independent variables



# ------- Principal Components Analysis -------
out1 <-  eigen(cor_mat)		# eigen decomposition of correlation matrix
va <-  out1$values		  	# eigenvalues
ve <-  out1$vectors		  	# eigenvector

# scree plot to find the number of benefits(components) to keep
plot(va, ylab = "Eigenvalues", xlab = "Component Nos")				# scree plot

ego <- va[va > 1]							      # eigenvalues > 1
nn <- nrow(as.matrix(ego))					# number of factors to retain
sprintf('There are %d factors being retained.',nn)

out2 <- ve[,1:nn]							              # eigenvectors associated with the reatined factors
out3 <- ifelse(abs(out2) < 0.3, 0, out2)		# ignore small values < 0.3

names <- colnames(x)            # extract names of the variables for out3
rownames(out3) <- names

# Component Scores
z <- x %*% out3

# linear regression on components scores
out4 <- lm(y ~ z)
summary(out4)           

# Z1 and Z2 shows negative coefficient, we need to flip those Z variables by multiplying by -1.
z[,1] <- (-1) * z[,1]
z[,2] <- (-1) * z[,2]

# Assign names to the retained factors
colnames(z) <- c('Benefit_Z1', 'Benefit_Z2', 'Benefit_Z3', 'Benefit_Z4')

# linear regression on components scores again with all coefficicients being converted to positive values
out5 <- lm(y ~ z)
summary(out5)  



# ------- Iso Preference Line and Ideal Vector -------

# Consider factors z1 and z2 with positive slopes on preferences, having z2 be the y-axis and z1 as the x-axis	
Z1 <- z[,1]
Z2 <- z[,2]
z.out <- cbind(Z1, Z2)

brand <- data[,1]
rownames(z.out) <- brand

# Plot, add labels
plot(Z1, Z2, main = "Brands in Z1 and Z2 space", xlab = "Benefit Z1", ylab = "Benefit Z2", col = "lightblue", pch = 19, cex = 2)		# Brand Map in Z1-Z2 space
text(z.out, labels = row.names(z.out), font = 2, cex = 0.5, pos = 1)						# labeling brands

# Slopes of iso-preference and ideal vector	
b1 <- as.vector(coef(out5)[2])
b2 <- as.vector(coef(out5)[3])
slope.iso.preference <-  - b1/b2						
slope.ideal.vector <-  b2/b1 	

sprintf('The slope of iso-preference is %s, and the slope of ideal-vector is %s.',slope.iso.preference, slope.ideal.vector)

# Angles of iso-preference and ideal vector	
angle.iso.preference <- atan(slope.iso.preference)*180/pi	
angle.ideal.vector <- atan(slope.ideal.vector)*180/pi

sprintf('The angle of iso-preference is %s, and the angle of ideal-vector is %s.',angle.iso.preference, angle.ideal.vector)



# ------- Find 95% confidence interval for the angle of the ideal vector. Use data bootstrap method to obtain CI -------
bb <- 1000
angle_ideal_vectors <- matrix(0, bb, 1)

for(ii in 1:bb) {
  new_z <- z[sample(10,10,replace = TRUE),]
  output <- lm(y~new_z)
  b_1 <- as.vector(abs(coef(output)[2]))
  b_2 <- as.vector(abs(coef(output)[3]))
  slope_ideal_vector <- b_2/b_1
  angle_ideal_vector <- atan(slope_ideal_vector)*180/pi
  angle_ideal_vectors[ii] <- angle_ideal_vector
}

# 95% CI for angle of Ideal-Vector from sorting
angle.CI.lower <- sort(angle_ideal_vectors)[25]		# 25th value in sorted rsq.out
angle.CI.upper2 <- sort(angle_ideal_vectors)[975]		# 975th value in sorted rsq.out

sprintf('The 95%% confidence interval for the angle of Ideal Vector is (%.5f, %.5f) degrees.',angle.CI.lower,angle.CI.upper2)
  

  
  
  
  
  
  

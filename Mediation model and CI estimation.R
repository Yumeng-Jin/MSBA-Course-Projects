# setwd("/Users/yumi/ucdavis/Winter Quarter/BAX-442/HW5")
rm(list=ls(all=TRUE)) #clear data


##### Load packages #####
library(ggplot2)
library(gridExtra)
library(Hmisc)
library(MASS)
library(dplyr)
library("readxl")


##### Read data #####
data <- read_excel("Heineken.xlsx", sheet = "Brand_6")
names(data)
head(data,5)


##### Rename the data names #####

data <- rename(data, Awareness = Awareness_Pct, Sales = Volume_Sales_1000Liter)

##### Plot data #####

p1 <- ggplot(data, aes(x = Week, y = Awareness)) + geom_line() + geom_point(colour="blue", size = 2, shape = 21, fill="white") 
p2 <- ggplot(data, aes(x = Week, y = Sales)) + geom_line() + geom_point(colour="red", size = 2, shape = 21, fill="white") 
grid.arrange(p1,p2, nrow = 2)  
theme_set(theme_bw())


##### Define variables #####

aware <- data$Awareness                      # dependent variables
sales <- data$Sales                          # dependent variables
 
ooh <- data$ADV_OOH_Euros                    # independent variable
socialmedia <- data$ADV_SocialMedia_Euros    # independent variable

temperature <- data$temp_mean                # control variable
covid <- data$covid_stringency_index         # control variable

lag.sales <- Lag(sales) 	                   # lag variable
lag.aware <- Lag(aware)                      # lag variable


##### Mediator regression #####

# regress aware to last period itself, ooh, socialmedia
m1 <- lm(aware ~ -1 + lag.aware + ooh + socialmedia)  
# drop intercept using -1   
summary(m1)
# lag is always significant


##### DV regression #####

m2 <- lm(sales ~ -1 + lag.sales + aware + ooh + socialmedia + temperature + covid)
summary(m2)


##### Steady State Effect [beta / (1 - lambda)] #####

# a-path
a1.est <- m1$coeff[2] / (1 - m1$coeff[1])
a2.est <- m1$coeff[3] / (1 - m1$coeff[1])

# b-path
b.est <- m2$coeff[2] / (1 - m2$coeff[1])

# c-path
c1.est <- m2$coeff[3] / (1 - m2$coeff[1])
c2.est <- m2$coeff[4] / (1 - m2$coeff[1])


##### mediation effects [a-path * b-path] #####

a1b.est <- a1.est * b.est
a2b.est <- a2.est * b.est

tot_ooh.est <- c1.est + a1b.est
tot_social.est <- c2.est + a2b.est


##### empirical results #####

# OOH
out1.emp.res <- matrix(c(a1.est, b.est, a1b.est, c1.est, tot_ooh.est), 5, 1)
colnames(out1.emp.res) <- c("Estimates")
rownames(out1.emp.res) <- c("OOH -> Aware",
                           "Aware -> Sales",
                           "OOH -> Aware -> Sales",
                           "OOH -> Sales",
                           "Total_OOH")
print(round(out1.emp.res, digits = 4))

# Social Media
out2.emp.res <- matrix(c(a2.est, b.est, a2b.est, c2.est, tot_social.est), 5, 1)
colnames(out2.emp.res) <- c("Estimates")
rownames(out2.emp.res) <- c("SocialMedia -> Aware",
                           "Aware -> Sales",
                           "SocialMedia -> Aware -> Sales",
                           "SocialMedia -> Sales",
                           "Total_Social")
print(round(out2.emp.res, digits = 4))


# The confidence intervals (CI) of the mediation and total effects are not normal
# and not known analytically. Also, the presence of lagged variables renders the
# standard bootstrap inapplicable. Hence, I will apply Monte Carlo bootstrap to
# obtain 95% CIs for the mediation effects, the direct effects, and the total effects.



##### Monte Carlo Inference #####

# create parameter vector and var-covar matrix of relevant coefficients from m1 and m2 models

m1.param <- matrix(cbind(m1$coeff[1], m1$coeff[2], m1$coeff[3]), nrow = 3, ncol = 1)
m1.vcov <- vcov(m1)

m2.param <- matrix(cbind(m2$coeff[1], m2$coeff[2], m2$coeff[3],
                         m2$coeff[4], m2$coeff[5], m2$coeff[6]), nrow = 6, ncol = 1)
m2.vcov <- vcov(m2)

m1.vcov
m2.vcov


##### Monte Carlo draws #####

n.MC <- 1000
draws.m1 <- mvrnorm(n.MC, m1.param, m1.vcov)
draws.m2 <- mvrnorm(n.MC, m2.param, m2.vcov)

# draws.m1
# draws.m2


##### Steady State Effect [beta / (1 - lambda)] #####

# a-path
a1.sim <- draws.m1[,2] / (1 - draws.m1[,1])
a2.sim <- draws.m1[,3] / (1 - draws.m1[,1])

# b-path
b.sim <- draws.m2[,2] / (1 - draws.m2[,1])

# c-path
c1.sim <- draws.m2[,3] / (1 - draws.m2[,1])
c2.sim <- draws.m2[,4] / (1 - draws.m2[,1])


##### mediation effects [a-path * b-path] #####

a1b.sim <- a1.sim * b.sim
a2b.sim <- a2.sim * b.sim

tot_ooh.sim <- c1.sim + a1b.sim
tot_social.sim <- c2.sim + a2b.sim


##### Results: MC Estimates and CIs #####

a1.res <- quantile(a1.sim, probs = c(0.5, 0.025, 0.975))
a2.res <- quantile(a2.sim, probs = c(0.5, 0.025, 0.975))

b.res <- quantile(b.sim, probs = c(0.5, 0.025, 0.975))

a1b.res <- quantile(a1b.sim, probs = c(0.5, 0.025, 0.975))
a2b.res <- quantile(a2b.sim, probs = c(0.5, 0.025, 0.975))

c1.res <- quantile(c1.sim, probs = c(0.5, 0.025, 0.975))
c2.res <- quantile(c2.sim, probs = c(0.5, 0.025, 0.975))

total_ooh.res <- quantile(tot_ooh.sim, probs = c(0.5, 0.025, 0.975))
total_social.res <- quantile(tot_social.sim, probs = c(0.5, 0.025, 0.975))


# OOH
out1.mediation.res <- matrix(0,5,3)
out1.mediation.res <- rbind(a1.res, b.res, a1b.res, c1.res, total_ooh.res)
colnames(out1.mediation.res) <- c("Median", "2.5% CIs", "97.5% CIs")
rownames(out1.mediation.res) <- c("OOH -> Aware",
                                  "Aware -> Sales", 
                                  "OOH -> Aware -> Sales",
                                  "OOH -> Sales",
                                  "Total_OOH")

# Social Media
out2.mediation.res <- matrix(0,5,3)
out2.mediation.res <- rbind(a2.res, b.res, a2b.res, c2.res, total_social.res)
colnames(out2.mediation.res) <- c("Median", "2.5% CIs", "97.5% CIs")
rownames(out2.mediation.res) <- c("SocialMedia -> Aware",
                                  "Aware -> Sales", 
                                  "SocialMedia -> Aware -> Sales",
                                  "SocialMedia -> Sales",
                                  "Total_Social")

# print result
print(round(out1.mediation.res, digits = 4))
print(round(out2.mediation.res, digits = 4))
print(round(out1.emp.res, digits = 4))
print(round(out2.emp.res, digits = 4))

# INTERPRET: confidence interval include 0 means not significant.
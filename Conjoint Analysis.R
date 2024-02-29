library(readxl)

# create conjoint analysis function - feed preference rank vector
conjoint_analysis <- function(preference_rank_vec) {
  
  # read design matrix from the external excel file
  #design_matrix <- read_excel('Design Matrix.xlsx')
  
  screen_75_inch <- c(1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0)
  screen_85_inch <- c(0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0)
  resolution_4k <- c(0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1)
  sony <- c(1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0)
  price <- c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1)
  
  design_matrix <- cbind(screen_75_inch, screen_85_inch, resolution_4k, sony, price)
  design_matrix <- split(design_matrix, col(design_matrix))
  names(design_matrix) <- c('Screen 75 inch','Screen 85 inch','Resolution 4K = 1','Sony = 1','Price (low = 0; hi =1)')
  
  attr_name <- names(design_matrix)
  
  # linear regression model to find partworth for independent(attribute) variables
  model <- lm(preference_rank_vec ~ design_matrix$`Screen 75 inch` + design_matrix$`Screen 85 inch` + design_matrix$`Resolution 4K = 1` + design_matrix$`Sony = 1` + design_matrix$`Price (low = 0; hi =1)`)
  
  
  
  # output 1: return partworth for each attribute level
  partworth <- matrix(round(model$coefficients,2))
  rownames(partworth) <- c('Intercept', attr_name[1:5])
  colnames(partworth) <- c('Coefficient')
  
  print('--------------------------------Partworth------------------------------------')
  print(partworth)

  
  
  # output 2: Attribute Importance of each attribute
  attribute <- c('Screen Size', 'Screen Resolution', 'Brand Name', 'Price')
  range <- c(abs(partworth[2] - partworth[3]), abs(partworth[4]), abs(partworth[5]), abs(partworth[6]))
  importance <- c(round(range[1]/sum(range),2), round(range[2]/sum(range),2),round(range[3]/sum(range),2),round(range[4]/sum(range),2))
  importance <- importance * 100
  importance <- sprintf("%.2f%%", importance)
  
  # create a dataframe for all three columns above, adding column names
  attr_import <- data.frame(attribute,range,importance)
  colnames(attr_import) <- c('Attributes', 'Range', 'Importance')
  print('--------------------------------Attribute Importance--------------------------------')
  print(attr_import)
  
  
  
  # output 3: Willingness to pay for each non-price attribute level
  price_saving <- 2500-2000
  price_partworth<- abs(partworth[6])
  price_per_utility <- price_saving/price_partworth
  
  # calculating WTP
  wtp <- c(price_per_utility*partworth[2],
           price_per_utility*partworth[3],
           price_per_utility*partworth[4],
           price_per_utility*partworth[5])
  wtp <- matrix(wtp)
  
  # adding row/col names
  rownames(wtp) <- rownames(partworth)[2:5]
  colnames(wtp) <- c('Willing to Pay')
  print('--------------------------------Willing to Pay--------------------------------')
  print(wtp)
  
  
  
  # output 4:Optimal price
  # profile matrix
  intercept <- c(1,1,1)
  screen_75 <- c(0,1,0)
  screen_85 <- c(1,0,1)
  resolution <- c(0,1,1)
  sony <- c(0,1,0)
  price <- c(1500,2500,2000)
  profile <- cbind(intercept,screen_75,screen_85,resolution,sony,price)
  rownames(profile) <- c('My Design', 'Competing Brand 1 - Sony', 'Competing Brand 2 - Sharp')
  colnames(profile) <- c('intercept','screen_75','screen_85','resolution','sony','price')
  
  price_range <- seq(1500, 2600, by = 100)
  
  # for loop to find market share for each price choice
  market_share <- c()
  for (i in price_range) {
    profile[1,6] <- i
    utility <-  profile[,1]*partworth[1,]+
                profile[,2]*partworth[2,]+
                profile[,3]*partworth[3,]+
                profile[,4]*partworth[4,]+
                profile[,5]*partworth[5,]+
                ((profile[,6]-2000)/500)*partworth[6,]
    
    # find attractiveness and market share
    attractiveness <- exp(utility)
    market_share_within_profile <- attractiveness/sum(attractiveness)
    
    market_share <- c(market_share, round(market_share_within_profile[1],3))
  }
  
  # Sale = Market Share * Market Size 
  market_size <- 100
  sales <- market_share * market_size
  
  # Margin = Price - Net Cost
  cost <- c(1000,500,1000,250,250)
  # cost <- c(1000,500,1000,250,250) * 0.8
  # The cost might need to be adjusted since it may cause the maximum profit to $0,
  # market share to 0$ under some preferences. We put the test result here to show
  # the correctness of our function.
  net_cost <- c(cost %*% profile[1,1:5])
  margin <- price_range - net_cost

  
  
  # Profit = Margin * Sales
  profit <- margin * sales
  
  # find optimal price
  optimal_price <- price_range[which.max(profit)]
  print('--------------------------------Optimal Price--------------------------------')
  result <- sprintf('The optimal price for my design is %d dollars.',optimal_price)
  print(result)
  
  
  
  # output 5:Maximum Profit
  print('--------------------------------Maximum Profit--------------------------------')
  maximum_profit <- max(profit)
  result <- sprintf('The maximum profit for my design is %f dollars.',maximum_profit)
  print(result)
  
  
  
  # output 6:Market Share associated with optimal price
  print('--------------------------------Market Share--------------------------------')
  my_market_share <- market_share[which.max(profit)]
  result <- sprintf('The market share for my design is %.2f',my_market_share)
  print(result)
  
  
  
  # output 7:Plot market shares as a function of prices
  plot(price_range, market_share, main="Market Share as A Function of Price", xlab="Price", ylab="Market Share", pch=19, col="blue")
  lines(price_range, market_share, type="l", col="red")
  
  
  
  # output 8:Plot profit as a function of prices
  plot(price_range, profit, main="Profit as A Function of Price", xlab="Price", ylab="Profit", pch=19, col="blue")
  lines(price_range, profit, type="l", col="red")
  
  
  
  # output 9:Plot sales as a function of prices
  plot(price_range, sales, main="Sales as A Function of Price", xlab="Price", ylab="Sales", pch=19, col="blue")
  lines(price_range, sales, type="l", col="red")
}



test_Eddy <- c(15, 21, 5, 20, 23, 1, 11, 24, 3, 14, 22, 7, 6, 16, 18, 4, 12, 19, 2, 9, 13, 8, 10, 17)
test_Jessica <- c(24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
test_Yumi <- c(15, 17, 5, 23, 24, 10, 13, 14, 3, 21, 22, 9, 6, 8, 1, 18, 20, 12, 4, 7, 1, 16, 19, 11)
test_Yuxin <- c(14, 16, 10, 23, 24, 19, 13, 15, 8, 21, 22, 11, 3, 6, 2, 17, 20, 7, 4, 5, 1, 12, 18, 9)
test_Sara <- c(9, 12, 7, 22, 24, 20, 10, 11, 8, 21, 23, 19, 3, 6, 2, 16, 18, 14, 4, 5, 1, 15, 17, 13)


conjoint_analysis(test_Yumi)
conjoint_analysis(test_Jessica)
conjoint_analysis(test_Eddy)
conjoint_analysis(test_Yuxin)
conjoint_analysis(test_Sara)












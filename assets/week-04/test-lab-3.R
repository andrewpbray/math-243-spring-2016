train <- read.csv("lab-3/crime-train.csv")
test <- read.csv("crime-test.csv")

n_groups <- 8
group <- LETTERS[1:n_groups]
p <- rep(1, n_groups)
MSE_train <- rep(0, n_groups)
MSE_test <- rep(0, n_groups)
results <- data.frame(group, p, MSE_train, MSE_test)

# GROUP A

group_A_fit <- function(training_data) {
  
  m1 <- lm(sqrt(ViolentCrimesPerPop) ~ racePctWhite+pctWInvInc+NumUnderPov+medIncome+
             TotalPctDiv+PctIlleg+PctHousOwnOcc+PctHousLess3BR+PctHousOwnOcc+NumStreet+PctEmploy, data = training_data)
  m1
}

group_A_MSE <- function(model, data) {
  yhat <- (predict(model,data))^2
  mse <- mean((yhat-data$ViolentCrimesPerPop)^2)
  mse
}

results[1, 2] <- 11
m <- group_A_fit(train)
results[1, 3] <- group_A_MSE(m, train)
results[1, 4] <- group_A_MSE(m, test)


# GROUP B

group_B_fit <- function(training_data) {
  
  m1 <- lm(ViolentCrimesPerPop ~ 
             state + 
             racePctWhite + 
             agePct65up + 
             pctUrban + 
             TotalPctDiv + 
             PersPerFam + 
             PctWorkMom + 
             PctIlleg + 
             HousVacant + 
             RentLowQ + 
             NumStreet, 
           data = training_data)
  
  m1
}


group_B_MSE <- function(model, data) {
  yhat <- predict(model, data)
  mse <- mean((yhat-data$ViolentCrimesPerPop)^2)
  mse
}

results[2, 2] <- 11
m <- group_B_fit(train)
results[2, 3] <- group_B_MSE(m, train)
results[2, 4] <- group_B_MSE(m, test)


# GROUP C

group_C_fit <- function(training_data) {
  library(dplyr)
  training_data <- training_data %>% mutate(state = as.factor(state)) %>% tbl_df()
  
  # Add state averages
  state.avg <- training_data %>%
    group_by(state) %>%
    summarise(state.PctIlleg = mean(PctIlleg),
              state.racepctblack = mean(racepctblack),
              state.PctPopUnderPov = mean(PctPopUnderPov),
              state.pupassist = mean(pctWPubAsst),
              state.english = mean(PctNotSpeakEnglWell),
              state.plumbing = mean(PctWOFullPlumb),
              state.density = mean(PopDens),
              state.drug = mean(LemasPctOfficDrugUn),
              state.income = mean(medIncome),
              state.popdens = mean(PopDens))
  
  write.csv(state.avg, file="state_averages.csv")
  
  training_data <- training_data %>%
    left_join(state.avg, by="state")
  
  # Add south indicator
  south <- c(1, 5, 12, 13, 21, 22, 28, 29, 37, 45, 47, 48, 51)
  training_data$south <- ifelse(training_data$state %in% south, TRUE, FALSE)
  
  # Run the model
  
  model1 <- lm(sqrt(ViolentCrimesPerPop) ~ log(PctIlleg + 1) + log(racepctblack + 1) + PctVacantBoarded 
               + PctPersDenseHous + TotalPctDiv + PctHousLess3BR + state.PctIlleg + medIncome 
               + state.racepctblack + state.PctPopUnderPov + state.english + state.plumbing 
               + state.drug + state.income + state.popdens + PopDens * pctUrban + agePct65up
               + NumInShelters + PctNotHSGrad + south, data = training_data)
  
  model1
}

group_C_MSE <- function(model, data) {
  library(dplyr)
  # Transform data
  # Add state averages
  state.avg.old <- read.csv('state_averages.csv')
  
  state.avg.new <- data %>%
    group_by(state) %>%
    summarise(state.PctIlleg = mean(PctIlleg),
              state.racepctblack = mean(racepctblack),
              state.PctPopUnderPov = mean(PctPopUnderPov),
              state.pupassist = mean(pctWPubAsst),
              state.english = mean(PctNotSpeakEnglWell),
              state.plumbing = mean(PctWOFullPlumb),
              state.density = mean(PopDens),
              state.drug = mean(LemasPctOfficDrugUn),
              state.income = mean(medIncome),
              state.popdens = mean(PopDens))
  state.avg.old$X <- NULL
  state.avg <- state.avg.old %>% 
    union(state.avg.new, by="state") %>% 
    distinct(state)
  data <- data %>%
    left_join(state.avg, by="state")
  
  # Add south indicator
  south <- c(1, 5, 12, 13, 21, 22, 28, 29, 37, 45, 47, 48, 51)
  data$south <- ifelse(data$state %in% south, TRUE, FALSE)
  
  # Predict
  yhat <- predict(model, data)
  # Output mean squared error
  (data$ViolentCrimesPerPop-yhat^2)^2 %>% mean() %>% return()

}

results[3, 2] <- 22
m <- group_C_fit(train)
results[3, 3] <- group_C_MSE(m, train)
results[3, 4] <- group_C_MSE(m, test)


# GROUP D

group_D_fit <- function(training_data) {
  crime2<- subset(training_data, ViolentCrimesPerPop>0)
  crime2<- transform(crime2, log_crime=log(ViolentCrimesPerPop))
  crime2<- transform(crime2, whitesq=racePctWhite^2)
  m <- lm(log_crime ~ whitesq + numbUrban + NumUnderPov + PctKids2Par + PctLargHouseOccup + TotalPctDiv, data = crime2)
  m
}

group_D_MSE <- function(model, data){
  data <- transform(data, log_crime = log(ViolentCrimesPerPop))
  data <- transform(data, whitesq = racePctWhite^2)
  y_hat <- exp(predict(model, data))
  y <- exp(data$log_crime)
  mse = mean((y_hat - y)^2)
  mse
}

results[4, 2] <- 6
m <- group_D_fit(train)
results[4, 3] <- group_D_MSE(m, train)
results[4, 4] <- group_D_MSE(m, test)


# GROUP E

group_E_fit <- function(test_data) {
  test_data[test_data=="?"] <- NA  #turning "?"s into NAs
  test_data$ViolentCrimesPerPop[test_data$ViolentCrimesPerPop == 0] <- NA
  test_data$pctUrbanRounded <-round(test_data$pctUrban,0)
  m1 <- lm(log(ViolentCrimesPerPop)~ TotalPctDiv+pctUrbanRounded+ PctIlleg*racePctWhite+HousVacant+PctKids2Par, data=test_data)
  m1
}

group_E_MSE <- function(vcrime, test_data) {
  test_data$pctUrbanRounded <-round(test_data$pctUrban,0)

  y_hat <- exp(predict(vcrime, test_data))
  mse = mean((y_hat - test_data$ViolentCrimesPerPop)^2)
}

results[5, 2] <- 7
m <- group_E_fit(train)
results[5, 3] <- group_E_MSE(m, train)
results[5, 4] <- group_E_MSE(m, test)


# GROUP F

group_F_fit <- function(training_data) {
  m1 <- lm(formula = ViolentCrimesPerPop ~ racePctWhite + I(MalePctDivorce^2)
           + PctIlleg + PctPersDenseHous + numbUrban, data = training_data)
  m1
}

group_F_MSE <- function(model, data){
  b0 <- model$coefficients[1] * rep( 1, length( data$ViolentCrimesPerPop ) )
  b1 <- model$coefficients[2] * data$racePctWhite
  b2 <- model$coefficients[3] * data$MalePctDivorce ^ 2
  b3 <- model$coefficients[4] * data$PctIlleg
  b4 <- model$coefficients[5] * data$PctPersDenseHous
  b5 <- model$coefficients[6] * data$numbUrban
  
  y_hats <- b0 + b1 + b2 + b3 + b4 + b5
  mean ( (y_hats - data$ViolentCrimesPerPop)^2 )
}

results[6, 2] <- 5
m <- group_F_fit(train)
results[6, 3] <- group_F_MSE(m, train)
results[6, 4] <- group_F_MSE(m, test)


# GROUP G

group_G_fit <- function(training_data) {
  
  training_data$rural <- ifelse(training_data$pctUrban < 0.15, 1, 0)
  
  m1 <- lm(ViolentCrimesPerPop ~ racePctWhite*rural + agePct12t29 + PctIlleg + log(TotalPctDiv +1) + pctWPubAsst, data = training_data)
  m1
}

group_G_MSE <- function(model, data) {
  
  data$rural <- ifelse(data$pctUrban < 0.15, 1, 0)
  
  y.predicted <- predict(model, data)
  y.actual <- data$ViolentCrimesPerPop
  
  mse <- mean((y.actual - y.predicted)^2)
  mse
}

results[7, 2] <- 7
m <- group_G_fit(train)
results[7, 3] <- group_G_MSE(m, train)
results[7, 4] <- group_G_MSE(m, test)


# GROUP H

group_H_fit <- function(training_data) {
  crimedata <- training_data %>% rename(crime = ViolentCrimesPerPop)
  M1 <- lm(crime~NumUnderPov + MalePctDivorce + PctPersDenseHous + pctUrban + racepctblack + I(racepctblack^2) + PctKids2Par, data=crimedata)
  
  return(M1)
  
}

group_H_MSE <- function(model, data) {
  new_data <- data %>% rename(crime = ViolentCrimesPerPop)
  fitted <- predict(model, newdata=new_data)
  MSE <- mean((fitted-new_data$crime)^2)
  
  return(MSE)
}

results[8, 2] <- 7
m <- group_H_fit(train)
results[8, 3] <- group_H_MSE(m, train)
results[8, 4] <- group_H_MSE(m, test)


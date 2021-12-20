#################### K353 Project by KBK Consulting  ####################
# clear R environment
rm(list = ls())
#  Data sets
load(url("https://www.dropbox.com/s/g852wf1akx3xljl/airbnb_project.rdata?dl=1"))
#install.packages("corrplot") <---- Run code, then comment it out
library(corrplot)
library(rpart)

################## DATA CLEANING ################## 
# Replace rare "No Refunds" & "Super Strict 30 Days" cancellation policy values with "Strict" 
names(table(property_info$CancellationPolicy)) 
property_info$CancellationPolicy[property_info$CancellationPolicy %in% c("No Refunds","Super Strict 30 Days")] <- "Strict"

# Find which columns have NA Values
na_checker <-apply(property_info, 2, function(x) any(is.na(x)))
col_with_na <- names(na_checker[as.numeric(na_checker) == 1])

# Replace neighborhoods with less than 20 properties with the value of "rare neighborhood"
tb_neighborhood <- table(property_info$Neighborhood)
rare_neighborhood <- names(tb_neighborhood[tb_neighborhood <= 20])
property_info$Neighborhood[property_info$Neighborhood %in% rare_neighborhood] <- "rare neighborhood"

# Replace neighborhoods with NA with the value of "rare neighborhood"
property_info$Neighborhood[is.na(property_info$Neighborhood)] = "rare neighborhood"

# Replace Superhost with NA with the value of "unknown host type"
property_info$Superhost[is.na(property_info$Superhost)] <- "unknown host type"

# Vector of uncleaned variables
col_vec_toclean <- col_with_na[!col_with_na %in% c("Neighborhood","Superhost")]
# Clean uncleaned variables
for (i in col_vec_toclean)
{
  property_info[[i]][is.na(property_info[[i]])] <- median(property_info[[i]], na.rm = TRUE)
}


################## Feature Engineering ################## - Adding New Features to Data Set
# Get Q1 Booked Column
listing_2016Q1_booking <- listing_2016Q1[listing_2016Q1$Status == "R",]
agg_booking_Q1 <- aggregate(Status~PropertyID,data = listing_2016Q1_booking, FUN = length) 
colnames(agg_booking_Q1)[2] <- "BookingQ1"
property_info <- merge(x = property_info, y = agg_booking_Q1, by = "PropertyID",all.x=TRUE)
property_info$BookingQ1[is.na(property_info$BookingQ1) == TRUE] <- 0
# Get Q2 Booked Column
listing_2016Q2_booking <- listing_2016Q2[listing_2016Q2$Status == "R",]
agg_booking_Q2 <- aggregate(Status~PropertyID,data = listing_2016Q2_booking, FUN = length) 
colnames(agg_booking_Q2)[2] <- "BookingQ2"
property_info <- merge(x = property_info, y = agg_booking_Q2, by = "PropertyID",all.x=TRUE)
property_info$BookingQ2[is.na(property_info$BookingQ2) == TRUE] <- 0
# Get Q1 Blocked Column
listing_2016Q1_block <- listing_2016Q1[listing_2016Q1$Status=="B",]
agg_block_Q1 <- aggregate(Status~PropertyID, data=listing_2016Q1_block, FUN=length)
colnames(agg_block_Q1)[2] <- "BlockedQ1"
property_info <- merge(x=property_info, y=agg_block_Q1, by='PropertyID', all.x=TRUE)
property_info$BlockedQ1[is.na(property_info$BlockedQ1)] <- 0
# Get Q2 Blocked Column
listing_2016Q2_block <- listing_2016Q2[listing_2016Q2$Status=="B",]
agg_block_Q2 <- aggregate(Status~PropertyID, data=listing_2016Q2_block, FUN=length)
colnames(agg_block_Q2)[2] <- "BlockedQ2"
property_info <- merge(x=property_info, y=agg_block_Q2, by='PropertyID', all.x=TRUE)
property_info$BlockedQ2[is.na(property_info$BlockedQ2)] <- 0
######## Get Q1 Average Price
avg_price_Q1 <- aggregate(Price~PropertyID, data=listing_2016Q1, FUN=median)
colnames(avg_price_Q1)[2] <- "AvgPriceQ1"
property_info <- merge(x=property_info, y=avg_price_Q1, by='PropertyID', all.x=TRUE)
property_info$AvgPriceQ1[is.na(property_info$AvgPriceQ1)] <- 0
######## Get Q2 Average Price
avg_price_Q2 <- aggregate(Price~PropertyID, data=listing_2016Q2, FUN=median)
colnames(avg_price_Q2)[2] <- "AvgPriceQ2"
property_info <- merge(x=property_info, y=avg_price_Q2, by='PropertyID', all.x=TRUE)
property_info$AvgPriceQ2[is.na(property_info$AvgPriceQ2)] <- 0


############### Split into Train/Test data #############
property_info_test <- property_info[property_info$PropertyID %in% PropertyID_test,]
property_info_train <-property_info[!property_info$PropertyID %in% PropertyID_test,]
# Add target variable to training data set
property_info_train <- merge(property_info_train, reserve_2016Q3_train, by = "PropertyID")


################## Exploratory Data Analysis ################## - Descriptive Analytics
# Histogram of Response Time grouped by Regular/Super Host
# Regular Host
par(bg="beige")
reg_hosts <- property_info[property_info$Superhost==FALSE,]
hist(reg_hosts$ResponseTimemin,
     breaks=20,probability=TRUE,
     main='Histogram of Host Response Time in Mins',
     xlab='Response Time in Mins',ylab='Probability',
     col=rgb(red=1,green=0,blue=0,alpha=0.5),
     border=FALSE,ylim=c(0,0.007))
# Super Host
super_hosts <- property_info[property_info$Superhost==TRUE,]
hist(super_hosts$ResponseTimemin,
     breaks=20,probability=TRUE,
     col=rgb(red=0,green=0,blue=1,alpha=0.5),
     border=FALSE,ylim=c(0,0.007),
     add=TRUE)
# Create Legend
legend("topright"
       , legend = c("Regular host", "Superhost")
       , fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5))
       , border = FALSE)
dev.copy(png,'host_response_time.png')
dev.off()

# Correlation Matrix
myvars <- c("NumberofReviews", "OverallRating","Longitude",
            "Bathrooms","MaxGuests","ResponseTimemin",
            "SecurityDeposit","CleaningFee","ExtraPeopleFee",
            "PublishedNightlyRate","MinimumStay",
            "NumberofPhotos","BookingQ1","BookingQ2","BlockedQ1","BlockedQ2",
            "NumReserveDays2016Q3","AvgPriceQ2")
property_info_corr <- property_info_train[,myvars]
# Create Correlation Table
mydata.cor = cor(na.omit(property_info_corr))
# Plot Correlation Matrix - ZOOM TO SEE IMAGE BETTER
par(bg="beige")
corrplot(mydata.cor, method='ellipse', order = 'AOE',type = 'lower',tl.col="#FF5A5F",
        diag = FALSE,tl.cex = .85, addCoef.col = 'black',number.cex = .85, bg = "transparent")
mtext("Correlation Matrix of Numerical Features", at=6.8, line=-6, cex=2.5)

dev.copy(png,'corr_matrix.png')
dev.off()


# Minimized Correlation Matrix
myvars <- c("NumReserveDays2016Q3","MaxGuests","Longitude",
            "BookingQ2","AvgPriceQ2","NumberofReviews",
            "BlockedQ1","BlockedQ2","SecurityDeposit")
property_info_corr <- property_info_train[,myvars]
# Create Correlation Table
mydata.cor = cor(na.omit(property_info_corr))
# Plot Correlation Matrix - ZOOM TO SEE IMAGE BETTER
par(bg=NA)
corrplot(mydata.cor, method='ellipse', order = 'AOE',type = 'lower',tl.col="#FF5A5F",
         diag = FALSE,tl.cex = .85,addCoef.col = 'black',number.cex = 0.85,bg = "transparent")


# Histograms
par(bg="beige")
hist(property_info$NumberofReviews, breaks = 50,
     main="Histogram of Number of Reviews",
     xlab = "Number of Reviews")


par(bg="beige")
hist(property_info$MaxGuests, breaks = 30,
     main="Histogram of Max Guests",
     xlab = "Number of Max Guests")


################### LINEAR REGRESSION MODELS ################### - Predictive Analytics
################### - MODEL 1 
# Fit Model
reg1 <- lm(formula = NumReserveDays2016Q3 ~ BookingQ2 +BookingQ1+ ResponseRate + NumberofReviews +BlockedQ2+BlockedQ1+MinimumStay, data = property_info_train)
# Predict Q3 Bookings on TRAIN 
pred1_train <- predict(object = reg1, newdata = property_info_train)
# MSE of TRAIN
mse_pred1_train <- mean((property_info_train$NumReserveDays2016Q3 - pred1_train)^2, na.rm = TRUE)
summary(reg1)
sqrt(mse_pred1_train) ####MSE = 15.96833 , R2 = 0.7283, Residual Standard Error = 15.98
# Prediction Vector (on TEST)
pred1_test <- predict(object = reg1, newdata = property_info_test)

################### - MODEL 2 
# Fit Model
reg1 <- lm(formula = NumReserveDays2016Q3 ~ BookingQ2 + ResponseRate + NumberofReviews+BlockedQ1+MinimumStay, data = property_info_train)
# Predict Q3 Bookings on TRAIN 
pred1_train <- predict(object = reg1, newdata = property_info_train)
# MSE of TRAIN
mse_pred1_train <- mean((property_info_train$NumReserveDays2016Q3 - pred1_train)^2, na.rm = TRUE)
summary(reg1)
sqrt(mse_pred1_train) #MSE = 16.06493 , R2 = 0.725, Residual Standard Error = 16.07
# Prediction Vector (on TEST data)
pred1_test <- predict(object = reg1, newdata = property_info_test) 

################### - MODEL 3 ## Third submission ##
# Fit Model
reg1 <- lm(formula = NumReserveDays2016Q3 ~ BookingQ2 + NumberofReviews+BlockedQ1+MaxGuests+SecurityDeposit+BlockedQ2, data = property_info_train)
# Predict Q3 Bookings on TRAIN 
pred1_train <- predict(object = reg1, newdata = property_info_train)
# MSE of TRAIN
mse_pred1_train <- mean((property_info_train$NumReserveDays2016Q3 - pred1_train)^2, na.rm = TRUE)
summary(reg1)
sqrt(mse_pred1_train) #MSE = 15.94647, R2 = 0.7291, Residual Standard Error = 15.95, [RMSE = 15.8299]
# Prediction Vector (on TEST data)
pred <- predict(object = reg1, newdata = property_info_test) 

################### - MODEL 4 
# Fit Model
reg1 <- lm(formula = NumReserveDays2016Q3 ~ BookingQ2 + NumberofReviews+BlockedQ1+MaxGuests+SecurityDeposit, data = property_info_train)
# Predict Q3 Bookings on TRAIN 
pred1_train <- predict(object = reg1, newdata = property_info_train)
# MSE of TRAIN
mse_pred1_train <- mean((property_info_train$NumReserveDays2016Q3 - pred1_train)^2, na.rm = TRUE)
summary(reg1)
sqrt(mse_pred1_train) #MSE = 16.04019 , R2 = 0.7259, Residual Standard Error = 16.05
# Prediction Vector (on TEST data)
pred <- predict(object = reg1, newdata = property_info_test) 

################### - MODEL 5 
# Fit Model
reg1 <- lm(formula = NumReserveDays2016Q3 ~ RequirePhoneVerification+BookingQ2 + BlockedQ1+SecurityDeposit, data = property_info_train)
# Predict Q3 Bookings on TRAIN 
pred1_train <- predict(object = reg1, newdata = property_info_train)
# MSE of TRAIN
mse_pred1_train <- mean((property_info_train$NumReserveDays2016Q3 - pred1_train)^2, na.rm = TRUE)
summary(reg1)
sqrt(mse_pred1_train) #MSE = 16.66843, R2 = 0.704 Residual Standard Error = 16.67
# Prediction Vector (on TEST data)
pred <- predict(object = reg1, newdata = property_info_test)

################### - MODEL 6 
# Fit Model
reg1 <- lm(formula = NumReserveDays2016Q3 ~ RequirePhoneVerification+BookingQ2 +NumberofReviews+ BlockedQ1+SecurityDeposit, data = property_info_train)
# Predict Q3 Bookings on TRAIN 
pred1_train <- predict(object = reg1, newdata = property_info_train)
# MSE of TRAIN
mse_pred1_train <- mean((property_info_train$NumReserveDays2016Q3 - pred1_train)^2, na.rm = TRUE)
summary(reg1)
sqrt(mse_pred1_train) #MSE = 16.04704 , R2 = 0.7256, Residual Standard Error = 16.05
# Prediction Vector (on TEST data)
pred <- predict(object = reg1, newdata = property_info_test)

################### - MODEL 7 
# Fit Model
reg1 <- lm(formula = NumReserveDays2016Q3 ~ MaxGuests+NumberofPhotos+BookingQ2+BlockedQ1+SecurityDeposit, data = property_info_train)
# Predict Q3 Bookings on TRAIN 
pred1_train <- predict(object = reg1, newdata = property_info_train)
# MSE of TRAIN
mse_pred1_train <- mean((property_info_train$NumReserveDays2016Q3 - pred1_train)^2, na.rm = TRUE)
summary(reg1)
sqrt(mse_pred1_train) #MSE = 16.59822 , R2 = 0.7065, Residual Standard Error = 16.6, [RMSE = 16.453] 
# Prediction Vector (on TEST data)
pred <- predict(object = reg1, newdata = property_info_test)  

################### - MODEL 8 
# Fit Model
reg1 <- lm(formula = NumReserveDays2016Q3 ~ ResponseTimemin+NumberofPhotos+BookingQ2+BlockedQ1+SecurityDeposit, data = property_info_train)
# Predict Q3 Bookings on TRAIN 
pred1_train <- predict(object = reg1, newdata = property_info_train)
# MSE of TRAIN
mse_pred1_train <- mean((property_info_train$NumReserveDays2016Q3 - pred1_train)^2, na.rm = TRUE)
summary(reg1)
sqrt(mse_pred1_train) #MSE = 16.57208  , R2 = 0.7074 , Residual Standard Error = 16.58
# Prediction Vector (on TEST data)
pred <- predict(object = reg1, newdata = property_info_test)

################### - MODEL 9
# Fit Model
reg1 <- lm(formula = NumReserveDays2016Q3 ~ MaxGuests+NumberofPhotos+BookingQ2+BlockedQ1+SecurityDeposit, data = property_info_train)
# Predict Q3 Bookings on TRAIN 
pred1_train <- predict(object = reg1, newdata = property_info_train)
# MSE of TRAIN
mse_pred1_train <- mean((property_info_train$NumReserveDays2016Q3 - pred1_train)^2, na.rm = TRUE)
summary(reg1)
sqrt(mse_pred1_train) #MSE = 16.59822 , R2 = 0.7065 , Residual Standard Error = 16.6
# Prediction Vector (on TEST data)
pred <- predict(object = reg1, newdata = property_info_test)  

################### - Submission 10 FINAL PREDICTION MODEL SUBMITTED (Model With Best RMSE Score) ################
# Fit Model
reg1 <- lm(formula = NumReserveDays2016Q3 ~ Superhost+Longitude+BookingQ2+AvgPriceQ2+NumberofReviews+BlockedQ1+MaxGuests+BlockedQ2, data = property_info_train)
# Predict Q3 Bookings on TRAIN 
pred1_train <- predict(object = reg1, newdata = property_info_train)
# MSE of TRAIN
mse_pred1_train <- mean((property_info_train$NumReserveDays2016Q3 - pred1_train)^2, na.rm = TRUE)
summary(reg1)
sqrt(mse_pred1_train) #MSE = 15.84467 , R2 = .7325 , Residual Standard Error = 15.86 [RMSE =15.6562]
# Prediction Vector (on TEST data)
pred <- predict(object = reg1, newdata = property_info_test)  




################### REGRESSION TREE MODELS ################### - Predictive Analytics
################### - MODEL 1 
# Fit Model
rt = rpart(formula = NumReserveDays2016Q3 ~  MaxGuests+NumberofPhotos+BookingQ2+BlockedQ1+SecurityDeposit, data = property_info_train,method = "anova")
# Predict Q3 Bookings on TRAIN 
pred_train <- predict(object = rt, newdata = property_info_train)
# MSE of TRAIN
mse_pred_train <- mean((property_info_train$NumReserveDays2016Q3 - pred_train)^2, na.rm = TRUE)
summary(rt)
sqrt(mse_pred_train) #MSE = 17.42342
# Prediction Vector (on TEST)
pred <- predict(object = rt, newdata = property_info_test)   

################### - MODEL 2 
# Fit Model
rt = rpart(formula = NumReserveDays2016Q3 ~ RequirePhoneVerification+BookingQ2 +NumberofReviews+ BlockedQ1+SecurityDeposit, data = property_info_train,method = "anova")
# Predict Q3 Bookings on TRAIN  
pred_train <- predict(object = rt, newdata = property_info_train)
# MSE of TRAIN
mse_pred_train <- mean((property_info_train$NumReserveDays2016Q3 - pred_train)^2, na.rm = TRUE)
summary(rt)
sqrt(mse_pred_train) #MSE = 16.61911
# Prediction Vector (on TEST)
pred <- predict(object = rt, newdata = property_info_test)  

################### - MODEL 3
# Fit Model
rt = rpart(formula = NumReserveDays2016Q3 ~ RequirePhoneVerification+BookingQ2 + BlockedQ1+SecurityDeposit, data = property_info_train,method = "anova")
# Predict Q3 Bookings on TRAIN 
pred_train <- predict(object = rt, newdata = property_info_train)
# MSE of TRAIN
mse_pred_train <- mean((property_info_train$NumReserveDays2016Q3 - pred_train)^2, na.rm = TRUE)
summary(rt)
sqrt(mse_pred_train) #MSE = 17.42342
# Prediction Vector (on TEST)
pred <- predict(object = rt, newdata = property_info_test)  

################### - MODEL 4
# Fit Model
rt = rpart(formula = NumReserveDays2016Q3 ~ BookingQ2 + NumberofReviews+BlockedQ1+MaxGuests+SecurityDeposit, data = property_info_train,method = "anova")
# Predict Q3 Bookings on TRAIN 
pred_train <- predict(object = rt, newdata = property_info_train)
# MSE of TRAIN
mse_pred_train <- mean((property_info_train$NumReserveDays2016Q3 - pred_train)^2, na.rm = TRUE)
summary(rt)
sqrt(mse_pred_train) #MSE = 16.61911
# Prediction Vector (on TEST)
pred <- predict(object = rt, newdata = property_info_test)  

################### - MODEL 5
# Fit Model
rt = rpart(formula = NumReserveDays2016Q3 ~ BookingQ2 + NumberofReviews+BlockedQ1+MaxGuests+SecurityDeposit+BlockedQ2, data = property_info_train,method = "anova")
# Predict Q3 Bookings on TRAIN 
pred_train <- predict(object = rt, newdata = property_info_train)
# MSE of TRAIN
mse_pred_train <- mean((property_info_train$NumReserveDays2016Q3 - pred_train)^2, na.rm = TRUE)
summary(rt)
sqrt(mse_pred_train) #MSE = 16.61911
# Prediction Vector (on TEST)
pred <- predict(object = rt, newdata = property_info_test)

################### - MODEL 6
# Fit Model
rt = rpart(formula = NumReserveDays2016Q3 ~ BookingQ2 + ResponseRate + NumberofReviews+BlockedQ1+MinimumStay, data = property_info_train,method = "anova")
# Predict Q3 Bookings on TRAIN 
pred_train <- predict(object = rt, newdata = property_info_train)
# MSE of TRAIN
mse_pred_train <- mean((property_info_train$NumReserveDays2016Q3 - pred_train)^2, na.rm = TRUE)
summary(rt)
sqrt(mse_pred_train) #MSE = 16.61911
# Prediction Vector (on TEST)
pred <- predict(object = rt, newdata = property_info_test)

################### - MODEL 7
# Fit Model
rt = rpart(formula = NumReserveDays2016Q3 ~ BookingQ2 +BookingQ1+ ResponseRate + NumberofReviews +BlockedQ2+BlockedQ1+MinimumStay, data = property_info_train,method = "anova")
# Predict Q3 Bookings on TRAIN 
pred_train <- predict(object = rt, newdata = property_info_train)
# MSE of TRAIN
mse_pred_train <- mean((property_info_train$NumReserveDays2016Q3 - pred_train)^2, na.rm = TRUE)
summary(rt)
sqrt(mse_pred_train) #MSE = 16.61911
# Prediction Vector (on TEST)
pred <- predict(object = rt, newdata = property_info_test)



################### SUBMIT!!!!!!!!! ################### 
## Save Prediction Vector for submission ##
save(pred, file = "/Users/joel/Desktop/")



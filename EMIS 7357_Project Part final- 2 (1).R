# Importing the data file
setwd("~/R/Hotel Bookings")
hotel_bookings = read.csv("hotel_bookings.csv")
str(hotel_bookings)
summary(hotel_bookings)


# Data pre-processing
# Count NA Values in Each Column of the DF
library(dplyr)
hotel_bookings %>% summarise(across(everything(), ~ sum(is.na(.)))) 

# Taking care of missing data
hotel_bookings$children = ifelse(is.na(hotel_bookings$children),
                          ave(hotel_bookings$children, FUN = function(x) mean(x, na.rm = TRUE)),
                          hotel_bookings$children)
hotel_bookings %>% summarise(across(everything(), ~ sum(is.na(.))))
summary(hotel_bookings)

#Removing Null rows
hotel_bookings <- na.omit(hotel_bookings)
attr(hotel_bookings,"na.action")
hotel_bookings<- filter(hotel_bookings, adr >= 0)

# validate the data
# in each row number of adults, children and babies altogether cannot be zero
nrow(hotel_bookings[hotel_bookings$adults == 0 & hotel_bookings$children == 0 & hotel_bookings$babies== 0,])
hotel_bookings<-hotel_bookings[!(hotel_bookings$adults == 0 & hotel_bookings$children == 0 & hotel_bookings$babies== 0),]

# number of stays in weekend and weekdays cannot be zero since booking count is per night
nrow(hotel_bookings[hotel_bookings$stays_in_weekend_nights== 0 & hotel_bookings$stays_in_week_nights == 0,])
hotel_bookings<-hotel_bookings[!(hotel_bookings$stays_in_weekend_nights== 0 & hotel_bookings$stays_in_week_nights == 0),]

# number of adults cannot be zero, children and infant cannot do the check in of hotel
nrow(hotel_bookings[hotel_bookings$adults== 0 ,])
hotel_bookings<-hotel_bookings[!(hotel_bookings$adults == 0), ]

# Encoding categorical data
hotel_bookings$hotel = factor(hotel_bookings$hotel,
                              levels = c('Resort Hotel', 'City Hotel'),
                              labels = c(0, 1))
hotel_bookings$customer_type = factor(hotel_bookings$customer_type,
                                      levels = c('Transient', 'Transient-Party', 'Group', 'Contract'),
                                      labels = c('Transient', 'Transient-Party', 'Group', 'Contract'))
hotel_bookings$reservation_status = factor(hotel_bookings$reservation_status,
                                            levels = c('Check-Out', 'Canceled', 'No-Show'),
                                            labels = c(1, 2, 3))
hotel_bookings$assigned_room_type = as.factor(hotel_bookings$assigned_room_type)
hotel_bookings$reserved_room_type = as.factor(hotel_bookings$reserved_room_type)
hotel_bookings$arrival_date_month = as.factor(hotel_bookings$arrival_date_month)
hotel_bookings$arrival_date_year = as.factor(hotel_bookings$arrival_date_year)
hotel_bookings$market_segment = as.factor(hotel_bookings$market_segment)
hotel_bookings$arrival_date_year = as.factor(hotel_bookings$arrival_date_year)
hotel_bookings$is_canceled = factor(hotel_bookings$is_canceled,
                              levels = c('1', '0'),
                              labels = c('Yes', 'No'))

#Creating a column that summarizes the total no. of stays in the hotels
hotel_bookings$total_stays = hotel_bookings$stays_in_weekend_nights + hotel_bookings$stays_in_week_nights
summary(hotel_bookings)
hotel_bookings$total_stays = as.numeric(hotel_bookings$total_stays)
hotel_bookings$adults = as.numeric(hotel_bookings$adults)
hotel_bookings$lead_time = as.numeric(hotel_bookings$lead_time)
str(hotel_bookings)
hotel_bookings_unscaled = hotel_bookings
hotel_bookings_unscaled <- hotel_bookings_unscaled %>% 
  mutate(stay_cost_total = adr * total_stays)
str(hotel_bookings_unscaled)

numerical_cols <- select_if(hotel_bookings, is.numeric)
numerical_col <- scale(numerical_cols)
hotel_bookings<- hotel_bookings %>% dplyr::mutate(numerical_col)                     
hotel_bookings$lead_time <- scale(hotel_bookings$lead_time)
hotel_bookings$lead_time
hotel_bookings$adr <- scale(hotel_bookings$adr)
hotel_bookings$adr

##### Plot see distribution #####
library(ggplot2)
g_price <- ggplot(hotel_bookings, aes(x= adr)) +
geom_histogram(bins=40, color="black", fill="light blue")


g_day <- ggplot(hotel_bookings, aes(x= total_days_booked)) +
geom_histogram(bins=40, color="black", fill="light blue")

g_leadtime <- ggplot(hotel_bookings, aes(x= lead_time)) +
  geom_histogram(bins=40, color="black", fill="light blue")

g_request <- ggplot(hotel_bookings, aes(x= total_of_special_requests)) +
  geom_histogram(bins=40, color="black", fill="light blue")

library(ggpubr)

ggarrange(g_price,g_leadtime,g_request, ncol = 2, nrow = 2)

##### Calculate IQR and upper limit for outliers #####
IQR_unit_price = IQR(hotel_bookings$adr)
IQR_lead = IQR(hotel_bookings$lead_time)

price_upper = quantile(hotel_bookings$adr,probs = 0.75)+1.5*IQR_unit_price
lead_upper = quantile(hotel_bookings$lead_time,probs = 0.75)+1.5*IQR_lead

##### Filter out outliers #####
hotel_bookings <- hotel_bookings %>%
  filter((adr<=price_upper) & (lead_time<=lead_upper))

##### Plot see distribution #####
g_price2 <- ggplot(hotel_bookings, aes(x= adr)) +
  geom_histogram(bins=40, color="black", fill="light blue")

g_leadtime2 <- ggplot(hotel_bookings, aes(x= lead_time)) +
  geom_histogram(bins=40, color="black", fill="light blue")

g_request2 <- ggplot(hotel_bookings, aes(x= total_of_special_requests)) +
  geom_histogram(bins=40, color="black", fill="light blue")

ggarrange(g_price2,g_leadtime2,g_request2, ncol = 2, nrow = 2)
scale(hotel_bookings)
summary(hotel_bookings)

hotel_bookings <- hotel_bookings %>% 
  mutate(total_stays = stays_in_weekend_nights + stays_in_week_nights,
         stay_cost_total = adr * total_stays)

summary(hotel_bookings$total_stays)
summary(hotel_bookings$stay_cost_total)

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(hotel_bookings$adr, SplitRatio = 0.80)
training_set = subset(hotel_bookings, split == TRUE)
test_set = subset(hotel_bookings, split == FALSE)


# Part 2 Predictive Analytics - a)
library(car)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = adr ~ total_stays + hotel + reserved_room_type + lead_time + customer_type + arrival_date_month +
                  + stay_cost_total,
               data = training_set)
summary(regressor)
# avPlots(regressor, ask=FALSE)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)
y_pred

# Computing R^2 on Testing Set
SSE = sum((test_set$adr - y_pred)^2)
SST = sum((test_set$adr - mean(training_set$adr))^2)
1 - SSE/SST
str(test_set)


# Part 2 Predictive Analytics - b)
# Removing unwanted columns - optional
gc(reset = TRUE)
hotel_bookings2 <- hotel_bookings
hotel_bookings2 <- subset (hotel_bookings2, select = -c(arrival_date_year:stays_in_week_nights))
hotel_bookings2 <- subset (hotel_bookings2, select = -c(babies:previous_bookings_not_canceled))
hotel_bookings2 <- subset (hotel_bookings2, select = -c(assigned_room_type:customer_type))
hotel_bookings2 <- subset (hotel_bookings2, select = -c(required_car_parking_spaces:reservation_status_date))
str(hotel_bookings2)

# Splitting new data into test and train dataset
library(caTools)
split = sample.split(hotel_bookings2$is_canceled, SplitRatio = 0.80)
training_set_glm = subset(hotel_bookings2, split == TRUE)
test_set_glm = subset(hotel_bookings2, split == FALSE)
training_set_glm %>% select(-contains('numerical'))
str(training_set_glm)

# # Feature Scaling
# new_training_set = subset(training_set_glm, select = -c(hotel, reserved_room_type))
# scaled_training_set = scale(new_training_set[,-1])
# new_test_set = subset(test_set_glm, select = -c(hotel, reserved_room_type))
# scaled_test_set = scale(new_test_set[,-1])
# nrow(scaled_training_set)

# # Attaching non-numeric variables to the scaled data sets
# scaled_training_set = cbind(scaled_training_set, is_canceled = training_set_glm$is_canceled)
# training_set_glm = as.data.frame(matrix(unlist(scaled_training_set)))
# scaled_training_set$is_canceled = as.factor(scaled_training_set$is_canceled)
# summary(scaled_training_set)
# str(scaled_training_set)

# scaled_test_set = cbind(scaled_test_set, is_canceled = test_set_glm$is_canceled)
# summary(scaled_test_set)
# str(scaled_test_set)

# Fitting Logistic Regression to Training Set
classifier = glm(formula = is_canceled ~ .,
                                 family = binomial,
                                 data = training_set_glm)
summary(classifier)

# Removing non-significant columns (backward elimination)
classifier = glm(formula = is_canceled ~ hotel + reserved_room_type + lead_time + adults + adr + total_stays + stay_cost_total,
                 family = binomial,
                 data = training_set_glm)
summary(classifier)

# Refining the Model
classifier = glm(formula = is_canceled ~ hotel + reserved_room_type + lead_time + adults + adr + stay_cost_total,
                 family = binomial,
                 data = training_set)
summary(classifier)
 
# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-2], na.action = na.omit)
prob_pred

#Converting to binary results
y_pred = ifelse(prob_pred > 0.5, 1, 0)
y_pred
 
# #Making confusion Matrix
cm = table(test_set[,2], y_pred)
cm
gc(reset = TRUE)

# Visualizing the Training Set Results
# AUC for training set
library(ROCR)
gc(reset = TRUE)
PredictTrain = predict(classifier, type="response")
ROCRpred = prediction(PredictTrain, training_set$is_canceled) #this transforms input data into a standardized format
ROCCurve = performance(ROCRpred, "tpr", "fpr") #computes performance measures true positive rate (tpr) and false positive rate (fpr)
plot(ROCCurve)
plot(ROCCurve, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(ROCRpred, "auc")@y.values)

# Evaluating the model
table(training_set$is_canceled, PredictTrain > 0.5)
#Accuracy for cut-off = 0.5 : ((53102+10814)/(53102+10814+24565+7031)) = 66.92%
table(training_set$is_canceled, PredictTrain > 0.7)
#Accuracy for cut-off = 0.7 : ((59033+2846)/(59033+2846+32533+1100)) = 64.79%
table(training_set$is_canceled, PredictTrain > 0.2)
#Accuracy for cut-off = 0.2 : ((10374+33770)/(10374+33770+1609+33770)) = 55.51%

# AUC for testing set
gc(reset = TRUE)
PredictTest = predict(classifier, type="response", newdata=test_set)
PredictTest
table(test_set$is_canceled, PredictTest > 0.5)
Accuracy = (11857 + 2472)/(11857 + 2472 + 5494 + 1541)
Accuracy #67.07%
ROCRpred2 = prediction(PredictTest, test_set$is_canceled)
ROCCurve2 = performance(ROCRpred2, "tpr", "fpr")
plot(ROCCurve2)
plot(ROCCurve2, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(ROCRpred2, "auc")@y.values)
gc(reset = TRUE)



# Part 2 Predictive Analytics - c)
# #k-means clustering
gc(reset = TRUE)
dataset = hotel_bookings_unscaled %>% filter(is_canceled == 'No')
dataset = dataset %>% filter(adr > 0)
dataset = dataset %>% filter(adults > 0)
nrow(dataset[dataset$is_canceled == 'Yes',])
str(dataset)
summary(dataset)
X = dataset[c(33,10)]
X <- na.omit(X)
X$total_stays <- as.numeric(X$total_stays)
# X$customer_type <- as.numeric(X$customer_type)
#X$adults <- as.numeric((X$adults))
glimpse(X)
str(X)

# Using the elbow method to find the optimal number of clusters
library(factoextra)
library(cluster)
set.seed(123)
wcss = vector()
for (i in 1:20) wcss[i] = sum(kmeans(X, i)$withinss)
plot(x = 1:20,
     y = wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')
 
df = scale(X)
df <- df[sample(1:nrow(df), 2000), ]
X2 <- X[sample(1:nrow(X), 2000), ]
str(df)
glimpse(df)

head(df)
summary(df)
gc(reset = TRUE)
memory.limit(size = 1e+12)
gap_stat <- clusGap(df,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 12,
                    B = 50)
fviz_gap_stat(gap_stat)
gc(reset = TRUE)

# Fitting K-Means to the dataset
set.seed(123)
kmeans = kmeans(x = X2,
                centers = 12,
                iter.max = 300,
                nstart = 20)

# Visualising the clusters
library(cluster)
clusplot(x = X2,
         clus = kmeans$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 1,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of Total Stays'),
         xlab = 'No. of Adults',
         ylab = 'Total Stays')
 
#  Hierarchical Clustering
#  Using the dendrogram to find the optimal number of clusters
gc(reset = TRUE)
memory.limit(size = 1e+13)
dendrogram = hclust(d = dist(X2, method = 'euclidean'),
                    method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Customers',
     ylab = 'Euclidean distances')
gc(reset = TRUE)

# Fitting Hierarchical Clustering to the dataset
hc = hclust(d = dist(X, method = 'euclidean'),
            method = 'ward.D')
y_hc = cutree(hc, 5)

# Visualising the clusters
library(cluster)
clusplot(x = df,
         clus = y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')


# Part 2 Predictive Analytics - d)
gc(reset = TRUE)  
library(caTools)
set.seed(123)

# Separating the data into two dataframes - one for Resort Hotel and one for City Hotel 
# For Resort Hotel
hotel_bookings_resort = subset(hotel_bookings_unscaled, hotel == 0)
set.seed(123)
split = sample.split(hotel_bookings_resort$adr, SplitRatio = 0.80)
resort_train = subset(hotel_bookings_resort, split == TRUE)
resort_test = subset(hotel_bookings_resort, split == FALSE)
summary(resort_train)
str(resort_train)

#For City Hotel
hotel_bookings_city = subset(hotel_bookings_unscaled, hotel == 1)
set.seed(123)
split = sample.split(hotel_bookings_city$adr, SplitRatio = 0.80)
city_train = subset(hotel_bookings_city, split == TRUE)
city_test = subset(hotel_bookings_city, split == FALSE)
summary(city_train)
str(city_train)

#Applying Multiple Linear Regression on Resort Dataset
resort_regressor = lm(formula = adr ~ total_stays + reserved_room_type + lead_time + customer_type + arrival_date_month +
                               arrival_date_year + reservation_status,
                               data = resort_train)
summary(resort_regressor)
str(resort_regressor)

# Predicting on test data
y_pred_resort = predict(resort_regressor, newdata = resort_test)
y_pred_resort

# Computing R^2 on Testing Set
SSE = sum((resort_test$adr - y_pred_resort)^2)
SST = sum((resort_test$adr - mean(resort_train$adr))^2)
1 - SSE/SST #Accuracy = 72.94%
str(resort_test)

#Applying Multiple Linear Regression on City Dataset
city_regressor = lm(formula = adr ~ reserved_room_type + lead_time + customer_type + arrival_date_month +
                        arrival_date_year + reservation_status + stay_cost_total,
                      data = city_train)
summary(city_regressor)

# Refining the Model
city_regressor = lm(formula = adr ~ 0 + total_stays + reserved_room_type + lead_time + customer_type + arrival_date_month +
                      arrival_date_year + reservation_status,
                    data =city_train)
summary(city_regressor)

# Predicting on test data
y_pred_city = predict(city_regressor, newdata =city_test)
y_pred_city

# Computing R^2 on Testing Set
SSE = sum((city_test$adr - y_pred_city)^2)
SST = sum((city_test$adr - mean(city_train$adr))^2)
1 - SSE/SST #Accuracy = 47.79%
str(city_test)


# Comparing the models
#Creating a validation set on hotel_bookings
set.seed(123)
split2 = sample.split(hotel_bookings_unscaled$adr, SplitRatio = 0.80)
training_set2 = subset(hotel_bookings_unscaled, split == TRUE)
test_set2 = subset(hotel_bookings_unscaled, split == FALSE)
Samples<-sample(seq(1,2),size=nrow(test_set2),replace=TRUE,prob=c(0.7,0.3))
Test<-test_set2[Samples==1,]
Validate<-test_set2[Samples==2,]
Validate
nrow(Validate[Validate$hotel == '1',])

# Performing multiple linear regression using Validate dataset
validate_regressor = lm(formula = adr ~ total_stays + reserved_room_type + lead_time + customer_type + arrival_date_month +
                      arrival_date_year + reservation_status,
                    data =Validate)
summary(validate_regressor)

# Refining the model
validate_regressor = lm(formula = adr ~ 0 + total_stays + reserved_room_type + lead_time + customer_type + arrival_date_month +
                          arrival_date_year + reservation_status,
                        data =Validate)

summary(validate_regressor)
str(validate_regressor)

#Checking the accuracy of the resort model using CART
library(rpart.plot)
library(rpart)
str(hotel_bookings_resort)
CART_resort = rpart(is_canceled ~ adr + reserved_room_type + arrival_date_month + lead_time + customer_type, data = hotel_bookings_resort, method="class", minbucket=50)
summary(CART_resort)
prp(CART_resort)
rpart.plot(CART_resort)

PredictTrainCART50_resort = predict(CART, newdata = Validate, type = "class")
MyTableTest50_resort=table(Validate$is_canceled, PredictTrainCART50)
MyTableTest50_resort
AccuTest50_resort=(MyTableTest50_resort[1,1]+MyTableTest50_resort[2,2])/sum(MyTableTest50_resort)
AccuTest50 # 100%


#Checking the accuracy of the city model using CART
str(hotel_bookings_city)
CART_city = rpart(is_canceled ~ adr + reserved_room_type + arrival_date_month + lead_time + customer_type, data = hotel_bookings_city, method="class", minbucket=50)
summary(CART_city)
prp(CART_city)
rpart.plot(CART_city)

PredictTrainCART50_city = predict(CART, newdata = Validate, type = "class")
MyTableTest50_city=table(Validate$is_canceled, PredictTrainCART50)
MyTableTest50_city
AccuTest50_city=(MyTableTest50_city[1,1]+MyTableTest50_city[2,2])/sum(MyTableTest50_city)
AccuTest50 # 100%

#Checking the accuracy of resort model on validate dataset
Validate <- subset(Validate, hotel == '0')
y_pred_resort_validate = predict(resort_regressor, newdata = Validate)
y_pred_resort_validate

SSE = sum((Validate$adr - y_pred_resort_validate)^2)
SST = sum(Validate$adr - mean(resort_train$adr)^2)
1 - SSE/SST #Accuracy = 72.94%
str(Validate)

#Checking the accuracy of city model on validate dataset
Validate <- subset(Validate, reserved_room_type != 'H')
Validate <- subset(Validate, hotel == '1')
y_pred_city_validate = predict(city_regressor, newdata = subset(Validate, reserved_room_type != 'H'))
y_pred_city_validate
SSE = sum((Validate$adr - y_pred_city_validate)^2)
SST = sum(Validate$adr - mean(city_train$adr)^2)
1 - SSE/SST #Accuracy = 72.94%
str(Validate)



# Part 2 Predictive Analytics - e)
hotel_bookings_rooms <- hotel_bookings_unscaled[c(4,5,7,21)]
hotel_bookings_rooms$assigned_room_type <- as.numeric(hotel_bookings_threshold$assigned_room_type)
str(hotel_bookings_rooms)
hotel_bookings_rooms %>%
                      group_by(arrival_date_year, arrival_date_month, arrival_date_day_of_month) %>%
                      dplyr::summarize(total_rooms = sum(assigned_room_type)) %>% 
                      as.data.frame()
str(hotel_bookings_rooms)

hotel_bookings_cancelled <- hotel_bookings_unscaled[c(4,5,7,2)]
hotel_bookings_cancelled$is_canceled = factor(hotel_bookings_cancelled$is_canceled,
                                    levels = c('Yes', 'No'),
                                    labels = c('1', '0'))
hotel_bookings_cancelled$is_canceled <- as.numeric(hotel_bookings_cancelled$is_canceled)
str(hotel_bookings_cancelled)
hotel_bookings_cancelled %>%
  group_by(arrival_date_year, arrival_date_month, arrival_date_day_of_month) %>%
  dplyr::summarize(total_cancelled = sum(is_canceled)) %>% 
  as.data.frame()
str(hotel_bookings_cancelled)

hotel_bookings_capacity <- hotel_bookings_rooms$assigned_room_type + hotel_bookings_cancelled$is_canceled
summary(hotel_bookings_capacity)
str(hotel_bookings_capacity)

hotel_bookings_threshold <- data.frame(predicted_occupancy = (hotel_bookings_rooms$assigned_room_type), 
                                       actual_occupancy = (hotel_bookings_rooms$assigned_room_type - hotel_bookings_cancelled$is_canceled), 
                                       overbooking_threshold = (hotel_bookings_capacity - hotel_bookings_cancelled$is_canceled))
print (hotel_bookings_threshold)
str(hotel_bookings_threshold)
print(mean(hotel_bookings_threshold$overbooking_threshold))
print(sd(hotel_bookings_threshold$overbooking_threshold))

print(mean(hotel_bookings_threshold$actual_occupancy))
print(sd(hotel_bookings_threshold$actual_occupancy))


# Plotting the overbooking threshold vs actual occupancy
hotel_bookings_threshold_plot = rbind(
                               data.frame(values=rnorm(20000, mean=2.309766, sd=1.844781), type="Overbooking Threshold"),
                                   data.frame(values=rnorm(20000, mean=0.6823502, sd=1.824139), type="Actual Occupancy")
                                )

threshold <- quantile(hotel_bookings_threshold_plot$values[hotel_bookings_threshold_plot$type=="Overbooking Threshold"], probs=0.95)

ggplot(hotel_bookings_threshold_plot, aes(x=values, color=type)) + geom_density() + geom_vline(xintercept=threshold)

library(pROC)
roc_score=roc(hotel_bookings_threshold$overbooking_threshold, hotel_bookings_threshold$actual_occupancy) #AUC score
plot(roc_score ,main ="ROC curve -- Overbooking Threshold ") #Threshold value nearest to 1, i.e 1.5


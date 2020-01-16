#This statement is used to clear the environment
rm(list=ls(all=T))

#The libraries are loaded
x=c("ggplot2","DMwR","corrgram","rpart","randomForest","DMwR")
install.packages(x)
lapply(x,require,character.only=T)
rm(x)

#The working directory is set
setwd("F:/Data Science/Edwisor/Project/Bike_Rental_Pred")

#Now the data is loaded
rental_data = read.csv("Day.csv", header=T, na.strings=c(" ","","NA"))

############################# Missing Value Analysis ################################################

missing_val = data.frame(apply(rental_data[,2:15],2,function(x){sum(is.na(x))}))
colnames(missing_val)="Total Missing Values"
rm(missing_val) #To free the memory allocated

#From the missing value analysis, we have found that there are no missing values in the dataset

################################# Outlier Analysis #################################################

#Creating Histograms to check the data distribution
for (i in 10:15)
{
  assign(colnames(rental_data)[i],ggplot(rental_data,aes_string(x=rental_data[,i]))+
           labs(x=colnames(rental_data)[i])+geom_histogram())
}

gridExtra::grid.arrange(temp, atemp, registered, ncol=3)
gridExtra::grid.arrange(hum, windspeed, casual, ncol=3)

#Creating Box and Whisker plots to check for outliers
for (i in 10:15)
{
  assign(colnames(rental_data)[i],
         ggplot(rental_data,aes_string(x=rental_data[,i],y=colnames(rental_data)[i]))+
           labs(x=colnames(rental_data)[i])+
           stat_boxplot(geom = "errorbar", width = 0.5) +
geom_boxplot(outlier.colour="red",fill = "grey" ,outlier.shape=18, outlier.size=1, notch=F))
}

gridExtra::grid.arrange(temp, atemp, registered, ncol=3)
gridExtra::grid.arrange(hum, windspeed, casual, ncol=3)

#Handling Outliers using KNN Imputation method

#Taking a copy of the data and initiating columns for outlier handling
df = rental_data
cnames = c("hum","windspeed","casual")

#Replace outliers with NA
for(i in cnames){
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  print(length(val)) #To visualise th total number of outliers
  df[,i][df[,i] %in% val] = NA
  }

#Impute missing values using KNN
library(DMwR)
install.packages("curl")
library(curl)
install.packages("class")
library(class)
??knn
??knnImputation
df = knnImputation(df, k = 3)
df["cnt"] = df["casual"]+df["registered"]

#Box and whisker plot to visualise after outlier analysis
for (i in cnames){
  assign(i,ggplot(df,aes_string(x=df[,i],y=i))+
  labs(x=i)+stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.colour="red",fill = "grey" ,outlier.shape=18, outlier.size=1, notch=F))
}

gridExtra::grid.arrange(hum, windspeed, casual, ncol=3)

#From the box and whisker plots, we can find that the outliers have considerably been reduced 
#So the model impact due to bias will be reduced.

#################################### Feature Selection ############################################

#Formation of Correlation matrix and heatmap based on the correlation between variables
corrgram(df[,3:16], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

df = subset(df, select = -c(instant,dteday,holiday,weekday,hum,windspeed,atemp,mnth))

#The variables of least importance and the variables which may lead to overfitting are removed and proceeded to the next phase.

####################################### Feature Scaling #########################################

#The column temp is already normalised and the columns 'casual', 'registered' and 'temp' are to be normalised
norm_var = c("casual","registered","cnt")
for (i in norm_var){
  hist(df[,i])
  df[,i] = (df[,i] - min(df[,i]))/
    (max(df[,i] - min(df[,i])))
}
print(head(df))

##################################### Decision Tree ##########################################
#Error Metrics
#MAPE
MAPE = function(act,pred){
  mean(abs(act-pred)/act)*100
}

#RMSE
RMSE = function(act, pred){
  (mean((act-pred)**2))**0.5
}
#The first model that we are using is the decision tree
train_index = sample(1:nrow(df),0.8*nrow(df)) #80% of the whole index is sampled for selecting the train data
#Train and test data are sampled
train_data = df[train_index,]
test_data = df[-train_index,]

#Now the decision tree model is built using rpart
casual_model = rpart(casual ~., data = train_data[,1:6], method = "anova")
registered_model = rpart(registered ~., data = train_data[,c(1:5,7)], method = "anova")
cnt_model = rpart(cnt ~., data = train_data[,c(1:5,8)], method = "anova")

#Using the model, the dependent variable is predicted
casual_prediction = predict(casual_model, test_data[,1:5])
registered_prediction = predict(registered_model, test_data[,1:5])
cnt_prediction = predict(cnt_model, test_data[,1:5])

#Model Evaluation

print("RMSE")
print(paste("casual - ",(RMSE(test_data[,6],casual_prediction))))
print(paste("registered - ",(RMSE(test_data[,7],registered_prediction))))
print(paste("cnt - ",(RMSE(test_data[,8],cnt_prediction))))

print("MAPE")
print(paste("casual - ",(MAPE(test_data[,6],casual_prediction))))
print(paste("registered - ",(MAPE(test_data[,7],registered_prediction))))
print(paste("cnt - ",(MAPE(test_data[,8],cnt_prediction))))

#The outputs obtained are given as comments

#RMSE
#"casual -  0.120012777101718"
#"registered -  0.106531368734166"
#"cnt -  0.0964989527097329"

#MAPE
#"casual -  47.6220988759675"
#"registered -  21.4695856369951"
#"cnt -  18.4708929146792"

#Now we can measure the error metrics for the Random forest in the below section

#################################### Random Forest ###########################################

#Now the random forest model is built using randomForest library
casual_model = randomForest(casual ~., data = train_data[,1:6], ntree = 10)
registered_model = randomForest(registered ~., data = train_data[,c(1:5,7)], ntree = 10)
cnt_model = randomForest(cnt ~., data = train_data[,c(1:5,8)], ntree = 10)

#Using the model, the dependent variable is predicted
casual_prediction = predict(casual_model, test_data[,1:5])
registered_prediction = predict(registered_model, test_data[,1:5])
cnt_prediction = predict(cnt_model, test_data[,1:5])

#Model Evaluation

print("RMSE")
print(paste("casual - ",(RMSE(test_data[,6],casual_prediction))))
print(paste("registered - ",(RMSE(test_data[,7],registered_prediction))))
print(paste("cnt - ",(RMSE(test_data[,8],cnt_prediction))))

print("MAPE")
print(paste("casual - ",(MAPE(test_data[,6],casual_prediction))))
print(paste("registered - ",(MAPE(test_data[,7],registered_prediction))))
print(paste("cnt - ",(MAPE(test_data[,8],cnt_prediction))))

#The outputs obtained are given as comments

#RMSE
#"casual -  0.14501263201758"
#"registered -  0.096473865409887"
#"cnt -  0.0885746302777854"

#MAPE
#"casual -  43.5856369951654"
#"registered -  18.2914679285633"
#"cnt -  17.4439937438519"

############################### Linear Regression ############################################

#The same train test data is used for linear regression too
#Model generation
casual_model = lm(casual ~., data = train_data[,1:6])
registered_model = lm(registered ~., data = train_data[,c(1:5,7)])
cnt_model = lm(cnt ~., data = train_data[,c(1:5,8)])

#Using the model, the dependent variable is predicted
casual_prediction = predict(casual_model, test_data[,1:5])
registered_prediction = predict(registered_model, test_data[,1:5])
cnt_prediction = predict(cnt_model, test_data[,1:5])

#Model Evaluation

print("RMSE")
print(paste("casual - ",(RMSE(test_data[,6],casual_prediction))))
print(paste("registered - ",(RMSE(test_data[,7],registered_prediction))))
print(paste("cnt - ",(RMSE(test_data[,8],cnt_prediction))))

print("MAPE")
print(paste("casual - ",(MAPE(test_data[,6],casual_prediction))))
print(paste("registered - ",(MAPE(test_data[,7],registered_prediction))))
print(paste("cnt - ",(MAPE(test_data[,8],cnt_prediction))))

#The outputs obtained are given as comments

#RMSE
#"casual -  0.128242142735652"
#"registered -  0.10249082905932"
#"cnt -  0.107447951947048"

#MAPE
#"casual -  54.2798720007824"
#"registered -  18.8597522591259"
#"cnt -  18.1615320907755"

#To visualise the summary of the model
summary(cnt_model)

#Based on the error metrics, for this dataset, RandomForest model performs better when compared to all the other models

################################ Sample Output ###############################################

#Sample input of 5 rows is fetched from the dataset
sample_input = df[c(100,200,300,400,500),]

#Random Forest model is built using the complete dataset
casual_model = randomForest(casual ~., data = df[,1:6], ntree = 10)
registered_model = randomForest(registered ~., data = df[,c(1:5,7)], ntree = 10)
cnt_model = randomForest(cnt ~., data = df[,c(1:5,8)], ntree = 10)

#Using these models the dependent variables are predicted for the sample_input
casual_prediction = predict(casual_model, sample_input[,1:5])
registered_prediction = predict(registered_model, sample_input[,1:5])
cnt_prediction = predict(cnt_model, sample_input[,1:5])

#Output dataframe is created and it is stored in csv file and exported
sample_output = data.frame(sample_input,casual_pd=casual_prediction,+registered_pd=registered_prediction,cnt_pd=cnt_prediction)
write.csv(sample_output,file = "R Sample Output.csv", row.names = F)


################################# Interesting Patterns ########################################

#Let us initiate the dataframe once again for finding patterns
rental_data = read.csv("Day.csv", header=T, na.strings=c(" ","","NA"))

#Pattern to check how cnt has varied on a daily basis
ggplot(rental_data, aes_string(x=rental_data$dteday,y=rental_data$cnt,group=1))+geom_line()

#Bar plot to check how cnt has varied on workingday and based on weather situation
ggplot(rental_data, aes_string(x=rental_data$workingday,y=rental_data$cnt,group=1))+geom_bar(stat="identity")
ggplot(rental_data, aes_string(x=rental_data$weathersit,y=rental_data$cnt,group=1))+geom_bar(stat="identity")

#Bar plot to check the relationship between temp, hum, windspeed with cnt variable
for (i in c("temp","hum","windspeed")){
  assign(i,ggplot(rental_data, aes_string(x=rental_data[,i],y=rental_data[,"cnt"]))+geom_point())
}

gridExtra::grid.arrange(temp,hum,windspeed,ncol=3)

#From these graphs, we can see that the total count of rental bikes increase mid year and decreases at the beginning and end of the year
#There is no dierence between holiday and working day in terms of count of rental bikes
#Durinng hotter days, more bkes are rented, but no relationship between hum, windspeed and count of rental bikes

#Thus we have trained a model which can predict the cost of rental bikes based on environmental and seasonal settings.
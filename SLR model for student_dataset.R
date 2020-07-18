#Simple Linear Regression MOdel for predicting the score depending on hours of study

#Setting the Working Directory
setwd("D:/Study Materials/Internship/Task1/FirstTask")
# Importing the dataset 
Student=read.csv("D:/Study Materials/Internship/Task1/FirstTask/Student task.csv")

# Exploratory Data Analysis
head(Student) # display the first 6 observations
summary(Student)
cor(Student) #Calculating and understanding correlation Between Hours and Scores
cor(Student$Hours, Student$Scores) # Calculating correlation Between Hours and Scores specifically
hist(Student$Hours) #showing the frequency distribution
hist(Student$Scores) #showing the frequency distribution
dev.copy(pdf,file="Histogram for Hours & Scores.pdf") #To save the graph as PDF
dev.off()

#Graphical Analysis
#Scatter plot: Visualise the linear relationship between the predictor and response
#Box plot: To spot any outlier observations in the variable. Having outliers in your predictor can drastically affect the predictions as they can affect the direction/slope of the line of best fit.
#Density plot: To see the distribution of the predictor variable. Ideally, a close to normal distribution (a bell shaped curve), without being skewed to the left or right is preferred.

#Using Scatter Plot To Visualise The Relationship between Hours of study and Score obtained
plot(Student, main="Scores ~ Hours") #Scatter plot general view
scatter.smooth(x=Student$Hours, y=Student$Scores, main="Scores ~ Hours")  # scatterplot using smooth function to see the most probable line
dev.copy(pdf,file="Scatter plot.pdf")
dev.off()

#Using BoxPlot To Check For Outliers
#Generally, an outlier is any datapoint that lies outside the 1.5 * inter quartile range (IQR).
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(Student$Hours, main="Hours", sub=paste("Outlier rows: ", boxplot.stats(Student$Hours)$out))  # box plot for 'Hours'
boxplot(Student$Scores, main="Scores", sub=paste("Outlier rows: ", boxplot.stats(Student$Scores)$out))  # box plot for 'Scores'
dev.copy(pdf,file="Box plot.pdf")
dev.off()

#Using Density Plot To Check Normality of the variables
install.packages('e1071')
library(e1071)  # for skewness function
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(Student$Hours), main="Density Plot: Hours", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Student$Hours), 2)))  # density plot for 'Hours'
polygon(density(Student$Hours), col="green")
plot(density(Student$Scores), main="Density Plot: Scores", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Student$Scores), 2)))  # density plot for 'Scores'
polygon(density(Student$Scores), col="red")
dev.copy(pdf,file="Density plot.pdf")
dev.off()

#Building Linear Regression Model
StudentLm = lm(Scores ~ Hours, data = Student) # building linear regression model on full data
print(StudentLm)
#Diagnosis of the above Linear Model
summary(StudentLm)

#Prediction of Score of a given hours of Study with SLR developed with Fulldata
Hours<- data.frame(Hours=c(9.25))
Prediction_FullData <- cbind(Hours, Score= predict(StudentLm,Hours))
Prediction_FullData

#Predicting Linear Models
#Preparing Dataset
# Splitting the dataset into the 
# Training set and Test set 
set.seed(19)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(Student), 0.7*nrow(Student))  # row indices for training data
trainingData <- Student[trainingRowIndex, ]  # model training data
testData  <- Student[-trainingRowIndex, ]   # test data

# Build the model on training data
#Fit the model on training data and predict Scores on test data
Student.lm <- lm(Scores ~ Hours, data=trainingData)  # build the model
Predicted_Scores <- predict(Student.lm, testData)  # predict Scores
summary(Student.lm) #Review diagnostic measures

#Calculate prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=testData$Scores, predicteds=Predicted_Scores))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
head(actuals_preds)
install.packages('DMwR')
library(DMwR)
DMwR::regr.eval(actuals_preds$actuals, actuals_preds$predicteds)


# Visualising the Training set results 
ggplot() + geom_point(aes(x = trainingData$Hours,  
                          y = trainingData$Scores), colour = 'blue') +
  geom_line(aes(x = trainingData$Hours, 
                y = predict(Student.lm, newdata = trainingData)), colour = 'green') +
  
  ggtitle('Scores vs Hours (TrainingData)') +
  xlab('Hours of Study') +
  ylab('Scores') 
dev.copy(pdf,file="TrainingData visualization.pdf")
dev.off()

# Visualising the Test set results 
ggplot() + geom_point(aes(x = testData$Hours,  
                          y = testData$Scores), colour = 'red') +
  geom_line(aes(x = trainingData$Hours, 
                y = predict(Student.lm, newdata = trainingData)), colour = 'blue') +
  
  ggtitle('Scores vs Hours (TestData)') +
  xlab('Hours of Study') +
  ylab('Scores')
dev.copy(pdf,file="TestData visualization.pdf")
dev.off()
#geom_point() : This function scatter plots all data points in a 2 Dimensional graph
#geom_line() : Generates or draws the regression line in 2D graph
#ggtitle() : Assigns the title of the graph
#xlab : Labels the X- axis
#ylab : Labels the Y-axis

#Prediction of Score of a given hours of Study
Hours<- data.frame(Hours=c(9.25))
Prediction <- cbind(Hours, Score= predict(Student.lm,Hours))
Prediction
write.csv(Prediction,'Student_score.csv') #To store Data as CSV


library(devtools)
library(tidyverse)
library(plyr)
library(readr)
library(stringi)
library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(readxl)
library(cowplot)
library(doParallel)

#import survey data
surveydata <- read_excel("C:\\Users\\Javier Villasmil\\Desktop\\Ubiqum\\Repositories\\Classification - Brand Prediction\\1. Dataset\\Survey_Key_and_Complete_Responses_excel.xlsx",sheet = "Survey Results Complete")

View(surveydata)
str(surveydata)
summary(surveydata)
head(surveydata)

########################################################### PRE-PROCESS ###############################################################

#Change "brand" variable type to factor
surveydata$brand <- replace(surveydata$brand, surveydata$brand == '0', "acer")
surveydata$brand <- replace(surveydata$brand, surveydata$brand == '1', "sony")
surveydata$brand <- as.factor(surveydata$brand)

summary(surveydata$brand) #check brand count
str(surveydata$brand) #check brand levels

#Change "zipcode" variable type to factor and rename the levels
surveydata$zipcode <- as.factor(surveydata$zipcode)
surveydata$zipcode <- revalue(surveydata$zipcode, c("0" = "New England", 
                                                   "1" = "Mid-Atlantic", 
                                                   "2" = "East North Central", 
                                                   "3" = "West North Central",
                                                   "4" = "South Atlantic",
                                                   "5" = "East South Cental",
                                                   "6" = "West South Central",
                                                   "7" = "Mountain",
                                                   "8" = "Pacific"))

summary(surveydata$zipcode) #check zipcode levels

#Change "education level" variable type to factor and rename the levels
surveydata$elevel <- as.factor(surveydata$elevel)
surveydata$elevel <- revalue(surveydata$elevel,c(  "0" = "Less than High School", 
                                                   "1" = "High School Degree", 
                                                   "2" = "Some College", 
                                                   "3" = "4-year college degree",
                                                   "4" = "Masters or Doctoral"))
summary(surveydata$elevel) #check elevel levels

#Change "car" variable type to factor and rename the levels
surveydata$car <- as.factor(surveydata$car)

summary(surveydata$car)


#Change "Age" to integer and "credit" to numeric
surveydata$age <- as.integer(surveydata$age)

surveydata$credit <- as.numeric(surveydata$credit)

summary(surveydata$age) #check age
summary(surveydata$credit) #check credit



########################################################### DATA VISUALIZATION ###############################################################

#decision tree to check variable importance
treeoflife <- rpart( brand ~ . , data = surveydata, method = "class")

printcp(treeoflife) # display the results 
plotcp(treeoflife)
summary(treeoflife) # detailed summary of splits

#plor decision tree
plot(treeoflife, uniform = TRUE, 
     main = "Classification Tree for Brand")
text(treeoflife, use.n=TRUE, all=TRUE, cex=.8)

#histograms
h1 <- ggplot(surveydata, aes(x=salary, fill = brand)) + 
  geom_histogram(color="black")+
  theme(text = element_text(size=10) ,axis.text.x = element_text(size = 5),axis.text.y = element_text(size = 5))

h2 <- ggplot(surveydata, aes(x=age, fill = brand)) + 
  geom_histogram(color="black")+
  theme(text = element_text(size=10) ,axis.text.x = element_text(size = 5),axis.text.y = element_text(size = 5))

h3 <- ggplot(surveydata, aes(x=credit, fill = brand)) + 
  geom_histogram(color="black")+
  theme(text = element_text(size=10) ,axis.text.x = element_text(size = 5),axis.text.y = element_text(size = 5))

h4 <- ggplot(surveydata, aes(x=elevel, fill = brand)) + 
  geom_histogram(binwidth = 0.2, color="black",stat = "count" )+
  theme(text = element_text(size=10) ,axis.text.x = element_text(size = 5),axis.text.y = element_text(size = 5))

h5 <- ggplot(surveydata, aes(x=car, fill = brand)) + 
  geom_histogram(binwidth = 0.2, color="black",stat = "count" )+
  theme(text = element_text(size=10) ,axis.text.x = element_text(size = 5),axis.text.y = element_text(size = 5))

h6 <- ggplot(surveydata, aes(x=zipcode, fill=brand)) + 
  geom_histogram(binwidth = 0.2,color="black",stat = "count" )+
  theme(text = element_text(size=10) ,axis.text.x = element_text(size = 5),axis.text.y = element_text(size = 5))

h7 <- ggplot(surveydata, aes(x=brand)) + 
  geom_histogram(aes(fill=brand),binwidth = 0.2,stat = "count" )+
  guides(fill=FALSE)+
  theme(text = element_text(size=10) ,axis.text.x = element_text(size = 5),axis.text.y = element_text(size = 5))

plot_grid(h1,h2,h3,h4,h5,h6,h7)

# Color and shape depend on factor (categorical variable)
p1 <- ggplot(surveydata, aes(x=age, y=salary, color=brand)) + geom_point(size=3)+
  theme(text = element_text(size=10) ,axis.text.x = element_text(size = 5),axis.text.y = element_text(size = 5))

p2 <- ggplot(surveydata, aes(x=age, y=credit, color=brand)) + geom_point(size=3)+
  theme(text = element_text(size=10) ,axis.text.x = element_text(size = 5),axis.text.y = element_text(size = 5))

p3 <- ggplot(surveydata, aes(x=car, y=salary, fill=brand)) + geom_col(size=3)+
  theme(text = element_text(size=10) ,axis.text.x = element_text(size = 5),axis.text.y = element_text(size = 5))

p4 <- ggplot(surveydata, aes(x=zipcode, y=salary, fill=brand)) + geom_col(size=3)+
  theme(text = element_text(size=10) ,axis.text.x = element_text(size = 5),axis.text.y = element_text(size = 5))

p5 <- ggplot(surveydata, aes(x=elevel, y=salary, fill=brand)) + geom_col(size=3)+
  theme(text = element_text(size=10) ,axis.text.x = element_text(size = 5),axis.text.y = element_text(size = 5))

plot_grid(p1,p2,p3,p4,p5)

########################################################### MODELS ###############################################################

#random sampling
set.seed(123) 

#cretes a subset with similar distribution among the variable selected
trainingindices <- createDataPartition(y = surveydata$brand, p = .75, list = FALSE)

#train subset
training <- surveydata[trainingindices,]

#test subset
testing  <- surveydata[-trainingindices,]

# Prepare Parallel Process
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

#knN Models
#control parameters for cross-validation
controlknn <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = TRUE)

set.seed(123)

#kNN with "all variables"
modeloKNNall <- train(brand ~ .,data = training,method = "knn",trControl = controlknn,preProc = c("center", "scale"),tuneLength = 10)

#kNN with "salary"
modeloKNNs <- train(brand ~ salary,data = training,method = "knn",trControl = controlknn,preProc = c("center", "scale"),tuneLength = 10)

#kNN with "salary+age"
modeloKNNsa <- train(brand ~ salary + age,data = training,method = "knn",trControl = controlknn,preProc = c("center", "scale"),tuneLength = 10)

#models
modeloKNNall
modeloKNNs
modeloKNNsa

#plots
ggplot(modeloKNNall)
ggplot(modeloKNNs)
ggplot(modeloKNNsa)

#predictions agaisnt the test set
predictionknnall <- predict(modeloKNNall, newdata = testing)
predictionknns <- predict(modeloKNNs, newdata = testing)
predictionknnsa <- predict(modeloKNNsa, newdata = testing)

#postResample
performance_kNN_all <- postResample(predictionknnall,testing$brand)
performance_kNN_s <- postResample(predictionknns,testing$brand)
performance_kNN_sa <- postResample(predictionknnsa,testing$brand)

#confusion matrix
cm1 <- confusionMatrix(data = predictionknnall, testing$brand)
cm2 <- confusionMatrix(data = predictionknns, testing$brand)
cm3 <- confusionMatrix(data = predictionknnsa, testing$brand)

cm1$table
cm2$table
cm3$table

##############################################################################################################################################

#Decision Tree Models

#control parameters for cross-validation
controltree <- trainControl(method = "repeatedcv", repeats = 3,number = 10,verboseIter = TRUE)

set.seed(123)

#dt with "all variables"
modeloTREEall <- train(brand ~ .,data = training,method = "rpart",trControl = controltree, preProc = c("center", "scale"),tuneLength = 10)

#dt with "salary"
modeloTREEs <- train(brand ~ salary,data = training,method = "rpart",trControl = controltree,preProc = c("center", "scale"),tuneLength = 10)

#dt with "salary+age"
modeloTREEsa <- train(brand ~ salary+age,data = training,method = "rpart",trControl = controltree,preProc = c("center", "scale"),tuneLength = 10)


#models
modeloTREEall
modeloTREEs
modeloTREEsa

#plots
ggplot(modeloTREEall)
ggplot(modeloTREEs)
ggplot(modeloTREEsa)

prp(modeloTREEall$finalModel, box.palette = "Reds", tweak = 1.2)
prp(modeloTREEs$finalModel, box.palette = "Reds", tweak = 1.2)
prp(modeloTREEsa$finalModel, box.palette = "Reds", tweak = 1.0)

#predictions agaisnt the test set
predictiontreeall <- predict(modeloTREEall, newdata = testing)
predictiontrees <- predict(modeloTREEs, newdata = testing)
predictiontreesa <- predict(modeloTREEsa, newdata = testing)

#postResample
performance_dt_all <- postResample(predictiontreeall,testing$brand)
performance_dt_s <- postResample(predictiontrees,testing$brand)
performance_dt_sa <- postResample(predictiontreesa,testing$brand)

#confusion matrix
cm4 <- confusionMatrix(data = predictiontreeall, testing$brand)
cm5 <- confusionMatrix(data = predictiontrees, testing$brand)
cm6 <- confusionMatrix(data = predictiontreesa, testing$brand)

cm4$table
cm5$table
cm6$table
##############################################################################################################################################

#saving best models, Knn / Decisiton Tree with "Salary + Age" as predictors
save(modeloKNNsa, file = "kNN_salaryage_brand.rda")
save(modeloTREEsa, file = "rpart_salaryage_brand.rda")

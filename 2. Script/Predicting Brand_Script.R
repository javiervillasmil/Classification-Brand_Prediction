install.packages('caret', dependencies = TRUE)
install.packages("readr")
install.packages("stringi")
install.packages("plyr")
install.packages("ggplot2")
install.packages("tidyselect")
install.packages("pacman")

library(devtools)
library(readr)
library(plyr)
library(stringi)
library(lattice)
library(ggplot2)
library(caret)
library(pacman)
library(plyr)

###############################################################################################################################
remove.packages('rpart.plot')
remove.packages('rpart')
remove.packages('rattle')
remove.packages('RColorBrewer')

install.packages('rattle')
install.packages('rpart')
install.packages('rpart.plot')
install.packages('RColorBrewer')

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
########################### SE IMPORTA LA DATA DESDE EXCEL - SELECCIONANDO EL SHEET CORRESPONDIENTE ###########################

library(readxl)
surveydata <- read_excel("C:\\Users\\Javier Villasmil\\Desktop\\Ubiqum\\Task 05 - Classification Brand Prediction\\Survey_Key_and_Complete_Responses_excel.xlsx",sheet = "Survey Results Complete")

View(surveydata)
str(surveydata)
head(surveydata)

########################### SE REEMPLANZAN LOS VALORES DE LA VARIABLE "BRAND" NUMERICOS POR LA MARCA CORRESPONDIENTE, LUEGO SE CAMBIA LA VARIABLE A FACTOR DE DOS NIVELES ###########################
surveydata$brand <- replace(surveydata$brand, surveydata$brand == '0', "acer")
surveydata$brand <- replace(surveydata$brand, surveydata$brand == '1', "sony")
surveydata$brand <- as.factor(surveydata$brand)

summary(surveydata$brand)
str(surveydata$brand)
########################## SE CAMBIA LA VARIABLE "ZIPCODE" FACTOR Y SE RE-ASIGNAN LOS NIVELES SIGUIENDO LA KEY SuMINISTRADA ###########################
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

summary(surveydata$zipcode)

########################## SE CAMBIA LA VARIABLE "ELEVEL" A FACTOR Y SE RE-ASIGNAN LOS NIVELES SIGUIENDO LA KEY SuMINISTRADA ###########################
surveydata$elevel <- as.factor(surveydata$elevel)
surveydata$elevel <- revalue(surveydata$elevel,c(  "0" = "Less than High School", 
                                                   "1" = "High School Degree", 
                                                   "2" = "Some College", 
                                                   "3" = "4-year college degree",
                                                   "4" = "Masters or Doctoral"))
summary(surveydata$elevel)

########################## SE CAMBIA LA VARIABLE "CAR" A FACTOR - NO SE REASIGNARON VALORES ###########################
surveydata$car <- as.factor(surveydata$car)

summary(surveydata$car)

########################## SE CAMBIA LA VARIABLE "AGE" A INTEGER Y "CREDIT" A NUMERIC - NO SE REASIGNARON VALORES ###########################
surveydata$age <- as.integer(surveydata$age)

surveydata$credit <- as.numeric(surveydata$credit)

summary(surveydata$age)
summary(surveydata$credit)

str(surveydata$age)
str(surveydata$credit)

##############################################################################################################################################
########################################################### PLOT RELATIONSHIPS ###############################################################

par(mfrow = c(1,1))
treeoflife <- rpart( brand ~ . , data = surveydata, method = "class")

printcp(treeoflife) # display the results 
plotcp(treeoflife) # visualize cross-validation results 
summary(treeoflife) # detailed summary of splits

plot(treeoflife, uniform = TRUE, 
     main = "Classification Tree for Brand")
text(treeoflife, use.n=TRUE, all=TRUE, cex=.8)



# Color and shape depend on factor (categorical variable)
ggplot(surveydata, aes(x=age, y=salary, color=brand)) + geom_point(size=4, alpha=0.6)

ggplot(surveydata, aes(x=age, y=credit, color=brand)) + geom_point(size=4, alpha=0.6)

ggplot(surveydata, aes(x=car, y=salary, color=brand)) + geom_point(size=4, alpha=0.6)

ggplot(surveydata, aes(x=zipcode, y=salary, color=brand)) + geom_point(size=4, alpha=0.6)

ggplot(surveydata, aes(x=elevel, y=salary, color=brand)) + geom_point(size=4, alpha=0.6)


ggplot(surveydata, aes(x=salary)) + 
  geom_histogram(color="white", fill=rgb(0.2,0.7,0.1,0.4))

ggplot(surveydata, aes(x=age)) + 
  geom_histogram(color="white", fill=rgb(0.2,0.7,0.1,0.4))

ggplot(surveydata, aes(x=credit)) + 
  geom_histogram(color="white", fill=rgb(0.2,0.7,0.1,0.4))

ggplot(surveydata, aes(x=elevel)) + 
  geom_histogram(binwidth = 0.2, color="white", fill=rgb(0.2,0.7,0.1,0.4),stat = "count" )

ggplot(surveydata, aes(x=car)) + 
  geom_histogram(binwidth = 0.2, color="white", fill=rgb(0.2,0.7,0.1,0.4),stat = "count" ) 

ggplot(surveydata, aes(x=zipcode)) + 
  geom_histogram(binwidth = 0.2, color="white", fill=rgb(0.2,0.7,0.1,0.4),stat = "count" )

ggplot(surveydata, aes(x=brand)) + 
  geom_histogram(binwidth = 0.2, color="white", fill=rgb(0.2,0.7,0.1,0.4),stat = "count" )

######################################### REVISAR FORMULACION - MAKES NO SENSE
p  <- ggplot(surveydata, aes(salary, colour=car, fill=car))
p  <- p + geom_density(alpha=0.55)
p

p  <- ggplot(surveydata, aes(salary, colour=brand, fill=brand))
p  <- p + geom_density(alpha=0.55)
p

p  <- ggplot(surveydata, aes(credit, colour=age, fill=brand))
p  <- p + geom_density(alpha=0.55)
p

p  <- ggplot(surveydata, aes(elevel, colour=salary, fill=brand))
p  <- p + geom_density(alpha=0.55)
p
##############################################################################################################################################
##############################################################################################################################################

set.seed(123)

trainingindices <- createDataPartition(
  y = surveydata$brand,
  ## the outcome data are needed
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

training <- surveydata[trainingindices,]
testing  <- surveydata[-trainingindices,]


##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
############################################################## SE GENERA EL MODELO KNN #######################################################

#PARAMETROS DE CONTROL PARA CADA MODELO - E.g, CrossValidation, Accuracy y Kappa
controlknn <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed(123)

#modelo con todas las variables
modeloKNNall <- train(
  brand ~ .,
  data = training,
  method = "knn",
  trControl = controlknn,
  preProc = c("center", "scale"),
  tuneLength = 10)

#modelos con solo salary
modeloKNNs <- train(
  brand ~ salary,
  data = training,
  method = "knn",
  trControl = controlknn,
  preProc = c("center", "scale"),
  tuneLength = 10)

#modelos con age y salary 
modeloKNNsa <- train(
  brand ~ salary + age,
  data = training,
  method = "knn",
  trControl = controlknn,
  preProc = c("center", "scale"),
  tuneLength = 10)

############################################################## VERIFICACION DEL MODELO KNN ###################################################

modeloKNNall
modeloKNNs
modeloKNNsa

ggplot(modeloKNNall)
ggplot(modeloKNNs)
ggplot(modeloKNNsa)

############################################################## PREDICCION DE VALORES CONTRA EL TEST SET + CONFUSION MATRIX ###################

predictionknnall <- predict(modeloKNNall, newdata = testing)
predictionknns <- predict(modeloKNNs, newdata = testing)
predictionknnsa <- predict(modeloKNNsa, newdata = testing)

p1 <- confusionMatrix(data = predictionknnall, testing$brand)
p2 <- confusionMatrix(data = predictionknns, testing$brand)
p3 <- confusionMatrix(data = predictionknnsa, testing$brand)

p1
p2
p3

asdasdasd <- resamples(list(p1 = modeloKNNall, p2=modeloKNNs, p3=modeloKNNsa))
summary(asdasdasd)
diffs <- diff(asdasdasd)

summary(diffs)
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
############################################################## SE GENERA EL MODELO DECISION TREE #############################################

#PARAMETROS DE CONTROL PARA CADA MODELO - E.g, CrossValidation, Accuracy y Kappa
controltree <- trainControl(method = "repeatedcv", repeats = 3,number = 10)

set.seed(123)
#modelo con todas las variables
modeloTREEall <- train(
  brand ~ .,
  data = training,
  method = "rpart",
  trControl = controlknn,
  preProc = c("center", "scale"),
  tuneLength = 10)

modeloTREEs <- train(
  brand ~ salary,
  data = training,
  method = "rpart",
  trControl = controlknn,
  preProc = c("center", "scale"),
  tuneLength = 10)

modeloTREEsa <- train(
  brand ~ salary+age,
  data = training,
  method = "rpart",
  trControl = controlknn,
  preProc = c("center", "scale"),
  tuneLength = 10)


############################################################## VERIFICACION DEL MODELO DECISION TREE #############################################
modeloTREEall
modeloTREEs
modeloTREEsa


ggplot(modeloTREEall)
ggplot(modeloTREEs)
ggplot(modeloTREEsa)

prp(modeloTREEall$finalModel, box.palette = "Reds", tweak = 1.2)
prp(modeloTREEs$finalModel, box.palette = "Reds", tweak = 1.2)
prp(modeloTREEsa$finalModel, box.palette = "Reds", tweak = 1.0)

############################################################## PREDICCION DE VALORES CONTRA EL TEST SET + CONFUSION MATRIX ###################

predictiontreeall <- predict(modeloTREEall, newdata = testing)
predictiontrees <- predict(modeloTREEs, newdata = testing)
predictiontreesa <- predict(modeloTREEsa, newdata = testing)

confusionMatrix(data = predictiontreeall, testing$brand)
confusionMatrix(data = predictiontrees, testing$brand)
confusionMatrix(data = predictiontreesa, testing$brand)

##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################

SurveyIncomplete<-read.csv("C:\\Users\\Javier Villasmil\\Desktop\\Ubiqum\\Task 05 - Classification Brand Prediction\\SurveyIncomplete.csv")

SurveyIncomplete$brand <- replace(SurveyIncomplete$brand, SurveyIncomplete$brand == '0', "acer")
SurveyIncomplete$brand <- replace(SurveyIncomplete$brand, SurveyIncomplete$brand == '1', "sony")
SurveyIncomplete$brand <- as.factor(SurveyIncomplete$brand)

SurveyIncomplete$zipcode <- as.factor(SurveyIncomplete$zipcode)
SurveyIncomplete$zipcode <- revalue(SurveyIncomplete$zipcode,c("0" = "New England", 
                                                   "1" = "Mid-Atlantic", 
                                                   "2" = "East North Central", 
                                                   "3" = "West North Central",
                                                   "4" = "South Atlantic",
                                                   "5" = "East South Cental",
                                                   "6" = "West South Central",
                                                   "7" = "Mountain",
                                                   "8" = "Pacific"))

SurveyIncomplete$elevel <- as.factor(SurveyIncomplete$elevel)
SurveyIncomplete$elevel <- revalue(SurveyIncomplete$elevel,c(  "0" = "Less than High School", 
                                                   "1" = "High School Degree", 
                                                   "2" = "Some College", 
                                                   "3" = "4-year college degree",
                                                   "4" = "Masters or Doctoral"))

SurveyIncomplete$car <- as.factor(SurveyIncomplete$car)

SurveyIncomplete$age <- as.integer(SurveyIncomplete$age)

SurveyIncomplete$credit <- as.numeric(SurveyIncomplete$credit)


PredSurveyIncomplete<-predict(modeloKNNsa, newdata = SurveyIncomplete)
summary(PredSurveyIncomplete)
head(SurveyIncomplete)

barplot(summary(PredSurveyIncomplete), col= c(rgb(0.2,0.4,0.6,0.6),"lightgreen"), xlab="Brand", ylab = "Quantity")

?boxplot
par(mfrow = c(3,3)) 
for(i in 1:(ncol(surveydata))){
  boxplot(surveydata[,i])}

  surveydata

  
#ggplot format plot <- ggplot(dataframe) + aes(x,y - aesthetics) + geom_point() + geom_bar() + geom_line() + scale_x_conitnuos() + ggtitle ("")

#plotly can accept ggplot plots
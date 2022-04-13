library(dplyr)
library(tidyverse)
library(tidyr)
library(superml)
library(caTools)
library(MASS)
library(pscl)
library(car)
library(InformationValue)
library(caret)
library(rpart)
library(C50)
library(mlr)
library(ggplot2)
#install.packages("mlr")

# Read data
data = read.table("C:\\Users\\ravis\\OneDrive\\Desktop\\R\\credit.data", sep = ",", na.strings = "?");

names(data) <- c('Sex','Age','Debt','Married','BankCust','EducLevel','Ethnicity','YearsEmployed','PriorDefault'
                 ,'Employed','CreditScore','DLicense','Citizen','ZipCode','Income','Approved')

head(data)
dim(data)
str(data)

summarizeColumns(data)



#count of NA in all the columns
cbind(
  lapply(
    lapply(data, is.na), sum)
)

#cannot take mean of Zipcode,Sex,Married,EducLevel,Ethnicity,BankCust
#Hence, I will take median of this value to treat the missing value

attach(data)
#clean data


#manual binning

table(data$Sex)
data$Sex = ifelse(data$Sex == "a",0,1)
table(data$Sex)

table(data$Age)
table(data$Debt)

table(data$Married)
data$Married = ifelse(data$Married != "u", 1,0)
table(data$Married)

table(data$BankCust)
data$BankCust = ifelse(data$BankCust != "g", 1, 0)
table(data$BankCust)

table(data$EducLevel)

table(data$Ethnicity)


table(data$YearsEmployed)

table(data$PriorDefault)
data$PriorDefault = ifelse(data$PriorDefault == "f", 0, 1)
table(data$PriorDefault)

table(data$Employed)
data$Employed = ifelse(data$Employed == "f", 0, 1)
table(data$Employed)

table(data$CreditScore)

table(data$DLicense)
data$DLicense = ifelse(data$DLicense == "f", 0 , 1)
table(data$DLicense)

table(data$Citizen)
data$Citizen = ifelse(data$Citizen != "g",1, 0)
table(data$Citizen)

table(data$Approved)
data$Approved = ifelse(data$Approved == "-",0,1)
table(data$Approved)

table(data$ZipCode)
table(data$Income)




str(data)
summary(data)
#count of NA in all the columns
cbind(
  lapply(
    lapply(data, is.na), sum)
)

#cannot take mean of Zipcode,Sex,Married,EducLevel,Ethnicity,BankCust
#Hence, I will take median of this value to treat the missing value


for(i in 1:ncol(data)){
  data[,i][is.na(data[,i])] <- median(data[,i], na.rm = TRUE)
}
#automatic binning

library(superml)

label <- LabelEncoder$new()
data$Ethnicity<- label$fit_transform(data$Ethnicity)
table(data$Ethnicity)

data$EducLevel<- label$fit_transform(data$EducLevel)
table(data$EducLevel)

#treating missing value

data$Approved = as.factor(data$Approved)

str(data)


attach(data)
#library(dplyr)
data1 = data %>% dplyr::select(-ZipCode,-BankCust)



str(data1)
attach(data1)
h <- hist(Income, breaks = 25, density = 10, xlab = "Income", main = "Distribution of Income")
xfit <- seq(min(Income), max(Income), length = 40)
yfit <- dnorm(xfit, mean = mean(Income), sd = sd(Income))
yfit <- yfit * diff(h$mids[1:2]) * length(Income)
lines(xfit, yfit, col = "red", lwd = 2)


data1 = data1 %>% drop_na()

## new

boxplot(data1$Debt~data1$Approved, main = "Debt vs Approved")
summary(data1$Debt)
b = data1$Debt[!data1$Debt %in% boxplot.stats(data1$Debt)$out]
summary(b)
data1$Debt = ifelse(data1$Debt > max(b), NA, data1$Debt)

boxplot(data1$Age, main = "Age")
summary(Age)
a = data1$Age[!data1$Age %in% boxplot.stats(data1$Age)$out]
summary(a)
data1$Age = ifelse(data1$Age > max(a), NA, data1$Age)

boxplot(YearsEmployed~Approved, main = "YearsEmployed vs Approved")
summary(data1$YearsEmployed)
y = data1$YearsEmployed[!data1$YearsEmployed %in% boxplot.stats(data1$YearsEmployed)$out]
summary(y)
data1$YearsEmployed = ifelse(data1$YearsEmployed > max(y), NA, data1$YearsEmployed)


boxplot(EducLevel~Approved, main = "EducationLevel vs Approved")

boxplot(Ethnicity~Approved, main = "Ethnicity vs Approved")
summary(Ethnicity)
e = data1$Ethnicity[!data1$Ethnicity %in% boxplot.stats(data1$Ethnicity)$out]
summary(e)
data1$Ethnicity = ifelse(data1$Ethnicity > max(e), NA, data1$Ethnicity)

data1 = data1 %>% drop_na()

boxplot(CreditScore~Approved)
boxplot(Income~Approved)
boxplot(Debt~Approved)



dim(data1)
str(data1)

#Visualization

counts_a <- table(data1$Age)
barplot(counts_a, las = 2, col = "green", main = "Age")

counts_d <- table(data1$Debt)
barplot(counts_d, las = 2, col = "red", main = "Debt")

counts_y <- table(data1$YearsEmployed)
barplot(counts_y, las = 2, col = "pink", main = "Years Employed")

counts_c <- table(data1$CreditScore)
barplot(counts_c, las = 2, col = "green", main = "Credit Score")


str(data1)
attach(data1)

#par(mfrow = c(2,2))

ggplot(data = data1) + 
  geom_bar(mapping = aes(x = Sex, fill = Approved), position = "dodge")

ggplot(data = data1) + 
  geom_bar(mapping = aes(x = Ethnicity, fill = Approved), position = "dodge")

ggplot(data = data1) + 
  geom_bar(mapping = aes(x = EducLevel, fill = Approved), position = "dodge")

ggplot(data = data1) + 
  geom_bar(mapping = aes(x = Married, fill = Approved), position = "dodge")

ggplot(data = data1) + 
  geom_bar(mapping = aes(x = PriorDefault, fill = Approved), position = "dodge")

ggplot(data = data1) + 
  geom_bar(mapping = aes(x = Employed, fill = Approved), position = "dodge")

ggplot(data = data1) + 
  geom_bar(mapping = aes(x = Approved, fill = Approved), position = "dodge")

ggplot(data = data1) + 
  geom_bar(mapping = aes(x = DLicense, fill = Approved), position = "dodge")


str(data1)





#library("caTools")
split = sample.split(data1$Approved, SplitRatio = 0.8)
train_data <- subset(data1, split == T)
test_data = subset(data1, split == F)

dim(train_data)
dim(test_data)



model = glm(formula = Approved~., family = "binomial", data = train_data)
summary(model)


#library("MASS")
step <- stepAIC(model,direction = "both")


model_1 = step(model, direction = "both")
summary(model_1)


caret::varImp(model_1)

#library("pscl")
pscl::pR2(model_1)["McFadden"]

#library("car")
car::vif(model_1)


pred = predict(model_1, test_data, type = "response")
#str(pred)
#table(pred,test_data$Approved)

pred_t = as.factor(ifelse(pred > 0.62, "1","0"))
#str(pred_t)

####
#install.packages("InformationValue")
library("InformationValue")


#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(test_data$Approved, pred)
optimal


#library("caret")

confusionMatrix(test_data$Approved, pred_t)


#calculate sensitivity
sensitivity(test_data$Approved,pred_t)

#calculate specificity
specificity(test_data$Approved, pred_t)

#calculate total misclassification error rate
misClassError(test_data$Approved, pred, threshold=optimal)


plotROC(test_data$Approved, pred)


pairs(data1)
install.packages("ggplot2")            # Packages need to be installed only once
install.packages("GGally")

library("ggplot2")                     # Load ggplot2 package
library("GGally")
ggpairs(data1)
##-----------------------------------

## classification

library("caret")
library("rpart")
set.seed(123)


training.samples <- data1$Approved %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- data1[training.samples, ]
test.data <- data1[-training.samples, ]


md1 <- rpart(Approved ~., data = train.data, method = "class")
# Plot the trees

only_count <- function(x, labs, digits, varlen)
{
  paste(x$frame$n)
}

boxcols <- c("pink", "palegreen3")[md1$frame$yval]

par(xpd=TRUE)
prp(md1, faclen = 0, cex = 0.8, node.fun=only_count, box.col = boxcols)

library(rpart.plot)

predicted.classes <- md1 %>% 
  predict(test.data, type = "class")

confusionMatrix(predicted.classes,test.data$Approved)





#table(test.data$Approved)

########
set.seed(125)
md2 <- train(
  Approved ~., data = train.data, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
plot(md2)
md2$bestTune

par(xpd = NA) # Avoid clipping the text in some device
plot(md2$finalModel)
text(md2$finalModel,  digits = 3)

md2$finalModel
#

# Make predictions on the test data
predicted.classes <- md1 %>% predict(test.data)
predicted.classes
# Compute model accuracy rate on test data
mean(predicted.classes == test.data$Approved)
confusionMatrix(predicted.classes,test.data$Approved)


#############################################

#install.packages("C50")
#library(C50)

#decision tree
str(data1)

data2 = data1 %>% dplyr::select(Married,YearsEmployed,PriorDefault,Employed,CreditScore,Income,Approved)

t_data_x = data1[1:450,1:13]
t_data_y = data1[1:450,14]

te_data_x = data1[451:540,1:13]
te_data_y = data1[451:540,14]


model_c <- C50::C5.0( t_data_x,t_data_y )
summary( model_c )

#boosting
model_c1 <-  C50::C5.0( t_data_x,t_data_y, trials=10 )
summary(model_c1)
plot(model_c1)



p <- predict( model_c1, te_data_x, type="class" )
table(p)
sum( p == te_data_y ) / length( p )

###########################################################

#In C5.0 used the post pruning method by Binomial Confidence Limit and 
#CART algorithm used pre_pruning method using Cost complexity model to carry on the classification for decision tree & rule set formation

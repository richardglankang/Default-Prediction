source("DataAnalyticsFunctions.R")
source("PerformanceCurves.R")
installpkg("dplyr")
library(dplyr)
installpkg("fastDummies")
library('fastDummies')
library(corrplot)
installpkg("ROSE")
library(ROSE)
installpkg("partykit")
library(partykit)
installpkg("tree")
library(tree)
installpkg("randomForest")
library(randomForest)
installpkg("libcoin")
library(libcoin)
installpkg("plyr")
library(plyr)
installpkg("glmnet")
library(glmnet)
installpkg("support")
library(support)


### import the dataset
Loan <- read.csv('data/Training Data.csv')
Loan_test <-read.csv('data/Test Data.csv')

### this is data preparation for using in jupyter notebook
### so I just comment those code
# Encoading
#Loan$Married.Single <- ifelse(Loan$Married.Single == "single", 0, 1)
#Loan$Car_Ownership <- ifelse(Loan$Car_Ownership =="no", 0, 1)
#dataf <- dummy_cols(Loan, 
                   # select_columns = 'House_Ownership')
#Loan <- subset(dataf, 
             #  select=-c(House_Ownership,Id))
#Loan_1 <- Loan[!duplicated(Loan), ]
#new_data <- ovun.sample(Risk_Flag ~ ., data = newLoan, method = "both", N = 50000, seed = 1)$data
#write.csv(new_data,"newLoan.csv")
### same procedure for test data
#Loan_test$Married.Single <- ifelse(Loan_test$Married.Single == "single", 0, 1)
#Loan_test$Car_Ownership <- ifelse(Loan_test$Car_Ownership =="no", 0, 1)
#dataf1 <- dummy_cols(Loan_test, select_columns = 'House_Ownership')
#Loan_test <- subset(dataf1, select=-c(House_Ownership,ID))
#write.csv(Loan_test,"newLoan_test.csv")


# Data exploration
head(Loan)
tail(Loan)
print(paste("Number of records: ", nrow(Loan)))
print(paste("Number of features: ", ncol(Loan)))
summary(Loan)
colnames(Loan)

# Unique cities
unique(Loan$CITY) 

# Unique state
unique(Loan$STATE)
#unique profession
unique(Loan$Profession)

#Checking Null values
is.na(Loan)
sum(is.na(Loan))
# No NUll Value

Loan_visual <- Loan
## visualize chart 
Loan_visual$Risk_Flag <- ifelse(Loan$Risk_Flag==1,"Yes","No")
Loan_visual <- subset(Loan_visual,select=-c(Id))

installpkg("lessR")
library(lessR)
## piechart of default or not default
risk_count <- as.data.frame(table(Loan_visual$Risk_Flag))
risk <-  Loan_visual$Risk_Flag
PieChart(risk, hole = 0, values = "%", data = risk_count,
         fill = c("lightblue", "lightcoral"), main = "Default Distribution")
## piechart of owning a house 
house_count <- as.data.frame(table(Loan_visual$House_Ownership))
house <-  Loan_visual$House_Ownership
PieChart(house, hole = 0, values = "%", data = house_count,
         fill = c("lightblue", "lightcoral","mediumpurple"), main = "House Ownership Status Distribution")

car_count <- as.data.frame(table(Loan_visual$Car_Ownership))
car <-  Loan_visual$Car_Ownership
PieChart(car, hole = 0, values = "%", data = car_count,
         fill = c("lightblue", "lightcoral"), main = "Car Ownership Distribution")

prof_count <- as.data.frame(table(Loan_visual$Profession))
PieChart(prof, hole = 0, values = "%", data = prof_count,
         fill = c("lightblue", "lightcoral"),cex=0.35,main = "Profession Distribution")
prof_count <- prof_count[order(prof_count$Freq,decreasing = TRUE),]
prof_count1 <- prof_count[1:5,]

PieChart(prof, hole = 0, values = "%", data = prof_count1,
         fill = c("lightblue", "lightcoral"),cex=1,main = "Profession Distribution")

state_count <- as.data.frame(table(Loan_visual$STATE))
state_count <- state_count[order(state_count$Freq,decreasing = TRUE),]
state_count1 <- state_count[1:5,]

prof <- as.data.frame(prof_count1$Var1[1:5])
plot(Risk_Flag~Income,data = Loan)

city_count <- as.data.frame(table(Loan_visual$CITY))
city <-  Loan_visual$CITY
PieChart(city, hole = 0, values = "%", data = city_count,
         fill = c("lightblue", "lightcoral"),cex=0.35,main = "City Distribution")



# To see the base
Risk <- nrow(subset(Loan,Loan$Risk_Flag==1))
Total <- nrow(Loan)
Risk_rate <- Risk/Total
Risk_rate

# Encoding
Loan$Married.Single <- ifelse(Loan$Married.Single == "single", 0, 1)
Loan$Married.Single
Loan$Car_Ownership <- ifelse(Loan$Car_Ownership =="no", 0, 1)
Loan$Car_Ownership 

# create three dummy variables for house_ownership
dataf <- dummy_cols(Loan, 
                    select_columns = 'House_Ownership')
# drop unesseary columns
Loan <- subset(dataf, 
               select=-c(House_Ownership,Id,House_Ownership_norent_noown))

### remove duplicate rows
Loan_1 <- Loan[!duplicated(Loan), ]


### Filtering out unreasonable records
### There is error in the data set. For example:
### An applicant who is 27 years old but has 20 years of experience.
### Those records are not reasonable and would affect our modeling results
### So we filter out those data for further analysis.

new_data <- Loan_1 %>%
  filter((Age-Experience>=14)& (Age-CURRENT_JOB_YRS>=14))



### change the variables' class to numeric and factor
new_data$Income <- as.numeric(new_data$Income)
new_data$Age <- as.numeric(new_data$Age)
new_data$Experience <- as.numeric(new_data$Experience)
new_data$Married.Single <- as.factor(new_data$Married.Single)
new_data$Car_Ownership <- as.factor(new_data$Car_Ownership)
new_data$House_Ownership_rented <- as.factor(new_data$House_Ownership_rented)
new_data$House_Ownership_owned <- as.factor(new_data$House_Ownership_owned)
new_data$CURRENT_JOB_YRS <- as.numeric(new_data$CURRENT_JOB_YRS)
new_data$CURRENT_HOUSE_YRS <- as.numeric(new_data$CURRENT_HOUSE_YRS)
new_data$CITY <- as.factor(new_data$CITY)
new_data$STATE <- as.factor(new_data$STATE)
new_data$Risk_Flag <- as.factor(new_data$Risk_Flag)

### see each variable's class
lapply(new_data,class)

num_record <- nrow(Loan_1)
print(paste("Number of records: ", nrow(new_data)))
print(paste("Number of records filtered: ", num_record-nrow(new_data)))
print(paste("Number of features: ", ncol(new_data)))


### Finding the correlation between each numeric column in data set
Numerical_Columns <- subset(new_data, 
                            select=-c(Profession,
                                      CITY,
                                      STATE,
                                      Married.Single,
                                      Car_Ownership,
                                      House_Ownership_owned,
                                      House_Ownership_rented,
                                      Risk_Flag)
                            )
correlation <- cor(Numerical_Columns)
correlation
### generate correlation plot
corrplot(correlation,method="number")

## factor plot of the percentage of default people in categorical variables
plot(Risk_Flag ~ Married.Single, 
     data=new_data, 
     col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)), 
     ylab="Default Rate") 

plot(Risk_Flag ~ Car_Ownership,
     data=new_data,
     col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),
     ylab="Default Rate") 

plot(Risk_Flag ~ House_Ownership_owned, 
     data=new_data, 
     col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)), 
     ylab="Default Rate") 

plot(Risk_Flag ~ House_Ownership_rented, 
     data=new_data,
     col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)), 
     ylab="Default Rate") 


plot(Risk_Flag~STATE,
     col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),
     data = new_data)

plot(Risk_Flag~CITY,
     col=c(rgb(1,0,0,0.5),
           rgb(0,0,1,0.5)),
     data = new_data)

plot(Risk_Flag~Profession,
     col=c(rgb(1,0,0,0.5),
           rgb(0,0,1,0.5)),
     data = new_data)


### Using contingency table to determine the independence of 
## Is default rate statistically independent of married status?
## Using the contingency table we have
m00 <- sum( (new_data$Married.Single == 1) & (new_data$Risk_Flag == 0) ) 
m01 <- sum( (new_data$Married.Single == 1) & (new_data$Risk_Flag == 1) ) 
m10 <- sum( (new_data$Married.Single == 0) & (new_data$Risk_Flag == 0) ) 
m11 <- sum( (new_data$Married.Single == 0) & (new_data$Risk_Flag == 1) ) 
# Construct the contingency table
ContingencyMatrix <- as.table(rbind(c(m00, m01), c(m10, m11)))
### perform the Pearson chi squares test for independent of factors
chisq.test(ContingencyMatrix)$p.value  # p-value of the statistical test 
### p-value = 0.46... we fail to reject independence


### Because there is only small group containing risk_flag = 1 in the training data set
### about 12.3%
### It would be difficult to discover relationship in the small group
### The method = "both" means oversample risk_flag = 1 and undersample risk_flag = 0 simutaneously
### After over sample, the two groups can reach approximately 50-50 ratio
### oversample the data set
new_data <- ovun.sample(Risk_Flag ~ ., data = new_data, method = "both", N = 50000, seed = 1)$data
#write.csv(new_data,"new_data1.csv")

### No significant distribution pattern
plot(Income~Age, data=new_data, xlab='Income', ylab='Age (years)', main='Default', col=ifelse(Risk_Flag==1,'red','lightblue'))

hist(new_data$Income,main="Distribution of Income",ylab="Income",col='red')


### Clustering using Principal Component Analysis
### to see how applicants' demographics in each cluster
###PCA###
installpkg("plfm")
library(plfm)
# select feature volumns 
feat <- new_data[,c(1,2,3,9,10)]
str(feat)
feat_scaled <- scale(feat)

### Lets compute the (Full) PCA
x <- model.matrix(~., feat)[,-1]
pca.x<- prcomp(x, scale=TRUE)
summary(pca.x)
pca.x
comp <- data.frame(pca.x$x[,1:4])

# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))

###plot the percentage of variance that each component explains  
pca.x.var <- pca.x$sdev^2
pca.x.var.per <- round(pca.x.var/sum(pca.x.var)*100,1)
barplot(pca.x.var.per,main="Scree Plot",xlab="Principle Component",ylab="Percent Variation")

###plot the variance that each component explains         
plot(pca.x,main="PCA: Variance Explained by Factors")
mtext(side=1, "Factors",  line=1, font=2)

## Lets plot so see how the customers look like in these components
customer.pc <- predict(pca.x) 
plot(customer.pc[,1:2], pch=21,  main="")
text(customer.pc[,1:2], labels=new_data[,1], col="blue", cex=1)

plot(customer.pc[,3:4], pch=21,  main="")
text(customer.pc[,3:4], labels=new_data[,1], col="blue", cex=1)

loadings <- pca.x$rotation[,1:5]
### For each factor lets display the top features that 
### are responsible for 3/4 of the squared norm of the loadings

v<-loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:ncol(x)],1]
loadingfit <- lapply(1:ncol(x), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]

v<-loadings[order(abs(loadings[,2]), decreasing=TRUE)[1:ncol(x)],2]
loadingfit <- lapply(1:ncol(x), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]

v<-loadings[order(abs(loadings[,3]), decreasing=TRUE)[1:ncol(x)],3]
loadingfit <- lapply(1:ncol(x), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]

v<-loadings[order(abs(loadings[,4]), decreasing=TRUE)[1:ncol(x)],4]
loadingfit <- lapply(1:ncol(x), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]


###Classification###

###decision tree
Churntree <- tree(Risk_Flag ~ Income+Experience, data=new_data) 
summary(Churntree)
# only 1 final node
# so do it in python

random_data <- sample(1:nrow(new_data), 0.8 * nrow(new_data)) 
### normalize data function
nor <-function(x) { 
  (x -mean(x))/(sd(x))  
}
x <- as.data.frame(lapply(new_data[,c(1,2,3,6,7)], nor))
### this define the training set
loan_train <- new_data[random_data,] 

### this sets the target variable for the training set and test set
loan_target_category <- new_data[random_data,11]
loan_test_category <- new_data[-random_data,11]
### this will be used to define the testing set

loan_test <- new_data[-random_data,] 
### this is what runs k-nn for prediction.
pr <- knn(loan_train,loan_test,cl=loan_target_category,k=15,prob=TRUE)
tab <- table(pr,loan_test_category)
accuracy <- function(x){
  sum(diag(x)/(sum(rowSums(x)))) * 100
  }
accuracy(tab)
#61.08%

####Regression####

##We found that there is no strong interaction effect

### Set Risk_Flag as My (target)
My <- new_data$Risk_Flag==1

### the features Mx need to be a matrix ([,-1] removes the first column which is the intercept
Mx <- model.matrix(Risk_Flag ~., data = new_data)[,-1]



### use cross valiadation to actually pick lambda for linear regression 
### compute the Lasso for all values of lambda
lassoCV <- cv.glmnet(Mx, 
                     My,
                     alpha=1,
                     family = "binomial")

### plot the fitting graph  
### red dots are mean values and the bars are the uncertainty
plot(lassoCV, 
     main="Fitting Graph for CV Lasso \n \n # of non-zero coefficients  ", 
     xlab = expression(paste("log(",lambda,")")))



### generate 100 different lamdas for LR
lasso <- glmnet(Mx,My,family = "binomial")
num.features <- ncol(Mx)
num.n <- nrow(Mx)
num.churn <- sum(My)
w <- (num.churn/num.n)*(1-(num.churn/num.n))
lambda.theory <- sqrt(w*log(num.features/0.05)/num.n)
lambda.theory
### and specifying lambda = lambda.theory
lassoTheory <- glmnet(Mx,My, family="binomial",lambda = lambda.theory)
### by calling the summary we see the list of object in sclassoTheory
summary(lassoTheory)
### these are the indices
support(lassoTheory$beta)
### these are the labels
colnames(Mx)[support(lassoTheory$beta)]
### there are in total
length(support(lassoTheory$beta))

### getting the minimum of the mean values of LR
min_lamda <- lassoCV$lamda.min
lamda_1se <- lassoCV$lambda.1se
lamda_1se

### Lasso on LR with minimum lamda
lasso_reg <- glmnet(Mx,
                    My, 
                    alpha = 1, 
                    lamda= min_lamda,
                    family = "binomial")
summary(lasso_reg)
### Show the coefficients of selected variables
coef(lasso_reg)

### Getting lasso data set by getting the selected variables
library(support)
features.min <- support(lasso$beta[,which.min(lassoCV$cvm)])
features.1se <- support(lasso$beta[,which.min( (lassoCV$lambda-lassoCV$lambda.1se)^2)])
features.theory <- support(lassoTheory$beta)
length(features.theory)
length(features.min)
length(features.1se)
data.min <- data.frame(Mx[,features.min],My)


### 5 Fold Cross Validation
nfold <- 5
n <- nrow(new_data)
### create a vector of fold memberships (random order)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]

### create an empty dataframe of out of sample R2 results
OOS <- data.frame(lr.R2=rep(NA,nfold), 
                  lasso.min.R2=rep(NA,nfold),
                  lasso.theory.R2=rep(NA,nfold),
                  null.R2=rep(NA,nfold)
) 


### Use a for loop to run through the nfold trails
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ### Logistic regression
  m.lr <-glm(Risk_Flag==1~., 
             data = new_data, 
             subset = train,
             family = "binomial"
  )
  
  m.lr$coefficients
  pred.lr <- predict(m.lr, 
                     newdata=new_data[-train,],
                     type="response"
  )
  OOS$lr.R2[k] <- R2(y=new_data$Risk_Flag[-train] == 1,
                     pred.lr,
                     family="binomial"
  )
  
  ### Lasso on Logistic Regression
  m.lr.l  <- glmnet(Mx[train,],
                    My[train], 
                    lambda = lassoCV$lambda.min,
                    family="binomial")

  pred.lr.l <- predict(m.lr.l, 
                       newx=Mx[-train,],
                       type="response")
  OOS$lasso.min.R2[k] <- R2(My[-train],
                            pred.lr.l,
                            family="binomial")
  
  lassoTheory <- glmnet(Mx[train,],
                        My[train], 
                        family="binomial",
                        lambda = lambda.theory)
  predlassotheory <- predict(lassoTheory, 
                             newx=Mx[-train,], 
                             type="response")
  OOS$lasso.theory.R2[k] <- R2(My[-train],
                               predlassotheory,
                               family="binomial")
  
  ### Null model
  model.nulll <-glm(Risk_Flag==1~1, 
                    data=new_data, 
                    subset=train,
                    family="binomial")
  pred.null <- predict(model.nulll, 
                       newdata=new_data[-train,], 
                       type="response")
  OOS$null.R2[k] <- R2(My[-train],
                       pred.null,family="binomial")
  
}
OOS
colMeans(OOS)
barplot(colMeans(OOS), 
        las=1, 
        xpd=FALSE,
        col=rgb(0,0,1,0.5),
        names.arg = c("Null","Lasso \n Min","Lasso \n Theory","Logistic"),
        ylab = "",
        horiz=TRUE,
        xlim=c(0.975*min(colMeans(OOS)),
               max(colMeans(OOS))), 
       
        xlab = "Average Out of Sample R-squared")

##Accuracy Performance
OOS1 <-data.frame(logistic=rep(NA,nfold), L.min=rep(NA,nfold), L.theory=rep(NA,nfold),null=rep(NA,nfold)) 
OOS1.TP <- data.frame(logistic=rep(NA,nfold), L.min=rep(NA,nfold), L.theory=rep(NA,nfold),null=rep(NA,nfold)) 
OOS1.TN <- data.frame(logistic=rep(NA,nfold), L.min=rep(NA,nfold), L.theory=rep(NA,nfold),null=rep(NA,nfold)) 
OOS1.FP <- data.frame(logistic=rep(NA,nfold), L.min=rep(NA,nfold),L.theory=rep(NA,nfold), null=rep(NA,nfold)) 
OOS1.FN <- data.frame(logistic=rep(NA,nfold),L.min=rep(NA,nfold),L.theory=rep(NA,nfold), null=rep(NA,nfold))

val <- .3
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ##Logistic regression
  model.logistic <-glm(Risk_Flag==1~., 
                       data=new_data, 
                       subset=train,
                       family="binomial")
  
  ## get predictions: type=response so we have probabilities
  
  pred.logistic <- predict(model.logistic,
                           newdata=new_data[-train,], 
                           type="response")
  
  # Logistic
  values <- FPR_TPR( (pred.logistic >= val) , My[-train] )
  values$TP<- sum((pred.logistic >= val)*My[-train])
  values$FP <- sum((pred.logistic >= val)*(!My[-train]))
  values$FN <- sum(!(pred.logistic >= val)*(My[-train]))
  values$TN <- sum(!(pred.logistic >= val)*(!My[-train]))
  OOS1$logistic[k] <- values$ACC
  OOS1.TP$logistic[k] <- values$TP
  OOS1.TN$logistic[k] <- values$TN
  OOS1.FP$logistic[k] <- values$FP
  OOS1.FN$logistic[k] <- values$FN
  
  ###minimum lasso  
  lassomin  <- glmnet(Mx[train,],
                      My[train], 
                      family="binomial",
                      lambda = lassoCV$lambda.min)
  
  
  predlassomin <- predict(lassomin,
                          newx=Mx[-train,],
                          type="response")
  
  
  values <- FPR_TPR( (predlassomin >= val) , My[-train] )
  values$TP<- sum((predlassomin >= val)*My[-train])
  values$FP <- sum((predlassomin >= val)*(!My[-train]))
  values$FN <- sum(!(predlassomin >= val)*(My[-train]))
  values$TN <- sum(!(predlassomin >= val)*(!My[-train]))
  OOS1$L.min[k] <- values$ACC
  OOS1.TP$L.min[k] <- values$TP
  OOS1.TN$L.min[k] <- values$TN
  OOS1.FP$L.min[k] <- values$FP
  OOS1.FN$L.min[k] <- values$FN
  
  #lasso theory 
  lassoTheory <- glmnet(Mx[train,],
                        My[train], 
                        family="binomial",
                        lambda = lambda.theory)
  predlassotheory <- predict(lassoTheory, 
                             newx=Mx[-train,], 
                             type="response")
  values <- FPR_TPR( (predlassotheory >= val) ,
                     My[-train] )
  values$TP<- sum((predlassotheory >= val)*My[-train])
  values$FP <- sum((predlassotheory >= val)*(!My[-train]))
  values$FN <- sum(!(predlassotheory >= val)*(My[-train]))
  values$TN <- sum(!(predlassotheory >= val)*(!My[-train]))
  OOS1$L.theory[k] <- values$ACC
  OOS1.TP$L.theory[k] <- values$TP
  OOS1.TN$L.theory[k] <- values$TN
  OOS1.FP$L.theory[k] <- values$FP
  OOS1.FN$L.theory[k] <- values$FN
  
  #Null model
  model.nulll <-glm(Risk_Flag==1~0, 
                    data=new_data,
                    subset=train,
                    family="binomial")
  pred.null <- predict(model.nulll, 
                       newdata=new_data[-train,],
                       type="response")
  values <- FPR_TPR( (pred.null >= val) , My[-train] )
  values$TP<- sum((pred.null >= val)*My[-train])
  values$FP <- sum((pred.null >= val)*(!My[-train]))
  values$FN <- sum(!(pred.null >= val)*(My[-train]))
  values$TN <- sum(!(pred.null >= val)*(!My[-train]))
  OOS1$null[k] <- values$ACC
  OOS1.TP$null[k] <- values$TP
  OOS1.TN$null[k] <- values$TN
  OOS1.FP$null[k] <- values$FP
  OOS1.FN$null[k] <- values$FN
  
  print(paste("Iteration",k,"of",nfold,"completed"))
}
OOS1

barplot(colMeans(OOS1), 
        las=1, 
        xpd=FALSE,
        col='lightblue1',
        names.arg = c("Null","Lasso \n Min","Lasso \n Theory","Logistic"),
        ylab="",
        horiz = TRUE,
        xlim=c(0.975*min(colMeans(OOS1)),
               max(colMeans(OOS1))), 
        xlab = "Average Out of Sample Accuracy")

### prediction is a probability score
### we convert to 1 or 0 via prediction > threshold
PerformanceMeasure <- function(actual, prediction, threshold=.5) {
  1-mean( abs( (prediction- actual) ) )  
}
OOS2 <- data.frame(logistic=rep(NA,nfold), 
                   lasso.min=rep(NA,nfold), 
                   lasso.theory=rep(NA,nfold), 
                   null=rep(NA,nfold)) 
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ##Logistic regression
  model.lr <-glm(Risk_Flag==1~., 
                       data=new_data, 
                       subset=train,
                       family="binomial")
  
  
  pred.lr <- predict(model.logistic,
                           newdata=new_data[-train,], 
                           type="response")
  
  OOS2$logistic[k] <- PerformanceMeasure(actual=My[-train], 
                                    pred=pred.lr)
  

  
  ### the Lasso estimates  
  m.lr.l  <- glmnet(Mx[train,],
                    My[train], 
                    family="binomial",
                    lambda = lassoCV$lambda.min)
  pred.lr.l <- predict(m.lr.l, 
                       newx=Mx[-train,], 
                       type="response")
  OOS2$lasso.min[k] <- PerformanceMeasure(actual=My[-train], 
                                      prediction=pred.lr.l)
  
  #lasso theory 
  lassoTheory <- glmnet(Mx[train,],
                        My[train], 
                        family="binomial",
                        lambda = lambda.theory)
  predlassotheory <- predict(lassoTheory, 
                             newx=Mx[-train,], 
                             type="response")
  OOS2$lasso.theory[k] <- PerformanceMeasure(actual=My[-train], 
                                            prediction=predlassotheory)
  
  #Null model
  model.nulll <-glm(Risk_Flag==1~0, 
                    data=new_data,
                    subset=train,
                    family="binomial")
  pred.null <- predict(model.nulll, 
                       newdata=new_data[-train,],
                       type="response")
  OOS2$null[k] <- PerformanceMeasure(actual=My[-train], 
                                         prediction=pred.null)
  
  print(paste("Iteration",k,"of",nfold,"completed"))

}    
OOS2
##average AUC score
colMeans(OOS2)
barplot(colMeans(OOS1), 
        las=1, 
        xpd=FALSE,
        col="rosybrown1",
        names.arg = c("Null","Lasso \n Min","Lasso \n Theory","Logistic"),
        ylab="",
        horiz = TRUE,
        xlim=c(0.975*min(colMeans(OOS1)),
               max(colMeans(OOS1))), 
        xlab = "Average Out of Sample Performance")


### KNN Model to visualize 200 points using the first two features: income and age 
installpkg("ElemStatLearn")
library(ElemStatLearn)
installpkg("class")
library(class)
new_data_train <- new_data[1:200,]
x<- cbind( (new_data_train[,1]-mean(new_data_train[,1]))/sd(new_data_train[,1]) ,
           (new_data_train[,2]-mean(new_data_train[,2]))/sd(new_data_train[,2]) )
g <- new_data_train$Risk_Flag
### this will be used to define the testing set
px1 <- seq(from=min(x[,1]), to=max(x[,1]), by=0.05)
px2 <- seq(from=min(x[,2]), to=max(x[,2]), by=0.05)
xnew <- expand.grid(x=px1, y=px2)
### this is what runs k-nn for prediction.
mod15 <- knn(train=x, test=xnew, cl=g, k=15, prob=TRUE)
#######################################################
### this is only to visualize
prob <- attr(mod15, "prob")
prob <- ifelse(mod15=="1", prob, 1-prob)
prob15 <- matrix(prob, length(px1), length(px2))

### Lets do some plotting
### fist we plot the data
par(mar=rep(2,4))
## creates a plot without labels (xlab="",ylab="")
plot(x, xlab="",ylab="")
## provides abox around the points
box()
#############################
### Now the regions 
###
### first for for prob <1/2 rule
par(mar=rep(2,4))
contour(px1, px2, prob15, levels=0.5, xlab="Income", ylab="Age", main="15-nearest neighbors (p<1/2 rule)")
points(x, col=ifelse(g==1, "coral", "cornflowerblue"))
gd <- expand.grid(x=px1, y=px2)
points(gd, pch=".", cex=1.2, col=ifelse(prob15>0.5, "coral", "cornflowerblue"))
box()
### next for prob <1/6 rule
par(mar=rep(2,4))
contour(px1, px2, prob15, levels=1/6, xlab="Income", ylab="Age", main="15-nearest neighbors (p<1/6 rule)")
points(x, col=ifelse(g==1, "coral", "cornflowerblue"))
gd <- expand.grid(x=px1, y=px2)
points(gd, pch=".", cex=1.2, col=ifelse(prob15>1/6, "coral", "cornflowerblue"))
box()

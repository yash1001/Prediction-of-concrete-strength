require(MASS)
require(boot)
require(ISLR)
require(faraway)
require(caret)
require(glmnet)
require(boot)
require(leaps)
require(rpart)




# read data and attach data
concrete = concrete <- data.frame(read.csv(file.choose()))

concrete_data = concrete_data <- read.csv(file.choose())


#############################################################################################
attach(concrete)
#View(concrete)
par(mfrow=c(1,1))
pairs(concrete,pch=19)
cor(concrete)


# fit additive model      

fit=lm(strength~.,data=concrete)
par(mfrow=c(2,2), mar=c(4,4,4,4))
plot(fit)  # residual diagnostics
summary(fit) # fit summary
vif(fit)

# scatterplot of fit versus actual
par(mfrow=c(1,1))
plot(fit$fitted.values,concrete$strength,pch=19, 
     xlab="fitted values", ylab="actual", main="Strength")
#points(red.sec$fitted.values,concrete$strength, xlab = "Fitted", ylab = "Actual", pch=1, col="red")
abline(a=0,b=1,lwd=2,col="blue")
legend("topleft",legend="fitted=actual",lwd=2,col="blue",bty="n")

# calculate AIC and BIC
n=dim(concrete)[1]
p=dim(concrete)[2]-1
AIC=n*log(sum(residuals(fit)^2)/n)+2*p
BIC=n*log(sum(residuals(fit)^2)/n)+log(n)*p
paste("AIC = ",AIC)
paste("BIC = ",BIC)

# calculate centered predictors
#concrete$cement_c=concrete$cement-mean(concrete$cement)
#concrete$slag_c=concrete$slag-mean(concrete$slag)
#concrete$fly_ash_c=concrete$fly_ash-mean(concrete$fly_ash)
#concrete$water_c=concrete$water-mean(concrete$water)
#concrete$superplasticizer_c=concrete$superplasticizer-mean(concrete$superplasticizer)
#concrete$age_c=concrete$age-mean(concrete$age)
#concrete$course_aggregate_c=concrete$course_aggregate-mean(concrete$course_aggregate)
#concrete$fine_aggregate_c=concrete$fine_aggregate-mean(concrete$fine_aggregate)


# Reduced: remove aggregate from predictors

fit.Re=lm(strength~cement+slag+fly_ash+water+superplasticizer+age,data=concrete)
par(mfrow=c(2,2))
plot(fit.Re)  # residual diagnostics
summary(fit.Re) # fit summary
vif(fit.Re)

# scatterplot of fit versus actual
par(mfrow=c(1,1))
plot(fit.Re$fitted.values,concrete$strength,pch=19, 
     xlab="fitted values", ylab="actual", main="Strength")
abline(a=0,b=1,lwd=2,col="blue")
legend("topleft",legend="fitted=actual",lwd=2,col="blue",bty="n")


# Box-Cox
box=boxcox(fit)
lambda=box$x[which.max(box$y)]
lambda
concrete$strength.box=(strength^lambda-1)/lambda

# refit additive model with transformed response

fit.bc=lm(strength.box~cement+slag+fly_ash+water+superplasticizer+course_aggregate+fine_aggregate+age,data=concrete)
par(mfrow=c(2,2))
plot(fit.bc)  # residual diagnostics
summary(fit.bc) # fit summary
vif(fit.bc)
mean((fit.bc$residuals)^2)


# fit interaction model      

fit.i=lm(strength~cement*slag*fly_ash*water*superplasticizer*course_aggregate*fine_aggregate*age,data=concrete)
#fit.i=lm(strength~.-strength.box,data=concrete)
par(mfrow=c(2,2),mar=c(3,3,3,3))
plot(fit.i)  # residual diagnostics
summary(fit.i) # fit summary
vif(fit.i)

# scatterplot of fit versus actual
par(mfrow=c(1,1))
plot(fit.i$fitted.values,concrete$strength,pch=19, 
     xlab="fitted values", ylab="actual", main="Strength")
abline(a=0,b=1,lwd=2,col="blue")
legend("topleft",legend="fitted=actual",lwd=2,col="blue",bty="n")

# Reduced: remove aggregate from predictors

fit.Re.i=lm(strength~cement*slag*fly_ash*water*superplasticizer*age,data=concrete)
par(mfrow=c(2,2))
plot(fit.Re.i)  # residual diagnostics
summary(fit.Re.i) # fit summary
vif(fit.Re.i)

# scatterplot of fit versus actual
par(mfrow=c(1,1))
plot(fit.Re$fitted.values,concrete$strength,pch=19, 
     xlab="fitted values", ylab="actual", main="Strength")
abline(a=0,b=1,lwd=2,col="blue")
legend("topleft",legend="fitted=actual",lwd=2,col="blue",bty="n")

# Box-Cox
box.i=boxcox(fit.i)
lambda.i=box.i$x[which.max(box.i$y)]
lambda.i
concrete$strength.box.i=(strength^lambda.i-1)/lambda.i

# refit additive model with transformed response

fit.bc.i=lm(strength.box.i~cement*slag*fly_ash*water*superplasticizer*course_aggregate*fine_aggregate*age,data=concrete)
par(mfrow=c(2,2))
plot(fit.bc.i)  # residual diagnostics
summary(fit.bc.i) # fit summary
vif(fit.bc.i)

mean((fit.bc.i$residuals)^2)

# fit 2nd Order model      

fit.2=lm(strength~poly(cement,slag,fly_ash,water,superplasticizer,course_aggregate,fine_aggregate,age,degree=2),data=concrete)
par(mfrow=c(2,2),mar=c(2,2,2,2))
plot(fit.2)  # residual diagnostics
summary(fit.2) # fit summary
vif(fit.2)

# scatterplot of fit versus actual
par(mfrow=c(1,1))
plot(fit.2$fitted.values,concrete$strength,pch=19,  
     xlab="fitted values", ylab="actual", main="Strength")
abline(a=0,b=1,lwd=2,col="blue")
legend("topleft",legend="fitted=actual",lwd=2,col="blue",bty="n")

# Reduced: remove aggregate from predictors

fit.Re.2=lm(strength~poly(cement,slag,fly_ash,water,superplasticizer,age,degree=2),data=concrete)
par(mfrow=c(2,2))
plot(fit.Re.2)  # residual diagnostics
summary(fit.Re.2) # fit summary
vif(fit.Re.2)

# scatterplot of fit versus actual
par(mfrow=c(1,1))
plot(fit.Re$fitted.values,concrete$strength,pch=19, 
     xlab="fitted values", ylab="actual", main="Strength")
abline(a=0,b=1,lwd=2,col="blue")
legend("topleft",legend="fitted=actual",lwd=2,col="blue",bty="n")

# Box-Cox
box.2=boxcox(fit.2)
lambda.2=box.2$x[which.max(box.2$y)]
lambda.2
concrete$strength.box.2=(strength^lambda.2-1)/lambda.2

# refit additive model with transformed response

fit.bc.2=lm(strength.box.2~poly(cement,slag,fly_ash,water,superplasticizer,course_aggregate,fine_aggregate,age,degree=2),data=concrete)
par(mfrow=c(2,2))
plot(fit.bc.2)  # residual diagnostics
summary(fit.bc.2) # fit summary
vif(fit.bc.2)


################## centered data and validation

################## centered data and validation

concrete_clean = data.frame(read.csv(file.choose()))

center_scale <- function(x) {
        scale(x, scale = FALSE)
}

#Interaction Terms
concrete$ce.sl=cement*slag
concrete$ce.fl=cement*fly_ash
concrete$ce.w=cement*water
concrete$ce.su=cement*superplasticizer
concrete$ce.co=cement*course_aggregate
concrete$ce.fi=cement*fine_aggregate
concrete$ce.a=cement*age
concrete$sl.fl=slag*fly_ash
concrete$sl.w=slag*water
concrete$sl.su=slag*superplasticizer
concrete$sl.co=slag*course_aggregate
concrete$sl.fi=slag*fine_aggregate
concrete$sl.a=slag*age
concrete$fl.w=fly_ash*water
concrete$fl.su=fly_ash*superplasticizer
concrete$fl.co=fly_ash*course_aggregate
concrete$fl.fi=fly_ash*fine_aggregate
concrete$fl.a=fly_ash*age
concrete$w.su=water*superplasticizer
concrete$w.co=water*course_aggregate
concrete$w.fi=water*fine_aggregate
concrete$w.a=water*age
concrete$su.co=superplasticizer*course_aggregate
concrete$su.fi=superplasticizer*fine_aggregate
concrete$su.a=superplasticizer*age
concrete$co.fi=course_aggregate*fine_aggregate
concrete$co.a=course_aggregate*age
concrete$fi.a=fine_aggregate*age

#2nd Order Terms
# create new columns in concrete with all second order
concrete$ce.2=cement^2
concrete$sl.2=slag^2
concrete$fl.2=fly_ash^2
concrete$w.2=water^2
concrete$su.2=superplasticizer^2
concrete$co.2=course_aggregate^2
concrete$fi.2=fine_aggregate^2
concrete$a.2=age^2

cntr1 = data.frame(center_scale(concrete))
cntr2 = data.frame(center_scale(concrete_clean))

fit_cntr = lm(concrete$strength~.-strength, data = cntr2)
plot(fit_cntr)
summary(fit_cntr)

############### Centered Terms  Complete 2nd Ordered Model
fit_cntr2 = lm(concrete$strength~.-strength-strength.box-strength.box.i-
                       strength.box.2-cement-slag-fly_ash-water-superplasticizer-
                       course_aggregate-fine_aggregate-age+concrete_clean$cement+concrete_clean$slag+
                 concrete_clean$fly_ash+concrete_clean$water+concrete_clean$superplasticizer+
                 concrete_clean$course_aggregate+concrete_clean$fine_aggregate+concrete_clean$age, data=cntr1)
plot(fit_cntr2)
summary(fit_cntr2)

############### Centered Terms  Complete 2nd Ordered Model
fit_cntr3 = lm(concrete$strength~.-strength-strength.box-strength.box.i-
                 strength.box.2, data=cntr1)
plot(fit_cntr3)
summary(fit_cntr3)

############### Reduced Second Order Model  ############



fit_cntr.re = lm(concrete$strength~.-strength-strength.box-strength.box.i-
                       strength.box.2-cement-slag-fly_ash-water-superplasticizer-
                       course_aggregate-fine_aggregate-age-ce.w-sl.fl-fl.fi-fl.a-su.co-co.fi-fi.a-ce.2-fl.2-w.2+
                   concrete_clean$cement+concrete_clean$slag+concrete_clean$water+concrete_clean$superplasticizer+
                   concrete_clean$course_aggregate+concrete_clean$fine_aggregate+concrete_clean$age, data=cntr1)

plot(fit_cntr.re)
summary(fit_cntr.re)


#############
## perfoming loocv on whole data set


set.seed(100)
fit.gen = glm(strength~., data=concrete_clean)
cv.glm(concrete_clean, fit.gen)$delta[1]
summary(fit.gen)
plot(fit.gen)






#############################
###### Best Subset Selection Raw Model   ###########

pred.sbs = function(object,newdata,id){
        form = as.formula(object$call[[2]])
        matrix = model.matrix(form,newdata)
        coefi = coef(object,id=id)
        xvars = names(coefi)
        return(matrix[,xvars]%*%coefi)
}



best_subset = regsubsets(strength~., data=concrete_clean, nvmax=8, method="exhaustive")
subset = summary(best_subset)
max(subset$rsq)

############# 5 Fold Cross Validation ##########
set.seed(100) #setting seed

k = 5  #set number of folds

# creating an index with id 1-5 to assign observations to folds
folds=sample(1:k,nrow(concrete_clean),replace=T) 
# create dummy matrix to store CV error estimates
cv.error=matrix(NA,k,8,dimnames=list(NULL,paste(1:8)))

# perform CV
for (j in 1:k){
        # pick models with lowest RSS with 1-9 predictors fit without kth fold
        best.model=regsubsets(strength~.,data=concrete_clean[folds!=j,],
                              nvmax=8,method="exhaustive")
        # estimate test error for all nine models by predicting kth fold 
        for (i in 1:8){
                pred=pred.sbs(best.model,concrete_clean[folds==j,],id=i)
                cv.error[j,i]=mean((concrete_clean$strength[folds==j]-pred)^2)  # save error est
        }
}


##Computing errors
mse.cv=apply(cv.error,2,mean)# compute mean MSE for each number of predictors
mse.cv

min=which.min(mse.cv)  # find minimum mean MSE

mse.cv[min]

par(mfrow=c(1,1))
plot(1:8,mse.cv,type="b",xlab="no. of predictors)",ylab="est. test MSE",main = "Cross Validation Result", ylim=c(80,240))
points(min,mse.cv[min],cex=2,col="red",lwd=2)
abline(h=c(80,120,160,200,240),lty=3)





#################################
##### ridge and lasso raw data #####

x = model.matrix(strength~., concrete_clean)[,-1]
y = concrete_clean$strength

grid = 10^seq(4,-2,length=100)
ridge.concrete = glmnet(x,y,alpha=0,lambda=grid)

coef(glmnet(x,y, alpha=0, lambda=grid))

plot(ridge.concrete, xlab="L2 Norm")


#### cross validation - Ridge Raw Model####

set.seed(100)
ridge_cv = cv.glmnet(x,y,alpha=0,lambda=grid)
plot(ridge_cv)

bestlambda_r = ridge_cv$lambda.min
ridge_mse = min(ridge_cv$cvm)
ridge_mse
bestlambda_r
rsq_r = 1 - (ridge_cv$cvm/var(y))
max(rsq_r)
plot(ridge_cv$lambda,rsq_r, main="lambda vs rsq")




ridge_coeff = predict(ridge.concrete,type="coefficients",s=bestlambda_r)
ridge_coeff
ridge_fit = predict(ridge.concrete, s = bestlambda_r, x)

plot(fit$fitted.values,concrete_clean$strength,pch=19,main="Ridge Regression Fitting" ,col="blue")
points(ridge_fit,concrete_clean$strength,col="red",lwd=1)
abline(a=0,b=1)
legend("topleft",legend=c("Fitted Values","Ridge Predicted values"),pch = c(19, NA),
        lwd=c(NA,1),col=c("blue","red"),bty="n")



########lasso regression on raw model##########

lasso.concrete = glmnet(x,y,alpha=1,lambda=grid)

coef(glmnet(x,y, alpha=1, type.measure="mse"))

plot(lasso.concrete, xlab="L2 Norm")



### cross validation for lasso raw #########

set.seed(100)
lasso_cv = cv.glmnet(x,y,alpha=1,type.measure = "mse")
plot(lasso_cv)

bestlambda_l = lasso_cv$lambda.min
lasso_mse = min(lasso_cv$cvm)
lasso_mse
bestlambda_l
rsq_l = 1 - (lasso_cv$cvm/var(y))
max(rsq_l)
plot(lasso_cv$lambda,rsq_l, main="lambda vs rsq")


### Lasso Coefficient
lasso_coeff = predict(lasso.concrete,type="coefficients",s=bestlambda_r)
lasso_coeff
lasso_fit = predict(lasso.concrete, s = bestlambda_l, x)

plot(fit$fitted.values,concrete_clean$strength,pch=19,main= "Lasso Predicted Values" ,col="blue")
points(lasso_fit,concrete_clean$strength,col="red",lwd=1)
abline(a=0,b=1)
legend("topleft",legend=c("Fitted Values","Lasso Predicted values"),pch = c(19, 1),col=c("blue","red"),bty="n")






###### pca analysis for raw model   #######
pca.obj=prcomp(concrete_data[,-9], center = TRUE,scale. = TRUE)
summary(pca.obj)
pca.obj$rotation
pca.obj$x
par(mfrow=c(1,1))
biplot(pca.obj,choices=c(1,8))

# pcr
pcr.mod=pcr(strength~.,data=concrete_data,scale=T,validation="CV")
summary(pcr.mod)
par(mar = c(2, 2, 2, 2))
validationplot(pcr.mod,val.type="MSEP")

# plotfitted values for OLS and PCR, compare with actual
lmod.pc=lm(strength~.,data=concrete_data)
fit.pcr=predict(pcr.mod,data=concrete_data,ncomp=8)
plot(lmod.pc$fitted.values,concrete_data$strength,pch=19,col="blue")
points(fit.pcr,concrete_data$strength,col="red",lwd=2)
abline(a=0,b=1)

# compare R2 for each fit
R2.pcr=cor(fit.pcr,concrete_data$strength)^2
R2.lm=cor(lmod.pc$fitted.values,concrete_data$strength)^2
R2.pcr
R2.lm

#PCR: Using OLS with PCs 

# determine PCs for all predictors in concrete data set
pca_concrete=prcomp(concrete_data[,-9],scale=T)
summary(pca_concrete)

# view PCs and loadings
concrete_data[,-9]
pca_concrete$rot #Eigen vectors
pca_concrete$x#pc values of all vectors

# perform OLS using PC1-PC8
concrete_pcr=data.frame(cbind(pca_concrete$x,concrete_data$strength))
colnames(concrete_pcr)=c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","strength")

pcr.mod=lm(strength~.,data=concrete_pcr[,c(1:8,9)])
summary(pcr.mod)

#Select Best PC count using Cross-Validation

# use 5-fold CV to determine which number of PCs
# has the lowest estimated test error

# set up for cross validation
k.pc=5  # set number of folds
set.seed(123)
# create an index with id 1-5 to assign observations to folds
folds.pc=sample(1:5,nrow(concrete_pcr),replace=T) 

# create dummy matrix to store CV error estimates
cv.err=matrix(NA,k,8,dimnames=list(NULL,paste(1:8)))

# perform CV
for (j in 1:k){
  # estimate test error for all 20 models by predicting kth fold 
  for (i in 1:8){
    lmod.cv=lm(strength~.,data=concrete_pcr[folds!=j,c(1:i,9)])
    pred=predict(lmod.cv,concrete_pcr[folds==j,])
    cv.err[j,i]=mean((concrete_pcr$strength[folds==j]-pred)^2)  # save error est
  }
}

mse.cv=apply(cv.err,2,mean) # cdompute mean MSE for each number of predictors
min=which.min(mse.cv)  # find minimum mean MSE
mse.cv[min]

# plot and put a red circle around lowest MSE
par(mfrow=c(1,1))
plot(1:8,mse.cv,type="b",xlab="no. of predictors)",ylab="est. test MSE")
points(min,mse.cv[min],cex=2,col="red",lwd=2)
abline(h=seq(0,0.003,0.0005),lty=3)


# best MSE estimate is for 8 
# fit best model (using all the data)

pcr.mod=lm(strength~.,data=concrete_pcr[,c(1:8,9)])
summary(pcr.mod)
par(mfrow=c(2,2))
plot(pcr.mod)
vif(pcr.mod)

pcr.mod=lm(strength~.,data=concrete_pcr[,c(1:3,5:8,9)])
summary(pcr.mod)
par(mfrow=c(2,2))
plot(pcr.mod)
vif(pcr.mod)

# compare to OLS
par(mfrow=c(1,1))
lmod.ols=lm(strength~.,data=concrete_data)
plot(lmod.ols$fitted.values,concrete_data$strength,pch=19,col="blue")
points(pcr.mod$fitted.values,concrete_data$strength,col="red",lwd=2)
abline(a=0,b=1)

# interpret?
View(pca_concrete$rot[,c(1:8)])

# fit with all PCs
pcr.mod=lm(strength~.,data=concrete_pcr)
summary(pcr.mod)
par(mfrow=c(2,2))
plot(pcr.mod)
vif(pcr.mod)

# compare to OLS
par(mfrow=c(1,1))
lmod.olsc=lm(strength~.,data=concrete_pcr)
plot(lmod.olsc$fitted.values,concrete_data$strength,pch=19,col="blue")
points(pcr.mod$fitted.values,concrete_data$strength,col="red",lwd=2)
abline(a=0,b=1)


################################################################


##### Tree Based Regression ####
require(ISLR)
require(rpart)
require(randomForest)
require(caret)
require(dplyr)
require(gbm)

# grow tree
set.seed(123)
tree.mod <- rpart(strength~.,method="anova", data=concrete_data,
                  minsplit=2,maxsurrogate=0)

# plot tree
par(mfrow=c(1,1))
plot(tree.mod, uniform=T, main="Regression Tree for strength")
text(tree.mod)
summary(tree.mod) # detailed summary of splits

# display the cross-validation results
printcp(tree.mod) 

# visualize cross-validation results
plotcp(tree.mod)

# create additional plots
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(tree.mod) # visualize cross-validation results  

# prune the tree
pfit=prune(tree.mod,
           cp=tree.mod$cptable[which.min(tree.mod$cptable[,"xerror"]),
                               "CP"])
# plot the pruned tree
par(mfrow=c(1,1))
plot(pfit, uniform=T,
     main="Pruned Regression Tree for strength")
text(pfit)

# plotfitted values for OLS and RT, compare with actual
lmod=lm(strength~.,data=concrete_data)
fit.tree=predict(pfit,data=concrete_data)
plot(lmod$fitted.values,concrete_data$strength,pch=19,col="blue")
points(fit.tree,concrete_data$strength,col="red",lwd=2)
abline(a=0,b=1)


#####           Bagging  Raw Model     ######   

# bootstrap n=10
arry=c(1:10)
arry
indx=sample(1:10,10,replace=T)
arry[indx]


set.seed(123)
bag.mod=randomForest(strength~.,data=concrete_data,mtry=8, importance=T, ntree=500)
bag.mod
plot(bag.mod)

varImpPlot(bag.mod,type=1,pch=19)

# plotfitted values for OLS and RT, compare with actual
lmod=lm(strength~.,data=concrete_data)
plot(lmod$fitted.values,concrete_data$strength,pch=19,col="blue")
points(bag.mod$predicted,concrete_data$strength,col="red",lwd=2)
abline(a=0,b=1)

#                    Random Forest                      #

# tune model parameter mtry using caret
control=trainControl(method="cv", number=5, search="grid")#5 folds
set.seed(123)
tunegrid=expand.grid(mtry=c(1:8))
rf_gridsearch=train(strength~.,data=concrete_data, method="rf", metric="RMSE", 
                    tuneGrid=tunegrid, trControl=control, verboseIter = TRUE)

print(rf_gridsearch)
plot(rf_gridsearch)

set.seed(123)
rf.mod=randomForest(strength~.,data=concrete_data,mtry=5, ntree=500, 
                    importance=T)
rf.mod
plot(rf.mod)

varImpPlot(rf.mod,type=1,pch=19)

# plotfitted values for OLS and RT, compare with actual
lmod=lm(strength~.,data=concrete_data)
plot(lmod$fitted.values,concrete_data$strength,pch=19,col="blue")
points(rf.mod$predicted,concrete_data$strength,col="red",lwd=2)
abline(a=0,b=1)


####### GBM #######

# create index for random sample of 50
set.seed(123)
indx=sample(1:dim(concrete_data)[1],50,replace=F)

# tune model parameter mtry using caret
control=trainControl(method="cv", number=5, search="grid")
set.seed(123)
tunegrid=expand.grid(n.trees=c(100,500,1000,2000,5000,7500),
                     interaction.depth=c(1,3,5),
                     shrinkage=c(0.001,0.005,0.01),
                     n.minobsinnode=c(1,3,5))
gb_gridsearch=train(strength~.,data=concrete_data, 
                    method="gbm", metric="RMSE",
                    tuneGrid=tunegrid, trControl=control)
#print(gb_gridsearch)
plot(gb_gridsearch)


#choose n.tree=2000, int.depth=5, shrink=0.01, minobs=3
set.seed(123)
gb.mod=gbm(strength~.,data=concrete_data,
           distribution = "gaussian",n.trees = 7500,
           shrinkage = 0.01, interaction.depth = 5, 
           n.minobsinnode=5)

summary(gb.mod,cBars=10)


# plotfitted values for RT, compare with lasso
plot(predict(gb.mod,data=concrete_data,n.trees=7500),concrete_data$strength,col="red",lwd=2)
points(fit.lasso,concrete_data$strength,
       col="blue",pch=19)
abline(a=0,b=1)

################# with only interaction predictors #########














############ LoocV  only interaction  ############
set.seed(100)
fit.gen.i = glm(strength~.-ce.2-sl.2-fl.2-w.2-su.2-co.2-fi.2-a.2-strength-strength.box-strength.box.i-
                       strength.box.2, data=concrete)
cv.glm(concrete, fit.gen.i)$delta[1]

#-strength-strength.box-strength.box.i-
#        strength.box.2-cement-slag-fly_ash-water-superplasticizer-
#        course_aggregate-fine_aggregate-age+concrete2$cement+concrete2$slag+
#        concrete2$fly_ash+concrete2$water+concrete2$superplasticizer+
#        concrete2$course_aggregate+concrete2$fine_aggregate+concrete2$age


summary(fit.gen.i)
plot(fit.gen.i)


######## best subset #########

best_subset = regsubsets(strength~.-ce.2-sl.2-fl.2-w.2-su.2-co.2-fi.2-a.2-strength.box-strength.box.i-
                                               strength.box.2, data=concrete, nvmax=36, method="exhaustive")
subset = summary(best_subset)
subset
max(subset$rsq)

set.seed(100) #setting seed
k.i = 5  #set number of folds

# creating an index with id 1-5 to assign observations to folds
folds=sample(1:k.i,nrow(concrete),replace=T) 
# create dummy matrix to store CV error estimates
cv.error.i=matrix(NA,k.i,36,dimnames=list(NULL,paste(1:36)))

# perform CV
# pick models with lowest RSS with 1-9 predictors fit without kth fold
for (j in 1:k.i){
        best.model.i=regsubsets(strength~.-ce.2-sl.2-fl.2-w.2-su.2-co.2-fi.2-a.2-strength.box-strength.box.i-
                                                   strength.box.2, data=concrete[folds!=j,], nvmax=36,method="exhaustive") # estimate test error for all nine models by predicting kth fold 
        for (i in 1:36){
                pred.i=pred.sbs(best.model.i,concrete[folds==j,],id=i)
                cv.error.i[j,i]=mean((concrete$strength[folds==j]-pred.i)^2)  # save error est
        }
}




##Computing errors
mse.cv.i=apply(cv.error.i,2,mean)# compute mean MSE for each number of predictors
mse.cv.i

min.i=which.min(mse.cv.i)  # find minimum mean MSE

mse.cv.i[min.i]

par(mfrow=c(1,1))
plot(1:36,mse.cv.i,type="b",xlab="no. of predictors)",ylab="est. test MSE",ylim=c(60,240))
points(min.i,mse.cv.i[min.i],cex=2,col="red",lwd=2)
abline(h=c(80,120,160,200,240),lty=3)



#################################
##### ridge and lasso interaction model #############


x.i = model.matrix(strength~.-ce.2-sl.2-fl.2-w.2-su.2-co.2-fi.2-a.2-strength.box-strength.box.i-
                          strength.box.2, concrete)[,-1]
y.i = concrete$strength

ridge.concrete.i = glmnet(x.i,y.i,alpha=0,lambda=grid)

coef(glmnet(x.i,y.i, alpha=0, lambda=grid))

plot(ridge.concrete.i, xlab="L2 Norm")



### cross validation ridge - interaction only #####

set.seed(100)
ridge_cv.i = cv.glmnet(x.i,y.i,alpha=0,lambda=grid)
plot(ridge_cv.i)

bestlambda_r.i = ridge_cv.i$lambda.min
ridge_mse.i = min(ridge_cv.i$cvm)
ridge_mse.i
bestlambda_r.i
rsq_r.i = 1 - (ridge_cv.i$cvm/var(y.i))
max(rsq_r.i)
plot(ridge_cv.i$lambda,rsq_r.i, main="lambda vs rsq")

ridge_coeff.i = predict(ridge.concrete.i,type="coefficients",s=bestlambda_r.i)
ridge_coeff.i
ridge_fit.i = predict(ridge.concrete.i, s = bestlambda_r.i, x.i)

plot(fit$fitted.values,concrete$strength,pch=19,main="Ridge Fitted", col="blue")
points(ridge_fit.i,concrete$strength,col="red",lwd=1)
abline(a=0,b=1)
legend("topleft",legend=c("Fitted Values","Ridge Predicted values"),pch = c(19, 1),col=c("blue","red"),bty="n")


##########lasso regression interaction   ##########


lasso.concrete.i = glmnet(x.i,y.i,alpha=1,lambda=grid) #lasso model

coef(glmnet(x.i,y.i, alpha=1, lambda=grid))

plot(lasso.concrete.i, xlab="L2 Norm")

### cross validation lasso interaction model ######

set.seed(100)
lasso_cv.i = cv.glmnet(x.i,y.i,alpha=1,type.measure = "mse")
plot(lasso_cv.i)

bestlambda_l.i = lasso_cv.i$lambda.min
lasso_mse.i = min(lasso_cv.i$cvm)
lasso_mse.i
bestlambda_l.i
rsq_l.i = 1 - (lasso_cv.i$cvm/var(y.i))
max(rsq_l.i)
plot(lasso_cv.i$lambda,rsq_l.i, main="lambda vs rsq")

lasso_coeff.i = predict(lasso.concrete.i,type="coefficients",s=bestlambda_r.i)
lasso_coeff.i
lasso_fit.i = predict(lasso.concrete.i, s = bestlambda_l.i, x.i)

plot(fit$fitted.values,concrete$strength,pch=19,main="Lasso Fitted", col="blue")
points(lasso_fit.i,concrete$strength,col="red",lwd=1)
abline(a=0,b=1)
legend("topleft",legend=c("Fitted Values","Lasso Predicted values"),pch = c(19, 1),col=c("blue","red"),bty="n")








################# with Full predictors #########
############ LoocV  full model  ############
set.seed(100)
fit.gen.2 = glm(strength~.-strength.box-strength.box.i-strength.box.2, data=concrete)
cv.glm(concrete, fit.gen.2)$delta[1]

#-strength-strength.box-strength.box.i-
#        strength.box.2-cement-slag-fly_ash-water-superplasticizer-
#        course_aggregate-fine_aggregate-age+concrete2$cement+concrete2$slag+
#        concrete2$fly_ash+concrete2$water+concrete2$superplasticizer+
#        concrete2$course_aggregate+concrete2$fine_aggregate+concrete2$age


summary(fit.gen.2)
plot(fit.gen.2)


######## best subset full model #########

best_subset.2 = regsubsets(strength~.-strength.box-strength.box.i-strength.box.2
                           , data=concrete, nvmax=44, method="exhaustive")
subset.2 = summary(best_subset.2)
subset.2
View(subset.2$cp)

max(subset.2$rsq)

set.seed(100) #setting seed
k.2 = 5  #set number of folds

# creating an index with id 1-5 to assign observations to folds
folds=sample(1:k.2,nrow(concrete),replace=T) 
# create dummy matrix to store CV error estimates
cv.error.2=matrix(NA,k.2,44,dimnames=list(NULL,paste(1:44)))

# perform CV
# pick models with lowest RSS with 1-9 predictors fit without kth fold
for (j in 1:k.2){
        best.model.2=regsubsets(strength~.-strength.box-strength.box.i-strength.box.2,
                                data=concrete[folds!=j,], nvmax=44,method="exhaustive") # estimate test error for all nine models by predicting kth fold 
        for (i in 1:44){
                pred.2=pred.sbs(best.model.2,concrete[folds==j,],id=i)
                cv.error.2[j,i]=mean((concrete$strength[folds==j]-pred.2)^2)  # save error est
        }
}



pred.2
##Computing errors
mse.cv.2=apply(cv.error.2,2,mean)# compute mean MSE for each number of predictors
mse.cv.2

min.2=which.min(mse.cv.2)  # find minimum mean MSE

mse.cv.2[min.2]

par(mfrow=c(1,1))
plot(1:44,mse.cv.2,type="b",xlab="no. of predictors)",ylab="est. test MSE",ylim=c(20,250))
points(min.2,mse.cv.2[min.2],cex=2,col="red",lwd=2)
abline(h=c(80,120,160,200,240),lty=3)



#################################
##### ridge and lasso full model #############


x.2 = model.matrix(strength~.-strength.box-strength.box.i
                   -strength.box.2, concrete)[,-1]
y.2 = concrete$strength

ridge.concrete.2 = glmnet(x.2,y.2,alpha=0,lambda=grid)

coef(glmnet(x.2,y.2, alpha=0, lambda=grid))

plot(ridge.concrete.2, xlab="L2 Norm")



### cross validation ridge - full model only #####

set.seed(100)
ridge_cv.2 = cv.glmnet(x.2,y.2,alpha=0,type.measure = "mse")
plot(ridge_cv.2)
ridge_cv.2$glmnet.fit

bestlambda_r.2 = ridge_cv.2$lambda.min
ridge_mse.2 = min(ridge_cv.2$cvm)
ridge_mse.2
bestlambda_r.2
rsq_r.2 = 1 - (ridge_cv.2$cvm/var(y.2))
max(rsq_r.2)
plot(ridge_cv.2$lambda,rsq_r.2, main="lambda vs rsq")

ridge_coeff.2 = predict(ridge.concrete.2,type="coefficients",s=bestlambda_r.2)
ridge_coeff.2
ridge_fit.2 = predict(ridge.concrete.2, s = bestlambda_r.2, x.2)
ridge_fit.2

plot(fit$fitted.values,concrete$strength,pch=19,main="Ridge Fitted", col="blue")
points(ridge_fit.2,concrete$strength,col="red",lwd=1)
abline(a=0,b=1)
legend("topleft",legend=c("Fitted Values","Ridge Predicted values"),pch = c(19, 1),col=c("blue","red"),bty="n")


##########lasso regression full   ##########


lasso.concrete.2 = glmnet(x.2,y.2,alpha=1,lambda=grid) #lasso model

coef(glmnet(x.2,y.2, alpha=1, lambda=grid))

plot(lasso.concrete.2, xlab="L2 Norm")



### cross validation lasso full model ######

set.seed(100)
lasso_cv.2 = cv.glmnet(x.2,y.2,alpha=1,type.measure = "mse")
plot(lasso_cv.2)

bestlambda_l.2 = lasso_cv.2$lambda.min
lasso_mse.2 = min(lasso_cv.2$cvm)
lasso_mse.2
bestlambda_l.2
rsq_l.2 = 1 - (lasso_cv.2$cvm/var(y.2))
max(rsq_l.2)
plot(lasso_cv.2$lambda,rsq_l.2, main="lambda vs rsq")




lasso_coeff.2 = predict(lasso.concrete.2,type="coefficients",s=bestlambda_r.2)
lasso_coeff.2
lasso_fit.2 = predict(lasso.concrete.2, s = bestlambda_l.2, x.2)

plot(fit$fitted.values,concrete$strength,pch=19,main="Lasso Fitted", col="blue")
points(lasso_fit.2,concrete$strength,col="red",lwd=1)
abline(a=0,b=1)
legend("topleft",legend=c("Fitted Values","Lasso Predicted values"),pch = c(19, 1),col=c("blue","red"),bty="n")





#Principal Component Regression

# calculate centered predictors
#cement_c=concrete_data$cement-mean(concrete_data$cement)
#slag_c=concrete_data$slag-mean(concrete_data$slag)
#fly_ash_c=concrete_data$fly_ash-mean(concrete_data$fly_ash)
#water_c=concrete_data$water-mean(concrete_data$water)
##superplasticizer_c=concrete_data$superplasticizer-mean(concrete_data$superplasticizer)
#age_c=concrete_data$age-mean(concrete_data$age)
#aggregate_c=concrete_data$course_aggregate-mean(concrete_data$course_aggregate)
#fine_aggregate_c=concrete_data$fine_aggregate-mean(concrete_data$fine_aggregate)

# add squares and second-order interactions to data frame
concrete_data$ce_2=concrete_data$cement^2
concrete_data$sl_2=concrete_data$slag^2
concrete_data$fl_2=concrete_data$fly_ash^2
concrete_data$w_2=concrete_data$water^2
concrete_data$su_2=concrete_data$superplasticizer^2
concrete_data$co_2=concrete_data$course_aggregate^2
concrete_data$fi_2=concrete_data$fine_aggregate^2
concrete_data$a_2=concrete_data$age^2

concrete_data$ce_sl=concrete_data$cement*concrete_data$slag
concrete_data$ce_fl=concrete_data$cement*concrete_data$fly_ash
concrete_data$ce_w=concrete_data$cement*concrete_data$water
concrete_data$ce_su=concrete_data$cement*concrete_data$superplasticizer
concrete_data$ce_co=concrete_data$cement*concrete_data$course_aggregate
concrete_data$ce_fi=concrete_data$cement*concrete_data$fine_aggregate
concrete_data$ce_a=concrete_data$cement*concrete_data$age
concrete_data$sl_fl=concrete_data$slag*concrete_data$fly_ash
concrete_data$sl_w=concrete_data$slag*concrete_data$water
concrete_data$sl_su=concrete_data$slag*concrete_data$superplasticizer
concrete_data$sl_co=concrete_data$slag*concrete_data$course_aggregate
concrete_data$sl_fi=concrete_data$slag*concrete_data$fine_aggregate
concrete_data$sl_a=concrete_data$slag*concrete_data$age
concrete_data$fl_w=concrete_data$fly_ash*concrete_data$water
concrete_data$fl_su=concrete_data$fly_ash*concrete_data$superplasticizer
concrete_data$fl_co=concrete_data$fly_ash*concrete_data$course_aggregate
concrete_data$fl_fi=concrete_data$fly_ash*concrete_data$fine_aggregate
concrete_data$fl_a=concrete_data$fly_ash*concrete_data$age
concrete_data$w_su=concrete_data$water*concrete_data$superplasticizer
concrete_data$w_co=concrete_data$water*concrete_data$course_aggregate
concrete_data$w_fi=concrete_data$water*concrete_data$fine_aggregate
concrete_data$w_a=concrete_data$water*concrete_data$age
concrete_data$su_co=concrete_data$superplasticizer*concrete_data$course_aggregate
concrete_data$su_fi=concrete_data$superplasticizer*concrete_data$fine_aggregate
concrete_data$su_a=concrete_data$superplasticizer*concrete_data$age
concrete_data$co_fi=concrete_data$course_aggregate*concrete_data$fine_aggregate
concrete_data$co_a=concrete_data$course_aggregate*concrete_data$age
concrete_data$fi_a=concrete_data$fine_aggregate*concrete_data$age



# pca analysis
pca.obj=prcomp(concrete_data[,-9], center = TRUE,scale. = TRUE)
summary(pca.obj)
pca.obj$rotation
pca.obj$x
par(mfrow=c(1,1))
biplot(pca.obj,choices=c(1,8))

# pcr
pcr.mod=pcr(strength~.,data=concrete_data,scale=T,validation="CV")
summary(pcr.mod)
par(mar = c(2.5, 2, 2, 2))
validationplot(pcr.mod,val.type="MSEP")

# plotfitted values for OLS and PCR, compare with actual
lmod=lm(strength~.,data=concrete_data)
fit.pcr=predict(pcr.mod,data=concrete_data,ncomp=20)
plot(lmod$fitted.values,concrete_data$strength,pch=19,col="blue")
points(fit.pcr,concrete_data$strength,col="red",lwd=2)
abline(a=0,b=1)

# compare R2 for each fit
R2.pcr=cor(fit.pcr,concrete_data$strength)^2
R2.lm=cor(lmod$fitted.values,concrete_data$strength)^2
R2.pcr
R2.lm

#PCR: Using OLS with PCs 

# determine PCs for all predictors in concrete data set
pca_concrete=prcomp(concrete_data[,-9],scale=T)
summary(pca_concrete)

# view PCs and loadings
View(concrete_data[,-9])
View(pca_concrete$rot) #Eigen vectors
View(pca_concrete$x)#pc values of all vectors

# perform OLS using PC1-PC20
concrete_pcr=data.frame(cbind(pca_concrete$x,concrete_data$strength))
colnames(concrete_pcr)=c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11",
	"PC12","PC13","PC14","PC15","PC16","PC17","PC18","PC19","PC20","PC21","PC22","PC23","PC24",
	"PC25","PC26","PC27","PC28","PC29","PC30","PC31","PC32","PC33","PC34","PC35","PC36","PC37",
	"PC38","PC39","PC40","PC41","PC42","PC43","PC44","strength")

pcr.mod=lm(strength~.,data=concrete_pcr[,c(1:20,45)])
summary(pcr.mod)

#Select Best PC count using Cross-Validation

# use 5-fold CV to determine which number of PCs
# has the lowest estimated test error

# set up for cross validation
k=5  # set number of folds
set.seed(123)
# create an index with id 1-5 to assign observations to folds
folds=sample(1:5,nrow(concrete_pcr),replace=T) 

# create dummy matrix to store CV error estimates
cv.err=matrix(NA,k,20,dimnames=list(NULL,paste(1:20)))

# perform CV
for (j in 1:k){
  # estimate test error for all 20 models by predicting kth fold 
  for (i in 1:20){
    lmod.cv=lm(strength~.,data=concrete_pcr[folds!=j,c(1:i,45)])
    pred=predict(lmod.cv,concrete_pcr[folds==j,])
    cv.err[j,i]=mean((concrete_pcr$strength[folds==j]-pred)^2)  # save error est
  }
}

mse.cv=apply(cv.err,2,mean) # cdompute mean MSE for each number of predictors
min=which.min(mse.cv)  # find minimum mean MSE

# plot and put a red circle around lowest MSE
par(mfrow=c(1,1))
plot(1:20,mse.cv,type="b",xlab="no. of predictors)",ylab="est. test MSE")
points(min,mse.cv[min],cex=2,col="red",lwd=2)



# best MSE estimate is for 20 
# fit best model (using all the data)

pcr.mod=lm(strength~.,data=concrete_pcr[,c(1:20,45)])
summary(pcr.mod)
par(mfrow=c(2,2))
plot(pcr.mod)
vif(pcr.mod)

pcr.mod=lm(strength~.,data=concrete_pcr[ ,c(1:45)])
summary(pcr.mod)
par(mfrow=c(2,2))
plot(pcr.mod)
vif(pcr.mod)


plot(concrete_pcr$PC1,concrete_pcr$PC2)
# compare to OLS
par(mfrow=c(1,1))
lmod=lm(strength~.,data=concrete_data)
plot(lmod$fitted.values,concrete_data$strength,pch=19,col="blue")
points(pcr.mod$fitted.values,concrete_data$strength,col="red",lwd=2)
abline(a=0,b=1)

# interpret?
View(pca_concrete$rot[,c(1:20)])

# fit with all PCs
pcr.mod=lm(strength~.,data=concrete_pcr)
summary(pcr.mod)
par(mfrow=c(2,2))
plot(pcr.mod)
vif(pcr.mod)

# compare to OLS
par(mfrow=c(1,1))
lmod=lm(strength~.,data=concrete_pcr)
plot(lmod$fitted.values,concrete_data$strength,pch=19,col="blue",main ="PCR vs OLS")
points(pcr.mod$fitted.values,concrete_data$strength,col="red",lwd=2)
abline(a=0,b=1)

#######################################################################
##### Tree Based Regression ####

require(ISLR)
require(rpart)
require(randomForest)
require(caret)
require(dplyr)
require(gbm)

# grow tree
set.seed(123)
tree.mod <- rpart(strength~.,method="anova", data=concrete_data,
                  minsplit=2,maxsurrogate=0)

# plot tree
par(mfrow=c(1,1))
plot(tree.mod, uniform=T, main="Regression Tree for strength")
text(tree.mod)
summary(tree.mod) # detailed summary of splits

# display the cross-validation results
printcp(tree.mod) 

# visualize cross-validation results
plotcp(tree.mod)

# create additional plots
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(tree.mod) # visualize cross-validation results  

# prune the tree
pfit=prune(tree.mod,
           cp=tree.mod$cptable[which.min(tree.mod$cptable[,"xerror"]),
                               "CP"])
# plot the pruned tree
par(mfrow=c(1,1))
plot(pfit, uniform=T,
     main="Pruned Regression Tree for strength")
text(pfit)

# plotfitted values for OLS and RT, compare with actual
lmod=lm(strength~.,data=concrete_data)
fit.tree=predict(pfit,data=concrete_data)
plot(lmod$fitted.values,concrete_data$strength,pch=19,col="blue")
points(fit.tree,concrete_data$strength,col="red",lwd=2)
abline(a=0,b=1)

#                        Bagging                        #

# bootstrap n=10
arry=c(1:10)
arry
indx=sample(1:10,10,replace=T)
arry[indx]


set.seed(123)
bag.mod=randomForest(strength~.,data=concrete_data,mtry=44, importance=T, ntree=500)
bag.mod
plot(bag.mod)

varImpPlot(bag.mod,type=1,pch=19)

# plotfitted values for OLS and RT, compare with actual
lmod=lm(strength~.,data=concrete_data)
plot(lmod$fitted.values,concrete_data$strength,pch=19,col="blue")
points(bag.mod$predicted,concrete_data$strength,col="red",lwd=2)
abline(a=0,b=1)

#                    Random Forest                      #

# tune model parameter mtry using caret
control=trainControl(method="cv", number=5, search="grid")#5 folds
set.seed(123)
tunegrid=expand.grid(mtry=c(1:44))
rf_gridsearch=train(strength~.,data=concrete_data, method="rf", metric="RMSE", 
                    tuneGrid=tunegrid, trControl=control, verboseIter = TRUE)

print(rf_gridsearch)
plot(rf_gridsearch)

set.seed(123)
rf.mod=randomForest(strength~.,data=concrete_data,mtry=12, ntree=500, 
                    importance=T)
rf.mod
plot(rf.mod)

varImpPlot(rf.mod,type=1,pch=19)

# plotfitted values for OLS and RT, compare with actual
lmod=lm(strength~.,data=concrete_data)
plot(lmod$fitted.values,concrete_data$strength,pch=19,col="blue")
points(rf.mod$predicted,concrete_data$strength,col="red",lwd=2)
abline(a=0,b=1)


####### GBM #######

# create index for random sample of 50
#set.seed(123)
#indx=sample(1:dim(concrete_data)[1],50,replace=F)

# tune model parameter mtry using caret
control=trainControl(method="cv", number=5, search="grid")
set.seed(123)
tunegrid=expand.grid(n.trees=c(100,500,1000,2000,5000,7500,20000),
                     interaction.depth=c(1,3,5),
                     shrinkage=c(0.001,0.005,0.01),
                     n.minobsinnode=c(1,3,5))
gb_gridsearch=train(strength~.,data=concrete_data, 
                    method="gbm", metric="RMSE",
                    tuneGrid=tunegrid, trControl=control)
print(gb_gridsearch)
plot(gb_gridsearch)


#choose n.tree=2000, int.depth=5, shrink=0.01, minobs=3
set.seed(123)
gb.mod=gbm(strength~.,data=concrete_data,
           distribution = "gaussian",n.trees = 7500,
           shrinkage = 0.01, interaction.depth = 5, 
           n.minobsinnode=5)

summary(gb.mod,cBars=10)


# plotfitted values for RT, compare with lasso
plot(predict(gb.mod,data=concrete_data,n.trees=7500),concrete_data$strength,col="red",lwd=2)
points(fit.lasso,concrete_data$strength,
       col="blue",pch=19)
abline(a=0,b=1)
caret::R2(predict(gb.mod,data=concrete_data,n.trees=7500),concrete_data$strength)
caret::RMSE(predict(gb.mod,data=concrete_data,n.trees=7500),concrete_data$strength)
#pmml(gb.mod,model.name="gbm_model",app.name="R/PMML")



######################################################################3
###### Reduced Second Order model with best subsets  #############

red.sec = lm(strength.box.2~.-fly_ash-ce.sl-ce.co-sl.co-sl.fi-w.a-co.a-sl.2-co.2-age-strength.box-
               strength-strength.box.i, data=concrete)
red.mse = mean((red.sec$residuals)^2)
red.mse
summary(red.sec)
plot(red.sec)
par(mfrow=c(1,1))
plot(red.sec$fitted.values,concrete$strength, xlab = "Fitted", ylab = "Actual",
     main="fitted vs actual", pch=19)

#########################################################################

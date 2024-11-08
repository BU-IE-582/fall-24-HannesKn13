require(data.table, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(glmnet, quietly = TRUE)
data_path='/home/baydogan/Courses/IE582/Fall23/Data/Housing_Data/housingdata.csv'
feat_names_path='/home/baydogan/Courses/IE582/Fall23/Data/Housing_Data/names.txt'
data=fread(data_path)
feat_names=fread(feat_names_path,header=F)
names(data)=feat_names$V1
head(data)

fit_lreg=lm(MEDV~.,data)
summary(fit_lreg)
predicted=predict(fit_lreg,data)
plot(data$MEDV,predicted)
abline(a=0,b=1,col=2)

#require(fpp2)
#checkresiduals(fit_lreg)

fit_lreg_wo_intercept=lm(MEDV~-1+.,data)
summary(fit_lreg_wo_intercept)
predicted_wo_intercept=predict(fit_lreg_wo_intercept,data)
plot(data$MEDV,predicted_wo_intercept)
abline(a=0,b=1,col=2)

perf_data=data.table(actual=data$MEDV,lin_reg=predicted,lin_reg_wo_intercept=predicted_wo_intercept)
perf_data=melt(perf_data,id.vars=1)
perf_data[,residual:=actual-value]
head(perf_data)

ggplot(perf_data, aes(x=variable, y=residual)) + geom_boxplot() 

perf_summary=perf_data[,list(mse=mean(residual^2),bias=mean(residual),mape=mean(abs(residual/actual))),by=list(variable)]
perf_summary

set.seed(1)
noisy_part=matrix(rnorm(nrow(data)*(ncol(data)-1),sd=10),nrow=nrow(data))
noisy_data=cbind(data,noisy_part)
head(noisy_data)

fit_lreg_noisy=lm(MEDV~.,noisy_data)
summary(fit_lreg_noisy)
predicted_noisy=predict(fit_lreg_noisy,noisy_data)
plot(noisy_data$MEDV,predicted_noisy)
abline(a=0,b=1,col=2)

perf_data=data.table(actual=data$MEDV,lin_reg=predicted,
                     lin_reg_wo_intercept=predicted_wo_intercept,
                     lin_reg_noisy=predicted_noisy)
perf_data=melt(perf_data,id.vars=1)
perf_data[,residual:=actual-value]
require(ggplot2,quietly=T)
ggplot(perf_data, aes(x=variable, y=residual)) + geom_boxplot() 
perf_summary=perf_data[,list(mse=mean(residual^2),bias=mean(residual),mape=mean(abs(residual/actual))),by=list(variable)]
perf_summary

noisy_mat=as.matrix(noisy_data[,-c('MEDV'),with=F])
cvfit=cv.glmnet(noisy_mat,noisy_data$MEDV,family='gaussian',nfolds=10)
cvfit

str(cvfit)

noisy_mat=as.matrix(noisy_data[,-c('MEDV'),with=F])
cvfitridge=cv.glmnet(noisy_mat,noisy_data$MEDV,family='gaussian',alpha=0,nfolds=10)
cvfitridge

coef(cvfitridge,s="lambda.1se")

plot(cvfitridge)

plot(cvfit)

coef(cvfit,s="lambda.min")

coef(cvfit,s="lambda.1se")

predicted_min=predict(cvfit,noisy_mat,s='lambda.min')
predicted_se=predict(cvfit,noisy_mat,s='lambda.1se')

perf_data=data.table(actual=data$MEDV,lin_reg=predicted,
                     lin_reg_wo_intercept=predicted_wo_intercept,
                     lin_reg_noisy=predicted_noisy,
                     lasso_min=predicted_min, lasso_se=predicted_se)
perf_data=melt(perf_data,id.vars=1)
perf_data[,residual:=actual-value]
require(ggplot2,quietly=T)
ggplot(perf_data, aes(x=variable, y=residual)) + geom_boxplot() 
perf_summary=perf_data[,list(mse=mean(residual^2),bias=mean(residual),mape=mean(abs(residual/actual))),by=list(variable)]
perf_summary

data_with_interactions = model.matrix(MEDV~-1 +.^3,data)
head(data_with_interactions,10)
str(data_with_interactions)

cvfit=cv.glmnet(data_with_interactions,data$MEDV,family='gaussian',nfolds=10, alpha=1)
print(cvfit)
str(cvfit)
plot(cvfit)

coef(cvfit,s='lambda.1se')

predicted_interaction=predict(cvfit,data_with_interactions,s='lambda.1se')
perf_data=data.table(actual=data$MEDV,lin_reg=predicted,
                     lin_reg_wo_intercept=predicted_wo_intercept,
                     lin_reg_noisy=predicted_noisy,
                     lasso_min=as.numeric(predicted_min), lasso_se=as.numeric(predicted_se), lasso3d_se = as.numeric(predicted_interaction))
perf_data=melt(perf_data,id.vars=1)
perf_data[,residual:=actual-value]
ggplot(perf_data, aes(x=variable, y=residual)) + geom_boxplot() 
perf_summary=perf_data[,list(mse=mean(residual^2),bias=mean(residual),mape=mean(abs(residual/actual))),by=list(variable)]
perf_summary

classification_data=copy(data)
classification_data[,is_expensive:=as.numeric(MEDV>quantile(MEDV,0.5))]
classification_data[,MEDV:=NULL]
head(classification_data,10)

log_reg=glm(is_expensive~.,classification_data,family='binomial')
summary(log_reg)

predicted=predict(log_reg,classification_data)
head(predicted)
predicted=predict(log_reg,classification_data,type='response')
head(predicted)

#? require(dummies)
#? model.matrix()

mat=as.matrix(classification_data[,-c('is_expensive'),with=F])
cvfit_class=cv.glmnet(mat,classification_data$is_expensive,family='binomial',type.measure='class')
plot(cvfit_class)

head(mat)

coef(cvfit_class,s="lambda.1se")

predicted_min=predict(cvfit_class,mat,s='lambda.min',type='response')
predicted_se=predict(cvfit_class,mat,s='lambda.1se',type='response')
summary(predicted_min)



threshold=0.5
# log reg predictions
pred_class=as.numeric(predicted>threshold)

# pen log reg predictions
pred_min=as.numeric(predicted_min>threshold)

# pen log reg predictions
pred_se=as.numeric(predicted_se>threshold)



table(classification_data$is_expensive,pred_class)


err_rate=1-sum(classification_data$is_expensive==pred_class)/nrow(classification_data)

err_rate

# pen log reg predictions
pred_min=as.numeric(predicted_min>threshold)

table(classification_data$is_expensive,pred_min)
err_rate=1-sum(classification_data$is_expensive==pred_min)/nrow(classification_data)
err_rate

table(classification_data$is_expensive,pred_se)
err_rate=1-sum(classification_data$is_expensive==pred_se)/nrow(classification_data)
err_rate

ds5<-DS_5_9
any(is.na(ds5)==TRUE)
str(ds5)
names(ds5)


#1/��������� ������������� ���������, ������ � �������� ����������� ���������� ����� ��������, � ��������� ������������� 
#�������� ����������� ������ �������� ������, ������������������ ���������� ����������

#����������� ������ �������� ������ ���������� �� ������� ��������� �� ���� ������������� � step-wise ���������, � ���������
#������� ������������ ������������ ������ ��� ������ ���������� ��� ���������� ������

#model_0<-lm(data=ds5, �����~0+�����+�������������+���������+������������+�������������+��������������+��������+
#          ����������������+���������������+���������������)
#summary(model_0)

        #����� �1
#install.packages('leaps')
library(leaps)
model_regsubset<-regsubsets(�����~.,data=ds5,nbest=2,method="exhaustive")
#install.packages('HH')
library(HH)
summaryHH(model_regsubset)
plot(summaryHH(model_regsubset))
model_1_1<-lm(data=ds5, �����~�������������+��������������+���������������)
summary(model_1_1) #������ ������������� �������: F=2.65, p=0.04905, R2=0.026
library(lessR)
Regression(data=ds5, �����~�������������+��������������+���������������)

#����� �2
#install.packages('MASS')
library(MASS)
model_1_2<-lm(data=ds5, �����~.)
model_step_wise<-stepAIC(model_1_2, direction = 'both')
summary(model_step_wise) #������ ������������� �������: F=3.157, p=0.04397, R2=0.0208
model_1_2<-model_step_wise
Regression(data=ds5, �����~�������������+���������������)

#����� �3
#install.packages('party')
library(party)
cf1<-cforest(�����~., data=ds5, control=cforest_unbiased(mtry=2,ntree=50))
varimp(cf1)
order(varimp(cf1))
model_1_3<-lm(data=ds5,�����~������������+���������������+�������������)
summary(model_1_3) #������ ������������� �� �������: F=2,322, p=0.075, R2=0.022

#����� �4
#install.packages('glmnet')
library(glmnet)
x_1_4<-model.matrix(�����~., data=ds5)
x_1_4=x_1_4[,-1]
model_glmnet<-glmnet(x=x_1_4, y=ds5$�����, type.measure='mse')
nams<-coef(model_glmnet, s=0.01)
row.names(nams)[order(nams, decreasing = TRUE)]
model_1_4<-lm(data = ds5, �����~������������+�������������+�������������)
summary(model_1_4) #������ ������������� �� �������: F=0,9211, p=0.4309, R2=0.009
Regression(data=ds5, �����~������������+�������������+�������������)

#����� �5
#install.packages('relaimpo')
library(relaimpo)
lmMod<-lm(�����~., data=ds5)
relImportance<-calc.relimp(lmMod, type = 'lmg', rela=T)
sort(relImportance$lmg, decreasing = T)
model_1_5<-lm(data=ds5,�����~���������������+�������������+��������������)
summary(model_1_5)#������ ������������� �������: F=2,65, p=0.04905, R2=0.026

#�������� ����������� �������������� ������, ����������� #�� ������ �2, ��� �������� ������� �� ��������� � ������� ��� 
#�������������� ������� �� ������������

#2/��������� ������������ ����������� ������-������� ��� ���������� ������
library(gvlma)
summary(gvlma(model_1_2))

#Global Stat=����� ���� - 
#Skewness=���������� +
#Kurtosis=������� -
#Link Function=���������� +
#Heteroscedasticity=�������������������� +
#����������� �������� � �������������� ����������� ������-�������, �������������� �������������, ������������ ��������� 
#�������� � ���������� � ��� ���������� � ���������� ������


#3/������������ ���������� � ������ ��������� ��� ���������� ���������� ���������, ��������� � ������� ������������� 
#����������������� ������, �������� ������
library(trafo)
assumptions(model_1_2, method = "ml",plotit = FALSE)

#�� ����������� ������� �������������� ���������� �� �������������� ���������, ����� ����� ������������ � ������� ����(untransformed)
#����� �������, �� ������ �� �� ��� ������ �������� ������������� ��������, ������������� �������������� ���������� � �������������� 
#������� ������-������� � �����, ������ ������ ��������������� ��������� ��� ���������������


#4/��������� ������ ���������, ������ � �������� ����������� ���������� ����� ��������, �� ���� ����������, � ����������� 
#������� �������������. �������� ������������� ���������� � ���� ������ � ������� ������ ������

#� ������� ������������� ���������: ������������ ����� � ��������� ���������
#LASSO
library(caret)
X<-ds5[,2:10]
model_lasso <- glmnet(as.matrix(X), ds5$�����, alpha = 1)
plot(model_lasso, xvar =  "lambda", label = TRUE, lwd = 2)
plot(model_lasso, xvar = "dev", label = TRUE)
plot(model_lasso, xvar = "norm", label = TRUE)
model_lasso_cv<-cv.glmnet(as.matrix(X), ds5$�����, alpha=1)
plot(model_lasso_cv)
coef(model_lasso_cv,s='lambda.min')
#��� �� ��������� �� ������

#����
X<-ds5[,2:10]
model_rr <- glmnet(as.matrix(X), ds5$�����, alpha = 0)
plot(model_rr, xvar =  "lambda", label = TRUE, lwd = 2)
plot(model_rr, xvar = "dev", label = TRUE)
plot(model_rr, xvar = "norm", label = TRUE)
model_rr_cv<-cv.glmnet(as.matrix(X), ds5$�����, alpha=0)
plot(model_rr_cv)
coef(model_rr_cv,s='lambda.min')
coef(model_rr_cv,s='lambda.1se')
model_rr_cv <- train(as.data.frame(X), ds5$�����, method = "glmnet",  
                     tuneGrid = expand.grid(.lambda = model_rr_cv$lambda.min, .alpha = 0),
                     trControl = trainControl(method = "cv"))
model_rr_cv$results #� ����� ������ �������� ����� ������������� �� ���������, ��������������� ����� ������� �� ������������ �� ��������� 
#� ������� ���������� �� ������ 2 (��������� �� ������ ���������� ���������)


#5/��������� ��������� ����������� ��������� ��� 10 � 90-�� ��������� �� ������ ������. �������� ������
library(quantreg)
rq_1<-rq(data = ds5, tau = 0.9, ����� ~ �������������+���������������)
summary(rq_1, se = "boot") #�������� �������� ���������� �� �������� 90 �������� ������ �������� ������ �������������
rq_2<-rq(data = ds5, tau = 0.1, ����� ~ �������������+���������������)
summary(rq_2, se = "boot") #�������� �������� ���������� �� �������� 10 �������� ������ �������� ������ ���������������
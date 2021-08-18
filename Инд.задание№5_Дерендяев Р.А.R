ds5<-DS_5_9
any(is.na(ds5)==TRUE)
str(ds5)
names(ds5)


#1/Постройте множественную регрессию, выбрав в качестве объясняемой переменной доход человека, и определив автоматически 
#наиболее оптимальный состав факторов модели, проинтерпретируйте полученные результаты

#Оптимальный состав факторов модели определяем по методам регрессии по всем подмножествам и step-wise регрессия, в остальных
#методах присутствует субъективный фактор при выборе параметров для построения модели

#model_0<-lm(data=ds5, Доход~0+Доход+Общительность+Интеллект+Устойчивость+Доминантность+Экспресивность+Смелость+
#          Чувствительность+Дипломатичность+Экстравертность)
#summary(model_0)

        #метод №1
#install.packages('leaps')
library(leaps)
model_regsubset<-regsubsets(Доход~.,data=ds5,nbest=2,method="exhaustive")
#install.packages('HH')
library(HH)
summaryHH(model_regsubset)
plot(summaryHH(model_regsubset))
model_1_1<-lm(data=ds5, Доход~Доминантность+Экспресивность+Дипломатичность)
summary(model_1_1) #модель статистически значима: F=2.65, p=0.04905, R2=0.026
library(lessR)
Regression(data=ds5, Доход~Доминантность+Экспресивность+Дипломатичность)

#метод №2
#install.packages('MASS')
library(MASS)
model_1_2<-lm(data=ds5, Доход~.)
model_step_wise<-stepAIC(model_1_2, direction = 'both')
summary(model_step_wise) #модель статистически значима: F=3.157, p=0.04397, R2=0.0208
model_1_2<-model_step_wise
Regression(data=ds5, Доход~Доминантность+Дипломатичность)

#метод №3
#install.packages('party')
library(party)
cf1<-cforest(Доход~., data=ds5, control=cforest_unbiased(mtry=2,ntree=50))
varimp(cf1)
order(varimp(cf1))
model_1_3<-lm(data=ds5,Доход~Устойчивость+Дипломатичность+Доминантность)
summary(model_1_3) #модель статистически не значима: F=2,322, p=0.075, R2=0.022

#метод №4
#install.packages('glmnet')
library(glmnet)
x_1_4<-model.matrix(Доход~., data=ds5)
x_1_4=x_1_4[,-1]
model_glmnet<-glmnet(x=x_1_4, y=ds5$Доход, type.measure='mse')
nams<-coef(model_glmnet, s=0.01)
row.names(nams)[order(nams, decreasing = TRUE)]
model_1_4<-lm(data = ds5, Доход~Устойчивость+Общительность+Доминантность)
summary(model_1_4) #модель статистически не значима: F=0,9211, p=0.4309, R2=0.009
Regression(data=ds5, Доход~Устойчивость+Общительность+Доминантность)

#метод №5
#install.packages('relaimpo')
library(relaimpo)
lmMod<-lm(Доход~., data=ds5)
relImportance<-calc.relimp(lmMod, type = 'lmg', rela=T)
sort(relImportance$lmg, decreasing = T)
model_1_5<-lm(data=ds5,Доход~Дипломатичность+Доминантность+Экспресивность)
summary(model_1_5)#модель статистически значима: F=2,65, p=0.04905, R2=0.026

#наиболее оптимальной представляется модель, построенная #по методу №2, как наиболее простая по сравнению с другими при 
#незначительном отличии кф детерминации

#2/Проверьте выполнимость предпосылок Гаусса-Маркова для полученной модели
library(gvlma)
summary(gvlma(model_1_2))

#Global Stat=общий итог - 
#Skewness=Ассиметрия +
#Kurtosis=эксцесс -
#Link Function=Линейность +
#Heteroscedasticity=Гетероскедастичность +
#принимается гипотеза о невыполнимости предпосылок Гаусса-Маркова, ненормальности распределения, однородности дисперсии 
#остатков и отсутствия в них корреляции и линейности модели


#3/Преобразуйте переменные в модели регрессии для устранения выявленных нарушений, постройте и оцените достоверность 
#скорректированной модели, сделайте выводы
library(trafo)
assumptions(model_1_2, method = "ml",plotit = FALSE)

#По результатам анализа преобразование переменных не представляется возможным, лучше всего использовать в текущем виде(untransformed)
#Таким образом, не смотря на то что модель является статистически значимой, невозможности преобразования переменных и невыполнимости 
#условий Гаусса-Маркова в целом, данную модель нецелесообразно применять для прогнозирования


#4/Постройте модель регрессии, выбрав в качестве объясняемой переменной доход человека, по всем переменным, с применением 
#методов регуляризации. Сравните достоверность полученной в этом пункте и пунктом раньше модели

#К методам регуляризации относятся: классическое лассо и гребневая регрессия
#LASSO
library(caret)
X<-ds5[,2:10]
model_lasso <- glmnet(as.matrix(X), ds5$Доход, alpha = 1)
plot(model_lasso, xvar =  "lambda", label = TRUE, lwd = 2)
plot(model_lasso, xvar = "dev", label = TRUE)
plot(model_lasso, xvar = "norm", label = TRUE)
model_lasso_cv<-cv.glmnet(as.matrix(X), ds5$Доход, alpha=1)
plot(model_lasso_cv)
coef(model_lasso_cv,s='lambda.min')
#все кф исключены из модели

#Ридж
X<-ds5[,2:10]
model_rr <- glmnet(as.matrix(X), ds5$Доход, alpha = 0)
plot(model_rr, xvar =  "lambda", label = TRUE, lwd = 2)
plot(model_rr, xvar = "dev", label = TRUE)
plot(model_rr, xvar = "norm", label = TRUE)
model_rr_cv<-cv.glmnet(as.matrix(X), ds5$Доход, alpha=0)
plot(model_rr_cv)
coef(model_rr_cv,s='lambda.min')
coef(model_rr_cv,s='lambda.1se')
model_rr_cv <- train(as.data.frame(X), ds5$Доход, method = "glmnet",  
                     tuneGrid = expand.grid(.lambda = model_rr_cv$lambda.min, .alpha = 0),
                     trControl = trainControl(method = "cv"))
model_rr_cv$results #в целом модель является также статистически не пригодной, характеризуется более высоким кф детерминации по сравнению 
#с моделью полученной по методу 2 (регрессия по методу наименьших квадратов)


#5/Постройте уравнение квантильной регрессии для 10 и 90-ых квантилей по уровню дохода. Сделайте выводы
library(quantreg)
rq_1<-rq(data = ds5, tau = 0.9, Доход ~ Доминантность+Дипломатичность)
summary(rq_1, se = "boot") #наиболее значимым параметром на величину 90 квантили дохода является фактор Доминантности
rq_2<-rq(data = ds5, tau = 0.1, Доход ~ Доминантность+Дипломатичность)
summary(rq_2, se = "boot") #наиболее значимым параметром на величину 10 квантили дохода является фактор Дипломатичности
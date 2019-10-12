
install.packages("MASS")
library(MASS)
options(digits=4)
set.seed(21)
sigma<-matrix(c(1,0.8,0.4,-0.6,
                0.8,1,0.6,-0.4,
                0.4,0.6,1,-0.1,
                -0.6,-0.4,-0.1,1),nrow=4,ncol=4)
mean<-c(200.8,56.4,8,42)
mydata<-mvrnorm(30,mean,sigma)
mydata<-as.data.frame(mydata)
names(mydata)<-c("y","x1","x2","x3")
x=mydata[,-1]
y=mydata[,1]


install.packages("mctest")
library(mctest)
#1.проверка мультиколлинеарности по числу обусловленности
omcdiag(x=x,y=y)
#получили число обусловленности=284.8>10

install.packages("mctest")
library(mctest)
#2.проверка мультиколлинеарности по методу Бэлсли
eigprop(x=x, Inter=T, prop=0.7)
#видна взаимосвязь между переменными(см.5я и 4я строки, в особенности между х1 и х3)
#отметим высокое значение q при свободном члене

install.packages("MASS")
install.packages("AER")
library(AER)
library(MASS)

#3.Метод пошаговой регресси устранения переменных
install.packages("lmtest")
library(lmtest)
linear<-lm(data=mydata,y~x1+x2+x3)
linear

#проверяем vif
install.packages("car")
library(car)
vif(linear)

#исключение переменных по критерию AIC
install.packages("MASS")
install.packages("AER")
library(AER)
library(MASS)
bw<-stepAIC(linear,direction='backward')
summary(bw)
#осталась всего одна переменная значащая х1, для которой отсутствует мультиколлинеарность

#перебираем множество всех подмножеств возможных регрессий
#1.
install.packages('leaps')
library(leaps)
lps<-regsubsets(y~x1+x2+x3, data=mydata, nbest=5)
install.packages("car")
library(car)
plot(x=lps,scale = "adjr2",main="Скорректированный коэффициент детерминации")

#2.
install.packages('leaps')
library(leaps)
lps<-regsubsets(y~x1+x2+x3, data=mydata, nbest=5)
install.packages("car")
library(car)
plot(lps,scale = "bic",main="BIC")

#3.
install.packages('leaps')
library(leaps)
lps<-regsubsets(y~x1+x2+x3, data=mydata, nbest=5)
install.packages("car")
library(car)
plot(lps,scale = "Cp",main="статистика Мэллоуса")
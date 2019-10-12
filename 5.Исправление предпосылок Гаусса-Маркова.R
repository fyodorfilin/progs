install.packages("mvtnorm")
install.packages("lmtest")
install.packages('orcutt')
install.packages('sandwich')
library(mvtnorm)
library(lmtest)
nn<-22

#подготовка данных
set.seed(nn*10)
sigma<-matrix(c(1,0.8,0.4,-0.6,
                0.8,1,0.7,-0.4,
                0.4,0.7,1,-0.1,
                -0.6,-0.4,-0.1,1),nrow=4,ncol=4)
mean<-c(nn*15,nn^2,(nn+20),(60-nn))
mydata<-rmvnorm(30,mean,sigma)
mydata<-as.data.frame(mydata)
names(mydata)<-c("y","x1","x2","x3")
head(mydata,n=5)

#построение регрессии
linear<-lm(data=mydata,y~x1+x2+x3)
#тест Дарбина-Уотсона
dwtest(linear)
#тест Бройша-Годфри 1-го порядка
bgtest(linear,1)
#тест Бройша-Годфри 2-го порядка
bgtest(linear,2)

#устранение автокорреляции
library(orcutt)
linear2<-cochrane.orcutt(linear)
linear2

#получение случайных остатков и корреляционной матрицы модифицированной модели
mydata[5]<-linear$residuals
mydata[6]<-mydata[5]^2
colnames(mydata)[5]<-'e'
colnames(mydata)[6]<-'e2'
mydata
correl<-cor(mydata[2:ncol(mydata)])
correl
#из корреляционной матрицы видно, что наибольшую корреляцию с случайными
#остатками имеет фактор х2, значит выбираем его для проверки теста Голдфрилда-Кванта

#тест Голдфрилда-Кванта
gqtest(formula=linear,data=mydata,fraction=0.33,order.by = ~x2)

#устранение гетероскедастичности
t1data<-mydata[,1:4]
t1data[1]<-t1data[1]/t1data[3]
t1data[2]<-t1data[2]/t1data[3]
t1data[4]<-t1data[4]/t1data[3]
t1data[3]<-1/t1data[3]
t1data
t2data<-log(t1data)
t2data
t1linear<-lm(data=t1data,y~x1+x2+x3)
t2linear<-lm(data=t2data,y~x1+x2+x3)
summary(t1linear)
summary(t2linear)
gqtest(formula=t1linear,data=t1data,fraction=0.33,order.by = ~x2)
gqtest(formula=t2linear,data=t2data,fraction=0.33,order.by = ~x2)

#устранение всего
library(sandwich)
NeweyWest(linear)
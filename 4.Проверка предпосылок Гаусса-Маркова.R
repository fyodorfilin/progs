install.packages("mvtnorm")
library(mvtnorm)

#собираем собственные данные
nn<-22
set.seed(nn*10)
sigma<-matrix(c(1,0.8,0.4,-0.6,
                0.8,1,0.7,-0.4,
                0.4,0.7,1,-0.1,
                -0.6,-0.4,-0.1,1),nrow=4,ncol=4)

mean<-c(nn*15,nn^2,(nn+20),(60-nn))
mydata<-rmvnorm(30,mean,sigma)
mydata<-as.data.frame(mydata)
names(mydata)<-c("y","x1","x2","x3")

install.packages('lmtest')
library(lmtest)

#строим линейную регрессию, оцениваем методом МНК
#вычисляем случайные остатки
linear<-lm(data=mydata,y~x1+x2+x3)
coef<-as.numeric(linear$coefficients)
mydata[5]<-mydata[1]-(coef[1] + mydata[2]*coef[2] + mydata[3]*coef[3] + mydata[4]*coef[4])
colnames(mydata)[5]<-'e'
mydata

#смотрим корреляцию факторов модели со случайными остатками
correl<-cor(mydata[2:ncol(mydata)])
correl
#из корреляционной матрицы видно, что наибольшую корреляцию с случайными
#остатками имеет фактор х3, значит выбираем его для проверки теста Голдфрилда-Кванта

#тест Дарбина-Уотсона
dwtest(linear)
#тест Бройша-Годфри 1-го порядка
bgtest(linear,1)
#тест Бройша-Годфри 2-го порядка
bgtest(linear,2)
#тест Голдфрилда-Кванта
gqtest(formula=linear,data=mydata,fraction=0.33,order.by = ~x3)
#аналог теста Уайта
bptest(formula=linear,data=mydata, varformula=~x1+x2+x3+x1^2+x2^2+x3^2+x1*x2+x1*x3+x2*x3)

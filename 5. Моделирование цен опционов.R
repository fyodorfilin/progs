library(xlsx)
library(NMOF)


#Загружаем данные
setwd("C:/Users/Елена/Desktop/Курсовая работа 2019/")
par_vol<-read.xlsx("Оценки параметров для модели волатильности.xlsx",1)
kor_koef<-read.xlsx("Коэффициент корреляции винеровских процессов.xlsx",1)
k<-as.double(par_vol[2])
fi<-as.double(par_vol[3])
ksi<-as.double(par_vol[4])
ro<-as.double(kor_koef[2])
mod_vol<-read.xlsx("Волатильность для модели(вспомогательный файл).xlsx",1)
mod_vol<-mod_vol[,2]
mod_price<-read.xlsx("Цена акции для модели(вспомогательный файл).xlsx",1)
mod_price<-mod_price[,2]
opt<-read.xlsx("Опционы на акции APPLE(fixed).xlsx",1)
opt<-opt[,2:ncol(opt)]
opt<-cbind.data.frame(opt,mod_vol,mod_price)


#Модель Хестона для опционов(сравнительный анализ)
mod_opt<-vector()
for (i in 1:nrow(opt)){
  mod_opt<-append(mod_opt,callHestoncf(S=opt[i,7],X=opt[i,2],tau=opt[i,4],r=opt[i,5],q=0,v0=opt[i,6],vT=fi,rho=ro,k=k,sigma=ksi))
}
last_price<-opt[,3]
m<-max(c(max(last_price),max(mod_opt)))
x<-1:length(last_price)
plot(x=x,y=last_price,main="Сравнение цен опционов по Хестону с реальной ценой продажи", ylab="Значения", xlab="Моменты времени t",type='l', col='red',lwd=2,ylim=c(0,m+10))
lines(x=x,y=mod_opt,type='l', col='blue',lwd=2)
legend("topright",legend=c("Послед. цена","Цена по Хестону"),fill=c("red","blue"))
comp<-data.frame(mod_opt,last_price)
#write.xlsx(comp,"Таблица для анализа результатов.xlsx")


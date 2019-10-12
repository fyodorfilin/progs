library(xlsx)
library(lmtest)


#Функция поиска всех возможных разбиений массива
dv<-function(data){
  del_v<-vector()
  l<-length(data)
  i<-2
  while(i<=l){
    if(l%%i==0) {del_v<-append(del_v,i)}
    i<-i+1
  }
  return(del_v)
}


#РС - анализ
rs<-function(k,log_doh){
  l<-length(log_doh)
  p<-l/k
  r<-vector()
  s<-vector()
  for (i in 1:p){
    mo<-mean(log_doh[((i-1)*k+1):(i*k)])
    d<-log_doh[((i-1)*k+1):(i*k)]-mo
    r<-append(r,(max(d)-min(d)))
    s<-append(s,sqrt(mean(d^2)))
  }
  r_s<-vector()
  for (i in 1:p){
    r_s<-append(r_s,(r[i]/s[i]))
  }
  return(mean(r_s))
}


#Загружаем данные
setwd("C:/Users/Елена/Desktop/фрактальные рынки/")
data<-read.xlsx("APPLE(fixed).xlsx",1)


#Формируем лог. доходности
opn<-data[,2]
cls<-data[,3]
log_doh<-vector()
for (i in 1:length(opn)){
  ld<-log(cls[i]/opn[i])
  log_doh<-append(log_doh,ld)
}


#Строим эконометрическую модель
delvect<-dv(data=log_doh)
ln_del<-log(delvect)
r_s<-vector()
for (i in delvect){
  r_s<-append(r_s,rs(k=i,log_doh=log_doh))
}
ln_r_s<-log(r_s)
mdl<-data.frame(ln_r_s,ln_del)


#Оценка параметров эконометрической модели
model_par<-lm(dat=mdl,ln_r_s~ln_del)
model_par<-model_par$coefficients
herst<-as.double(model_par[2])
herst
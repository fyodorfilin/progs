install.packages("mvtnorm")
install.packages("glmnet")
install.packages("lmtest")
library(mvtnorm)
library(glmnet)
library(lmtest)
nn<-22

#фиксируем генерацию данных
set.seed(nn*10)

#ковариационная матрица
sigma<-matrix(c(1,0.8,0.4,-0.6,
                0.8,1,0.7,-0.4,
                0.4,0.7,1,-0.1,
                -0.6,-0.4,-0.1,1),nrow=4,ncol=4)
#вектор мат. ожиданий
mean<-c(nn*15,nn^2,(nn+20),(60-nn))

#генерация случайных нормальных величин
mydata<-rmvnorm(30,mean,sigma)
mydata<-as.data.frame(mydata)
names(mydata)<-c("y","x1","x2","x3")
head(mydata,n=5)

#корреляционная матрица
correl<-cor(mydata[2:ncol(mydata)])

#обратная корреляционная матрица
ob_correl<-solve(correl)

#статистика мультиколлинеарности Феррера Глобера
n<-nrow(mydata[2:ncol(mydata)])
k<-ncol(mydata[2:ncol(mydata)])
fg<-(-1)*(n-1-(2*k+5)/6)*log(det(correl))

#статистики мультиколлинеарности каждого фактора Феррера Глобера
cc<-diag(ob_correl)
f<-vector()
for (i in 1:k){
  fi<-(as.numeric(cc[i])-1)*(n-k-1)/k
  f<-append(f,fi)
}

#статистики попарной мультиколлинеарности Феррера Глобера
t<-vector()
ob_correl<-as.data.frame(ob_correl)
for (i in 1:k){
  for (j in 1:k){
    if (i<j){
      rij<-(-1)*t(ob_correl[j])[i]/sqrt(t(ob_correl[i])[i]*t(ob_correl[j])[j])
      tij<-rij*sqrt(n-k-1)/sqrt(1-rij^2)
      t<-append(t,abs(tij))
    }
  }  
}

#проверка гипотез
fgkr<-qchisq(p=0.95,df=0.5*k*(k-1))
tkr<-qt(p=0.95,df=n-k-1)
fkr<-qf(p=0.95,df1=k,df2=n-k-1)  
print(paste0('статистика FG: ',round(fg,2)))
print(paste0('значение FG(крит): ',round(fgkr,2)))
if(fg<fgkr){
  print('гимотеза о мультиколлинеарности данных отвергается')}
if(fg>=fgkr){
  print('гимотеза о мультиколлинеарности данных принимается')}
print('статистики F: ')
print(paste(round(f,2)))
print(paste0('значение F(крит): ',round(fkr,2)))
kk<-0
for (i in f){
  if (i>=fkr){
    k<-k+1
  }
}
if(k==0){
  print('гимотеза о мультиколлинеарности данных отвергается')}
if(k!=0){
  print('гимотеза о мультиколлинеарности данных принимается, см. данные')}
print('статистики T: ')
print(paste(round(t,2)))
print(paste0('значение F(крит): ',round(tkr,2)))
kk<-0
for (i in t){
  if (i>=tkr){
    k<-k+1
  }
}
if(k==0){
  print('гимотеза о мультиколлинеарности данных отвергается')} else{
  print('гимотеза о мультиколлинеарности данных принимается, см. данные')}

#критерий вздутия
vif<-vector()
data<-mydata[2:ncol(mydata)]
cd<-colnames(data)
for (i in 1:ncol(data)){
  for (j in 1:ncol(data)){
    for (a in 1:ncol(data)){
      if ((i!=j)&(i!=a)&(j!=a)){
        f<-as.formula(paste0(cd[i],'~',cd[j],'+',cd[a]))
        linear<-lm(dat=data,f)
        rsq<-as.numeric(summary(linear)[8])
        vf<-1/(1-rsq)
        vif<-append(vif,(vf))
      }  
    }  
  }
}
vif<-unique(round(vif,2))
print('различные значения VIF:')
print(vif)

k=0
for (i in length(vif)){
  if (vif[i]>10){
    k<-k+1
  }
}
if (k>0){
  print('гипотеза о мультиколлинеарности данных принимается')
} else {print('гипотеза о мультиколлинеарности данных отвергается')} 
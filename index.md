### <p dir='rtl' align='right'>خوشامدگویی</p>

 <p dir='rtl' align='right'>در این بخش کد و برنامه های کامپیوتری رساله دکتری با نام زیر قرار گرفته است:</p>

<div align="center">
  "ساخت توابع مفصل جدید با استفاده از روش تغییر شکل"
</div>  
  
<p dir='rtl' align='right'>همچنین:</p>

 <p dir='rtl' align='right'>کد های فصل های سه و چهار پس از چاپ مقالات مستخرج، در این وبگاه قرار خواهند گرفت.</p>

For more Info. see this [link.](https://hamb8066.github.io/homepage/)


### <p dir='rtl' align='right'>فصل دوم - جدول 2.2 </p>

```markdown

##############################
######Table 2.2 R-codes#######
##############################

####################
###Spearman's Rho###
####################
a=0
######################Distortion functions#########################
r=-30
ginv=function(t){
(exp(r*t)-1)/(exp(r)-1)
}
g=function(t){
log(((exp(r)-1)*t+1)^(1/r))
}
######################Copulas#########################
alpha=0.1
basecop=function(u,v){
min((u^alpha)*v,(v^alpha)*u)
}
distcop=function(u,v){
ginv(basecop(g(u),g(v)))
}
#########################Spearman rho##################
#install.packages("R2Cuba", repos="http://cran.um.ac.ir")
#require(R2Cuba)
integrand <- function(arg) {
u <- arg[1]
v <- arg[2]
ff <-ginv(min(((g(u))^alpha) * g(v),((g(v))^alpha) * g(u))) 
return(ff)
}
NCOMP <- 1
#sink("aaa")
a=cuhre(2, NCOMP,  integrand,lower=c(0,0),upper=c(1,1))$value
#sink()
12*a-3

##########################LOOP##########################
pp=seq(0.1,0.9,0.1)
kk=c(0.01,0.8,1,10,100,400,700)
#seq(-37,-1,1)
a=matrix(0,length(pp),length(kk))
#install.packages("R2Cuba", repos="http://cran.um.ac.ir")
#require(R2Cuba)
for(j in 1:length(kk)){
ginv=function(t){
(exp((kk[j])*t)-1)/(exp(kk[j])-1)
}
g=function(t){
log(((exp(kk[j])-1)*t+1)^(1/(kk[j])))
}
for(i in 1:length(pp)){

integrand <- function(arg) {
u <- arg[1]
v <- arg[2]
ff <-ginv(min(((g(u))^pp[i]) * g(v),((g(v))^pp[i]) * g(u))) 
return(ff)
}
NCOMP <- 1
#sink("aaa")
a[i,j]=cuhre(2, NCOMP,  integrand,lower=c(0,0),upper=c(1,1))$value
#sink()
#12*a[i,j]-3
}
}
data=12*a-3
#######################LABEL###########################
rownames(data)=c("0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9")
colnames(data)=c("0.01","0.8","1","10","100","400","700")
data
######################function###########################

spear=function(r,alpha){
a=0
ginv=function(t){
(exp(r*t)-1)/(exp(r)-1)
}
g=function(t){
log(((exp(r)-1)*t+1)^(1/r))
}
integrand <- function(arg) {
u <- arg[1]
v <- arg[2]
ff <-ginv(min(((g(u))^alpha) * g(v),((g(v))^alpha) * g(u))) 
return(ff)
}
NCOMP <- 1
#sink("aaa")
a=cuhre(2, NCOMP,  integrand,lower=c(0,0),upper=c(1,1))$value
#sink()
12*a-3
}


#########################
####Bloomqvist's beta####
#########################

######################Distortion functions#########################
r=10
alpha=0.9
ginv=function(t){
(exp(r*t)-1)/(exp(r)-1)
}
g=function(t){
log(((exp(r)-1)*t+1)^(1/r))
}
#####################Gini's Gamma##################
integrand=function(x) 4*(ginv(min(((g(x))^alpha) * g(1-x),((g(1-x))^alpha) * g(x)))-x+ginv(min(((g(x))^alpha) * g(x),((g(x))^alpha) * g(x))))
integrate(Vectorize(integrand), lower = 0, upper = 1)[[1]]
#####################################################


######################function########################
gini=function(alpha,r){
ginv=function(t){
(exp(r*t)-1)/(exp(r)-1)
}
g=function(t){
log(((exp(r)-1)*t+1)^(1/r))
}
integrand=function(x) 4*(ginv(min(((g(x))^alpha) * g(1-x),((g(1-x))^alpha) * g(x)))-x+ginv(min(((g(x))^alpha) * g(x),((g(x))^alpha) * g(x))))
integrate(Vectorize(integrand), lower = 0, upper = 1)[[1]]
}
######################################################


##########################LOOP##########################
pp=seq(0.1,0.9,0.1)
kk=c(0.01,0.8,1,10,100,400,700)
a=matrix(0,length(pp),length(kk))
for(j in 1:length(kk)){
ginv=function(t){
(exp((kk[j])*t)-1)/(exp(kk[j])-1)
}
g=function(t){
log(((exp(kk[j])-1)*t+1)^(1/(kk[j])))
}
for(i in 1:length(pp)){
integrand=function(x) 4*(ginv(min(((g(x))^pp[i]) * g(1-x),((g(1-x))^pp[i]) * g(x)))-x+ginv(min(((g(x))^pp[i]) * g(x),((g(x))^pp[i]) * g(x))))
a[i,j]=integrate(Vectorize(integrand), lower = 0, upper = 1)[[1]]
}
}
a
#######################LABEL###########################
rownames(a)=c("0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9")
colnames(a)=c("0.01","0.8","1","10","100","400","700")
a

####################
####Gini's gamma####
####################
######################Distortion functions#########################
r=10
alpha=0.05
ginv=function(t){
(exp(r*t)-1)/(exp(r)-1)
}
g=function(t){
log(((exp(r)-1)*t+1)^(1/r))
}
#####################Bloomqvist's beta##################
delta=function(x) ginv(min(((g(x))^alpha) * g(x),((g(x))^alpha) * g(x)))
4*delta(0.5)-1
#####################################################


######################function########################
bbloom=function(alpha,r){
ginv=function(t){
(exp(r*t)-1)/(exp(r)-1)
}
g=function(t){
log(((exp(r)-1)*t+1)^(1/r))
}
delta=function(x) ginv(min(((g(x))^alpha) * g(x),((g(x))^alpha) * g(x)))
4*delta(0.5)-1
}
######################################################


##########################LOOP##########################
pp=seq(0.1,0.9,0.1)
kk=c(0.01,0.8,1,10,100,400,700)
b=matrix(0,length(pp),length(kk))
for(j in 1:length(kk)){
ginv=function(t){
(exp((kk[j])*t)-1)/(exp(kk[j])-1)
}
g=function(t){
log(((exp(kk[j])-1)*t+1)^(1/(kk[j])))
}
for(i in 1:length(pp)){
delta=function(x) ginv(min(((g(x))^pp[i]) * g(x),((g(x))^pp[i]) * g(x)))
b[i,j]=4*delta(0.5)-1
}
}
b
#######################LABEL###########################
rownames(b)=c("0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9")
colnames(b)=c("0.01","0.8","1","10","100","400","700")
b

#########################
######Kendall's Tau######
#########################

rm(list=ls())
####################Distortion functions#########################
a=0.99
r=100
g=function(t){
(exp(r*t)-1)/(exp(r)-1)
}
ginv=function(t){
log(((exp(r)-1)*t+1)^(1/r))
}
gdiff=function(t){
(r*exp(r*t))/(exp(r)-1)
}
################int.1###################
h1=function(v,w) {
(a*g(v*w)*((g(v))^(2*a-1))*gdiff(v*w)*gdiff(v)*v)/((gdiff(ginv(g(v*w)*((g(v))^(a)))))^2)
}
integrand1 <- function(arg) {
w <- arg[1]
v <- arg[2]
ff <-h1(v,w) 
return(ff)
}
NCOMP <- 1
#sink("aaa")
a1=cuhre(2, NCOMP,  integrand1,lower=c(0,0),upper=c(1,1))$value
#sink("bbb")
################int.2###################
h2=function(v,w){
(a*((g(w*(1-v)+v))^(2*a-1))*g(v)*gdiff(w*(1-v)+v)*gdiff(v)*(1-v))/((gdiff(ginv(g(w*(1-v)+v)*((g(v))^(a)))))*(gdiff(ginv(((g(w*(1-v)+v))^(a))*g(v)))))
}
integrand2 <- function(arg) {
w <- arg[1]
v <- arg[2]
ff <-h2(v,w) 
return(ff)
}
NCOMP <- 1
#sink("aaa")
a2=cuhre(2, NCOMP,  integrand2,lower=c(0,0),upper=c(1,1))$value
#sink("bbb")
#####################final touch#################
1-4*(a1+a2)
```

### <p dir='rtl' align='right'>فصل دوم - جدول 2.3 </p>

```markdown
#p.180 book: Exponential Distribution_ Theory, Methods and Applications by 
x<-c(0.27,0.4,0.69,0.79,2.75,3.91,9.88,13.95,15.93,27.80,53.24,82.85,89.29,100.58,215.10)
y<-c(0.35,0.59,0.96,0.99,1.69,1.97,2.07,2.58,2.71,2.9,3.67,3.99,5.35,13.77,25.50)
x<-c(0, 0, 0, 0, 2, 3, 10, 14, 16, 28, 53, 83, 89, 100, 215)
y<-c(0, 0, 0, 1, 1, 2, 2, 2, 2, 3, 4, 4, 5, 14, 25)
sum(round(x,0)==round(y,0))
length(x)
length(y)
summary(x)
summary(y)
```

### <p dir='rtl' align='right'>فصل دوم - جدول 2.4 </p>

```markdown
#p.180 book: Exponential Distribution_ Theory, Methods and Applications by 
x<-c(0.27,0.4,0.69,0.79,2.75,3.91,9.88,13.95,15.93,27.80,53.24,82.85,89.29,100.58,215.10)
y<-c(0.35,0.59,0.96,0.99,1.69,1.97,2.07,2.58,2.71,2.9,3.67,3.99,5.35,13.77,25.50)
x<-c(0, 0, 0, 0, 2, 3, 10, 14, 16, 28, 53, 83, 89, 100, 215)
y<-c(0, 0, 0, 1, 1, 2, 2, 2, 2, 3, 4, 4, 5, 14, 25)
hist(x)
hist(y)
aa<-cbind(x,y)
library(MASS)
par.x<-fitdistr(x,"exponential")$estimate
par.y<-fitdistr(y,"exponential")$estimate
ks.test(x,pexp,par.x)
ks.test(y,pexp,par.y)
```

### <p dir='rtl' align='right'>فصل دوم - شکل 2.2 </p>

```markdown
install.packages("extraDistr")
library(extraDistr)#Truncated Poisson distibution 

rMO<-function(n,lam1,lam2,lam12){
  x1<-rexp(n,lam1)
  x2<-rexp(n,lam2)
  x12<-rexp(n,lam12)
  return(cbind(pmin(x1,x12),pmin(x2,x12)))
}
rMO(5,1,2,3)

#########################
rTMO.g1<-function(m,th,lam1,lam2,lam12){
x<-0
y<-0
for(i in 1:m){
N<-rtpois(1, th, a = 1, b = Inf)
aa<-rMO(N,lam1,lam2,lam12)
x[i]<-max(aa[,1])
y[i]<-max(aa[,2])
}
  return(cbind(x,y))
}
bb<-rTMO.g1(1000,3,1,2,9)
plot(pobs(bb))

par(mfrow=c(1,4))
aa<-rMO(1000,1,2,9)
plot(pobs(aa),col=6,xlab="u",ylab="v",main="MO model")
text(0.6,0.1,paste(expression(Tau),"=",round(cor(aa,method="kendall")[1,2],3)),cex=1.5)

bb<-rTMO.g1(1000,4,1,2,9)
plot(pobs(bb),col=3,xlab="u",ylab="v",main="Poisson-MO with r=4")
text(0.6,0.1,paste(expression(Tau),"=",round(cor(bb,method="kendall")[1,2],3)),cex=1.5)

bb<-rTMO.g1(1000,20,1,2,9)
plot(pobs(bb),col=3,xlab="u",ylab="v",main="Poisson-MO with r=20")
text(0.6,0.1,paste(expression(Tau),"=",round(cor(bb,method="kendall")[1,2],3)),cex=1.5)

bb<-rTMO.g1(1000,70,1,2,9)
plot(pobs(bb),col=3,xlab="u",ylab="v",main="Poisson-MO with r=70")
text(0.6,0.1,paste(expression(Tau),"=",round(cor(bb,method="kendall")[1,2],3)),cex=1.5)


```


### <p dir='rtl' align='right'>فصل دوم - شکل 2.3 </p>

```markdown
install.packages("extraDistr")
library(extraDistr)#Truncated Poisson distibution 

rMO<-function(n,lam1,lam2,lam12){
  x1<-rexp(n,lam1)
  x2<-rexp(n,lam2)
  x12<-rexp(n,lam12)
  return(cbind(pmin(x1,x12),pmin(x2,x12)))
}
rMO(5,1,2,3)

#########################
rTMO.g2<-function(m,th,lam1,lam2,lam12){
  x<-0
  y<-0
  for(i in 1:m){
    N<-rlgser(1, th)
    aa<-rMO(N,lam1,lam2,lam12)
    x[i]<-max(aa[,1])
    y[i]<-max(aa[,2])
  }
  return(cbind(x,y))
}
bb<-rTMO.g1(1000,3,1,2,9)
plot(pobs(bb))

par(mfrow=c(1,4))
aa<-rMO(1000,1,2,3)
plot(pobs(aa),col=6,xlab="u",ylab="v",main="MO model")
text(0.6,0.1,paste(expression(Tau),"=",round(cor(aa,method="kendall")[1,2],3)),cex=1.5)

bb<-rTMO.g2(1000,0.04,1,2,3)
plot(pobs(bb),col=3,xlab="u",ylab="v",main="Log-MO with r=0.04")
text(0.6,0.1,paste(expression(Tau),"=",round(cor(bb,method="kendall")[1,2],3)),cex=1.5)

bb<-rTMO.g2(1000,0.8,1,2,3)
plot(pobs(bb),col=3,xlab="u",ylab="v",main="Log-MO with r=0.8")
text(0.6,0.1,paste(expression(Tau),"=",round(cor(bb,method="kendall")[1,2],3)),cex=1.5)

bb<-rTMO.g2(1000,0.96,1,2,3)
plot(pobs(bb),col=3,xlab="u",ylab="v",main="Log-MO with r=0.96")
text(0.6,0.1,paste(expression(Tau),"=",round(cor(bb,method="kendall")[1,2],3)),cex=1.5)

```

### <p dir='rtl' align='right'>فصل دوم - شکل 2.4 </p>

```markdown
install.packages("extraDistr")
library(extraDistr)#Truncated Poisson distibution 

rMO<-function(n,lam1,lam2,lam12){
  x1<-rexp(n,lam1)
  x2<-rexp(n,lam2)
  x12<-rexp(n,lam12)
  return(cbind(pmin(x1,x12),pmin(x2,x12)))
}
rMO(5,1,2,3)

#########################
rTMO.g1<-function(m,th,lam1,lam2,lam12){
  x<-0
  y<-0
  for(i in 1:m){
    N<-rtpois(1, th, a = 1, b = Inf)
    aa<-rMO(N,lam1,lam2,lam12)
    x[i]<-max(aa[,1])
    y[i]<-max(aa[,2])
  }
  return(cbind(x,y))
}
bb<-rTMO.g1(1000,3,1,2,9)
plot(pobs(bb))

par(mfrow=c(1,4))
aa<-rMO(1000,1,1,9)
plot(pobs(aa),col=6,xlab="u",ylab="v",main="CA model")
text(0.6,0.1,paste(expression(Tau),"=",round(cor(aa,method="kendall")[1,2],3)),cex=1.5)

bb<-rTMO.g1(1000,10,1,1,9)
plot(pobs(bb),col=3,xlab="u",ylab="v",main="Poisson-CA with r=10")
text(0.6,0.1,paste(expression(Tau),"=",round(cor(bb,method="kendall")[1,2],3)),cex=1.5)

bb<-rTMO.g1(1000,50,1,1,9)
plot(pobs(bb),col=3,xlab="u",ylab="v",main="Poisson-CA with r=50")
text(0.6,0.1,paste(expression(Tau),"=",round(cor(bb,method="kendall")[1,2],3)),cex=1.5)

bb<-rTMO.g1(1000,900,1,1,9)
plot(pobs(bb),col=3,xlab="u",ylab="v",main="Poisson-CA with r=900")
text(0.6,0.1,paste(expression(Tau),"=",round(cor(bb,method="kendall")[1,2],3)),cex=1.5)

```

### <p dir='rtl' align='right'>فصل دوم - شکل 2.5 </p>

```markdown
install.packages("extraDistr")
library(extraDistr)#Truncated Poisson distibution 

rMO<-function(n,lam1,lam2,lam12){
  x1<-rexp(n,lam1)
  x2<-rexp(n,lam2)
  x12<-rexp(n,lam12)
  return(cbind(pmin(x1,x12),pmin(x2,x12)))
}
rMO(5,1,2,3)

#**********************
rTMO.g1<-function(m,th,lam1,lam2,lam12){
  x<-0
  y<-0
  for(i in 1:m){
    N<-rtpois(1, th, a = 1, b = Inf)
    aa<-rMO(N,lam1,lam2,lam12)
    x[i]<-max(aa[,1])
    y[i]<-max(aa[,2])
  }
  return(cbind(x,y))
}

par(mfrow=c(1,4))
aa<-rMO(1000,0.01,1,9)
plot(pobs(aa),col=6,xlab="u",ylab="v",main="MO model")
text(0.6,0.1,paste(expression(Tau),"=",round(cor(aa,method="kendall")[1,2],3)),cex=1.5)

bb<-rTMO.g1(1000,10,0.01,1,9)
plot(pobs(bb),col=3,xlab="u",ylab="v",main="Poisson-MO with r=10")
text(0.6,0.1,paste(expression(Tau),"=",round(cor(bb,method="kendall")[1,2],3)),cex=1.5)

bb<-rTMO.g1(1000,50,0.01,1,9)
plot(pobs(bb),col=3,xlab="u",ylab="v",main="Poisson-MO with r=50")
text(0.6,0.1,paste(expression(Tau),"=",round(cor(bb,method="kendall")[1,2],3)),cex=1.5)

bb<-rTMO.g1(1000,900,0.01,1,9)
plot(pobs(bb),col=3,xlab="u",ylab="v",main="Poisson-MO with r=900")
text(0.6,0.1,paste(expression(Tau),"=",round(cor(bb,method="kendall")[1,2],3)),cex=1.5)

```

### <p dir='rtl' align='right'>فصل دوم - شکل 2.6 </p>

```markdown
install.packages("extraDistr")
library(extraDistr)#Truncated Poisson distibution 

rMO<-function(n,lam1,lam2,lam12){
  x1<-rexp(n,lam1)
  x2<-rexp(n,lam2)
  x12<-rexp(n,lam12)
  return(cbind(pmin(x1,x12),pmin(x2,x12)))
}
rMO(5,1,2,3)

#**********************
rTMO.g2<-function(m,th,lam1,lam2,lam12){
  x<-0
  y<-0
  for(i in 1:m){
    N<-rlgser(1, th)
    aa<-rMO(N,lam1,lam2,lam12)
    x[i]<-max(aa[,1])
    y[i]<-max(aa[,2])
  }
  return(cbind(x,y))
}
bb<-rTMO.g1(1000,3,1,2,9)
plot(pobs(bb))

par(mfrow=c(1,4))
aa<-rMO(1000,0.01,2,3)
plot(pobs(aa),col=6,xlab="u",ylab="v",main="MO model")
text(0.6,0.1,paste(expression(Tau),"=",round(cor(aa,method="kendall")[1,2],3)),cex=1.5)

bb<-rTMO.g2(1000,0.04,0.01,2,3)
plot(pobs(bb),col=3,xlab="u",ylab="v",main="Log-MO with r=0.04")
text(0.6,0.1,paste(expression(Tau),"=",round(cor(bb,method="kendall")[1,2],3)),cex=1.5)

bb<-rTMO.g2(1000,0.8,0.01,2,3)
plot(pobs(bb),col=3,xlab="u",ylab="v",main="Log-MO with r=0.8")
text(0.6,0.1,paste(expression(Tau),"=",round(cor(bb,method="kendall")[1,2],3)),cex=1.5)

bb<-rTMO.g2(1000,0.96,0.01,2,3)
plot(pobs(bb),col=3,xlab="u",ylab="v",main="Log-MO with r=0.96")
text(0.6,0.1,paste(expression(Tau),"=",round(cor(bb,method="kendall")[1,2],3)),cex=1.5)

```

### <p dir='rtl' align='right'>فصل دوم - شکل 2.7 </p>

```markdown

x<-0.2
y<-0.5
tt<-0.5
p1<-1
p2<-2
p3<-3

TMO.g1.density<-function(t,s,th,lam1,lam2,lam3){
if(t<s) AA=(((lam1*th*(lam2+lam3))/(exp(th)-1))*exp(-lam1*t-(lam2+lam3)*s)*exp(th*exp(-lam1*t-(lam2+lam3)*s))*(1+th*exp(-lam1*t-(lam2+lam3)*s)))
  if(t>s) AA=(((lam2*th*(lam1+lam3))/(exp(th)-1))*exp(-(lam1+lam3)*t-lam2*s)*exp(th*exp(-(lam1+lam3)*t-lam2*s))*(1+th*exp(-(lam1+lam3)*t-lam2*s)))
  if(t==s) AA=((th*lam3)/(exp(th)-1)*exp(-(lam1+lam2+lam3)*s)*exp(th*exp(-(lam1+lam2+lam3)*s)))
  return(AA)
}
g1.density(x,y,tt,p1,p2,p3)

minusloglikelihood<-function(dd){
  th=dd[1]
  lam1=dd[2]
  lam2=dd[3]
  lam3=dd[4]
  ff<-Vectorize(TMO.g1.density)
  AAA=-sum(log(0.01+ff(aa[,1],aa[,2],th,lam1,lam2,lam3)))
  if(th<0.01 | lam1<0.01 | lam2<0.01 | lam3<0.01) AAA=1e+100
  #if(th<L1 | th>U1 | lam1<L2 | lam1>U2 | lam2<L3 | lam2>U3 | lam3<L4 | lam3>U4) AAA=-1e+100
  return(AAA)
}
minusloglikelihood(c(1,1,2,3))

 mse.TMO.g1<-function(n,th,lam1,lam2,lam3){
        L1<-th-0.8
        U1<-th+0.8
        L2<-lam1-0.8
        U2<-lam1+0.8
        L3<-lam2-0.8
        U3<-lam2+0.8
        L4<-lam3-0.8
        U4<-lam3+0.8
    aa<-rTMO.g1(n,th,lam1,lam2,lam3)
    minusloglikelihood<-function(th,lam1,lam2,lam3){
    #th<-d[1];lam1<-d[2];lam2<-d[3];lam3<-d[4]
    ff<-Vectorize(TMO.g1.density)
    AAA=-sum(log(10+ff(aa[,1],aa[,2],th,lam1,lam2,lam3)))
    #if(th<0 | lam1<0 | lam2<0 | lam3<0) AAA=1e+100
    if(th<L1 | th>U1 | lam1<L2 | lam1>U2 | lam2<L3 | lam2>U3 | lam3<L4 | lam3>U4) AAA=1e+100
    return(AAA)
    }
    #minusloglikelihood(1,1,2,3)
    #minusloglikelihood<-Vectorize(minusloglikelihood,c("th","lam1","lam2","lam3"))
    library(bbmle)
    fit<-mle2(minuslog=minusloglikelihood, start=list(th=th,lam1=lam1,lam2=lam2,lam3=lam3),method="BFGS")@coef 
    mse<-cbind( (fit[1]-th)^2 , (fit[2]-lam1)^2 , (fit[3]-lam2)^2 , (fit[4]-lam3)^2 )
    return(mse)
    }
    library(tictoc) #install.packages("tictoc")
    tic()
    a<-replicate(3,mse.TMO.g1(25,1,1,2,3))
    mean(a[,1,])
    mean(a[,2,])
    mean(a[,3,])
    mean(a[,4,])
    toc()
    
    #####mse##### PARALLEL
    library(foreach) #install.packages("foreach")
    library(doParallel) #install.packages("doParallel")
    library(parallel) #install.packages("parallel")
    library(tictoc) #install.packages("tictoc")

    
    registerDoParallel(makeCluster(detectCores()))
    tic()
    A=foreach(j=1:1000,.combine='rbind') %dopar% mse.TMO.g1(50,1,1,2,3)
    toc()
    stopImplicitCluster()
    stopCluster(makeCluster(detectCores()))
    colMeans(A)
    
    A<-0
    n<-0
    mse.th<-0
    mse.lam1<-0
    mse.lam2<-0
    mse.lam3<-0
    
    
    registerDoParallel(makeCluster(detectCores()))
    tic()
    A=foreach(j=1:1000,.combine='rbind') %dopar% mse.TMO.g1(10,1,1,2,3)
    toc()
    stopImplicitCluster()
    stopCluster(makeCluster(detectCores()))
    n[1]<-10
    mse.th[1]<-colMeans(A)[1]
    mse.lam1[1]<-colMeans(A)[2]
    mse.lam2[1]<-colMeans(A)[3]
    mse.lam3[1]<-colMeans(A)[4]
    
    
    registerDoParallel(makeCluster(detectCores()))
    tic()
    A=foreach(j=1:1000,.combine='rbind') %dopar% mse.TMO.g1(20,1,1,2,3)
    toc()
    stopImplicitCluster()
    stopCluster(makeCluster(detectCores()))
    n[2]<-20
    mse.th[2]<-colMeans(A)[1]
    mse.lam1[2]<-colMeans(A)[2]
    mse.lam2[2]<-colMeans(A)[3]
    mse.lam3[2]<-colMeans(A)[4]

    
    registerDoParallel(makeCluster(detectCores()))
    tic()
    A=foreach(j=1:1000,.combine='rbind') %dopar% mse.TMO.g1(35,1,1,2,3)
    toc()
    stopImplicitCluster()
    stopCluster(makeCluster(detectCores()))
    n[3]<-35
    mse.th[3]<-colMeans(A)[1]
    mse.lam1[3]<-colMeans(A)[2]
    mse.lam2[3]<-colMeans(A)[3]
    mse.lam3[3]<-colMeans(A)[4]
    
    
    registerDoParallel(makeCluster(detectCores()))
    tic()
    A=foreach(j=1:1000,.combine='rbind') %dopar% mse.TMO.g1(50,1,1,2,3)
    toc()
    stopImplicitCluster()
    stopCluster(makeCluster(detectCores()))
    n[4]<-50
    mse.th[4]<-colMeans(A)[1]
    mse.lam1[4]<-colMeans(A)[2]
    mse.lam2[4]<-colMeans(A)[3]
    mse.lam3[4]<-colMeans(A)[4]
    
    
    registerDoParallel(makeCluster(detectCores()))
    tic()
    A=foreach(j=1:1000,.combine='rbind') %dopar% mse.TMO.g1(100,1,1,2,3)
    toc()
    stopImplicitCluster()
    stopCluster(makeCluster(detectCores()))
    n[5]<-100
    mse.th[5]<-colMeans(A)[1]
    mse.lam1[5]<-colMeans(A)[2]
    mse.lam2[5]<-colMeans(A)[3]
    mse.lam3[5]<-colMeans(A)[4]

  n<-c(10,20,35,50,100)
    mse.th<-c(0.3591186,0.3584046,0.3565793,0.3257254,0.3214427)
  mse.lam1<-c(0.3382835,0.3130130,0.2734315,0.2610089,0.2203300)
  mse.lam2<-c(0.5109399,0.4422954,0.3976469,0.3571630,0.3288546) 
  mse.lam3<-c(0.2359293,0.1976644,0.1878772,0.1821945,0.1765632)

cbind(n,mse.th,mse.lam1,mse.lam2,mse.lam3)

  par(mfrow=c(1,4))
    plot(n,mse.th,type="l",col=2,xlab="sample size",ylab="MSE",main=expression(paste("MSE of ",hat(theta))),lwd=2)
    plot(n,mse.lam1,type="l",col=4,xlab="sample size",ylab="MSE",main=expression(paste("MSE of ",hat(lambda[1]))),lwd=2)
    plot(n,mse.lam2,type="l",col=6,xlab="sample size",ylab="MSE",main=expression(paste("MSE of ",hat(lambda[2]))),lwd=2)
    plot(n,mse.lam3,type="l",col=9,xlab="sample size",ylab="MSE",main=expression(paste("MSE of ",hat(lambda[3]))),lwd=2)Rplot
    par(mfrow=c(1,1))

```

### <p dir='rtl' align='right'>فصل دوم - شکل 2.8 </p>

```markdown
#p.180 book: Exponential Distribution_ Theory, Methods and Applications by 
x<-c(0.27,0.4,0.69,0.79,2.75,3.91,9.88,13.95,15.93,27.80,53.24,82.85,89.29,100.58,215.10)
y<-c(0.35,0.59,0.96,0.99,1.69,1.97,2.07,2.58,2.71,2.9,3.67,3.99,5.35,13.77,25.50)
x<-c(0, 0, 0, 0, 2, 3, 10, 14, 16, 28, 53, 83, 89, 100, 215)
y<-c(0, 0, 0, 1, 1, 2, 2, 2, 2, 3, 4, 4, 5, 14, 25)

plot(x,y,col="red")
```

### <p dir='rtl' align='right'>فصل سوم - شکل 3.2 </p>

```markdown
NA
```

### <p dir='rtl' align='right'>فصل سوم - شکل 3.3 </p>

```markdown
NA
```

### <p dir='rtl' align='right'>فصل سوم - شکل 3.4 </p>

```markdown
NA
```

### <p dir='rtl' align='right'>فصل سوم - شکل 3.5 </p>

```markdown
NA
```

### <p dir='rtl' align='right'>فصل سوم - شکل 3.6 </p>

```markdown
NA
```

### <p dir='rtl' align='right'>فصل سوم - شکل 3.7 </p>

```markdown
NA
```

### <p dir='rtl' align='right'>فصل سوم - شکل 3.8 </p>

```markdown
NA
```

### <p dir='rtl' align='right'>فصل سوم - شکل 3.9 </p>

```markdown
NA
```

### <p dir='rtl' align='right'>فصل سوم - جدول 3.1 </p>

```markdown
NA
```

### <p dir='rtl' align='right'>فصل سوم - جدول 3.2 </p>

```markdown
NA
```

### <p dir='rtl' align='right'>فصل سوم - جدول 3.3 </p>

```markdown
NA
```

### <p dir='rtl' align='right'>فصل چهارم - شکل 4.1 </p>

```markdown
NA
```

### <p dir='rtl' align='right'>فصل چهارم - شکل 4.2 </p>

```markdown
NA
```

### <p dir='rtl' align='right'>فصل چهارم - شکل 4.3 </p>

```markdown
NA
```

### <p dir='rtl' align='right'>فصل چهارم - شکل 4.4 </p>

```markdown
NA
```

### <p dir='rtl' align='right'>فصل چهارم - شکل 4.5 </p>

```markdown
NA
```

### <p dir='rtl' align='right'>فصل چهارم - شکل 4.6 </p>

```markdown
NA
```

### <p dir='rtl' align='right'>فصل چهارم - شکل 4.7 </p>

```markdown
NA
```

### <p dir='rtl' align='right'>فصل چهارم - جدول 4.1 </p>

```markdown
NA
```


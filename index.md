### <p dir='rtl' align='right'>خوشامدگویی</p>

 <p dir='rtl' align='right'>در این بخش کد و برنامه های کامپیوتری رساله دکتری با نام زیر قرار گرفته است:</p>

<div align="center">
  "ساخت توابع مفصل جدید با استفاده از روش تغییر شکل"
</div>



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














### <p dir='rtl' align='right'>فصل سوم</p>


### <p dir='rtl' align='right'>فصل چهارم</p>



```markdown
Syntax highlighted code block

# Header 1
## Header 2
### Header 3

- Bulleted
- List

1. Numbered
2. List

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```

For more details see [GitHub Flavored Markdown](https://guides.github.com/features/mastering-markdown/).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/hamb8066/phdthesis/settings). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://docs.github.com/categories/github-pages-basics/) or [contact support](https://github.com/contact) and we’ll help you sort it out.

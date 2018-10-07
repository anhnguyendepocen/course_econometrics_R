## Figure 1

library(tidyverse)
library(quantmod)
library(data.table)
library(viridis)

my.names <- data.table(var=c("CPILFESL","GDPC1"),
                       name=c("CPI","GDP"))
df= getSymbols('CPILFESL',src='FRED', auto.assign=F) 
df = data.frame(date=time(df), coredata(df))
df.gdp_us =getSymbols('GDPC1',src='FRED', auto.assign=F) 
df.gdp_us = data.frame(date=time(df.gdp_us), coredata(df.gdp_us))
df<-merge(df,df.gdp_us,by="date")
dt<-data.table(df)
dt %>% gather(var,value,-date) %>% data.table() -> dt2
dt2<-merge(dt2,my.names,by="var")

# Base 100 pour la date t = 1 

dt2=dt2[,id:=1:.N, by=var]  
dt2=dt2[,var0:=100*value/sum(ifelse(id==1,value,0)),by=var] 

ggplot(data=dt2,aes(x=date,y=var0,color=name,linetype=name))+
  geom_line(size=1.1)+
  scale_y_log10(breaks=c(100,200,400,800))+
  theme_minimal()+theme(plot.caption=element_text(hjust=0),
                        legend.position="top")+
  guides(linetype=F)+
  scale_color_viridis(name="Variable",discrete=T,end=0.8)

# Figure 2

rm(list = ls())

my.names <- data.table(var=c("MICH","A191RL1Q225SBEA"),
                       name=c("Inflation_Expectation","GDP Change"))
df= getSymbols('MICH',src='FRED', auto.assign=F) 
df = data.frame(date=time(df), coredata(df))
df.gdp_us_c =getSymbols('A191RL1Q225SBEA',src='FRED', auto.assign=F) 
df.gdp_us_c = data.frame(date=time(df.gdp_us_c), coredata(df.gdp_us_c))

df<-merge(df,df.gdp_us_c,by="date")
dt<-data.table(df)
dt %>% gather(var,value,-date) %>% data.table() -> dt2
dt2<-merge(dt2,my.names,by="var")

ggplot(data=dt2,aes(x=date,y=value,color=name,linetype=name))+
  geom_line(size=1.1)+
  theme_minimal()+theme(plot.caption=element_text(hjust=0),
                        legend.position="top")+
  guides(linetype=F)+
  scale_color_viridis(name="Variable",discrete=T,end=0.8)

# Chocs non persistents processus TS

rm(list = ls())

T = 100
e = rnorm(T)
y = rep(NA,T)
y[1] = 0

for (t in 2:50){
  y[t] = 0.2*t + 0.8 *y[t-1] + e[t]
}
for (t in 51:T){
  y[t] = 0.2*t + 0.8 *y[t-1] 
}

plot(y,type='l')

# Figure 3 - Marche aléatoire

rm(list = ls())

T=200; 
e = rnorm(T)
y = rep(NA,T)
y[1] = 0
for (t in 2:T){
  y[t] = e[t]+ y[t-1] 
}

plot(y,type='l')


# Petite expérience sur les régressions fallacieuses

rm(list = ls())

N = 1000
T=500 
t_value = rep(NA,N)
p_value = rep(NA,N)
for (i in 1:N){
  e = rnorm(T)
  u = rnorm(T)
  y = rep(NA,T)
  x = rep(NA,T)
  y[1] = 0
  x[1] = 0
  for (t in 2:T){
    y[t] = e[t]+ y[t-1] 
    x[t] = x[t-1] + u[t]
  }
  results_sum <- summary(lm(y~x))
  results_coeff <- results_sum$coefficients
  beta.estimate <- results_coeff["x", "Estimate"] 
  std.error <- results_coeff["x", "Std. Error"]  
  t_value[i] <- beta.estimate/std.error  
  p_value[i] <- 2*pt(-abs(t_value[i]), df=T-1)  
}
plot(density(t_value))
abline(v=1.96,col="blue")
abline(v=-1.96,col="blue")
sum(p_value<0.05)/N



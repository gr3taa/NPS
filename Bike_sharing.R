
day <- read.csv("Dataset/day.csv")
hour <- read.csv("Dataset/hour.csv")
library(dplyr)
library(tidyr)
#controllo quante osservazioni orarie per ogni giorno
blocchi <- rle(hour$dteday)
blocchi$values   
blocchi$lengths

#Trasforma `dati_orari` per avere una colonna per ogni ora
hour_wide <- hour %>%
  pivot_wider(
    names_from = hr, 
    values_from = cnt,
    names_prefix = "cnt_hour_"
  )
#dataset che compatta le righe
hour_wide_sommato <- hour_wide %>%
  group_by(dteday) %>%
  summarise(across(starts_with("cnt_hour"), sum, na.rm = TRUE))
#Unisci `dati_giornalieri` e `dati_orari_wide` per data
day <- day %>%
  left_join(hour_wide_sommato, by = "dteday")
#grafici ---------------------------------
matplot(t(day[,17:40]), type='l')

mese_anno <- paste(day$yr, day$mnth, sep = "_")
rle <- rle(mese_anno)
len <- rle$lengths
color_vector <- rainbow(24)
col <- rep (color_vector, len)
matplot(t(day[,17:40]), type='l', col=col)

col_holyday <- ifelse(day$holiday ==1, 'red', 'blue')
matplot(t(day[,17:40]), type='l', col=col_holyday)

col_working <- ifelse(day$workingday==1, 'red', 'blue')
matplot(t(day[,17:40]), type='l', col=col_working)
matplot(t(day[, 17:40]), 
        type = 'l', 
        col = ifelse(day$workingday == 1, 'blue', 
                     ifelse(day$holiday == 1, 'red', 'green')))

col_weather <- rainbow(4)[day$weathersit]
matplot(t(day[,17:40]), type='l', col=col_weather)

prop_casual <- day$casual/day$cnt
plot(prop_casual, col=col_working, pch=16)
shapiro.test(prop_casual[col_working=='blue'])

#Permutational ANOVA ----------------------------------
attach(hour)
B<-1000
weathersit<-as.factor(weathersit)
fit <- aov(cnt~weathersit)
g<-nlevels(weathersit)
n<-dim(hour)[1]
summary(fit)
plot(weathersit, cnt, xlab='weathersit',col=rainbow(g),main='Original Data')
T0 <- summary(fit)[[1]][1,4]  # extract the test statistic
T_stat <- numeric(B) 
for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  cnt_perm <- cnt[permutation]
  fit_perm <- aov(cnt_perm ~ weathersit)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)
p_val <- sum(T_stat>=T0)/B
p_val#0

#Multiple regression ----------------------------------------
##Hours----
result<-lm(cnt ~ hum +temp + windspeed + hr)
summary(result)
#Let’s start with a global test
T0_glob <- summary(result)$adj.r.squared
T_H0glob <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  
  Y.perm.glob <- cnt[permutation]
  T_H0glob[perm] <- summary(lm(Y.perm.glob ~ hum +temp + windspeed +hr))$adj.r.squared
}
sum(T_H0glob>=T0_glob)/B#0

T0_x1 <- abs(summary(result)$adj.r.squared)
T0_x2 <- abs(summary(result)$adj.r.squared)
T0_x3 <- abs(summary(result)$adj.r.squared)
T0_x4 <- abs(summary(result)$adj.r.squared)

regr.H01 <- lm(cnt~temp+windspeed+hr)
residuals.H01 <- regr.H01$residuals

regr.H02 <- lm(cnt~hum+windspeed+hr)
residuals.H02 <- regr.H02$residuals

regr.H03 <- lm(cnt~hum+temp+hr)
residuals.H03 <- regr.H03$residuals

regr.H04 <- lm(cnt~hum+temp+windspeed)
residuals.H04 <- regr.H04$residuals

T_H01 <- T_H02 <- T_H03 <- T_H04 <- numeric(B)

for(perm in 1:B){
  permutation <- sample(n)
  
  residuals.H01.perm <- residuals.H01[permutation]
  Y.perm.H01 <- regr.H01$fitted + residuals.H01.perm
  T_H01[perm] <- abs(summary(lm(Y.perm.H01 ~ hum+temp+windspeed+hr))$adj.r.squared)
  
  residuals.H02.perm <- residuals.H02[permutation]
  Y.perm.H02 <- regr.H02$fitted + residuals.H02.perm
  T_H02[perm] <- abs(summary(lm(Y.perm.H02 ~ hum+temp+windspeed+hr))$adj.r.squared)
  
  residuals.H03.perm <- residuals.H03[permutation]
  Y.perm.H03 <- regr.H03$fitted + residuals.H03.perm
  T_H03[perm] <- abs(summary(lm(Y.perm.H03 ~ hum+temp+windspeed+hr))$adj.r.squared)
  
  residuals.H04.perm <- residuals.H04[permutation]
  Y.perm.H04 <- regr.H04$fitted + residuals.H04.perm
  T_H04[perm] <- abs(summary(lm(Y.perm.H04 ~ hum+temp+windspeed+hr))$adj.r.squared)
  
}

sum(T_H01>=T0_x1)/B#0
sum(T_H02>=T0_x2)/B#0
sum(T_H03>=T0_x3)/B#0.5
sum(T_H04>=T0_x4)/B#0

detach(hour)
##Day------
attach(day)
n<- dim(day)[1]
result<-lm(cnt ~ hum +temp + windspeed)
summary(result)
shapiro.test(result$residuals)$p
qqnorm(result$residuals)
qqline(result$residuals)
#Let’s start with a global test
T0_glob <- summary(result)$adj.r.squared
T_H0glob <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  
  Y.perm.glob <- cnt[permutation]
  T_H0glob[perm] <- summary(lm(Y.perm.glob ~ hum +temp + windspeed))$adj.r.squared
}
sum(T_H0glob>=T0_glob)/B#0

T0_x1 <- abs(summary(result)$adj.r.squared)
T0_x2 <- abs(summary(result)$adj.r.squared)
T0_x3 <- abs(summary(result)$adj.r.squared)

regr.H01 <- lm(cnt~temp+windspeed)
residuals.H01 <- regr.H01$residuals

regr.H02 <- lm(cnt~hum+windspeed)
residuals.H02 <- regr.H02$residuals

regr.H03 <- lm(cnt~hum+temp)
residuals.H03 <- regr.H03$residuals

T_H01 <- T_H02 <- T_H03 <- numeric(B)

for(perm in 1:B){
  permutation <- sample(n)
  
  residuals.H01.perm <- residuals.H01[permutation]
  Y.perm.H01 <- regr.H01$fitted + residuals.H01.perm
  T_H01[perm] <- abs(summary(lm(Y.perm.H01 ~ hum+temp+windspeed))$adj.r.squared)
  
  residuals.H02.perm <- residuals.H02[permutation]
  Y.perm.H02 <- regr.H02$fitted + residuals.H02.perm
  T_H02[perm] <- abs(summary(lm(Y.perm.H02 ~ hum+temp+windspeed))$adj.r.squared)
  
  residuals.H03.perm <- residuals.H03[permutation]
  Y.perm.H03 <- regr.H03$fitted + residuals.H03.perm
  T_H03[perm] <- abs(summary(lm(Y.perm.H03 ~ hum+temp+windspeed))$adj.r.squared)
}

sum(T_H01>=T0_x1)/B#0.01
sum(T_H02>=T0_x2)/B#0
sum(T_H03>=T0_x3)/B#0.06

#fda working----------
w1_fd=subset(fd, day$weathersit==1)
plot(w1_fd)
w2_fd=subset(fd, day$weathersit==2)
plot(w2_fd)
w3_fd=subset(fd, day$weathersit==3)
plot(w3_fd)

weathersit <- day$weathersit
ITP.result <- ITPaovbspline(day_hour ~ weathersit, B=1000,nknots=20,order=3,method="responses") #ITPlmbspline per la regressione
summary(ITP.result)
plot(ITP.result,plot.adjpval = TRUE, xrange=c(0,23))

#fda weather --------------------
work_fd=subset(fd, day$workingday==1)
plot(work_fd)
we_fd=subset(fd, day$workingday==0)
plot(we_fd)
meandiff=median_fData(work_fd,type='MBD')-median_fData(we_fd,type='MBD')
plot(meandiff)
T0=(sum(abs(meandiff$values)))
pool_fd=append_fData(work_fd,we_fd)
n_m <- 500
T0_perm = numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  pool_perm=pool_fd[permutation,]
  perm_m = pool_perm[1:n_m,] 
  perm_f = pool_perm[(n_m+1):n,] 
  med_m = median_fData(perm_m,type='MBD')
  med_f = median_fData(perm_f,type='MBD')
  meandiff=fData (0:23, med_m$values - med_f$values) 
  T0_perm[perm]=sum(abs(meandiff$values))
}
sum(T0_perm >=T0)/B

library(fdatest)
groups <- day$workingday
day_hour <- as.matrix(day[,17:40])
ITP.result <- ITPaovbspline(day_hour ~ groups, B=1000,nknots=20,order=3,method="responses") #ITPlmbspline per la regressione
summary(ITP.result)
plot(ITP.result,plot.adjpval = TRUE, xrange=c(0,23))
# working day vs non working day----------------
a<-subset(day,day$workingday==1)
matplot(t(a[,17:40]), type='l')
b<-subset(day,day$workingday==0)
matplot(t(b[,17:40]), type='l')
##distribuzione della variabile cnt-------------------------
x<- a$cnt
y<-b$cnt
perm_t_test=function(x,y,iter=1e3){
  T0=abs(mean(x)-mean(y))  # define the test statistic
  T_stat=numeric(iter) # a vector to store the values of each iteration
  x_pooled=c(x,y) # pooled sample
  n=length(x_pooled)
  n1=length(x)
  print(sprintf("Using %s iterations to estimate the permutational distribution. There are actually %s possible permutations", iter, factorial(n)))
  for(perm in 1:iter){ # loop for conditional MC
    # permutation:
    permutation <- sample(1:n)
    x_perm <- x_pooled[permutation]
    x1_perm <- x_perm[1:n1]
    x2_perm <- x_perm[(n1+1):n]
    # test statistic:
    T_stat[perm] <- abs(mean(x1_perm) - mean(x2_perm))
  }
  # p-value
  p_val <- sum(T_stat>=T0)/iter
  return(p_val)
}
p.value <- perm_t_test(x,y,iter=1e3) #0.093

#outlier detection with roahd---------------
fd<-fData(seq(0,23),(day[,17:40]))
plot(fd)
invisible(fbplot(fd, main="Magnitude outliers"))
invisible(outliergram(fd))
out_shape <- outliergram(fd, display = FALSE)
out_shape$ID_outliers

#Grafici casual user-------------
col_working <- ifelse(day$workingday==1, 'red', 'blue')
plot(day$casual)
plot(day$registered)
col_holyday <- ifelse(day$holiday ==1, 'red', 'blue')
matplot(t(day1[,41:64]), type='l', col=col_holyday)

col_working <- ifelse(day$workingday==1, 'red', 'blue')
matplot(t(day1[,41:64]), type='l', col=col_working)

col_weekday <- ifelse(day$weekday==1, 'red', 'blue')
#dataset casual e registered----------------------
#Trasforma `dati_orari` per avere una colonna per ogni ora
hour_wide_register <- hour %>%
  pivot_wider(
    names_from = hr, 
    values_from = registered,
    names_prefix = "cnt_hour_"
  )
hour_wide_casual <- hour %>%
  pivot_wider(
    names_from = hr, 
    values_from = casual,
    names_prefix = "cnt_hour_"
  )
#dataset che compatta le righe
hour_wide_sommato_reg <- hour_wide_register %>%
  group_by(dteday) %>%
  summarise(across(starts_with("cnt_hour_"), sum, na.rm = TRUE))
hour_wide_sommato_cas <- hour_wide_casual %>%
  group_by(dteday) %>%
  summarise(across(starts_with("cnt_hour_"), sum, na.rm = TRUE))
day1 <- day %>%
  left_join(hour_wide_sommato_cas, by = "dteday")
day2<- day %>%
  left_join(hour_wide_sommato_reg, by = "dteday")
matplot(t(hour_wide_sommato_reg[,2:25]), type='l')
matplot(t(hour_wide_sommato_cas[,2:25]), type='l')

#Permutational ANOVA ---------------------
attach(hour)
B<-1000
weathersit<-as.factor(weathersit)
fit <- aov(casual~weathersit)
g<-nlevels(weathersit)
n<-dim(hour)[1]
summary(fit)
plot(weathersit, casual, xlab='weathersit',col=rainbow(g),main='Original Data')
T0 <- summary(fit)[[1]][1,4]  # extract the test statistic
T_stat <- numeric(B) 
for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  cnt_perm <- casual[permutation]
  fit_perm <- aov(cnt_perm ~ weathersit)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)
p_val <- sum(T_stat>=T0)/B
p_val#0


#sign paired test-----
##Casual vs Registered----------------
t1 = day$registered
t2 = day$casual

wilcox.test(x=t1, y=t2, paired=T, alternative = "greater")
# p-value < 2.2e-16
# alternative hypothesis: true location shift is greater than 0
Y <- t1-t2
n <- length(Y)
ranks <- rank(abs(Y))
W.plus  <- sum(ranks[Y > 0])
W.minus <- sum(ranks[Y < 0])
set.seed(24021979)
B<-1000
W.sim <- numeric(B)
for (k in 1:B)
{
  ranks.temp <- sample(1:n)
  signs.temp <- 2*rbinom(n, 1, 0.5) - 1
  W.temp <- sum(signs.temp*ranks.temp)
  W.sim[k] <- W.temp
}

hist(W.sim, xlim=c(-n*(n+1)/2, n*(n+1)/2), breaks = 50)
abline(v = W.plus, col='red')
abline(v = 0, lwd=3)
#Two multivariate populations test (Permutational test)---------------------
##Casual vs Registered ---------------------
t1 = day1[,41:64]
t2 = day2[,41:64]

t1.mean = colMeans(t1)
t2.mean = colMeans(t2)

matplot(seq(0,23),
        t(rbind(t1.mean,t2.mean)), 
        type='l', col=c(1,2), lty=1,
        main="Sample multivariate means")

n1 = dim(t1)[1]
n2 = dim(t2)[1]
n  = n1 + n2

T20 = as.numeric(t(t1.mean-t2.mean) %*% (t1.mean-t2.mean))  # matrix (vector) product
#T20 <- norm(t1.mean-t2.mean) same as before
T20
B<-1000
T2 = numeric(B)
set.seed(19)
for(perm in 1:B){
  # Random permutation of indexes
  # When we apply permutations in a multivariate case, we keep the units together
  # i.e., we only permute the rows of the data matrix
  t_pooled = rbind(t1,t2)
  permutation = sample(n)
  t_perm = t_pooled[permutation,]
  t1_perm = t_perm[1:n1,]
  t2_perm = t_perm[(n1+1):n,]
  
  # Evaluation of the test statistic on permuted data
  t1.mean_perm = colMeans(t1_perm)
  t2.mean_perm = colMeans(t2_perm)
  T2[perm]  = t(t1.mean_perm-t2.mean_perm) %*% (t1.mean_perm-t2.mean_perm) 
}
hist(T2,xlim=range(c(T2,T20)))
abline(v=T20,col=3,lwd=4)
p_val = sum(T2>=T20)/B
p_val#0

##Casual working day vs weekend-------------------------------
a<-subset(day1,day1$workingday==1)
matplot(t(a[,41:64]), type='l')
b<-subset(day1,day1$workingday==0)
matplot(t(b[,41:64]), type='l')

t1 = a[,41:64]
t2 = b[,41:64]

t1.mean = colMeans(t1)
t2.mean = colMeans(t2)

matplot(seq(0,23),
        t(rbind(t1.mean,t2.mean)), 
        type='l', col=c(1,2), lty=1,
        main="Sample multivariate means")

n1 = dim(t1)[1]
n2 = dim(t2)[1]
n  = n1 + n2

T20 = as.numeric(t(t1.mean-t2.mean) %*% (t1.mean-t2.mean))  # matrix (vector) product
#T20 <- norm(t1.mean-t2.mean) same as before
T20
B<-1000
T2 = numeric(B)
set.seed(19)
for(perm in 1:B){
  # Random permutation of indexes
  # When we apply permutations in a multivariate case, we keep the units together
  # i.e., we only permute the rows of the data matrix
  t_pooled = rbind(t1,t2)
  permutation = sample(n)
  t_perm = t_pooled[permutation,]
  t1_perm = t_perm[1:n1,]
  t2_perm = t_perm[(n1+1):n,]
  
  # Evaluation of the test statistic on permuted data
  t1.mean_perm = colMeans(t1_perm)
  t2.mean_perm = colMeans(t2_perm)
  T2[perm]  = t(t1.mean_perm-t2.mean_perm) %*% (t1.mean_perm-t2.mean_perm) 
}
hist(T2,xlim=range(c(T2,T20)))
abline(v=T20,col=3,lwd=4)
p_val = sum(T2>=T20)/B
p_val#0


#regression ------------------
plot(day$cnt, day$temp, col=col_working, pch=16)
boxplot(day$cnt ~ day$workingday)

attach(day)
fm <- lm(cnt ~ temp + hum)
summary(fm)
plot(fm$residuals)

model_linear_spline <- lm(cnt ~ bs(temp, knots=c(10000),degree=3), data=day)
temp.grid=(seq(range(temp)[1],range(temp)[2],by=0.01))
preds=predict(model_linear_spline,list(temp=temp.grid),se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
with(day, plot(temp ,cnt ,xlim=range(temp.grid) ,cex =.5, col =" darkgrey ",main='Custom cut Fit'))
lines(temp.grid,preds$fit ,lwd =2, col =" blue")
matlines(temp.grid ,se.bands ,lwd =1, col =" blue",lty =3)

#Spearman's correlation index between casual and registered----------
fCasual<-fData(seq(0,23),(day1[,41:64]))
fRegistered<-fData(seq(0,23),(day2[,41:64]))
bivariate_data <- as.mfData(list(fCasual, fRegistered))
plot(bivariate_data)
cor_spearman(bivariate_data, ordering='MHI')#0.7146697

#outlier detection with roahd---------------
invisible(fbplot(fCasual, main="Magnitude outliers"))
invisible(outliergram(fCasual))
out_shape <- outliergram(fCasual, display = FALSE)
out_shape$ID_outliers
invisible(fbplot(fRegistered, main="Magnitude outliers"))
invisible(outliergram(fRegistered))
out_shape <- outliergram(fRegistered, display = FALSE)
out_shape$ID_outliers

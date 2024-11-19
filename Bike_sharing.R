
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

#fda----------
library(splines)
library(fda)
library(roahd)
fd=fData(0:23,(day[,17:40]))
plot(fd)
work_fd=subset(fd, day$workingday==1)
plot(work_fd)
we_fd=subset(fd, day$workingday==0)
plot(we_fd)
meandiff=(median_fData(work_fd,type='MBD'))-(median_fData(we_fd,type='MBD'))


#Seconda domanda grafici-------------
col_working <- ifelse(day$workingday==1, 'red', 'blue')
plot(day$casual)
plot(day$registered)
#Trasforma `dati_orari` per avere una colonna per ogni ora
hour_wide_register <- hour %>%
  pivot_wider(
    names_from = hr, 
    values_from = registered,
    names_prefix = "cnt_hour_reg"
  )
hour_wide_casual <- hour %>%
  pivot_wider(
    names_from = hr, 
    values_from = casual,
    names_prefix = "cnt_hour_cas"
  )
#dataset che compatta le righe
hour_wide_sommato_reg <- hour_wide_register %>%
  group_by(dteday) %>%
  summarise(across(starts_with("cnt_hour_reg"), sum, na.rm = TRUE))
hour_wide_sommato_cas <- hour_wide_casual %>%
  group_by(dteday) %>%
  summarise(across(starts_with("cnt_hour_cas"), sum, na.rm = TRUE))

matplot(t(hour_wide_sommato_reg[,2:25]), type='l')
matplot(t(hour_wide_sommato_cas[,2:25]), type='l')

#sign paired test-----
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


day <- read.csv("day.csv")
hour <- read.csv("hour.csv")
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

matplot(t(day[,17:40]), type='l')

mese_anno <- paste(day$yr, day$mnth, sep = "_")
rle <- rle(mese_anno)
len <- rle$lengths
color_vector <- rainbow(24)
col <- rep (color_vector, len)
matplot(t(day[,17:40]), type='l', col=col)

colori_nome <- sapply(color_vector, function(x) name_col(x))

col_holyday <- ifelse(day$holiday ==1, 'red', 'blue')
matplot(t(day[,17:40]), type='l', col=col_holyday)

col_working <- ifelse(day$workingday==1, 'red', 'blue')
matplot(t(day[,17:40]), type='l', col=col_working)

col_weather <- rainbow(4)[day$weathersit]
matplot(t(day[,17:40]), type='l', col=col_weather)

prop_casual <- day$casual/day$cnt
plot(prop_casual, col=col_working, pch=16)
shapiro.test(prop_casual[col_working=='blue'])

#Permutational ANOVA 
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

#Multiple regression
result<-lm(cnt ~ hum +temp + windspeed)
summary(result)
#Let’s start with a global test
T0_glob <- summary(result)$adj.r.squared
T_H0glob <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  
  Y.perm.glob <- cnt[permutation]
  T_H0glob[perm] <- summary(lm(cnt ~ hum +temp + windspeed))$adj.r.squared
}
sum(T_H0glob>=T0_glob)/B#1

T0_x1 <- abs(summary(result)$coefficients[2,1])
T0_x2 <- abs(summary(result)$coefficients[3,3])
T0_x3 <- abs(summary(result)$coefficients[4,3])

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
  T_H01[perm] <- abs(summary(lm(Y.perm.H01 ~ hum+temp+windspeed))$coefficients[2,3])
  
  residuals.H02.perm <- residuals.H02[permutation]
  Y.perm.H02 <- regr.H02$fitted + residuals.H02.perm
  T_H02[perm] <- abs(summary(lm(Y.perm.H02 ~ hum+temp+windspeed))$coefficients[3,3])
  
  residuals.H03.perm <- residuals.H03[permutation]
  Y.perm.H03 <- regr.H03$fitted + residuals.H03.perm
  T_H03[perm] <- abs(summary(lm(Y.perm.H03 ~ hum+temp+windspeed))$coefficients[4,3])
  
}

sum(T_H01>=T0_x1)/B#0
sum(T_H02>=T0_x2)/B#0
sum(T_H03>=T0_x3)/B#0.1



detach(hour)
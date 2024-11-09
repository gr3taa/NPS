
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

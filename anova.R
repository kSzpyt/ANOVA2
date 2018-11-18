

library(readxl)
library(dplyr)
library(tseries)
library(car)
library(sjstats)
# AGE   = wiek matki (w latach)
# LWT   = Weight in Pounds at the Last Menstrual Period
# RACE  = rasa (1 = biała, 2 = czarna, 3 = inna)
# SMOKE = palenie w trakcie ciąży (1 = tak, 0 = nie)
# PTL   = liczba wcześniejszych przedwczesnych porodów (0, 1,...)
# HT    = nadciśnienie (1 = tak, 0 = nie)
# UI    = presence of uterine irritability
# FTV   = liczba wizyt u lekarza w pierwszym trymestrze ciąży (0, 1, 2,...)
# BWT   = waga urodzonego dziecka (w gramach)


data <- read_xls("urodzenia.xls")
View(data)
summary(data) 
attach(data)
#normalność rozkłądu zmiennej zależnej
shapiro.test(BWT)
jarque.bera.test(BWT)
#jest normalnie

attach(data)
par(mfrow = c(2,3))
boxplot(BWT ~ RACE, xlab = "RACE", ylab = "BWT", main = "safasf")
boxplot(BWT ~ SMOKE, xlab = "SMOKE", ylab = "BWT")
boxplot(BWT ~ PTL, xlab = "PTL", ylab = "BWT")  
boxplot(BWT ~ HT, xlab = "HT", ylab = "BWT")
boxplot(BWT ~ UI, xlab = "UI", ylab = "BWT")
boxplot(BWT ~ FTV, xlab = "FTV", ylab = "BWT")

par(mfrow = c(2,3))
hist(RACE, breaks = c(0, 1, 2, 3))
hist(SMOKE, breaks = 2)
hist(PTL, breaks = 4)
hist(HT, breaks = 2)
hist(UI, breaks = 2)
hist(FTV, breaks = 6)



a <- NULL
b <- NULL
for (x in 1:3) {
  a[x] <- shapiro.test(data$BWT[which(data$RACE == x)])$p.value
  a[x+3] <- jarque.bera.test(data$BWT[which(data$RACE == x)])$p.value
  if(x < 3)
  {
    b[x] <- shapiro.test(data$BWT[which(data$SMOKE == x - 1)])$p.value
    b[x+2] <- jarque.bera.test((data$BWT[which(data$SMOKE == x - 1)]))$p.value
  }
}

data %>%
  group_by(SMOKE, RACE) %>%
  summarise(pValue = shapiro.test(BWT)$p.value)

data %>%
  group_by(SMOKE, RACE) %>%
  summarise(pValue = jarque.bera.test(BWT)$p.value)
#w każdej z podgrup nie ma przesłanek do odrzucenia hipotezy o normalnośći rozkładu
mean(a < 0.05)
mean(b < 0.05)

library(psych)
#describeBy(BWT, RACE)
describeBy(BWT, FTV)


bartlett.test(BWT ~ RACE)
bartlett.test(BWT ~ SMOKE)
bartlett.test(split(BWT, list(RACE, SMOKE)))

leveneTest(BWT, as.factor(RACE))
leveneTest(BWT, as.factor(SMOKE))
leveneTest(BWT ~ RACE*SMOKE)
#oba testy nie wskazują aby wariancje w grupach były istotnie 

data$RACE <- as.factor(data$RACE)
data$SMOKE <- as.factor(data$SMOKE)

fit <- aov(BWT ~ RACE * SMOKE, data)
fit_s <- summary(fit)

str(fit)
str(fit_s)
par(mfrow = c(2, 2))
plot(fit)


#fit_s[[1]][["Pr(>F)"]]

#nie ma interakcji pomiędzy zmiennymi 
interaction.plot(RACE, SMOKE, BWT)



TukeyHSD(fit)




#efekty eksperymentalne
eta_sq(fit)[,2]

eta_sq(fit, partial = TRUE)

omega_sq(fit)

ee <- data.frame(eta_sq(fit)[,2], eta_sq(fit, partial = TRUE)[,2], omega_sq(fit)[,2])
names(ee) <- c("eta kwadrat", "cząstkowe eta kwadrat", "omega kwadrat")
rownames(ee) <- eta_sq(fit)[,1]
ee








describeBy(BWT, RACE)
describeBy(BWT, list(SMOKE, RACE))

winedata <- read.table('D:\\Backup\\Documents\\GitHub\\portfolio\\data\\winequality-red.csv', sep = ';', header = TRUE)
winedata <- read.table('/Users/max/Documents/Github/Red-wine-quality-regression-analysis/data/winequality-red.csv',sep =';',header = TRUE)

fit <- lm(quality ~ ., data = winedata)
summary(fit)
plot(fitted.values(fit), jitter(rstandard(fit)))
plot(rstandard(fit))
length(rstandard(fit)[rstandard(fit) > 4]) + length(rstandard(fit)[rstandard(fit) < -4])
qqnorm(rstandard(fit))
abline(a=0,b=1)


library(nortest)
ad.test(rstandard(fit))

library(MASS)
fitsl <- step(fit, direction = 'both')

library(car)
vif(fit)
vif(fitsl)
anova(fitsl,fit)

summary(fitsl)
ad.test(rstandard(fitsl))
plot(fit)

install.packages('ordinal')
library(ordinal)
fito <- clm(as.factor(quality)~ ., data = winedata, link = 'logit')
vif(fito)

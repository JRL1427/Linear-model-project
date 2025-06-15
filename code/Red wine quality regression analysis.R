winedata <- read.table('D:\\Backup\\Documents\\GitHub\\portfolio\\data\\winequality-red.csv', sep = ';', header = TRUE)
winedata <- read.table('/Users/max/Documents/Github/Red-wine-quality-regression-analysis/data/winequality-red.csv',sep =';',header = TRUE)


library(ggplot2)
ggplot(winedata, aes(x= quality)) + geom_bar(fill = 'steelblue') + geom_text(stat ='count', aes(label = ..count..), vjust = -0.5) + labs(title ='barplot of red wine quaility', x = 'quality', y = 'sample number') + theme_classic()

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
fito <- clm(as.factor(quality)~ free.sulfur.dioxide + pH + total.sulfur.dioxide + chlorides + sulphates + volatile.acidity + alcohol, data = winedata, link = 'logit')
fito2 <- clm(as.factor(quality)~ pH + total.sulfur.dioxide + chlorides + sulphates + volatile.acidity + alcohol, data = winedata, link = 'logit')
fitnull <- clm(as.factor(quality) ~ 1, data = winedata, link = 'logit')
fitologlog <- clm(as.factor(quality)~ free.sulfur.dioxide + pH + total.sulfur.dioxide + chlorides + sulphates + volatile.acidity + alcohol, data = winedata, link = 'loglog')
anova(fito, fitnull)
anova(fito,fito2)
AIC(fito, fitologlog)


install.packages('rcompanion')
library(rcompanion)
nagelkerke(fit = fito, null = fitnull)

summary(fito)
confint(fito)
exp(coef(fito))
exp(confint(fito))

fittest <- polr(as.factor(quality) ~ free.sulfur.dioxide + pH + total.sulfur.dioxide + chlorides + sulphates + volatile.acidity + alcohol, data = winedata, Hess = TRUE)
install.packages("brant") 
library(brant)
brant(fittest)

install.packages("VGAM")
library(VGAM)
fitppom <- vglm(
  quality ~ free.sulfur.dioxide + pH + total.sulfur.dioxide + chlorides + sulphates + volatile.acidity + alcohol,
  data = winedata,
  family = cumulative(parallel = FALSE)
)



install.packages('ordinalNet')
library(ordinalNet)
X <- model.matrix(~ (free.sulfur.dioxide + pH + total.sulfur.dioxide + chlorides + sulphates + volatile.acidity + alcohol)^2, data = winedata)[ ,-1]
y <- as.factor(as.ordered(winedata$quality))
fitonet <- ordinalNet(X,y, family = 'cumulative', link ='logit', alpha = 0.5)

pred1 <- predict(fitonet,X,type ='class')
confusionmatrix <- table(Predicted = pred1, Actual = y)
mean(pred1 ==y)






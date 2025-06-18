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


winedata$fixed.acidity <- scale(winedata$fixed.acidity)
winedata$volatile.acidity <- scale(winedata$volatile.acidity)
winedata$residual.sugar <- scale(winedata$residual.sugar)
winedata$chlorides <- scale(winedata$chlorides)
winedata$free.sulfur.dioxide <- scale(winedata$free.sulfur.dioxide)
winedata$total.sulfur.dioxide <- scale(winedata$total.sulfur.dioxide)
winedata$density <- scale(winedata$density)
winedata$pH <- scale(winedata$pH)
winedata$sulphates <- scale(winedata$sulphates)
winedata$alcohol <- scale(winedata$alcohol)

fittest <- polr(as.factor(quality) ~ free.sulfur.dioxide + pH + total.sulfur.dioxide + chlorides + sulphates + volatile.acidity + alcohol, data = winedata, Hess = TRUE)
fittest <- polr(as.factor(quality) ~ ., data = winedata, Hess = TRUE)
coef(summary(fittest))

install.packages("brant") 
library(brant)
brant(fittest)

predictors <- colnames(winedata)[1:11]
print(predictors[1])
summary(winedata[predictors])

ggplot(winedata, aes(x = k)) + geom_histogram(bins = 30) + ggtitle("Distribution of x")

winedata$fixed.acidity <- log1p(winedata$fixed.acidity) #right skewed
winedata$volatile.acidity <- log1p(winedata$volatile.acidity) #right skewed
winedata$citric.acid <- cut(winedata$citric.acid, breaks = c(-0.1,0,0.3,0.6,Inf),labels = c('zero','low','medium','high')) #zero-inflated data, we see abnormal number of 0, so we make it a factor, notice we also worry about non-monotonic effect, so we do not order it
winedata$free.sulfur.dioxide <- log1p(winedata$free.sulfur.dioxide) #right skewed, and after transformation, it has gaps between data ranges, but it is tolarable
winedata$total.sulfur.dioxide <- log1p(winedata$total.sulfur.dioxide) #right skewed
winedata$alcohol <- log1p(winedata$alcohol) #right skewed






library(VGAM)


fitgom <- vglm(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides +free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, family = cumulative(parallel = FALSE), data = winedata)
fitvar <- polr(as.factor(quality) ~ fixed.acidity,data = winedata, Hess = TRUE)
fitvar <- vglm(quality ~ fixed.acidity + volatile.acidity, family = cumulative(parallel = FALSE), data = winedata)
brant(fitvar)
table(winedata$quality)
boxplot(fixed.acidity ~ quality, data = winedata)
boxplot(volatile.acidity ~ quality, data = winedata)


kruskal.test(alcohol ~ quality, data = winedata)



install.packages('ordinalNet')
library(ordinalNet)
X <- model.matrix(~ (free.sulfur.dioxide + pH + total.sulfur.dioxide + chlorides + sulphates + volatile.acidity + alcohol)^2, data = winedata)[ ,-1]
y <- as.factor(as.ordered(winedata$quality))
fitonet <- ordinalNet(X,y, family = 'cumulative', link ='logit', alpha = 0.5)

pred1 <- predict(fitonet,X,type ='class')
confusionmatrix <- table(Predicted = pred1, Actual = y)
mean(pred1 ==y)






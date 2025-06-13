winedata <- read.table('winequality-red.csv', sep = ';', header = TRUE)
fit <- lm(quality ~ ., data = winedata)
library(MASS)
fitsl <- step(fit, direction = 'backward')

library(car)
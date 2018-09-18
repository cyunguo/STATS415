install.packages("MASS")

library(MASS)
data(Boston)

#### Exploratory Data Analysis ####
pairs(subset(Boston, select = c("medv", "rm", "lstat", "age", "crim")),
      main = "Scatterplot matrix for variables in Boston dataset")

#### Simple Linear Regression ####
mod1 <- lm(medv ~ lstat, data = Boston)

mod1
summary(mod1)

plot(Boston$medv ~ Boston$lstat,
     xlab = "Proportion of households with low SES",
     ylab = "Median house value",
     pch = 20, col = "dimgray")
abline(a = mod1$coefficients[1], b = mod1$coefficients[2],
       col = "blue", lwd = 4)

plot(mod1$residuals ~ mod1$fitted.values, main = "Model 1 Residual Plot",
     xlab = "Fitted values", ylab = "Residuals")
abline(a = 0, b = 0, col = "gray60")

#### Multiple Linear Regression ####
mod1sq <- lm(medv ~ lstat + I(lstat^2), data = Boston)
summary(mod1sq)

plot(mod1sq$residuals ~ mod1sq$fitted.values, main = "Model 1.2 Residual Plot",
     xlab = "Fitted values", ylab = "Residuals")
abline(a = 0, b = 0, col = "gray60")

mod2 <- lm(medv ~ lstat + age + rm, data = Boston)
summary(mod2)

plot(mod2, which = 1, add.smooth = F)

anova(mod1, mod2)

nullModel <- lm(medv ~ 1, data = Boston)
anova(nullModel, mod2)

#### Categorical Predictors ####
mod3 <- lm(medv ~ lstat + rm + chas, data = Boston)
summary(mod3)

data(iris)
names(iris)

is(iris$Species)
table(iris$Species)

irismod1 <- lm(Sepal.Length ~ . - Species, data = iris)
summary(irismod1)

irismod2 <- lm(Sepal.Length ~ ., data = iris)
summary(irismod2)

head(iris$Species)
iris$Species <- factor(iris$Species, levels = c("versicolor", "virginica", "setosa"))
head(iris$Species)

irismod3 <- lm(Sepal.Length ~ ., data = iris)
summary(irismod3)

iris$virginica  <- as.numeric(iris$Species == 'virginica')
iris$versicolor <- as.numeric(iris$Species == 'versicolor')
iris$setosa     <- as.numeric(iris$Species == 'setosa')

irismod4 <- lm(Sepal.Length ~ . - Species - setosa, data = iris)
summary(irismod4)

#### Interactions ####
data(iris)
irismod5 <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
summary(irismod5)

anova(irismod2, irismod5)

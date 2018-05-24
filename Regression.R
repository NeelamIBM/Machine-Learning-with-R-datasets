#regression
launch <- read.csv("challenger.csv")
b <- cov(launch$temperature, launch$distress_ct) /
  var(launch$temperature)
pairs(launch)
b
a <- mean(launch$distress_ct) - b * mean(launch$temperature)
a
r <- cov(launch$temperature, launch$distress_ct) /
  (sd(launch$temperature) * sd(launch$distress_ct))
r
#Or we can use the correlation function instead
cor(launch$temperature, launch$distress_ct)
#solve() takes the inverse of a matrix
#t() is used to transpose a matrix
# %*% multiplies two matrices
reg <- function(y, x) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  solve(t(x) %*% x) %*% t(x) %*% y
}
str(launch)
#Since temperature is the third column of the launch data, we can run the reg() function as follows
reg(y = launch$distress_ct, x = launch[3])[,1]
reg(y = launch$distress_ct, x = launch[3:5])[,1]
insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)
str(insurance)
summary(insurance$charges)
hist(insurance$charges)
table(insurance$region)
cor(insurance[c("age", "bmi", "children", "charges")])
pairs(insurance[c("age", "bmi", "children", "charges")])
install.packages("psych")
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "charges")])
ins_model <- lm(charges ~ age + children + bmi + sex +
                  smoker + region, data = insurance)
#OR
ins_model <- lm(charges ~ ., data = insurance)
ins_model
summary(ins_model)
insurance$age2 <- insurance$age^2
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)
ins_model2 <- lm(charges ~ age + age2 + children + bmi + sex +
                   bmi30*smoker + region, data = insurance)
summary(ins_model2)

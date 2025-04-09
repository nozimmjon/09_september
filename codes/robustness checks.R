
hist(residuals(model), breaks = 30, col = "lightblue", main = "Residual Distribution")
qqnorm(residuals(model))
qqline(residuals(model), col = "red")



plot(fitted(model), residuals(model), main = "Residuals vs Fitted", xlab = "Fitted Values", 
     ylab = "Residuals", pch = 20)
abline(h = 0, col = "red")

library(car)
vif(model)

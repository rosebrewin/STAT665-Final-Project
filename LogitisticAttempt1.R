data <- read.csv("bibleData4.csv")
data <- data[, -1]
head(data)
x <- glm(language ~ . ,data = data[c(1:900, 1001:1900),], family = "binomial")
summary(x)

pr <- predict(x, newdata = data[c(901:1000, 1901:2000), ], type = "response")
testerror <- mean(abs(pr - data[c(901:1000, 1901:2000), 1]))
testerror

# SVM does better with smaller amount of training data
# Look at predictions of logistic with varying amounts of training data

ns <- c(20, 50, 100, 500, 900)

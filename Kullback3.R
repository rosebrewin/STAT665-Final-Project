### Create Kullback kernel SVM 
library(kernlab)

KL <- function(p, q) {
  vec <- p*log(p/q)
  return(sum(vec, na.rm=T))
}

mykernel <- function(p, q) {
  exp(-0.01*(KL(p, q) + KL(q, p)))  
}
class(mykernel) <- "kernel"

### Test on data (using french and english here)
english <- read.csv("STAT665-Final-Project/englishBible.csv")
english <- english + 0.0000000000001
english$language <- 1
englishtrain <- english[1:700, ]
englishtest <- english[701:1000, ]

french <- read.csv("STAT665-Final-Project/frenchBible.csv")
french <- french + 0.0000000000001
french$language <- 0
frenchtrain <- french[1:700, ]
frenchtest <- french[701:1000, ]

train <- rbind(englishtrain, frenchtrain)
test <- rbind(englishtest, frenchtest)

model <- ksvm(language ~ ., data=train, kernel=mykernel, type='C-svc')
pr1 <- predict(model, newdata=test, type='response')
plot(pr1)



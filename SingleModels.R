library(kernlab)

# Single letter files
x <- read.csv("trainSingle.csv", stringsAsFactors = FALSE)
y <- read.csv("testSingle.csv", stringsAsFactors = FALSE)

unique(x$language)
str(x$language)
  
# Recode language (0 = English)
x$language <- ifelse(x$language == 0, 0, 1)
y$language <- ifelse(y$language == 0, 0, 1)


############################## Logistic Regression ############################# 
lr.0 <- glm(language ~ ., data = x, family = "binomial")
summary(lr.0)

pr.0 <- predict(lr.0, newdata = y, type = "response")
plot(pr.0)

# Make predictions binary (0 = English, 1 = other)
pr.0 <- ifelse(pr.0 < .5, 0, 1)
plot(pr.0)

# Table of predictions v. truth
table(pr.0 == y$language)
1-mean(pr.0 == y$language)

# Misclassification rate

############################# SVM with Linear Kernel ###########################


sv.0 <- ksvm(language ~ ., data=x, kernel="vanilladot", scaled=F, type="C-svc")
sv.0

pr.svl <- predict(sv.0, newdata = y, type = "response")
table(pr.svl == y$language)

# Misclassification error
1-mean(pr.svl == y$language)
plot(pr.svl)



############################# SVM with Gaussian Kernel #########################

sv.g <- ksvm(language ~ ., data=x, kernel="rbfdot", scaled=F, type="C-svc")
sv.g

pr.svg <- predict(sv.g, newdata = y, type = "response")
table(pr.svg == y$language)

# Misclassification error
1-mean(pr.svg == y$language)
plot(pr.svg)

# If needed
# sigmas <- c(1/10, 1/100, 1/1000)
# error <- NULL
# for(i in 1:length(sigmas)) {
#   pr.svg <- predict(sv.g, newdata = y, type = "response")
#   error[i] <- mean(pr.sv == y$language)
# }

############################# SVM with Kullback Kernel #########################
xnew <- x
xnew[,-1] <- xnew[,-1] + .000000000000000001

KL <- function(p, q) {
  vec <- p*log(p/q)
  return(sum(vec, na.rm=T))
}

mykernel <- function(p, q) {
  exp(-0.01*(KL(p, q) + KL(q, p)))  
}

class(mykernel) <- "kernel"

sv.k <- ksvm(language ~ ., data=x, kernel=mykernel, type="C-svc")
warnings()
# Time consuming (?)

pr.svk <- predict(sv.k, newdata = y, type = "response")
table(pr.svk == y$language)

# Misclassification error
1-mean(pr.svk == y$language)
plot(pr.svk)

# 0.242381



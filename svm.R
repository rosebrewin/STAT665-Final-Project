library(kernlab)

data <- read.csv("bibleData4.csv")
data <- data[, -1]


sigmas <- c(1, 1/10, 1/100, 1/1000)
testerror <- NULL
for (i in 1:length(sigmas)) {
  SVmodel <- ksvm(language ~ ., data=data[c(1:900,1001:1900), ],
                  kernel="rbfdot", kpar=list(sigma=sigmas[i]), scaled=F,
                  type="C-svc")
  pr <- predict(SVmodel, newdata=data[c(901:1000, 1901:2000), ], type='response')
  testerror[i] <- mean(abs(pr - data[c(901:1000, 1901:2000), 1]))
}



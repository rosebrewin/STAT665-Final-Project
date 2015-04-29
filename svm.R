library(kernlab)

data <- read.csv("bibleData4.csv")
data <- data[, -1]


sigmas <- c(10, 1, 1/10, 1/100, 1/1000)
testerror <- NULL
trainerror <- NULL
for (i in 1:length(sigmas)) {
  SVmodel <- ksvm(language ~ ., data=data[c(1:200,1001:1200), ],
                  kernel="rbfdot", kpar=list(sigma=sigmas[i]), scaled=F,
                  type="C-svc")
  pr <- predict(SVmodel, newdata=data[c(901:1000, 1901:2000), ], type='response')
  testerror[i] <- mean(abs(pr - data[c(901:1000, 1901:2000), 1]))
  pr2 <- predict(SVmodel, newdata = data[c(1:200, 1001:1200), ], type = 'response')
  trainerror[i] <- mean(abs(pr2 - data[c(1:200, 1001:1200), 1]))
}

testerror
trainerror


testerror <- NULL
trainerror <- NULL
ns <- c(10, 20, 30, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900)
i <- 2
for (j in 1:length(ns)) {
  SVmodel <- ksvm(language ~ ., data=data[c(1:ns[j],1001:(1000+ns[j])), ],
                  kernel="rbfdot", kpar=list(sigma=sigmas[i]), scaled=F,
                  type="C-svc")
  pr <- predict(SVmodel, newdata=data[c(ns[j]:1000, (1000+ns[j]):2000), ], type='response')
  testerror[j] <- mean(abs(pr - data[c(ns[j]:1000, (1000+ns[j]):2000), 1]))
  pr2 <- predict(SVmodel, newdata=data[c(1:ns[j],1001:(1000+ns[j])), ], type='response')
  trainerror[j] <- mean(abs(pr2 - data[c(1:ns[j],1001:(1000+ns[j])), 1]))
}

plot(testerror ~ ns)

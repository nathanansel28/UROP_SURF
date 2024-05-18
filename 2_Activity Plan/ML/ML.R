setwd("C:\\SUTD\\UROP\\UROP_ScrapingTrafficData\\24.02.20 Activity Plan ML\\ML")

trip_OD <-read.csv("trip_OD.csv")
head(trip_OD)
data <- trip_OD[, c("dt", "distance", "aver_speed")]

model <- 1/aver_speed ~ sqrt(dt) + 0
result <-lm(model,trip_OD)
summary(result)

model <- 1/aver_speed ~ I(dt^(1/3)) + 0
result <-lm(model,trip_OD)
summary(result)

model <- 1/aver_speed ~ I(dt^(1/4)) + 0
result <-lm(model,trip_OD)
summary(result)

model <- 1/aver_speed ~ I(dt^(1/5)) + 0
result <-lm(model,trip_OD)
summary(result)

fit <- 1 / result$fitted.values
aver_speed <- trip_OD$aver_speed
r1 <- c(0,max(trip_OD$aver_speed))
plot(fit,aver_speed,xlim=r1,ylim=r1)
lines(r1,r1)
title("Actual vs. Fitted Values")

residuals <- aver_speed - 1/fit
plot(aver_speed,residuals)
lines(r1,c(0,0))
residualfit <- lowess(aver_speed,residuals,f=0.8)
lines(residualfit,col=c("red"))
title("Residuals vs. Actual Values")
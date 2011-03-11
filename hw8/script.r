# Alex Buchanan
# ST 511
# HW 8

"7.23"

data = read.csv("ex0723.csv", header=T)
attach(data)

plot(INTERVAL~DURATION, 
     main="Old Faithful eruption time interval \n based on duration of previous eruption", 
     ylab="Interval (minutes)", xlab="Duration of previous (minutes)")

linear_model = lm(INTERVAL~DURATION)
summary(linear_model)
abline(linear_model$coefficients)
est_duration = data.frame(DURATION=seq(1,5))
conf.bands = data.frame(predict(linear_model, est_duration, interval='confidence'))
lines(cbind(est_duration, conf.bands$lwr), lty=2)
lines(cbind(est_duration, conf.bands$upr), lty=2)

detach(data)

"7.26"
data = read.csv("ex0726.csv", header=T)
attach(data)

denmark_lm = lm(denmark~year)
denmark_lm
summary(denmark_lm)

netherlands_lm = lm(netherlands~year)
netherlands_lm
summary(netherlands_lm)

canada_lm = lm(canada~year)
canada_lm
summary(canada_lm)

usa_lm = lm(usa~year)
usa_lm
summary(usa_lm)


detach(data)

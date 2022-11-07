
# fred five year breakeven
fivebe <- ts_fred("T5YIEM", start = date.start)

# vivek
vivek <- read.csv("data/5_year_canadian_break_even_inflation_2.csv")
vivek <- ts(vivek$Value, start = c(2018,01), end = c(2022, 10), frequency = 12)
plot(vivek)

###### vivek's method
date.start3 <- '2018-01-01'

fivebe3 <- window(fivebe, start = c(2018, 01), end = c(2022, 10))

db2 <- cbind(vivek, fivebe3)

mod3 <- tslm(vivek ~ fivebe3, db2)
summary(mod3)

autoplot(db2[ , "vivek"], series = "vivek") +
  autolayer(fitted(mod3), series = "Fitted") +
  xlab("year") + ylab("") +
  ggtitle("FRED 5 YR BEIR Predicts Canada's 5 YR BEIR")

# index overlapping and predictive data
ind.overlap <- complete.cases(cbind(fivebe,vivek))
ind.predict <- is.na(cbind(fivebe,vivek)[,"vivek"])
df.overlap <- as.data.frame(cbind(fivebe,vivek)[ind.overlap,])
df.predict <- as.data.frame(cbind(fivebe,vivek)[ind.predict,])
# fit a model
linear_model <- lm(vivek~fivebe, data = df.overlap)
# predict new values
vivek.hat <- predict(linear_model, newdata = df.predict)
# splice
breakeven <- ts(c(vivek.hat, vivek), start = c(2003,01), frequency = 12)
# plot
plot(breakeven)
lines(vivek, col = 'red')


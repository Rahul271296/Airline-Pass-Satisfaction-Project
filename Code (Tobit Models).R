#' Tobit Regression (for censored data)
#' Data AirlinePassengerSatisfaction.csv (100,000 obs x 21 variables)
#' Objective: To predict customer satisfaction with airline service
#' DV measured on a 1-5 scale (terrible/bad/neutral/good/excellent)

setwd("C:/Users/Desktop/SDM/Data")
setwd("/Users/Desktop/SDM/Data")
d <- read.csv("AirlinePassengerSatisfaction.csv")
str(d)
View(d)

d$Gender = factor(d$Gender)
d$CustomerType = factor(d$CustomerType)
d$Class = factor(d$Class)
d$Gender = relevel(d$Gender, ref="Female")
d$CustomerType = relevel(d$CustomerType, ref="Personal Travel")
d$Class = relevel(d$Class, ref="Eco")

colSums(is.na(d)) 
hist(d$Satisfaction)

library(PerformanceAnalytics)
df <- d[, c(2, 4, 6, 12)]
str(df)
chart.Correlation(df) 

ols = lm(Satisfaction ~ CustomerType + Class + Gender + FlightDistance + 
        OnlineBooking + OnlineCheckin + AirportCheckin + GateLocation +
        Boarding + SeatComfort + Legroom + Cleanliness + OnboardService +
        FoodAndDrink + InflightEntertainment + BaggageHandling +
        DepartureDelay + ArrivalDelay, data=d)

library(AER)
tobit = tobit(Satisfaction ~ CustomerType + Class + Gender + FlightDistance + 
        OnlineBooking + OnlineCheckin + AirportCheckin + GateLocation +
        Boarding + SeatComfort + Legroom + Cleanliness + OnboardService +
        FoodAndDrink + InflightEntertainment + BaggageHandling +
        DepartureDelay + ArrivalDelay, left=1, right=5, data=d)
summary(tobit)

library(stargazer)
stargazer(ols, tobit, type="text", single.row=TRUE)

tobit2 = tobit(Satisfaction ~ CustomerType + Class + Gender + FlightDistance + 
         OnlineBooking + OnlineCheckin + AirportCheckin + GateLocation +
         Boarding + SeatComfort + Legroom + Cleanliness + OnboardService +
         FoodAndDrink + InflightEntertainment + BaggageHandling +
         DepartureDelay + ArrivalDelay, left=3, right=5, data=d)

# install.packages("censReg")
library(censReg)
tobit3 <- censReg(Satisfaction ~ CustomerType + Class + Gender + FlightDistance + 
                    OnlineBooking + OnlineCheckin + AirportCheckin + GateLocation +
                    Boarding + SeatComfort + Legroom + Cleanliness + OnboardService +
                    FoodAndDrink + InflightEntertainment + BaggageHandling +
                    DepartureDelay + ArrivalDelay, left=1, right=5, data=d)

stargazer(ols, tobit, tobit2, tobit3, type="text", single.row=TRUE)
AIC(ols, tobit1, tobit2, tobit3)
BIC(ols, tobit1, tobit2, tobit3)

#' Note: If you want to do left censoring only but not right censoring 
#' (e.g., analyzing wages which is left censored at 0 but has no right censor),
#' you can use right=Inf in the above formulas.
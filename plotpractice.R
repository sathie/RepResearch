#Read data
payments <- read.csv("/home/sarah/Uni/R/RepResearch/payments.csv")

#Make a plot that answers the question: 
#What is the relationship between mean covered charges (Average.Covered.Charges) and 
#mean total payments (Average.Total.Payments) in New York?

require(dplyr)
paymentsNY <- filter(payments, Provider.City == "NEW YORK")

require(ggplot2)
p <- ggplot(paymentsNY, aes(Average.Covered.Charges, Average.Total.Payments))
p + geom_point(alpha = 0.4, colour = "blue", size = 2) + 
  geom_smooth(method = lm, colour = "black", size = 0.5)

#Make a plot (possibly multi-panel) that answers the question: 
#How does the relationship between mean covered charges (Average.Covered.Charges) and 
#mean total payments (Average.Total.Payments) vary by medical condition (DRG.Definition) 
#and the state in which care was received (Provider.State)?

q <- ggplot(payments, aes(log(Average.Covered.Charges), log(Average.Total.Payments)))
q + geom_point(alpha = 0.1, colour = "blue", size = 0.5) + 
  facet_grid(Provider.State ~ DRG.Definition) +
  geom_smooth(method = lm, colour = "black", size = 0.5)

q + geom_point(aes(colour = DRG.Definition), alpha = 0.2) + 
  geom_smooth(aes(colour = DRG.Definition), method = lm, size = 0.5) + 
  facet_wrap(~Provider.State, nrow = 3) + 
  theme(legend.position = "bottom", legend.direction = "vertical")

q + geom_point(aes(colour = Provider.State), alpha = 0.2) + 
  geom_smooth(aes(colour = Provider.State), method = lm, size = 0.5) + 
  facet_wrap(~DRG.Definition, nrow = 3) + theme(legend.position = "bottom")

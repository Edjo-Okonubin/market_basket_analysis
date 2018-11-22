#Load required packages.
library(arules)
library(arulesViz)
library(datasets)

#Load the datatset
data("Groceries")

#Create an item frequency plot for the top 20 items.
itemFrequencyPlot(Groceries, topN = 20, type = "absolute")

#Get the rules
rules <-apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))

#Show the top 5 rules
options(digits = 2)
inspect(rules[1:5])

#Get summary information
summary(rules)

#Sort rules by confidence.
rules_conf<- sort(rules, by="confidence", decreasing=TRUE)

inspect(head(rules_conf))

#Sort rules by lift.
rules_lift <- sort(rules, by = "lift", decreasing = TRUE)
inspect(head(rules_lift))

#Tuning algorithm to get shorter but stronger rules.

rules <- apriori(Groceries, parameter = list(supp=0.001, conf = 0.8, maxlen = 3))
summary(rules)

#Remove redundant rules.
rules<- rules[!is.redundant(rules)]
inspect(head(rules))

###Targeting specific items.
#1. Items bought by customers before buying "whole milk"
rules <- apriori(data = Groceries, parameter = list (supp = 0.001, conf = 0.15, maxlen = 2), 
                 appearance = list(default="lhs", rhs = "whole milk"), control = list (verbose = F)) #get rules that lead to the purchase of "whole milk"

rules_conf <- sort(rules, by = "confidence", decreasing = TRUE) # High confidence rules

inspect(head(rules_conf))

#2. Items bought by customers after/along with "whole milk".
rules <- apriori(data = Groceries, parameter = list(supp = 0.001, conf = 0.15, minlen = 2), 
                 appearance = list(default="rhs", lhs = "whole milk"), control = list (verbose = F)) #those who bought milk, also bought...

rules_conf <- sort(rules, by = "confidence", decreasing = TRUE) # High confidence rules.

inspect(head(rules_conf))

###Visualize the data.
plot(rules, method = "graph", engine = "interactive", shading = NA)

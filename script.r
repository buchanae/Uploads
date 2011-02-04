# Alex Buchanan
# ST 511
# HW 4


library(Sleuth2)

"start 3.29"
survived = subset(case0201, Status=="Survived")
perished = subset(case0201, Status=="Perished")

t.test(survived["Humerus"], perished["Humerus"], var.equal=TRUE)

perished_subset = subset(perished, Humerus!="659")
t.test(survived["Humerus"], perished_subset["Humerus"], var.equal=TRUE)


"start 3.33"

data = read.csv("3.33.csv", header=T)
# log transforming data makes the conclusion clearer
ldata = log(data)
t.test(ldata["lesser"], ldata["greater"])
# run again to get useful confidence interval
t.test(data["lesser"], data["greater"])

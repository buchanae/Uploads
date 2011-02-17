# Alex Buchanan
# ST 511
# HW 5


# 4.32

library(Sleuth2)

attach(ex0432)

diff = Placebo - Marijuana

length(diff>0)
length(which(diff>0))
binom.test(13, 15, alternative='greater')

wilcox.test(diff, exact=FALSE, alternative='greater')

detach(ex0432)

# Salvage logging

biscuit = read.csv("biscuit.csv", header=T)

attach(biscuit)

wilcox.test(percentlost~action, exact=FALSE, correct=FALSE, conf.int=TRUE)
t.test(percentlost~action, var.equal=TRUE)

detach(biscuit)

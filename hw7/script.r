# Alex Buchanan
# ST 511
# HW 7

data = read.csv("diets.csv", header=T)
attach(data)
aovdata = aov(WtLoss24~Group)
anova(aovdata)
model.tables(aovdata, type='mean')

TukeyHSD(aovdata)

detach(data)

"6.15"

sp2 = (8 * (3.82^2 + 5.26^2 + 4.66^2 + 4.91^2 + 3.53^2))/40
sp = sqrt(sp2)
sp

c0 = .5 ^ 2
c1 = (-2/3) ^ 2
se = sp * sqrt((c0 + c0 + c1 + c1 + c1)/9)
mult = qt(.975, 40)
g = (28.8 + 26.2)/2 - (30.2 + 31.1 + 30.2)/3
g - mult * se
g + mult * se

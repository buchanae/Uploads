# Alex Buchanan
# ST 511
# HW 7

data = read.csv("diets.csv", header=T)
attach(data)
aovdata = aov(WtLoss24~Group)
anova(aovdata)
model.tables(aovdata, type='mean')
mult = qtukey(0.95, 3, 269)/sqrt(2)


low_carb_mean = 5.487
low_carb_n = 85

low_fat_mean = 3.304
low_fat_n = 94

medit_mean = 4.602
medit_n = 93

"Low-Carbohydrate vs Low-Fat"
sub = mult * sqrt(33.509) * sqrt(1/low_carb_n + 1/low_fat_n)
low_carb_mean - low_fat_mean - sub
low_carb_mean + low_fat_mean - sub

"Low-Fat vs Mediterranean"
sub = mult * sqrt(33.509) * sqrt(1/low_fat_n + 1/medit_n)
low_fat_mean - medit_mean - sub
low_fat_mean + medit_mean - sub

"Low-carb vs Mediterranean"
sub = mult * sqrt(33.509) * sqrt(1/low_carb_n + 1/medit_n)
low_carb_mean - medit_mean - sub
low_carb_mean + medit_mean - sub

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

# Alex Buchanan
# ST 511
# HW 2


#
# 2.13:  Fish Oil and Blood Pressure
#
"start 2.13"

fishoil = c(8, 12, 10, 14, 2, 0, 0)
regular = c(-6, 0, 1, 2, -3, -4, 2)

"a) mean and std dev"
mean(fishoil)
mean(regular)

sd(fishoil)
sd(regular)

"b) pooled estimate of std dev"
n1 = length(fishoil)
s1 = sd(fishoil)

n2 = length(regular)
s2 = sd(regular)

sp = sqrt((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2))/(n1 + n2 - 2))

"pooled estimate is..."
sp

"c) std error is..."
se = sp * sqrt((1/n1) + (1/n2))
se

"d) degrees of freedom..."
(n1 + n2 - 2)

"e) 95% confidence interval..."
diff = mean(regular) - mean(fishoil)
diff - 2.179 * se
diff + 2.179 * se

"f) t-statistic"
(diff - 0)/se


#
# 2.14:  Fishoil and Blood Pressure with R
#
"start 2.14"

t.test(fishoil, regular)


#
# Male vs Female Intelligence
#

"start male vs female intelligence"

data = read.csv("intelligence.csv", header=T)
# columns are sex, afqt, arith, word, parag, math
attach(data)

males = subset(data, sex=="male")
females = subset(data, sex=="female")


t.test(males["afqt"], females["afqt"])
boxplot(afqt ~ sex, 
        ylab="Score",
        names=c("Females (5927)", "Males (5951)"),
        main="AFQT Combined Score, Males vs Females")


t.test(males["arith"], females["arith"])
boxplot(arith ~ sex, 
        ylab="Score",
        names=c("Females (5927)", "Males (5951)"),
        main="AFQT Arithmetic Score, Males vs Females")


t.test(males["word"], females["word"])
boxplot(word ~ sex, 
        ylab="Score",
        names=c("Females (5927)", "Males (5951)"),
        main="AFQT Word Knowledge Score, Males vs Females")


t.test(males["parag"], females["parag"])
boxplot(parag ~ sex, 
        ylab="Score",
        names=c("Females (5927)", "Males (5951)"),
        main="AFQT Paragraph Comprehension Score, Males vs Females")


t.test(males["math"], females["math"])
boxplot(math ~ sex, 
        ylab="Score",
        names=c("Females (5927)", "Males (5951)"),
        main="AFQT Math Knowledge Score, Males vs Females")

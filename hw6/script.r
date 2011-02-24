# Alex Buchanan
# ST 511
# HW 6


#
# 5.24
#

data = read.csv("524.csv", header=T)
attach(data)

aov_data = aov(level~group)
aov_data
anova(aov_data)

model.tables(aov_data)

df = length(group) - length(unique(group))
sp = 21

t_stat = (0.2174 - (-0.6993)) / (sp * sqrt(1/6 + 1/12))
t_stat

pval = (1 - pt(t_stat, df))
pval

detach(data)

#
# Education and Income
#

data = read.csv('income2.csv', header=T)

attach(data)

plot(income~educ)

aov_data = aov(income~educ)
aov_data
anova(aov_data)

model.tables(aov_data)

df = length(educ) - length(unique(educ))
sp = 44133.9

college_est = 17521
college_n = 522
grad_est = 26246
grad_n = 484
hs_est = -15496
hs_n = 1410

# compare college vs high school
t_stat = (college_est - hs_est) / (sp * sqrt(1/college_n + 1/hs_n))
t_stat

# calc one-side p-value
pval = (1 - pt(t_stat, df))
pval

# confidence intervals
(college_est - hs_est) - qt(.975, df) * (sp * sqrt(1/college_n + 1/hs_n))
(college_est - hs_est) + qt(.975, df) * (sp * sqrt(1/college_n + 1/hs_n))

# compare grad vs college
t_stat = (grad_est - college_est) / (sp * sqrt(1/grad_n + 1/college_n))
t_stat

pval = (1 - pt(t_stat, df))
pval

(grad_est - college_est) - qt(.975, df) * (sp * sqrt(1/grad_n + 1/college_n))
(grad_est - college_est) + qt(.975, df) * (sp * sqrt(1/grad_n + 1/college_n))

detach(data)

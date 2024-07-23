## HW 4
## 15.6
typing <- read.table(file="data/hw_04_prob1.txt", header=TRUE)
print(typing)
typing$type <- factor(typing$type)
typing$sub <- factor(typing$sub)
levels(typing$type)

o <- lm(score ~ type + sub, data=typing)
summary(o)
anova(o)

tapply(typing$score, typing$type, mean)

## a) write a statistical model for the experiment and estimate the parameters
## y_ij = mu + tau_i + beta_j + e_ij
## where mu is the baseline mean - the mean calculated for the first categorical treatment level - determined by lexicographical order
## tau_i - change in mu based on treatment level - tau_1 is included in the baseline mean (mu), so for model purposes, tau_1 = 0
## beta_j - change in mu based on block level
## e_ij - error term associated with each subject/treatment combination
## we can see from the summary output that the intercept coefficient (baseline mean) is the mean for the Classical music factor/category with an estimated mu of 23.000. The change in mu for the Hard Rock factor/category is -2.857 and the change in mu for the No Music factor/category is -2.143.
## the estimated parameters are:
## mu = 23.000 = estimated mean score for Classical
## tau_2 = -2.857 - estimated mean score for Hard Rock = 23.000 -2.857 = 20.143
## tau_3 = -2.143 - estimated mean score for No Music = 23.000 -2.143 = 20.857
## All we are interested in is if there is any change to mu with a change in Type of Music, so we can ignore the blocking estimates. In fact, any change due to blocking was already taken into account when we decided to block due to the heterogeneity of the subjects. The Randomized Complete Block Design is designed to cancel out the effects of the heterogeneity of the subjects when we look for differences between the taus.

## b) Are there differences in the mean typing efficiencies for the types of music?
anova(o)
## based on the ANOVA results, we can conclude that there is a difference in the mean typing efficiencies based on the result of the F-test for the Type response. The null hypothesis for the F-test states that all taus in the statistical model are equal to zero - H_0: tau_1 = tau_2 = tau_3 = 0. The results in the ANOVA test record an F-value of 6.5436 and p-value of 0.0119776, which is less than our significance level of 0.05 which means it is unlikely or not plausible that all three taus are equal to zero. Therefore there is some difference between the taus and that difference contributes to at least one change in means for the types of music.

## 15.8
watermelon <- read.table(file="data/hw_04_prob2.txt", header=TRUE)
print(watermelon)

watermelon$row <- factor(watermelon$row)
watermelon$col <- factor(watermelon$col)
watermelon$trt <- factor(watermelon$trt)

boxplot(wgt ~ trt, data=watermelon)

## a) write an appropriate statistical model for the experiment
## y_ijk = mu + tau_i + rho_j + gamma_k + e_ijk
## mu is the base mean, tau is the change in mean based on treatment, rho is the change in mean based on row effect, gamma is the change in mean based on column effect, and e is error.
## Our focus in the analysis is to determine if there is a treatment effect and so our hypothesis test is to determine if all taus are equal to zero (is there at least one tau that exists). Rho and Gamma values are the same as blocking effects - we're not interested in their values (i.e. they don't tell us anything we don't already know).

## b) Run an ANOVA and interpret the resultant p-values
o <- lm(wgt ~ row + col + trt, data=watermelon)
anova(o)
summary(o)
## From the results of the ANOVA analysis, we see that both the row and column effects could plausibly be equal to zero through their respective p-values (0.6961 and 0.1867) - both of which are greater than the 0.05 significance level. thus we cannot reject either of their null hypotheses and conclude that they do not significantly contribute to the model. However, the treatment effect has a very small p-value, 4.818e-08, which is well below the 0.05 significance level, which means we can conclude that it is not plausible that the treatment effects are all equal to zero. Therefore, we conclude that somewhere at least one treatment effect contributes to the change in mu - i.e. there is at least one difference in the resulting weight of the plant based on the treatment.
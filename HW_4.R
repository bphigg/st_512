## HW 4
## 15.6
typing <- read.table(file="data/hw_04_prob1.txt", header=TRUE)
print(typing)
typing$type <- factor(typing$type)
typing$sub <- factor(typing$sub)
levels(typing$type)

o <- lm(score ~ type + sub, data=typing)
summary(o)

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

#### a) write an appropriate statistical model for the experiment
## y_ijk = mu + tau_i + rho_j + gamma_k + e_ijk
## mu is the base mean, tau is the change in mean based on treatment, rho is the change in mean based on row effect, gamma is the change in mean based on column effect, and e is error.
## Our focus in the analysis is to determine if there is a treatment effect and so our hypothesis test is to determine if all taus are equal to zero (is there at least one tau that exists). Rho and Gamma values are the same as blocking effects - we're not interested in their values (i.e. they don't tell us anything we don't already know).

#### b) Run an ANOVA and interpret the resultant p-values
o <- lm(wgt ~ row + col + trt, data=watermelon)
anova(o)
summary(o)
## From the results of the ANOVA analysis, we see that both the row and column effects could plausibly be equal to zero through their respective p-values (0.6961 and 0.1867) - both of which are greater than the 0.05 significance level. thus we cannot reject either of their null hypotheses and conclude that they do not significantly contribute to the model. However, the treatment effect has a very small p-value, 4.818e-08, which is well below the 0.05 significance level, which means we can conclude that it is not plausible that the treatment effects are all equal to zero. Therefore, we conclude that somewhere at least one treatment effect contributes to the change in mu - i.e. there is at least one difference in the resulting weight of the plant based on the treatment.

## 17.6
plaque_dna <- read.table(file="data/exercise_17-5.txt", header=TRUE)
print(plaque_dna)

plaque_dna$analyst <- factor(plaque_dna$analyst)
plaque_dna$subject <- factor(plaque_dna$subject)

library(lme4)

oo <- lmer(dna ~ (1 | subject) + (1 | analyst), data=plaque_dna)
summary(oo)

library(EMSaov)
EMSanova(dna ~ subject + analyst, data=plaque_dna, type=c("R", "R"))

library(lmerTest)

o <- lmer(dna ~ (1 | subject) + (1 | analyst), data=plaque_dna)
summary(o)
ranova(o)

## reduced F-test
oo_red <- lmer(dna ~ (1 | subject), data=plaque_dna)
anova(oo_red, oo)

#### a) what are the Expected Mean Squares
## From the EMSanova() function:
## subject - 10.49522
## analyst - 0.16650
## error - 0.0195

#### b) Run an ANOVA and test for a significant analyst effect
## Using the R package lmerTest we can fit the model using lmer() function and run the model through the random effects anova function, ranova(). The results show a Chisq p-value of 0.0001934 for the analyst effect, which is well below the significance value of 0.05, indicating that we can reject the null hypothesis that the analyst effect is equal to zero. Therefore we can conclude that the analyst effect is greater than zero and thus at least one analyst has an effect on the dna readings from the plaque samples.

#### c) estimate the variance components associated with analysts, subject, and error
## From the summary() of the model:
## subject variance - 2.0951
## analyst variance - 0.0147
## error variance - 0.0195
## What proportion of the total variation is associated with each source?
## total variance = subject variance + analyst variance + error variance
## total variance = 2.0951 + 0.0147 + 0.0195 = 2.1293
## proportional subject variation = subject variance / total variance = 
2.0951 / 2.1293 
## proportional analyst variation = analyst variance / total variance = 
0.0147 / 2.1293
## proportional error variation = error variance / total variance =
0.0195 / 2.1293

## 17.7 Beer Pasteurization
beer <- read.table(file="data/exercise_17-7.txt", header=TRUE)
beer$lab <- factor(beer$lab)
beer$process <- factor(beer$process)
print(beer)
#### a) write an appropriate linear statistical model for the data; identify all terms in the model
## There are two factors in this design - pastuerizing process and the labs analyzing the samples. Both are randomly chosen, so the study type is Random Effects in a Factorial Design. The appropriate statistical model is:
## y_ijk = mu + alpha_i + beta_j + (alphabeta)_ij + e_ijk
## where mu is the base mean that includes alpha_1 (effect for lab_1), beta_1 (effect for process_1), and all interactions (alphabeta) associated with either lab_1 or process_1.
## alpha_i is the change in mu associated with the lab assignment
## beta_j is the change in mu associated with the process assignment
## (alphabeta)_ij is the change in mu associated with the interaction effect of lab and process assignments
## e_ijk is the change in mu associated with the individual error of the measured unit

#### b) Expected Mean Squares

library(EMSaov)
EMSanova(counts ~ lab * process, data=beer, type=c("R", "R"))
## From the EMSanova() function:
## lab - 1481884.748
## process - 1960953.594
## lab:process (interaction) - 416609.378
## error - 1091.175

#### c) state the null and alternative hypothesis testing for 1) interaction effect, 2) process effect, 3) lab effect
## 1) interaction - H_0: (alphabeta) = 0; H_A: (alphabeta) != 0
## H_0 - there is no interaction effect and so it's coefficient is equal to zero for all interactions
## H_A - there is at least one interaction effect
## 2) process - H_0: beta = 0; H_A: beta != 0
## H_0 - there is no process effect and so it's coefficient is equal to zero
## H_A - there is a process effect and so it's coefficient is something other than zero
## 3) lab - H_0: alpha = 0; H_A: alpha != 0
## H_0 - there is no lab effect and it's coefficient is equal to zero
## H_A - there is a lab effect and it's coefficient is something other than zero

## 17.8
## a) run an ANOVA and test for significant effects
library(lmerTest)

o_ <- lmer(counts ~ (1 | lab) + (1 | process) + (1 | lab:process), data=beer)
summary(o_)
ranova(o_)
## from the ranova() output we can see that the p-value(Chisq) is extremely small for the interaction effect - near zero. Therefore, we can conclude that there is a significant interaction effect. From this conclusion, we do not need to test the individual effects because in order to keep the interaction in the model (which we want to do) we need to also keep the individual effects. However, the test results for both individual effects are also significant given the their respective p-value(Chisq) values.

#### b) estimate the variance components associated with the effects. calculate the proportion of total variation associated with each effect.
## process variance- 128696
## lab variance- 53264
## lab:process variance- (interaction) - 207759
## error variance-  1091
## total variance- 128696 + 53264 + 207759 + 1091 = 390810
## proportional process variation:
128696 / 390810
## proportional lab variation:
53264 / 390810
## proportional lab:process variation:
207759 / 390810
## proportional error variation:
1091 / 390810

#### c) is the effect due to laboratory or process greater?
## the effect due to process is greater than the effect due to lab. It's p-value(Chisq) is significantly smaller than the p-value(Chisq) for the lab effect - this indicates that the plausibility of being zero, or not having an effect, is greater for the lab effect. Thus the process effect is stronger. Additionally, if we look at the proportion of total variation associated with each effect, we see that the proportion of variation attributed to the process effect is nearly twice as much as the proportion of variance attributed to the lab effect (process = 0.33, lab = 0.14) - this indicates that the process effect explains nearly 33% of the variance versus the lab effect explaining nearly 14%, so because process explains a larger portion of the variance, it is the stronger effect.
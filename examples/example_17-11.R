
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Example 17.11 in Ott & Longnecker

library(EMSaov)
library(lme4)

tablet <- read.table(file="../Data/example_17-11.txt", header=TRUE)
print(tablet)

tablet$site <- factor(tablet$site)
tablet$batch <- factor(tablet$batch)

boxplot(y ~ batch * site, data=tablet)

# WRONG -- treats batch as *crossed* with site
EMSanova(y ~ batch + site, data=tablet, type=c("R", "F"))

# RIGHT -- treats batch as *nested* in site
EMSanova(y ~ batch + site, data=tablet, type=c("R", "F"), nested=c("site", NA))
o.lmer <- lmer(y ~ site + (1 | site:batch), data=tablet)
summary(o.lmer)


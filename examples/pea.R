
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for pea plant growth versus sugar treatment example

install.packages("emmeans")
library(emmeans)

# Read in data, print, draw basic plot

pea <- read.table(file="data/pea.txt", header=TRUE)
head(pea)

# Create a factor version of the variable, save to data set pea

pea$trt <- factor(pea$trt)

# Basic numerical & graphical summaries

table(pea$trt)
plot(length ~ trt, data=pea)
tapply(pea$length, pea$trt, mean)
tapply(pea$length, pea$trt, sd)

# One-way ANOVA model fit

pea.lm <- lm(length ~ trt, data=pea)
summary(pea.lm)
anova(pea.lm)

# Comparing different treatment level means w/ contrasts

levels(pea$trt) 

# w1 = avg(sugars) vs control

w1 <- c(0.25, 0.25, 0.25, 0.25, -1)

pea.emm <- emmeans(pea.lm, 'trt')
pea.contr <- contrast(pea.emm, list("avg_vs_control"=w1)) 
summary(pea.contr, infer=c(TRUE, TRUE)) 

# w2 = 1g1f vs avg(2g, 2f)
# w3 = 2f vs 2s

w2 = c(1, -0.5, -0.5, 0, 0)       
w3 <- c(0, 1, 0, -1, 0)   

W <- list("avg_vs_control"=w1, "gf_vs_avg"=w2, "f_vs_s"=w3)
pea.contr <- contrast(pea.emm, W)
summary(pea.contr, infer=c(TRUE, TRUE)) 

# Is there an effect within sugars?

w1 <- c(1, -1, 0, 0, 0)
w2 <- c(0, 1, -1, 0, 0)
w3 <- c(0, 0, 1, -1, 0)

W.sugar <- list("1_vs_2"=w1, "2_vs_3"=w2, "3_vs_4"=w3)
pea.contr.sugar <- contrast(pea.emm, W.sugar)
test(pea.contr.sugar, joint=TRUE)

# Full partition of sum of squares
# You don't need to understand this code (it's hard to understand!)
# This is just so you can see the full SS partition in a table

contrasts(pea$trt) <- cbind(w1, w2, w3, w4=c(0.25, 0.25, 0.25, 0.25, -1))
cnames <- list("1g1f_vs_2f"=1,
               "2g_vs_2g"=2,
               "2g_vs_2s"=3,
               "avg_vs_control"=4
               )
o.orth <- aov(length ~ trt, data=pea)
summary(o.orth, split=list(trt=cnames))

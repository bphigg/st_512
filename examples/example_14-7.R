
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Example 14.7 in Ott & Longnecker

# Read in data

work <- read.table(file="../Data/example_14-7.txt", header=TRUE)
print(work)

# Marginal summaries

tapply(work$performance, work$noise, mean)
tapply(work$performance, work$gender, mean)
tapply(work$performance, work$exp, mean)

# Code factors as *factors*

work$noise <- factor(work$noise)
work$gender <- factor(work$gender)
work$exp <- factor(work$exp)

# Three-way ANOVA with interaction

o.work <- lm(performance ~ noise * gender * exp, data=work)
summary(o.work)
anova(o.work)

# Looks like three-way interaction isn't important...
# Use partial F test to check 

oo.work <- lm(performance ~ noise + gender + exp + noise:gender + noise:exp + gender:exp, 
              data=work)
summary(oo.work)
anova(oo.work, o.work)
anova(oo.work)

# Looks like only the noise-gender interaction might be needed

ooo.work <- lm(performance ~ noise + gender + exp + noise:gender, data=work)
summary(ooo.work)
anova(ooo.work, oo.work)

# Check the gain in error/residual dfs: full to reduced models

anova(ooo.work)
anova(o.work)





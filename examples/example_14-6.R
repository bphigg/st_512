
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Example 14.6 in Ott & Longnecker

# Read in data

citrus <- read.table(file="data/example_14-6.txt", header=TRUE)
print(citrus)

# Marginal summaries

tapply(citrus$yield, citrus$pesticide, mean)
tapply(citrus$yield, citrus$variety, mean)

# Code factors as *factors*

citrus$pest <- factor(citrus$pesticide)
citrus$vari <- factor(citrus$variety)

# Graphical summaries

boxplot(yield ~ pest * vari, data=citrus)

ylim <- range(citrus$yield)
boxplot(yield ~ pest, subset=(vari==1), data=citrus, 
        ylim=ylim, xlim=c(0.5, 4.5), 
        at=1:4-0.3, boxwex=0.25, col="pink")
boxplot(yield ~ pest, subset=(vari==2), data=citrus, add=TRUE, 
        at=1:4, boxwex=0.25, col="lightgreen")
boxplot(yield ~ pest, subset=(vari==3), data=citrus, add=TRUE, 
        at=1:4+0.3, boxwex=0.25, col="lightblue")

with(citrus, interaction.plot(pest, vari, yield))

# Two-way ANOVA with interaction

o.citrus <- lm(yield ~ pest * vari, data=citrus)
summary(o.citrus)
anova(o.citrus)

# Looks like interaction isn't important...
# Use partial F test to check

oo.citrus <- lm(yield ~ pest + vari, data=citrus)
summary(oo.citrus)
anova(oo.citrus, o.citrus)



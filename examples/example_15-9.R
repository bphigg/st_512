
## ST512 
## Author: Prof Ryan Martin (www4.stat.ncsu.edu/~rmartin)
## R code for Example 15.9 in Ott & Longnecker

bread <- read.table(file="data/example_15-9.txt", header=TRUE)
print(bread)

bread$flour <- factor(bread$flour)
bread$temp <- factor(bread$temp)
bread$day <- factor(bread$day)

with(bread, interaction.plot(temp, flour, protein))

o <- lm(protein ~ temp * flour + day, data=bread)
anova(o)

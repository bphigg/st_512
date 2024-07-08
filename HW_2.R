## 12.11
medical <- read.table("data/medical.txt", header=TRUE)
head(medical)
# a)
med_fit <- lm(SystolicBP ~ Age + Weight, data=medical)
summary(med_fit)
# Estimation for 1st order model
# estimated_SystolicBP = 57.2644 + 5.8041(Age_i) + 3.3162(Weight_i)

# b)
# From the model summary, the calculated Residual Standard Error = 2.454
# We use residuals to estimate error variance, so Residual Standard Error = Residual Standard Deviation

# c)
# beta_hat_2, the coefficient for Weight, represents the estimated change in the Systolic Blood Pressure corresponding to a unit change in Weight while holding all other variables fixed. So for each unit increase in Weight, we expect the Systolic Blood Pressure to increase by 3.3162 units, while all other variables (i.e. Age) are unchanged.
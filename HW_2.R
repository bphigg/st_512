## 12.11
medical <- read.table("data/medical.txt", header=TRUE)
head(medical)
plot(medical)
# a)
y <- medical$SystolicBP
x_1 <- medical$Age
x_2 <- medical$Weight
med_fit <- lm(y ~ x_1 + x_2)
summary(med_fit)
# Estimation for 1st order model
# estimated_SystolicBP = 57.2644 + 5.8041(Age_i) + 3.3162(Weight_i)

# b)
# From the model summary, the calculated Residual Standard Error = 2.454
# We use residuals to estimate error variance, so Residual Standard Error = Residual Standard Deviation

# c)
# beta_hat_2, the coefficient for Weight, represents the estimated change in the Systolic Blood Pressure corresponding to a unit change in Weight while holding all other variables fixed. So for each unit increase in Weight, we expect the Systolic Blood Pressure to increase by 3.3162 units, while all other variables (i.e. Age) are unchanged.

# 12.19
x_1sq <- x_1^2
x_2sq <- x_2^2

med_fit_order2 <- lm(y ~ x_1 + x_2 + x_1sq + x_2sq)
summary(med_fit_order2)

# a) 
#Overall, the quadratic model is not a very good fit compared to the 1st order model. This is because the p-values for all of the betas are relatively large, indicating that there is not enough evidence to outright reject the null hypothesis that the beta values are equal to zero - indicating that the beta values do not strongly contribute to predicting or estimating the response (Systolic BP).

# b
anova(med_fit, med_fit_order2)
# The results of the ANOVA analysis comparing the reduced model (the 1st order model from 12.11) to the full model (quadratic model) produced a p-value of 0.07576. This is greater than the significance level of 0.05 and indicates that we cannot reject the hypothesis that the ommitted variables (beta_3 and beta_4) are equal to zero. Thus it is possible that the coefficients for the second order terms are zero and do not contribute to the model.

#c)
# The first order model appears to be the better model. The p-values for the coefficients in the first order model are all below the significance threshold, with the intercept and x_1 coefficients being near zero. This indicates that the coefficients do contribute to the response estimates. The p-values for the quadratic model, on the other hand, do not have strong p-values. All but one exceed the significance level of 0.05 which indicates that its probable that any or all of the coefficients could actually be zero and thus not contributing to the estimated response value. Finally, the ANOVA test comparing the two models also calculated a p-value greater than the significance threshold, indicating that the two variables being tested - 2nd order variables - do not contribute to or enhance the simpler or reduced model, ie the 1st order model.

# 12.30
# a) estimate the mean SysBP for infant age=4 and weight=3
# estimated_SystolicBP = 57.2644 + 5.8041(Age_i) + 3.3162(Weight_i)
57.2644 + 5.8041*4 + 3.3162*3

# b) Provide 95% CI for the above estimate
predict(med_fit, data.frame(x_1=4, x_2=3), interval="conf", level=0.95)

# c) Provide 95% Prediction Interval for the estimate
predict(med_fit, data.frame(x_1=4, x_2=3), interval="pred", level=0.95)

# compare b) and c)
# The fitted value for estimated mean and prediction are the same since both use the same linear model. However, the 95% intervals for the values is different. The 95% prediction interval is larger than the 95% confidence interval for estimated mean. This is because the prediction interval is based on the estimated mean plus an additional error term, which in turn increases the interval. 

# 12.23
# a)
# an R^2 value of 58.2% means that 58.2% of the variation in the data is explained by the model. This value is calculated using values obtained in the Partition of the Sum-of-Squares equation: SS(Total) = SS(Error) + SS(Model), where SS(Total) is the total sum of squares in the data (sum of the squared distance between the response and the mean of the responses); SS(Error) is the sum of the squared distance between the response and the modeled response; and SS(Model) is the sum of the squared distance between the modeled response and the mean of the actual response). R^2 is the ratio of SS(Model) to SS(Total) which returns the ratio of the model to the total. The closer R^2 is to 1, the closer the SS(Model) is to SS(Total) which indicates that the model accounts for variability well (i.e. there is low SS(Error)).

# b)
fitness <- read.table(file="data/example_12-6.txt", header=TRUE)
print(fitness)
o <- lm(oxygen ~ weight + age + time + rate, data=fitness)
summary(o)
fitness$weight_sq <- fitness$weight**2
fitness$age_sq <- fitness$age**2
fitness$time_sq <- fitness$time**2
fitness$rate_sq <- fitness$rate**2
o_quad <- lm(oxygen ~ weight + age + time + rate + weight_sq + age_sq + time_sq + rate_sq, data = fitness)
summary(o_quad)

# fitting a quadratic model and comparing the R^2 results shows an increase of
0.5925 - 0.5815

# c)
# The quadratic model has NO slope coefficients below the 0.05 significance level. In fact, they all look pretty bad in confirming the individual hypothesis test that each coefficient could plausibly be zero which means none would be useful in the model.

# d)
# perform an ANOVA test on the full model (quadratic) and the reduced model (first-order)
anova(o, o_quad)

# The resultant p-value is very large compared to the 0.05 significance level. Therefore, we cannot reject the null hypothesis that posits the quadratic group of variables (those missing from the reduced model) do NOT contribute to or improve the fit of the first-order model.

# e)
# The first-order model would be strongly recommended over the combined first- and second-order model. All measurements indicate that the first- and second-order model is a weaker fit - the small increase to R^2, the high p-values of all the second-order model coefficients and the failure to reject the null hypothesis regarding the squared terms in the ANOVA test.

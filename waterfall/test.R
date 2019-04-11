
library(auxml)
library(MASS)

data(Boston)

water_feature(mod0, "medv", testcase)
feature_contrib(mod0, "medv", testcase, plot = TRUE)
feature_contrib_prop(mod0, "medv", testcase, plot = TRUE)


library(MASS)
library(caret)
library(lime)
library(auxml)
library(magrittr)

data(Boston)

b2 <- preProcess(Boston, method = "center") %>% predict(., Boston)
train_df <- b2[-c(1:4),]
test_df  <- b2[c(1:4),]

# lime
mod0 <- train(medv ~., data = train_df, method = "lm")
explainer <- lime(train_df, mod0, bin_continuous = TRUE, quantile_bins = FALSE)
explanation <- explain(test_df[2,-which(colnames(test_df) == "medv")], explainer, n_features = 13)
plot_features(explanation)

# auxml
mod1 <- lm(data = train_df, medv ~.)


water_feature(mod1, "medv", test_df[2,])
feature_contrib(mod1, "medv", testcase1, plot = TRUE)
feature_contrib_prop(mod1, "medv", testcase1, plot = TRUE)

# the binning of continuous features in lime makes it a little uninterpretable, especially if
# the binning looks arbitrary from the eyes of a business person.
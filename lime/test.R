
# https://cran.r-project.org/web/packages/lime/vignettes/Understanding_lime.html
library(MASS)
library(lime)
data(biopsy)

biopsy$ID <- NULL
biopsy <- na.omit(biopsy)
names(biopsy) <- c('clump thickness', 'uniformity of cell size', 
                   'uniformity of cell shape', 'marginal adhesion',
                   'single epithelial cell size', 'bare nuclei', 
                   'bland chromatin', 'normal nucleoli', 'mitoses',
                   'class')

set.seed(4)
test_set <- sample(seq_len(nrow(biopsy)), 4)
prediction <- biopsy$class
biopsy$class <- NULL
model <- lda(biopsy[-test_set, ], prediction[-test_set])

predict(model, biopsy[test_set, ])

explainer <- lime(biopsy[-test_set,], model, bin_continuous = TRUE, quantile_bins = FALSE)
explanation <- explain(biopsy[test_set, ], explainer, n_labels = 1, n_features = 4)
explanation[, 2:9]

plot_features(explanation, ncol = 1)


# ----

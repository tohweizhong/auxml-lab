
library(kernlab)
library(caret)
library(magrittr)
library(ggplot2)

data(spam)
set.seed(123)

# mean centering
s2 <- preProcess(spam, method = "center") %>% predict(., spam)

idx <- createDataPartition(s2$type, p = 0.8, list = FALSE)
train <- s2[idx,]
test <- s2[-idx,]

mod0 <- glm(data = train, type ~., family =  binomial(link = logit))

sm <- summary(mod0)
betas <- sm$coefficients[,1]

testcase <- test[1,]
pred <- predict(mod0, testcase)

# dot product between feature vector and beta
featvec <- testcase[-which(testcase %>% names == "type")] %>% as.matrix
betas2 <- betas[-1]

nm <- names(betas)
#betas2 %*% t(featvec)

# feature contributions
featcont <- betas2*featvec
featcont <- c(betas[1], featcont, pred)
names(featcont) <- c(nm, "Prediction")





# waterfall chart on feature contribution
plotdata <- data.frame(coef = names(featcont), featcont = featcont, row.names = NULL)
plotdata$coef <- factor(plotdata$coef, levels = plotdata$coef)
plotdata$id <- seq_along(plotdata$coef)
plotdata$Impact <- ifelse(plotdata$featcont > 0, "+ve", "-ve")
plotdata[plotdata$coef %in% c("(Intercept)", "Prediction"), "Impact"] <- "Initial/Net"
plotdata$end <- cumsum(plotdata$featcont)
plotdata$end <- c(head(plotdata$end, -1), 0)
plotdata$start <- c(0, head(plotdata$end, -1))
plotdata <- plotdata[, c(3, 1, 4, 6, 5, 2)]

gg <- ggplot(plotdata, aes(coef, fill = Impact)) +
 geom_rect(aes(x = coef,
               xmin = id - 0.45,
               xmax = id + 0.45,
               ymin = end,
               ymax = start)) +
 theme_minimal() +
 #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
 scale_fill_manual(values=c("darkred", "darkgreen", "darkblue")) +
 theme(axis.text.x=element_text(angle=90, hjust=1))
 #coord_flip()
 

if(sign(plotdata$end[1]) != sign(plotdata$start[nrow(plotdata)]))
 gg <- gg + geom_hline(yintercept = 0)
gg
dengue_2015 <- read.csv("predict_data/data_2015.csv",
                   header=TRUE, stringsAsFactors=FALSE)
summary(dengue_2015)
head(dengue_2015)
fit <- lm(curdr ~ predr + prendr + predtr + prerain, data=dengue_2015)
fit
dengue_2015 <- read.csv("predict_data/data_2015_only_dengue_rate.csv",
                        header=TRUE, stringsAsFactors=FALSE)
summary(dengue_2015)
head(dengue_2015)
fit <- lm(curdr ~ predr + prendr, data=dengue_2015)
fit
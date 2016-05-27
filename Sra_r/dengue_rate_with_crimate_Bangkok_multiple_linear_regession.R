dengue_2013_2014 <- read.csv("predict_data/bangkok_predict_data_with_climate_2013-2014.csv",
                        header=TRUE, stringsAsFactors=FALSE)
summary(dengue_2013_2014)
head(dengue_2013_2014)
fit <- lm(curdr ~ predr + predtr + prerain, data=dengue_2013_2014)
fit
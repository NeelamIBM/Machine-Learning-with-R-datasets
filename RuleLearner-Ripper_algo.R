#identifying poisonous mushrooms with rule learners
mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = TRUE)
str(mushrooms)
mushrooms$veil_type <- NULL
table(mushrooms$type)
install.packages("RWeka")
library(RWeka)
mushroom_1R <- OneR(type ~ ., data = mushrooms)
mushroom_1R
summary(mushroom_1R)
#For a more sophisticated rule learner, we will use JRip(), a Java-based
#implementation of the RIPPER rule learning algorithm
mushroom_JRip <- JRip(type ~ ., data = mushrooms)
summary(mushroom_JRip)

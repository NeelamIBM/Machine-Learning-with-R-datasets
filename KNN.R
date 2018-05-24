wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)
wbcd <- wbcd[-1]
table(wbcd$diagnosis)
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#The lapply() function of R takes a list and applies a function to each element of the
#list. As a data frame is a list of equal-length vectors, we can use lapply() to apply
#normalize() to each feature in the data frame
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)
summary(wbcd_n$smoothness_mean)
summary(wbcd_n$radius_mean)
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
install.packages("class")
library(class)
#The knn() function in the class package provides a standard, classic
#implementation of the kNN algorithm. For each instance in the test data, the
#function will identify the k-nearest neighbors, using Euclidean distance, where k is
#a user-specified number. The test instance is classified by taking a "vote" among the
#k-Nearest Neighbors-specifically, this involves assigning the class of the majority of
#the k neighbors. A tie vote is broken at random

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)
install.packages("gmodels")
library(gmodels)
#The next step of the process is to evaluate how well the predicted classes in the
#wbcd_test_pred vector match up with the known values in the wbcd_test_labels
#vector. To do this, we can use the CrossTable() function in the gmodels
#package
#Specifying prop.chisq= FALSE will remove the chi-square values that are not needed, from the output:
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)
wbcd_z <- as.data.frame(scale(wbcd[-1]))
#To standardize a vector, we can use R's built in scale() function, which by default
#rescales values using the z-score standardization. The scale() function offers the
#additional benefit that it can be applied directly to a data frame, so we can avoid use
#of the lapply() function.
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)

#The mean of a z-score standardized variable should always be zero, and the range
#should be fairly compact. A z-score greater than 3 or less than -3 indicates an
#extremely rare value. The previous summary seems reasonable.
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                        cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
             prop.chisq=FALSE)
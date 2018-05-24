install.packages("xgboost")
library(xgboost)
install.packages("Matrix")
library(Matrix)
options(scipen=999)


# ---------------------------------------------------
# Load
orig.train <- read.csv("train.csv", stringsAsFactors = F)
orig.test <- read.csv("test.csv", stringsAsFactors = F)
sample.submission <- read.csv("Sample.csv", stringsAsFactors = F)

# ---------------------------------------------------
# Merge
orig.test$TARGET <- -1
merged <- rbind(orig.train, orig.test)

# ---------------------------------------------------
# Convert
feature.train.names <- names(orig.train)[-1]
for (f in feature.train.names) {
  if (class(merged[[f]]) == "numeric") {
    merged[[f]] <- merged[[f]] / max(merged[[f]])
  } else if (class(merged[[f]]) == "integer") {
    u <- unique(merged[[f]])
    if (length(u) == 1) {
      merged[[f]] <- NULL
    } else if (length(u) < 200) {
      merged[[f]] <- factor(merged[[f]])
    }
  }
}

# ---------------------------------------------------
# Split
train <- merged[merged$TARGET != -1, ]
test <- merged[merged$TARGET == -1, ]


# ---------------------------------------------------
# Features
feature.names <- names(train)
feature.names <- feature.names[-grep('^ID$', feature.names)]
feature.names <- feature.names[-grep('^TARGET$', feature.names)]
feature.formula <- formula(paste('TARGET ~ ', paste(feature.names, collapse = ' + '), sep = ''))

# ---------------------------------------------------
# Matrix
indexes <- sample(seq_len(nrow(train)), floor(nrow(train)*0.85))

data <- sparse.model.matrix(feature.formula, data = train[indexes, ])
sparseMatrixColNamesTrain <- colnames(data)
dtrain <- xgb.DMatrix(data, label = train[indexes, 'TARGET'])
rm(data)
dvalid <- xgb.DMatrix(sparse.model.matrix(feature.formula, data = train[-indexes, ]),
                      label = train[-indexes, 'TARGET'])
dtest <- sparse.model.matrix(feature.formula, data = test)

watchlist <- list(valid = dvalid, train = dtrain)

# ---------------------------------------------------
# XGBOOST
params <- list(booster = "gbtree", objective = "binary:logistic",
               max_depth = 6, eta = 0.02,
               colsample_bytree = 0.75, subsample = 0.95)
model <- xgb.train(params = params, data = dtrain,
                   nrounds = 500, early.stop.round = 50,
                   eval_metric = 'auc', maximize = T,
                   watchlist = watchlist, print.every.n = 10)

pred <- predict(model, dtest)

# ---------------------------------------------------
# SAVE
submission <- data.frame(ID = test$ID, TARGET = pred)
write.csv(submission, 'xgboost_first_simple.csv', row.names=FALSE, quote = FALSE)
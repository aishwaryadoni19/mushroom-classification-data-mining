

install.packages('rpart')
install.packages('caret')
install.packages('rpart.plot')
install.packages('rattle')
install.packages('readxl')


library(rpart)
library(caret)
library(rpart.plot)
library(rattle)
library(readxl)


mushrooms <- read_excel("Downloads/mushrooms.xlsx")
str(mushrooms)

# Check missing values
sum(is.na(mushrooms))  # or:
nrow(mushrooms) - sum(complete.cases(mushrooms))

# Remove redundant column
mushrooms$veil.type <- NULL


table(mushrooms$class, mushrooms$odor)


number.perfect.splits <- apply(X = mushrooms[-1], MARGIN = 2, FUN = function(col){
  t <- table(mushrooms$class, col)
  sum(t == 0)
})

# Sort in descending order
order <- order(number.perfect.splits, decreasing = TRUE)
number.perfect.splits <- number.perfect.splits[order]

# Plot
par(mar = c(10, 2, 2, 2))
barplot(number.perfect.splits,
        main = "Number of perfect splits vs feature",
        xlab = "", ylab = "Feature",
        las = 2, col = "wheat")


set.seed(12345)
train <- sample(1:nrow(mushrooms), size = ceiling(0.80 * nrow(mushrooms)), replace = FALSE)
mushrooms_train <- mushrooms[train, ]
mushrooms_test <- mushrooms[-train, ]


penalty.matrix <- matrix(c(0, 1, 10, 0), byrow = TRUE, nrow = 2)


tree <- rpart(class ~ .,
              data = mushrooms_train,
              parms = list(loss = penalty.matrix),
              method = "class")

rpart.plot(tree, nn = TRUE)



cp.optim <- tree$cptable[which.min(tree$cptable[,"xerror"]), "CP"]
tree <- prune(tree, cp = cp.optim)

pred <- predict(object = tree, mushrooms_test[-1], type = "class")

t <- table(mushrooms_test$class, pred)
confusionMatrix(t)


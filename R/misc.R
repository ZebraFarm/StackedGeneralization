#find MSE of all k models against true.
#find MSE of SG model against true.

# NOW RUN FOR N = 50:500
# NOW RUN FOR d = 3:10
# NOW RUN FOR k = 3:10 (must change regularization parameters)

```

# Random Stuff

## Testing Caret
```{r}
rand = sample(nrow(iris))
train.dat <- iris[rand[1:130],]
test.dat <- iris[rand[131:150],]


c50 <- function(train.dat){
  
  library(C50) # C5.0
  model_C50 = train(x = train.dat[,1:4], train.dat[,5], method = 'C5.0')
  pred_C50 = predict(model_C50,newdata = iris[,1:4]) 
  return(pred_C50)
}

#library(kernlab) # svmLinear
#library(klaR) # nb
library(LogicReg) # logreg
# knn
# rf
# xgbLinear

model = train(x = train.dat[,1:4], train.dat[,5], method = 'rf')
model

predModel = predict(model,newdata = iris[,1:4]) 
confusionMatrix(data = predModel, reference = iris[,5])$table
```

```{r}
names(getModelInfo())
```

## Origianl Thinking
```{r}

set.seed(99)
rand = sample(150)
k = 5

tr.data = iris[rand[1:100],]
te.data = iris[rand[101:150],]

# Level 0

## Split k sets 
## This also means that k models will be used

set.seed(100)
part_data   <- split(tr.data,k)

## Train k models

out.models <- train_level(part_data, k)
out.pred   <- predict_level(out.models, tr.data)

# Add Extra Info
true.label = tr.data[,5]
#EXTRA_INFO = (true.label - predicted)^2 # Squared Error
#in.data <- cbind(out.pred, EXTRA_INFO)
```




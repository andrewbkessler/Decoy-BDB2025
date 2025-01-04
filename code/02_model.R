library(tidyverse)
library(xgboost)
library(Metrics)
library(vip)
library(DiagrammeR)
options(scipen = 999)

### Load Prepared Data & fix decoy to numeric
decoy_data_join <- read_csv("data/model_data_noOL_w_ball.csv") |> 
  mutate(decoy = ifelse(decoy=='Y',1,0)) |> 
  rename(label = decoy) |> 
  select(-down, -M_pos,
         -D1_x, -D2_x, -D3_x, -D4_x, -D5_x, -D6_x, -D7_x, -D8_x, -D9_x, -D10_x, -D11_x,
         -D1_y, -D2_y, -D3_y, -D4_y, -D5_y, -D6_y, -D7_y, -D8_y, -D9_y, -D10_y, -D11_y,
         -O1_x, -O2_x, -O3_x, -O4_x, -O5_x)

### Remove unique_play_id
model_data <- decoy_data_join |> 
  select(-unique_play_id)

### Making train and test datasets
smp_size <- floor(0.80 * nrow(model_data))
set.seed(1997)
sample <- sample(seq_len(nrow(model_data)), size = smp_size)
train <- as.matrix(model_data[sample, ])
test <- as.matrix(model_data[-sample, ])

### Divide training data into sub_train & validate
subsmp_size <- floor(0.80 * nrow(train))
subsample <- sample(seq_len(nrow(train)), size = subsmp_size)

### Get subtraining sample
sub_train <- train[subsample, ]
valid <- train[-subsample, ]

### Get training, test, & validation subsets
numcols <- ncol(model_data)
train_y <- sub_train[,numcols]
train_x <- sub_train[,-numcols]
valid_y <- valid[,numcols]
valid_x <- valid[,-numcols]
test_y <- test[,numcols]
test_x <- test[,-numcols]

### Format training and validation data
gb_train <- xgb.DMatrix(data = as.matrix(train_x), label = train_y )
gb_valid <- xgb.DMatrix(data = as.matrix(valid_x), label = valid_y )

### Train xgb, evaluating against the validation
watchlist <- list(train = gb_train, valid = gb_valid)
decoy_model <- xgb.train(data = gb_train, 
                     max.depth = 8, 
                     eta = 0.01, 
                     nthread = 2, 
                     nround = 1000, 
                     watchlist = watchlist, 
                     objective = "binary:logistic", 
                     early_stopping_rounds = 50,
                     print_every_n = 100)

### Variable importance
vip(decoy_model)

### Model Tree of best iteration
tree_plot <- xgb.plot.tree(model = decoy_model, trees = decoy_model$best_iteration, plot_width = 1000, 
                           plot_height = 1000, render = FALSE)

# Extract tree information
tree_info <- xgb.model.dt.tree(model = decoy_model)
head(tree_info)

### Format test data
dtest <- xgb.DMatrix(data =  as.matrix(test_x))

### Test the model on external test data
y_hat_valid <- predict(decoy_model, dtest)

### Calculate LogLoss
logloss <- logLoss(test_y, y_hat_valid)
print(logloss)

### Check test accuracy
test_accuracy <- mean(as.numeric(as.numeric(y_hat_valid > 0.5) == test_y))
print(test_accuracy)

### Get decoy predictions
decoy_preds <- as.data.frame(
  matrix(predict(decoy_model, as.matrix(model_data %>% select(-label))))
) |>
  dplyr::rename(decoy_odds = V1)

### Join predictions back to dataset
decoy_projs <- cbind(select(decoy_data_join, unique_play_id, label), decoy_preds) |>
  rename(decoy = label)
### Uncomment row below to save decoy projections (to be used in different script)
# write_csv(test_projs, 'data/decoy_projs.csv')

test_projs <- decoy_data_join[-sample, ] |> 
  select(unique_play_id, label) |> 
  cbind(y_hat_valid) |> 
  rename(decoy = label, decoy_odds = y_hat_valid)
### Uncomment row below to save test projections (to be used in different script)
# write_csv(test_projs, 'data/test_projs.csv')

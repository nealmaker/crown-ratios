library("tidyverse")
library("caret")
library("Rborist")

# load data from GitHub
temp <- tempfile()
download.file("https://github.com/nealmaker/fia-data-nf/raw/master/rda/nf-fia.rda", 
              temp)
load(temp)

# load preproc from big-rdas file
load("../big-rdas/preproc-op.rda")

# remove trees that died and unwanted variables
nf_fia <- nf_fia %>%
  filter(status_change == "lived") %>% 
  select(cr_rate, spp, dbh_mid, cr_mid, ba_mid, bal_mid, 
         forest_type_s, lat, lon) 

# test set is 20% of full dataset
test_size <- .2

set.seed(10)
index <- createDataPartition(nf_fia$cr_rate, times = 1, p = test_size, list = FALSE)

train <- nf_fia[-index,]
test <- nf_fia[index,]


#####################################################################
# Preprocess data
#####################################################################

train_tran <- predict(preproc_op, train)
test_tran <- predict(preproc_op, test)

x <- train_tran[,-1]
y <- train_tran[,1]


#####################################################################
# Train model
#####################################################################

# calculates RMSE:
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


set.seed(1)
cr_model <- train(x, y,
                  method = "ranger",
                  num.trees = 200,
                  importance = 'impurity',
                  tuneGrid = data.frame(mtry = seq(2, 8, by = 2),
                                        splitrule = rep("variance", 4),
                                        min.node.size = rep(5, 4)))


#####################################################################
# Results 
#####################################################################

cr_model$results

plot(cr_model)

varImp(cr_model, scale = F)


#####################################################################
# Prediction 
#####################################################################

df <- data.frame(spp = factor(rep("white pine", 4), levels = levels(train$spp)),
                 dbh_mid = rep(12, 4),
                 cr_mid = seq(10, 70, 20),
                 ba_mid = rep(150, 4),
                 bal_mid = seq(225, 0, -75),
                 forest_type_s = factor(rep("White pine", 4),
                                        levels = levels(train$forest_type_s)),
                 lat = rep(44.7, 4),
                 lon = rep(-73.6, 4))

df_trans <- predict(preproc_op, newdata = df)

df_pred <- df %>% 
  mutate(y_hat = predict(cr_model, newdata = df_trans))


#####################################################################
# Save
#####################################################################

# STOP! Too big to fit on GitHub
# save(cr_model, file = "rda/cr_model.rda")
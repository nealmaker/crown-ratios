library("tidyverse")
library("caret")
library("Rborist")

# load data from GitHub
temp <- tempfile()
download.file("https://github.com/nealmaker/fia-data-nf/raw/master/rda/nf-fia.rda", 
              temp)
load(temp)

# remove trees that died and unwanted variables
nf_fia <- nf_fia %>%
  filter(status_change == "lived") %>% 
  select(cr_rate, spp, dbh_mid, cr_mid, crown_class_s, tree_class_s,
         ba_mid, bal_mid, forest_type_s, stocking_s, landscape, 
         site_class, slope, aspect, lat, lon, elev) 

# test set is 20% of full dataset
test_size <- .2

set.seed(10)
index <- createDataPartition(nf_fia$cr_rate, times = 1, p = test_size, list = FALSE)

train <- nf_fia[-index,]
test <- nf_fia[index,]

x <- train[,-1]
y <- train[,1]


#####################################################################
# Train model
#####################################################################

set.seed(1)
cr_growth_model_full <- train(x, y,
                  method = "ranger",
                  num.trees = 200,
                  importance = 'impurity',
                  tuneGrid = data.frame(mtry = seq(2, 14, by = 4),
                                        splitrule = rep("variance", 4),
                                        min.node.size = rep(5, 4)))


#####################################################################
# Results 
#####################################################################

cr_growth_model_full$results

plot(cr_growth_model_full)

varImp(cr_growth_model_full, scale = F)


#####################################################################
# Prediction 
#####################################################################

df <- data.frame(spp = factor(rep("white pine", 4), levels = levels(train$spp)),
                 dbh_mid = rep(12, 4),
                 cr_mid = seq(10, 70, 20),
                 crown_class_s = c(5, 4, 3, 2),
                 tree_class_s = rep(2, 4),
                 ba_mid = rep(150, 4),
                 bal_mid = seq(225, 0, -75),
                 forest_type_s = factor(rep("White pine", 4),
                                        levels = levels(train$forest_type_s)),
                 stocking_s = rep(2, 4),
                 landscape = factor(rep("rolling uplands", 4),
                                    levels = levels(train$landscape)),
                 site_class = rep(5, 4),
                 slope = rep(0, 4),
                 aspect = rep(0, 4),
                 lat = rep(44.7, 4),
                 lon = rep(-73.6, 4),
                 elev = rep(1400, 4))

df_pred <- df %>% 
  mutate(y_hat = predict(cr_growth_model_full, newdata = df))


#####################################################################
# Save
#####################################################################

# STOP! Too big to fit on GitHub
# save(cr_growth_model_full, file = "../big-rdas/cr-growth-model-full.rda")

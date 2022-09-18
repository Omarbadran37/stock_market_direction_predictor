svm_mod <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

svm_workflow <- 
  workflow() %>% 
  add_model(svm_mod) %>% 
  add_recipe(stock_recipe)



cv <- vfold_cv(rows_instockprice_train, v = 3, strata = target_direction_10)


set.seed(345)
svm_res <- 
  svm_workflow %>% 
  tune_grid(cv,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

svm_res %>% 
  collect_metrics()%>%
  arrange(desc(mean))

svm_res %>%
  collect_predictions()


autoplot(svm_res)


svm_res %>% 
  show_best(metric = "roc_auc")

svm_best <- 
  svm_res %>% 
  select_best(metric = "roc_auc")
svm_best

last_svm_workflow <- finalize_workflow(
  svm_workflow,
  svm_best
)



train_fit_svm <- fit(last_svm_workflow,rows_instockprice_train)

test_pred_svm <- predict(train_fit_svm, rows_instockprice_test, type = "prob") %>%
  bind_cols(predict(train_fit_svm, rows_instockprice_test)) %>%
  bind_cols(select(rows_instockprice_test, target_direction_10)) %>%
  glimpse()

test_pred_svm %>%
  sens(truth = target_direction_10,estimate = .pred_class)

test_pred_svm%>%
  spec(truth = target_direction_10,estimate = .pred_class)

test_pred_svm %>%
  roc_auc(truth = target_direction_10,estimate = .pred_buy)

test_pred_svm %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_10), 
               alpha = 0.5)+
  ggtitle("SVM")+
  xlab("Probability of Yes")

gb_mod <- boost_tree(
  trees = 500, 
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune(),                         ## step size
) %>% 
  set_engine("xgboost", num.threads = cores) %>% 
  set_mode("classification")

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), rows_instockprice_train),
  learn_rate(),
  size = 15
)

gb_workflow <- 
  workflow() %>% 
  add_model(gb_mod) %>% 
  add_recipe(stock_recipe)

set.seed(234)
xgb_res <- gb_workflow %>%
  tune_grid(
  cv,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE),
  metrics = metric_set(roc_auc)
)

xgb_res %>% 
  collect_metrics()%>%
  arrange(desc(mean))

xgb_res %>%
  collect_predictions()


autoplot(xgb_res)


xgb_res %>% 
  show_best(metric = "roc_auc")

xgb_best <- 
  xgb_res %>% 
  select_best(metric = "roc_auc")
xgb_best

last_gb_workflow <- finalize_workflow(
  gb_workflow,
  xgb_best
)


train_fit_gb<- last_gb_workflow %>%
  fit(rows_instockprice_train)

last_gb_workflow %>%
  pull_workflow_fit() %>%
  vip(geom = "point", num_features = 50)

test_pred_gb <- predict(train_fit_gb, rows_instockprice_test, type = "prob") %>%
  bind_cols(predict(train_fit_gb, rows_instockprice_test)) %>%
  bind_cols(select(rows_instockprice_test, target_direction_5)) %>%
  glimpse()

test_pred_gb %>%
  sens(truth = target_direction_5,estimate = .pred_class)

test_pred_gb%>%
  spec(truth = target_direction_5,estimate = .pred_class)

test_pred_gb %>%
  roc_auc(truth = target_direction_5,estimate = .pred_buy)

test_pred_gb %>%
  ggplot() +
  geom_density(aes(x = .pred_buy, fill = target_direction_5), 
               alpha = 0.5)+
  ggtitle("GB")+
  xlab("Probability of Yes")

saveRDS(train_fit_gb, "stock_pick_model_10day_xgb.rds")


test_pred_gb$higher_threshold <- if_else(test_pred_gb$.pred_buy>=0.65,1,0)

test_pred_gb$higher_threshold<- if_else(test_pred_gb$higher_threshold == 1, "buy","sell")

test_pred_gb$higher_threshold <- factor(test_pred_gb$higher_threshold, levels=c("buy","sell"))

test_pred_gb%>%
  spec(truth = target_direction_10,estimate = as.factor(higher_threshold))

test_pred_gb%>%
  sens(truth = target_direction_10,estimate = as.factor(higher_threshold))
test_pred_gb %>%
  conf_mat( truth = target_direction_10, estimate =higher_threshold)

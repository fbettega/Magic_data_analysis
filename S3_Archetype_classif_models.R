library("rjson")
# library(factoextra)
# library(distances)
# library(SSLR)
# library(FactoMineR)
# prévoir un regroupement automatique au dela de ce que j'ai fait
library(tidyverse)
library(tidymodels)
library(future)
library(baguette)
library('xgboost')
source("S2_Source_mtg_new_card.R")






################################################################################
######################## Model  ################################################
################################################################################
model_generic_grid_fun <- function(data, model, rerun_ml_fun,rerun_grid,grid = NULL, model_name, data_name) {
  print(model_name)
  print(Sys.time())
  if (rerun_ml_fun) {
    start.time <- Sys.time()
    
    if(model_name == "xgboost_tidy"){
      plan(sequential)
      
    }else{
    plan(multisession, workers = 10,gc = TRUE)
    }
    cv <- vfold_cv(data, v = 5,strata = Archetype, repeats = 3)
    rf_spec <- model
    
    
    recipe_model <- 
      recipe(Archetype ~ .,data = data
      ) %>% 
      update_role(id, new_role = "id variable")
    # Entraînement du modèle
    model_workflow <- workflow() %>%
      add_model(rf_spec) %>%
      add_recipe(recipe_model)
    
    
    if (
      rerun_grid |
      !file.exists(
        paste0(
          "data/ml_model/grid/Grid_search_",
          model_name, "_predict_archetype_", data_name, ".rds")
      )
    ) {
      print("rerun grid search")
      search_grid <- grid
      tree_res <- 
        model_workflow %>% 
        tune_grid(
          resamples = cv,
          grid = search_grid,
          control = control_grid(
            # verbose = TRUE,
            save_pred = TRUE
            ),
          metrics = metric_set(roc_auc,accuracy ,brier_class )
        ) 
      
      
      saveRDS(
        tree_res, 
        paste0(
          "data/ml_model/grid/Grid_search_",model_name,
          "_predict_archetype_", data_name, ".rds"
        )
      )
      
      
      grid_search_time <- Sys.time() - start.time
      print("Gridsearch end")
      print(grid_search_time)
    } else{
      tree_res <- read_rds(
        paste0(
          "data/ml_model/grid/Grid_search_",model_name, 
          "_predict_archetype_", data_name, ".rds"
        )
      )
    }
    # Spécification du modèle
    best_metrics <- tree_res %>% 
      select_best(metric = "roc_auc")
    
    final_model <- model_workflow %>% 
      finalize_workflow(best_metrics) %>% 
      fit(
        data
      )
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(model_name)
    print(time.taken)
    
    saveRDS(final_model, paste0("data/ml_model/", model_name, "_predict_archetype_", data_name, ".rds"))
    plan(sequential)
    # eco memory when all run
    final_model <- NULL
  } else {
    final_model <- readRDS(
      paste0(
        "data/ml_model/", model_name,
        "_predict_archetype_", data_name, ".rds"
      )
    )
  }
  return(final_model)
}
######################## Model  ################################################
# With caret ranger rf 4.7 - 4.9 h

json_parsing <- fromJSON(file = "MTGOArchetypeParser_20231227/data_FB_test.json")

# df_export_pre_60_filter
df_export_pre_60_filter <- json_parsing %>%
  as_tibble() %>%
  unnest_wider(Data) %>%
  unnest_wider(Archetype) %>%
  unnest_wider(ReferenceArchetype,
               names_sep = "_"
  ) %>%
  mutate(across(c(Wins, Losses, Draws), ~ as.numeric(.))) %>%
  mutate(matches = Wins + Losses + Draws) %>%
  mutate(Base_Archetype = Archetype) %>%
  mutate(Archetype = Archetype_agreger(Base_Archetype, Color)) %>%
  rownames_to_column(var = "id") %>%
  group_by(Archetype) %>%
  mutate(
    Archetype_count = n()
  ) %>%
  ungroup() %>%
  mutate(
    Date = as_datetime(Date),
    Week = as.integer(
      ceiling(
        difftime(
          Date,
          (min(Date) - as.difftime(1, unit = "days")),
          units = "weeks"
        )
      )
    )
  ) %>%
  mutate(Archetype = make.names(Archetype))


rm(json_parsing)
Not_60_cards_main <- df_export_pre_60_filter %>%
  unnest_longer(Mainboard) %>%
  unnest_wider(Mainboard, names_sep = "_") %>%
  group_by(id) %>%
  summarise(Number_of_main_deck_cards = sum(Mainboard_Count)) %>%
  filter(Number_of_main_deck_cards < 60)

df_export <- df_export_pre_60_filter %>%
  filter(id %notin% Not_60_cards_main$id)

rm(df_export_pre_60_filter, Not_60_cards_main)




# Otion one rf on raw data

rerun_ml <- TRUE  # FALSE

if(rerun_ml){
  saveRDS(df_export,"data/intermediate_result/base_classif_data.rds")
}

min_number_of_arch <- 20

# grouping conditionnels si low sample sizes a reflechir ex rogue delver faeries
# peut etre creatures combo

known_arch <- df_export %>%
  filter(!str_detect(Archetype, "_fallback|Unknown") & Archetype_count >=  min_number_of_arch) %>%
  prett_fun_classif("Mainboard") %>% 
  mutate(
    Archetype = as.factor(Archetype)
  )
################################################################################


################################################################################
rm(df_export)
rerun_grid_par <- FALSE


model_rf_base <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
) %>%
  set_engine("ranger") %>%
  set_mode("classification")

grid_rf_base <- grid_regular(
finalize(mtry(), known_arch),
                             trees(),
                             min_n(),
                             levels = 5)
# Grid aroud 9 hours
# pred Time difference of 3.75581 mins
Result_raw_rf <- model_generic_grid_fun(
  data = known_arch,
  model = model_rf_base,
  rerun_ml_fun = rerun_ml,#rerun_ml,
  rerun_grid = rerun_grid_par,
  grid = grid_rf_base,
  model_name = "RF_tidy",
  data_name = "raw_data"
)



model_regression_base <- multinom_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_engine("glmnet") %>%
  set_mode("classification")
# Grid aroud 11 hours
# pred Time difference of 20.1837 mins
# a regarder nombreuse error for small lambda
grid_regression_base <- grid_regular(
  penalty(),
  mixture(),
  levels = 5
)


Result_raw_regression <- model_generic_grid_fun(
  data = known_arch ,
  model = model_regression_base,
  rerun_ml_fun = rerun_ml,#rerun_ml,
  rerun_grid = rerun_grid_par,
  grid = grid_regression_base,
  model_name = "regression_tidy",
  data_name = "raw_data"
)
# # xgboost
# Grid search time 16.3h
# pred Time difference of 3.528562 mins
model_xgboost_base <- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune(),                         ## step size
) %>%
  set_engine(
    "xgboost",
    # tree_method = 'gpu_hist'
    tree_method = "hist",
    device = "cuda"
    ) %>%
  set_mode("classification")


grid_xgboost_base <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),                     ## first three: model complexity
  sample_size = sample_prop(),
  finalize(
    mtry(),
    known_arch
    ),
  learn_rate(),
  size = 30
)

Result_raw_xgboost <- model_generic_grid_fun(
  data = known_arch,
  model = model_xgboost_base,
  rerun_ml_fun = rerun_ml,#rerun_ml,
  rerun_grid = rerun_grid_par,
  grid = grid_xgboost_base,
  model_name = "xgboost_tidy",
  data_name = "raw_data"
)

# decision tree with bagtree
# Grid 5h
# pred 9.871191 mins
model_decision_tree_base <-
  bag_tree(
    cost_complexity = tune(),
    tree_depth = tune()
  ) %>%
  set_engine("rpart") %>%
  set_mode("classification")



grid_decision_tree_base <-
  grid_latin_hypercube(
    cost_complexity(),
    tree_depth(range = c(5, 30)),
    size = 50)


Result_raw_decision_tree <- model_generic_grid_fun(
  data = known_arch,
  model = model_decision_tree_base,
  rerun_ml_fun = rerun_ml,#rerun_ml,
  rerun_grid = rerun_grid_par,
  grid = grid_decision_tree_base,
  model_name = "decision_tree_tidy",
  data_name = "raw_data"
)


model_decision_tree_base_c5 <-
  bag_tree(
    min_n =  tune()
  ) %>%
  set_engine("C5.0") %>%
  set_mode("classification")

grid_decision_tree_base_c5 <-
  grid_latin_hypercube(
    min_n(),
    size = 25)



Result_raw_decision_treec5 <- model_generic_grid_fun(
  data = known_arch,
  model = model_decision_tree_base_c5,
  rerun_ml_fun = rerun_ml,#rerun_ml,
  rerun_grid = rerun_grid_par,
  grid = grid_decision_tree_base_c5,
  model_name = "decision_c5_tree_tidy",
  data_name = "raw_data"
)

# # # svm
# # 
# # ## grid > 42 h to slow
# model_svm_base <-
#   svm_rbf(
#     cost = tune(),
#     rbf_sigma = tune()
#   ) %>%
#   set_engine("kernlab") %>%
#   set_mode("classification")
# 
# 
# 
# grid_svm_base <-
#   grid_latin_hypercube(
#     cost(),
#     rbf_sigma(),
#     levels = 10)
# 
# Result_raw_svm <- model_generic_grid_fun(
#   data = known_arch ,
#   model = model_svm_base,
#   rerun_ml_fun = rerun_ml,#rerun_ml,
#   rerun_grid = rerun_grid_par,
#   grid = grid_svm_base,
#   model_name = "SVM_tidy",
#   data_name = "raw_data"
# )


# # KNN
# # grid 8h
## pred Time difference of 1.600804 hours
model_knn_base <- nearest_neighbor(
  neighbors = tune()
) %>%
  set_engine("kknn") %>%
  set_mode("classification")

grid_knn_base <- grid_regular(
  neighbors(range = c(10, 150)),
  levels = 5
)

Result_raw_knn <- model_generic_grid_fun(
  data = known_arch,
  model = model_knn_base,
  rerun_ml_fun = rerun_ml,#rerun_ml,
  rerun_grid = rerun_grid_par,
  grid = grid_knn_base,
  model_name = "knn_tidy",
  data_name = "raw_data"
)








# temp_not_selected_arch <-  df_export %>%
#   filter(!str_detect(Archetype, "_fallback|Unknown") & Archetype_count <  min_number_of_arch)
# 
# sort(table(temp_not_selected_arch$Archetype))
# sort(table(known_arch$Archetype))
# 
# sort(table(df_export$Archetype))






################################################################################
# temp opti svm

# # 
# 
# debug_df_index <- caret::createDataPartition(known_arch$Archetype, p = .1, list = FALSE) 
# debug_df <- known_arch[debug_df_index,]
# 
# 
# show_engines("svm_rbf")
# 
# 
# 
# model_svm_base <-
#   svm_rbf(
#     cost = tune(),
#     rbf_sigma = tune()
#   ) %>%
#   set_engine("kernlab") %>%
#   set_mode("classification")
# 
# 
# 
# grid_svm_base <-
#   grid_latin_hypercube(
#     cost(),
#     rbf_sigma(),
#     size = 10)
# 
# Result_raw_svm <- model_generic_grid_fun(
#   data = debug_df ,
#   model = model_svm_base,
#   rerun_ml_fun = rerun_ml,#rerun_ml,
#   rerun_grid = rerun_grid_par,
#   grid = grid_svm_base,
#   model_name = "debug_kernlab_SVM_tidy",
#   data_name = "debug_kernlab_raw_data"
# )
# 
# 
# model_svm_base_liquid <-
#   svm_rbf(
#     cost = tune(),
#     rbf_sigma = tune()
#   ) %>%
#   set_engine("liquidSVM") %>%
#   set_mode("classification")
# 
# 
# Result_raw_svm <- model_generic_grid_fun(
#   data = debug_df ,
#   model = model_svm_base_liquid,
#   rerun_ml_fun = rerun_ml,#rerun_ml,
#   rerun_grid = rerun_grid_par,
#   grid = grid_svm_base,
#   model_name = "debug_liquid_SVM_tidy",
#   data_name = "debug_liquid_raw_data"
# )
################################################################################
# 
# temp_test <- known_arch %>%
#   filter(Archetype %in% c('Storm','Food','Merfolk')) %>%
#   select_if(~ !is.numeric(.) || sum(.) != 0) %>%
#   mutate(Archetype = as.factor(as.character(Archetype)))
# 
# 
# debug_short <- model_generic_fun(
#   data = temp_test,
#   model = RF_sub_fun,
#   rerun_ml_fun = rerun_ml,
#   model_name = "temp",
#   data_name = "raw_data"
# )
# Time difference of 24.11757 secs


# plan(multisession)





# no grid search
# 
# debug_short <- model_generic_fun(
#   data = temp_test,
#   model = RF_sub_fun,
#   rerun_ml_fun = TRUE,#rerun_ml,
#   model_name = "temp",
#   data_name = "raw_data"
# )







################################################################################
# DATA
# Time difference of 7.489706 mins
# Result_raw_rf <- model_generic_fun(
#   data = known_arch,
#   model = rand_forest(
#   ) %>%
#     set_engine("ranger")%>%
#     set_mode("classification"),
#   rerun_ml_fun = rerun_ml,
#   model_name = "RF_tidy",
#   data_name = "raw_data"
# )

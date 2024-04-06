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

# A reflechir grouping conditionnel basé sur la quantité des arch cumulé
Archetype_agreger <- function(Archetype_to_agreg, color_agreg = NULL) {

  # color_group <- list(
  #   Delver = list(
  #     color = c("UR","UBR"),
  #     groupe = c("Murktide","UBlackX Control"),
  #     fallback = c("URedX Control")
  #     )
  #   )

  name_group <- list(
    # here because of debug grouping with self
    Yawgmoth = c("Yawgmoth"),
    `Hardened Scales` = c("Hardened Scales"),
    Prowess = c("Prowess"),
    Dredge = c("Dredge"),
    `Heliod Combo` = c("Heliod Combo","Soul Sisters"),
    `Glimpse Combo` = c("Glimpse Combo"),
    `Land Destruction` = c("Ensoul","Land Destruction"),
    `Living End` = c("Living End"),
    `Hammer Time` = c("Hammer Time"),
    Mill = c("Mill"),
    Shadow = c("Shadow"),
    Merfolk = c("Merfolk"),
    

    Devotion = c("Nykthos Leyline", "Green Devotion"),
    # ajouter point d'accroche pour red aggro
    `UWhiteX Control` = c(
      "Azorius Control _fallback",
       "Taking Turns"#,
    ),
    # Group UB base control and midrange in maccro archetype deck share with UW are in the UW groups
    
    `UBlackX Control` = c(
      "Dimir Control _fallback", 
      "Rogues"
    ),
    # Not murktide UR control
    `URedX Control` = c(
      "Izzet Control _fallback",
      "Reclamation",
      "Delver",
       "Faeries"
    ),
    
    `The Rock Midrange` = c("Saga Party"),
    
    `RWx aggro` = c(
      "Mono Red Aggro _fallback",
      "Mono Red Midrange _fallback",
      "Boros Aggro _fallback",
      "Boros Midrange _fallback",
      "Mono White Midrange _fallback",
      "Obosh Red"
      ),
    
    
    # groupe eldra avec eldra tron
    Tron = c("Eldrazi"),
    ############################ Réfléxion a mener #################################
    # groupe all deck blade a reflechir sur le fait de grouper avec blink
    Stoneblade = c(
      "Grief Blade",
      "Stoneblade",
      "Emeria Control"
    ),
    # Groupe breach value and murktide
    Murktide = c(
      "Murktide",
      "Breach Value"
    ),
    
    # Pack rhinos
    Footfalls = c("Footfalls 4 C", "Footfalls"),
    # Regroupement de tout les rakdos midrange et scam
    Scam = c(
      "Scam", "Rakdos Midrange _fallback",
      # "Mardu Midrange _fallback",
      "Skelementals"
    ),
    # Regroupement de mono B midrange et coffer
    `Coffers Control` = c(
      "Mono Black Midrange",
      "Mono Black Scam",
      "The Rack",
      "Coffers Control" # ,"Mono Black Midrange _fallback"
    ),
    
    
    
    # Merge all rock soupes together
    # `Golgarix Midrange` = c(
    #   "Golgari Midrange _fallback", "Jund Midrange _fallback",
    #   "Abzan Midrange _fallback", "Jund Aggro", "Jund Midrange"
    # ),
    # Merge the two combo breach potentiellement breach storm groupable avec les autres storms
    
    # `Breach combo` = c(
    #   "Breach Storm", "Grinding Breach"
    # ),
    # Disctuable merge goryo et reanimator
    Reanimator = c(
      "Reanimator",
      "Goryo Reanimator"
    ),
    # Regroupement de toutes les version tuant avec vaalakut, gros doutes sur l'inclusion de titanshift
    Scapeshift = c(
      "Scapeshift", "Guildpact Valakut", "Blue Scapeshift", "Titan Shift",
      "Niv To Light"
    ),
    `Amulet Titan` = c(
      "Timeless Lotus"
    ),
    # Merge les 2 versions de gob
    Goblins = c(
      "Goblin Whack",
      "Goblins"
    ),
    # Merge the two combo breach potentiellement breach with storm groupable
    # Ascendancy Combo et adnauseam groupe avec le reste même si pas vraiment storm
    Storm = c(
      "Breach Storm", "Grinding Breach",
      "Ad Nauseam", "Ascendancy Combo",
      "Grixis Storm", "Boros Storm", "Mono Red Storm",
      "Gifts Storm", "Twiddle Storm"
    ),
    # Regroupement de toutes les 4/5C soupe avec des betes
    `Omnath Control` = c(
      "Omnath Control", "Elementals", "Beans Cascade", "Saheeli Combo","Tameshi Bloom"
    ),
    # Regroupement de toutes les soupes sans lands
    Belcher = c("Belcher", "Oops All Spells"),
    # Meta groupes avec les soupes foods
    Food = c(
      "Asmo Food", "Manufactor Combo",
      "Crabvine","Hollow One"
    ),
    Zoo = c(
      "Blue Zoo", "Black Zoo", "Bushwhacker Zoo", "Domain Zoo"
    ),
    
    Convoke = c("Kuldotha Aggro"),
    `Free Spells` = c("Electro End","Free Spells"),
    `Combo Artifact` = c("Lantern","Combo Artifact","Affinity"),
    
    Creativity = c("Izzet Through Breach"),
    ############################################################################
    # not  enougth data 
    Enchantress = c("Enchantress","Enduring Ideal"),
    # `Kiki Jiki` = c("Kiki Jiki", "Kiki Chord"),
    Creature_combo = c(
      "Neobrand",
      "Vivien Combo",
      "Sacrifice",
      "Kiki Jiki", "Kiki Chord",
      "Neobrand",
      "Devoted Combo",
      "Kethis Combo",
      "Discover Combo"
    )
  )
  
  
  regex_group <- list(
    Creativity = "Creativity",
    # Pack tron
    Tron = "Tron$",
    Scam = "Scam$",
    # Merge tout les titan sauf titan shift
    `Amulet Titan` = "Titan$",
    # Groupement de tout les Burn quelquesois les couleurs
    Burn = "Burn",
    Blink = "Blink$",
    Convoke = "Convoke$",
    `Combo Artifact` = "Combo Artifact$",
    `Black Or Red Midrange` = "The Rock Midrange$"
  )
  
  nested_if_else <- ""
  ##############################################################################
  
  # Delver Gestion
  # Gestion du problème de l'absence des couleurs dans les archetypes des mathcups
  # for (i in seq_along(color_group)){
  #   nested_if_else <- paste0(
  #     nested_if_else,'if_else(Archetype_to_agreg == "',paste0(names(color_group[i])) ,'" & is.null(color_agreg[1]),"',
  #     color_group[[i]]$fallback,'",'
  #   )
  #   for (u in seq_along(color_group[[i]]$color )){
  #     nested_if_else <- paste0(
  #       nested_if_else,"if_else(Archetype_to_agreg == ",'"',paste0(names(color_group[i])),'"',
  #       " & color_agreg ==",'"', color_group[[i]]$color[[u]],'"',",",'"',
  #       color_group[[i]]$groupe[[u]],'",'
  #     )
  #   }
  #
  # }
  
  for (i in seq_along(name_group)) {
    nested_if_else <- paste0(
      nested_if_else, "if_else(Archetype_to_agreg %in% ", "c(", paste0('"', name_group[[i]], '"', collapse = ","), "),", '"',
      names(name_group[i]), '",'
    )
  }
  
  for (i in seq_along(regex_group)) {
    nested_if_else <- paste0(
      nested_if_else, 'if_else(str_detect(Archetype_to_agreg,"',
      regex_group[[i]], '"', "),", '"',
      names(regex_group[i]), '",'
    )
  }
  
  
  
  
  
  number_of_nested_ifelse <-
    # length(color_group) +
    # lapply(color_group, function(x){
    #   length(x$color)
    # }) %>% unlist() %>% sum() +
    length(name_group) +
    length(regex_group)
  
  
  
  nested_if_else <- paste0(nested_if_else, "Archetype_to_agreg", paste0(rep(")", number_of_nested_ifelse), collapse = ""))
  
  # nested_if_else <- paste0(nested_if_else,"Archetype_to_agreg",paste0(rep(")",length(name_group)),collapse = "") )
  
  
  res <- eval(parse(text = nested_if_else))
  
  return(res)
}



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
    plan(multisession, workers = 5,gc = TRUE)
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
            save_pred = TRUE,
            verbose = TRUE
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
# a regarder nombreuse error for small lambda
grid_regression_base <- grid_regular(
  penalty(),
  mixture(),
  levels = 5
)


Result_raw_regression <- model_generic_grid_fun(
  data = known_arch %>%
    mutate(across(starts_with("color_"),
                  ~as.numeric(.))
    ),
  model = model_regression_base,
  rerun_ml_fun = rerun_ml,#rerun_ml,
  rerun_grid = rerun_grid_par,
  grid = grid_regression_base,
  model_name = "regression_tidy",
  data_name = "raw_data"
)
# # KNN
# # grid 8h
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


# # xgboost
# The tree method `gpu_hist` is deprecated since 2.0.0. To use GPU training, set the `device` parameter to CUDA instead.
# Change   E.g. tree_method = "hist", device = "cuda"
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
    tree_method = 'gpu_hist'
    ) %>%
  set_mode("classification")

# 16.3h
grid_xgboost_base <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),                     ## first three: model complexity
  sample_size = sample_prop(),
  finalize(
    mtry(),
           known_arch %>% select(-starts_with("color_"))),

  learn_rate(),
  size = 30
)

Result_raw_xgboost <- model_generic_grid_fun(
  data = known_arch %>% select(-starts_with("color_")),
  model = model_xgboost_base,
  rerun_ml_fun = rerun_ml,#rerun_ml,
  rerun_grid = rerun_grid_par,
  grid = grid_xgboost_base,
  model_name = "xgboost_tidy",
  data_name = "xgboost_data"
)

# decision tree with bagtree
# Grid 5h
model_decision_tree_base <-
  # decision_tree(
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



# svm
model_svm_base <- 
  svm_rbf(
    cost = tune(),
    rbf_sigma = tune()
  ) %>%
  set_engine("kernlab") %>% 
  set_mode("classification")



grid_svm_base <-  
  grid_regular(
    cost(),
    rbf_sigma(),
    levels = 5)

Result_raw_svm <- model_generic_grid_fun(
  data = known_arch ,
  model = model_svm_base,
  rerun_ml_fun = rerun_ml,#rerun_ml,
  rerun_grid = rerun_grid_par,
  grid = grid_svm_base,
  model_name = "SVM_tidy",
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

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
library("xgboost")
source("sources/S2_Source_mtg_new_card.R",local = TRUE)

# format_param <- format_date_en_cours$format_param
# date_cut <- format_date_en_cours$date_cutoff


################################################################################
# just a list of magic colors
color_comb_list <- c(
  Corlorless = "C",
  `Mono White` = "W",
  `Mono Red` = "R",
  `Mono Blue` = "U",
  `Mono Black` = "B",
  `Mono Green` = "G",
  
  Boros = "WR",
  Selesnya = "WG",
  Dimir = "UB",
  Gruul = "RG",
  Rakdos = "BR",
  Azorius = "WU",
  Orzhov = "WB",
  Golgari = "BG",
  Izzet = "UR",
  Simic = "UG",
  
  Jund = "BRG",
  Mardu = "WBR",
  Jeskai = "WUR",
  Grixis = "UBR",
  Naya = "WRG",
  Sultai = "UBG",
  Abzan = "WBG",
  Esper = "WUB",
  Bant = "WUG",
  Temur = "URG",
  
  WUBG = "WUBG", # Not-R
  WUBR = "WUBR", # Not-G
  WURG = "WURG", # Not-B
  WBRG = "WBRG", # Not-U
  UBRG = "UBRG", # Not-W
  
  `5 Color` = "WUBRG"
)

# install.packages("ranger")

# download from https://github.com/dmlc/xgboost/releases
# install.packages("xgboost_r_gpu_win64_82d846bbeb83c652a0b1dff0e3519e67569c4a3d.tar.gz", repos = NULL, type="source")
# install.packages("glmnet")

rerun_ml <- TRUE # TRUE FALSE
rerun_grid_par <- FALSE
options(future.globals.maxSize = 40097152000)

################################################################################
######################## Model  ################################################
################################################################################
model_generic_grid_fun <- function(data,
                                   model,
                                   rerun_ml_fun,
                                   rerun_grid,
                                   grid = NULL,
                                   model_name,
                                   data_name,
                                   naming_format = format_param,
                                   number_of_core = 1) {
  print(model_name)
  print(Sys.time())
  if (rerun_ml_fun) {
    start.time <- Sys.time()

    if (model_name == "xgboost_tidy") {
      plan(sequential)
    } else {
      plan(multisession,
        workers = number_of_core
        # ,gc = TRUE
      )
    }

    cv <- vfold_cv(data, v = 5, strata = Archetype, repeats = 3)
    rf_spec <- model


    recipe_model <-
      recipe(Archetype ~ ., data = data) %>%
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
            model_name, "_predict_archetype_", data_name, ".rds"
          )
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
          metrics = metric_set(roc_auc, accuracy, brier_class)
        )


      saveRDS(
        tree_res,
        paste0(
          "data/ml_model/grid/Grid_search_", model_name,
          "_predict_archetype_", data_name, ".rds"
        )
      )


      grid_search_time <- Sys.time() - start.time
      print("Gridsearch end")
      print(grid_search_time)
      # close session and reinit
      # plan(sequential)
      #
      # plan(multisession, workers = 8,gc = TRUE)
    } else {
      tree_res <- read_rds(
        paste0(
          "data/ml_model/grid/Grid_search_", model_name,
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

    saveRDS(final_model, paste0("data/ml_model/", model_name,
                                "_predict_archetype_",naming_format,"_", data_name, ".rds"))
    # stopCluster(cl = cl)
    plan(sequential)

    # eco memory when all run
    final_model <- NULL
  } else {
    final_model <- readRDS(
      paste0(
        "data/ml_model/", model_name,
        "_predict_archetype_",naming_format,"_", data_name, ".rds"
      )
    )
  }
  return(final_model)
}
######################## Model  ################################################
# With caret ranger rf 4.7 - 4.9 h


# browser()

if(length(  list.files(
  paste0("ArchetypeParser/MTGOFormatData_FB/Formats/",format_param,"/Fallbacks/"),
  full.names = TRUE
)) > 0){
fall_back_list <- lapply(
  list.files(
    paste0("ArchetypeParser/MTGOFormatData_FB/Formats/",format_param,"/Fallbacks/"),
    full.names = TRUE
  ),
  function(x) {
    base_parsing_res <- fromJSON(
      file = x
    ) %>% 
      purrr::list_modify("CommonCards" = NULL) %>% 
      purrr::compact()
  }
) %>% 
  bind_rows() %>% 
  mutate(
    Name = str_remove(Name,"^Generic")
  )

}

if(  length(list.files(
  paste0("ArchetypeParser/MTGOFormatData_FB/Formats/",format_param,"_FB/Archetypes/"),
  full.names = TRUE
)) > 0 ){
Archetype_parser_list <- lapply(
  list.files(
    paste0("ArchetypeParser/MTGOFormatData_FB/Formats/",format_param,"_FB/Archetypes/"),
    full.names = TRUE
  ),
  function(x) {
    base_parsing_res <- fromJSON(
      file = x
    ) %>% 
      purrr::list_modify("Conditions" = NULL) %>% 
      purrr::compact()
    
    base_archetype <- base_parsing_res %>% 
      purrr::list_modify("Variants" = NULL) %>% 
      purrr::compact() %>% 
      as_tibble() %>% 
      mutate(
        file = str_extract(x,"(?<=Archetypes/)(.+)(?=\\.json)"),
        type = "base",
        parser_base_arch = Name,
        .before = 1
      )
    if (!is.null(base_parsing_res$Variants) ) {
      variant_list <- lapply(base_parsing_res$Variants, function(y){
        y %>% 
          purrr::list_modify("Conditions" = NULL) %>% 
          purrr::compact() %>% 
          as_tibble() %>% 
          mutate(
            file = str_extract(x,"(?<=Archetypes/)(.+)(?=\\.json)"),
            type = "variant",
            parser_base_arch = base_archetype$Name,
            .before = 1
          )
      }) %>% 
        bind_rows()
      
      res <- rbind(base_archetype,variant_list)
      
    } else {
      res <- base_archetype
    }
    
    return(res)
  }
) %>% 
  bind_rows() %>% 
  mutate(
    join_name =  str_remove_all(str_remove_all(Name,"\\s+"),"Generic")
  ) 

}


# acdd exeception if fallback name = archetype name 
# if(any(Archetype_parser_list$parser_base_arch %in%  fall_back_list$Name)){
#   stop(
#     paste0("Archetype name in fallback : ", 
#            Archetype_parser_list$parser_base_arch[Archetype_parser_list$parser_base_arch %in%  fall_back_list$Name])
#     )
# }



json_parsing <- fromJSON(file = paste0(
  "data/intermediate_result/parser_outpout/",
  format_date_en_cours$format_param,"_",
  "data.json"
  )
                         )

# df_export_pre_60_filter
df_export_pre_60_filter <- json_parsing %>%
  as_tibble() %>%
  unnest_wider(Data) %>%
  # temp filter for useless buged tournament 
  # filter(
  #   TournamentFile  !=  'adri-birthday-modern-68213-2024-10-03'
  # ) %>% 
  unnest_wider(Archetype) %>%
  unnest_wider(ReferenceArchetype,
    names_sep = "_"
  ) %>%
  mutate(across(c(Wins, Losses, Draws), ~ as.numeric(.))) %>%
  mutate(
    matches = Wins + Losses + Draws,
    join_arch = str_remove_all(Archetype,"\\s+")
    ) %>%  
  
  # handle variation in naming introduce by parser : remove generic and add space
  left_join(
    Archetype_parser_list %>% select(Name,join_name,parser_base_arch,type),
    by = join_by(join_arch == join_name) 
  ) %>%  
  select(-join_arch,-Name) %>% 
  mutate(
    type = ifelse(Archetype == "Unknown","Unknown",type),
    type = ifelse(is.na(type),"Fallback",type),
    parser_base_arch = ifelse(
      type == "Fallback",
      trimws(
        str_remove(
          Archetype,
          paste0(names(color_comb_list),collapse = "|")
          )
        ),                        
      parser_base_arch                 ),
    Base_Archetype = Archetype) %>%
  # mutate(Archetype = Archetype_agreger(Base_Archetype, Color)) %>%
  # mutate(Base_Archetype = ReferenceArchetype_Archetype) %>%
  # mutate(Archetype = ReferenceArchetype_Archetype) %>%
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
  ) 
# %>%
#   mutate(Archetype = make.names(Archetype))







format_bann_cards <- tryCatch(
  scryr::scry_cards(paste0("banned:",format_param)),
                              error = function(e) {
                                data.frame(name = "No card bann in format")
                              }
  )



df_export_pre_60_filter_remove_bann <- Ban_patch(
  df = df_export_pre_60_filter,
  vec_of_ban_cards = c(format_bann_cards$name)
)  %>% 
  # add rules if a fall back is more than 1% meta it become an archetype
  mutate(type = ifelse(type == "Fallback" & Archetype_count/nrow(.) > 0.01,"variant",type))



################################################################################



# a <- df_export_pre_60_filter_remove_bann %>%
#   select(  Archetype,Color,parser_base_arch,type,
#            Base_Archetype,
#            Archetype_count) %>% 
#   distinct(Base_Archetype,.keep_all = TRUE) %>% 
#   mutate(percent = Archetype_count*100/sum(Archetype_count)) %>% 
#   group_by(parser_base_arch) %>% 
#   mutate(n_base_parser = sum(Archetype_count,na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   mutate(percent_tot = n_base_parser*100/sum(Archetype_count))











################################################################################
rm(json_parsing)
Not_60_cards_main <- df_export_pre_60_filter_remove_bann %>%
  unnest_longer(Mainboard) %>%
  unnest_wider(Mainboard, names_sep = "_") %>%
  group_by(id) %>%
  summarise(Number_of_main_deck_cards = sum(Mainboard_Count) ) %>%
  mutate(Valide_deck = Number_of_main_deck_cards >= 60) 





df_export <- df_export_pre_60_filter_remove_bann %>%
  left_join(Not_60_cards_main, by = c("id")) %>% 
  mutate(
    Valide_deck = if_else(
      is.na(Valide_deck),
      FALSE,Valide_deck),
    Number_of_main_deck_cards = if_else(
      is.na(Number_of_main_deck_cards),0,
      Number_of_main_deck_cards)    
         ) %>%
  group_by(Archetype) %>%
  mutate(
    Archetype_count = n()
  ) %>%
  ungroup()
  # filter(id %notin% Not_60_cards_main$id)

rm(df_export_pre_60_filter, Not_60_cards_main,df_export_pre_60_filter_remove_bann)






saveRDS(df_export, paste0("data/intermediate_result/",format_param,"_base_classif_data.rds"))  

# Otion one rf on raw data

if (rerun_ml) {
  saveRDS(df_export, paste0("data/intermediate_result/temp_",format_param,"_next_time_model_rerun_data.rds"))
  unlink(paste0("data/intermediate_result/",format_param,"not_train_col.rds"))
  }





min_number_of_arch <- 50 #20

# grouping conditionnels si low sample sizes a reflechir ex rogue delver faeries
# peut etre creatures combo

known_arch <- df_export %>%
  # filter(!str_detect(Archetype, "_fallback|Unknown") & Archetype_count >= min_number_of_arch) %>%
  filter( type != "Fallback" & type != "Unknown"  & Archetype_count >= min_number_of_arch) %>%
  prett_fun_classif("Mainboard") %>%
  mutate(
    Archetype = as.factor(Archetype)
  )
################################################################################

if (!rerun_ml) {
  previous_data <- readRDS(paste0("data/intermediate_result/",format_param,"_base_classif_data.rds"))   %>%
    filter( type != "Fallback" & type != "Unknown"  & Archetype_count >= min_number_of_arch) %>%
    prett_fun_classif("Mainboard")
  
  not_train_col <- colnames(known_arch)[colnames(known_arch) %notin% colnames(previous_data)] %>% 
    saveRDS(paste0("data/intermediate_result/",format_param,"not_train_col.rds"))
}
################################################################################
rm(df_export)


# change search strategy : https://uliniemann.com/blog/2022-07-04-comparing-hyperparameter-tuning-strategies-with-tidymodels/


# Grid aroud 28 hours
# pred Time difference of 10.87117 mins
model_decision_tree_base_c5 <-
  bag_tree(
    min_n = tune()
  ) %>%
  set_engine("C5.0") %>%
  set_mode("classification")

grid_decision_tree_base_c5 <-
  grid_space_filling(
    min_n(),
    size = 25
  )




Result_raw_decision_treec5 <- model_generic_grid_fun(
  data = known_arch,
  model = model_decision_tree_base_c5,
  rerun_ml_fun = rerun_ml, # rerun_ml,
  rerun_grid = rerun_grid_par,
  grid = grid_decision_tree_base_c5,
  model_name = "decision_c5_tree_tidy",
  data_name = "raw_data",
  number_of_core = 5
)




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
  levels = 5
)

# Grid aroud 11 hours
# pred Time difference of 33.02217 mins
# a regarder nombreuse error for small lambda
Result_raw_rf <- model_generic_grid_fun(
  data = known_arch,
  model = model_rf_base,
  rerun_ml_fun = rerun_ml, # rerun_ml,
  rerun_grid = rerun_grid_par,
  grid = grid_rf_base,
  model_name = "RF_tidy",
  data_name = "raw_data",
  number_of_core = 10
)



model_regression_base <- multinom_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

grid_regression_base <- grid_regular(
  penalty(),
  mixture(),
  levels = 5
)


Result_raw_regression <- model_generic_grid_fun(
  data = known_arch,
  model = model_regression_base,
  rerun_ml_fun = rerun_ml, # rerun_ml,
  rerun_grid = rerun_grid_par,
  grid = grid_regression_base,
  model_name = "regression_tidy",
  data_name = "raw_data",
  number_of_core = 12
)
# # xgboost
# Grid search time 16.3h
# pred Time difference of 4.765273 mins
model_xgboost_base <- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(), ## first three: model complexity
  sample_size = tune(), mtry = tune(), ## randomness
  learn_rate = tune(), ## step size
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
  loss_reduction(), ## first three: model complexity
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
  rerun_ml_fun = rerun_ml, # rerun_ml,
  rerun_grid = rerun_grid_par,
  grid = grid_xgboost_base,
  model_name = "xgboost_tidy",
  data_name = "raw_data",
  number_of_core = 1
)







# # KNN
# # grid 8h
## pred Time difference of 2.181388 hours
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
  rerun_ml_fun = rerun_ml, # rerun_ml,
  rerun_grid = rerun_grid_par,
  grid = grid_knn_base,
  model_name = "knn_tidy",
  data_name = "raw_data",
  number_of_core = 12
)





# length(unique(a$Archetype))

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



# temp_not_selected_arch <-  df_export %>%
#   filter(!str_detect(Archetype, "_fallback|Unknown") & Archetype_count <  min_number_of_arch)
#
# temp_fall_back <- df_export %>%
#   filter(str_detect(Archetype, "_fallback"))
#
# sort(table(temp_not_selected_arch$Archetype))
# sort(table(known_arch$Archetype))
#
# sort(table(df_export$Archetype))
#
# sort(table(temp_fall_back$Archetype))




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

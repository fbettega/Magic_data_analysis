library("rjson")
# library(factoextra)
# library(distances)
# library(SSLR)
# library(FactoMineR)
# prévoir un regroupement automatique au dela de ce que j'ai fait
library(tidyverse)
library(tidymodels)
library(kknn)
library(future)

source("sources/S2_Source_mtg_new_card.R",local = TRUE)

# scale_this <- function(x){
#   (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
# }




No_pred <- FALSE #TRUE FALSE

################################################################################
my_table_from_model <- function(pred_class,pred_proba,data,string){
  # Gestion d'un bug a la con sur le predict
  predict_Result <- cbind(data,pred_class ) %>% 
    select(id,Archetype,.pred_class) %>% 
    left_join(
      cbind(
        id = data$id,
        pred_proba
        
      ) %>%
        pivot_longer(-id,
                     names_to = ".pred_class",
                     values_to = "proba"
        ) %>% 
        mutate(.pred_class = str_remove(.pred_class,"\\.pred_")),
      by = c("id", ".pred_class")
    ) %>% 
    rename(!!paste0(string,"_pred") := `.pred_class`,
           !!paste0(string,"_proba") := proba)
  
  return(predict_Result)
}



bind_proba_with_list_of_model <- function(model_list,
                                          pred_data,base_data,
                                          naming_format = format_param){
  list_of_table <- lapply(
    seq_along(model_list),
    function(x){
      print(x)
      plan(multisession, workers = 7
           #,gc = TRUE
      )
      model <- readRDS(
        paste0(
          "data/ml_model/", model_list[[x]][1],
          "_predict_archetype_",naming_format,"_", model_list[[x]][2], ".rds"
        )
      )
      predict_class <- model %>%
        predict(pred_data, type = "class")
      predict_proba <- model %>%
        predict(pred_data, type =  "prob")
      plan(sequential)
      return(
        list(
          class = predict_class,
           proba = predict_proba
          )
        )
      } 
  )
  
  Best_pred_using_vote <- 
    cbind(
      id = pred_data$id,
    (lapply(
    list_of_table, function(x){
    x$proba %>% 
      mutate_all(~./length(list_of_table))
      # mutate_all(~scale_this(.)/length(list_of_table))
  }
  ) %>% 
    reduce(`+`))
    ) %>% 
     pivot_longer(-c(id)) %>% 
     group_by(id) %>% 
     filter(value == max(value)) %>% 
    mutate(name = str_remove(name,"\\.pred_")) %>%
    left_join(
      base_data %>%
        select(id, Archetype , AnchorUri),
      by = "id"
    )
  
  table_merge <- lapply(
    seq_along(list_of_table),function(x){
    
    my_table_from_model(
      pred_class = list_of_table[[x]]$class,
      pred_proba = list_of_table[[x]]$proba,
      data = pred_data ,
      string = names(model_list[x])
    )
    }
    ) %>% 
    reduce(inner_join, by = c("id","Archetype")) %>%
    left_join(
      base_data %>%
        select(id, Archetype,AnchorUri),
      by = "id"
    )
  
  return(
    list(
    table_each_model_pred = table_merge,
    table_vote_pred = Best_pred_using_vote
    )
    )
}


# Otion one rf on raw data
df_export <- read_rds( paste0("data/intermediate_result/",format_param,"_base_classif_data.rds"))
  

if (No_pred){
  write_rds(df_export, paste0("data/",format_param,"_data_meta_en_cours.rds"))
}else {

if(file.exists(paste0("data/intermediate_result/",format_param,"not_train_col.rds"))) {
  
  exclude_col <- read_rds(paste0("data/intermediate_result/",format_param,"not_train_col.rds"))
} else {
  exclude_col <- character()
}

# min_number_of_arch <- 20
min_number_of_arch <- 50
known_arch <- df_export %>%
  # filter(!str_detect(Archetype, "_fallback|Unknown") & Archetype_count >=  min_number_of_arch) %>%
  filter( type != "Fallback" & type != "Unknown"  & Archetype_count >= min_number_of_arch)  %>%
  prett_fun_classif("Mainboard") %>% 
  mutate(
    Archetype = as.factor(Archetype)
    ) %>% 
  select(-any_of(exclude_col))




fall_back_df_temp <- df_export %>%  
  filter(Date >= as.Date(date_cut,tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))
         ) %>%
  # filter(str_detect(Archetype, "_fallback|Unknown") ##| Archetype_count < min_number_of_arch
  filter( type == "Fallback" | type == "Unknown"  | Archetype_count < min_number_of_arch)  %>%
  prett_fun_classif("Mainboard") %>%
  ungroup() 

colone_not_present_in_fallback <-
  colnames(known_arch)[colnames(known_arch) %notin% colnames(fall_back_df_temp)]


fall_back_df <- fall_back_df_temp %>%
  select(any_of(colnames(known_arch))) %>% 
  cbind(
    data.frame(
      matrix(0,
             ncol = length(colone_not_present_in_fallback),
             nrow = nrow(.)
      )
    ) %>%
      rename_all(~colone_not_present_in_fallback)
  )

rm(fall_back_df_temp)





list_of_model_for_prediction <- list(
  regression = c("regression_tidy", "raw_data"),
  rando_forest = c("RF_tidy","raw_data"),
  knn = c("knn_tidy", "raw_data"),
  decision_tree_c5 = c("decision_c5_tree_tidy","raw_data"),
  xgboost = c("xgboost_tidy", "raw_data")
)

model_res_table <- bind_proba_with_list_of_model(model_list = list_of_model_for_prediction,
  pred_data = fall_back_df,
  base_data = df_export
  )


write_rds(model_res_table,paste0("data/intermediate_result/",format_param,"_result_pred.rds"))
Voting_df <- model_res_table$table_vote_pred
pred_table_df <- model_res_table$table_each_model_pred



Voting_df_upper_treshold <- Voting_df %>% filter(value > 0.3) 


DF_post_archetype_pred <- df_export %>%  
  filter(
    Date >= as.Date(date_cut,tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))
         ) %>% 
  left_join(
    Voting_df_upper_treshold %>% 
      select(id,name),
    by = "id") %>% 
  mutate(
    Archetype = str_replace_all(if_else(is.na(name),Archetype,name),"\\."," ") 
    ) %>% 
  select(-name) 


sort (table(DF_post_archetype_pred$Archetype))
  



write_rds(DF_post_archetype_pred, paste0("data/",format_param,"_data_meta_en_cours.rds"))





}



# # # # explore model 
# grid_debug <- read_rds(
#     paste0(
#       "data/ml_model/grid/Grid_search_",
#       "decision_c5_tree_tidy",# decision_c5_tree_tidy "regression_tidy" ,# modif "decision_tree_tidy" RF_tidy "xgboost_tidy" knn_tidy
#       "_predict_archetype_",
#       "raw_data", # modif
#       ".rds"
#     )
#   )
# #
# #
# a <- grid_debug %>%
#   collect_metrics()

# regression_tidy 0.9995339
# RF_tidy 0.9997542
# knn_tidy 0.99405553
# decision_tree_tidy  0.95501741
# xgboost 0.999818309




# model_debug <- readRDS(
#   paste0(
#     "data/ml_model/",
#     "decision_tree_tidy",#"regression_tidy" ,# modif "decision_tree_tidy" RF_tidy
#     "_predict_archetype_",
#     "raw_data", # modif
#     ".rds"
#   )
# )
# 
# 
# 
# 
# model_debug2 <- readRDS(
#   paste0(
#     "data/ml_model/",
#     "decision_tree_tidy" ,# modif "decision_tree_tidy"
#     "_predict_archetype_",
#     "raw_data", # modif
#     ".rds"
#   )
# )
# 
# 
# 
# 
# 
# 

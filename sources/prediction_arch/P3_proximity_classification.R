library("rjson")
 # library(factoextra)
# library(distances)
# library(SSLR)
# library(FactoMineR)
# prévoir un regroupement automatique au dela de ce que j'ai fait
library(tidyverse)
library(tidymodels)
library(future)
# library(baguette)
# library("xgboost")
source("sources/MTG_function_base_FB/global_mtg_sources.R")
source("sources/S2_Source_mtg_new_card.R",local = TRUE)

# library("ggdendro")
# format_param <- "Modern"
# format_param <- format_date_en_cours$format_param


grouping_close_df <- function(
    df = Grouping_dist_matrix
){
  
  already_select_arch = data.frame(
    Archetype.x = character(),
    Archetype.y = character()
  )
  
  
  df_iteration <- df 
  while(nrow(df_iteration) > 0 ) {
    
    # print(nrow(df_iteration))
    
    already_select_arch <- bind_rows(
      already_select_arch,
      data.frame(
        Archetype.x = df_iteration$Archetype.x[1],
        Archetype.y = df_iteration$Archetype.y[1]
      )
    )
    df_iteration <-  
      df %>%  
      filter(
        Archetype.x %notin% c(already_select_arch$Archetype.x,already_select_arch$Archetype.y),
        Archetype.y %notin% c(already_select_arch$Archetype.x,already_select_arch$Archetype.y)
      )
  }
  return(already_select_arch)
}





# df_export_pre_60_filter
df_export_pre_60_filter <- read_rds(paste0("data/intermediate_result/proximity_data/",format_param,"pre_proximity.rds"))


Not_60_cards_main <- df_export_pre_60_filter %>%
  unnest_longer(Mainboard) %>%
  unnest_wider(Mainboard, names_sep = "_") %>%
  group_by(id) %>%
  summarise(Number_of_main_deck_cards = sum(Mainboard_Count) ) %>%
  mutate(Valide_deck = Number_of_main_deck_cards >= 60) 






df_export <- df_export_pre_60_filter %>%
  # left_join(Not_60_cards_main, by = c("id")) %>% 
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

colname_deck_list <- "Mainboard"

data_wide_total <- df_export  %>%
  filter(Valide_deck) %>% 
  select(
    id, Archetype, Color, any_of(colname_deck_list) # ,Sideboard
  ) %>%
  unnest_longer(!!colname_deck_list) %>%
  unnest_wider(!!colname_deck_list, names_sep = "_") %>%
  select(-Color) %>%
  group_by(id, !!rlang::sym(paste0(colname_deck_list, "_CardName"))) %>%
  mutate(!!rlang::sym(paste0(colname_deck_list, "_Count")) := sum(!!rlang::sym(paste0(colname_deck_list, "_Count")))) %>%
  distinct()  %>%
  mutate(!!rlang::sym(paste0(colname_deck_list, "_CardName")) := 
           sanitize_string(!!rlang::sym(paste0(colname_deck_list, "_CardName"))                                                                 )
  ) %>% 
  # fin de la gestion
  # cards matrix wild
  pivot_wider(
    names_from = !!rlang::sym(paste0(colname_deck_list, "_CardName")),
    # names_prefix = "Mainboard_",
    values_from = !!rlang::sym(paste0(colname_deck_list, "_Count")),
    values_fill = 0,
    names_repair = "universal_quiet"
  ) %>% 
  mutate(
    Archetype = as.factor(Archetype)
  )
rm(df_export)

tictoc::tic("test")


# 1. Supprimer temporairement la colonne `id` et trouver les lignes distinctes
data_dedup <- data_wide_total %>%
  ungroup() %>% 
  select(-id) %>%
  distinct() %>%
  mutate(dedup_key = as.character(row_number()))  # Ajoute une clé unique

# 2. Rejoindre cette version dédupliquée à la dataframe de départ
data_wide <- data_wide_total  %>%
  left_join(data_dedup, by = colnames(data_wide_total)[!colnames(data_wide_total) %in% "id"])



Dist_matrice_basale_from_wide <- as.matrix(
  data_dedup %>%
    filter(Archetype != "Unknown") %>% 
    column_to_rownames("dedup_key") %>%
    select(-Archetype) 
)




dist_test <- proxyC::simil(
  Dist_matrice_basale_from_wide,
  method = "ejaccard"
)


long_dist_mat <- (
  1 - as.matrix(dist_test)
                  ) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  right_join(data_dedup %>%
              select(dedup_key,Archetype) %>% 
              mutate(Archetype = as.character(Archetype)
                     ),by = join_by(rowname  == dedup_key)
            ) %>%
  left_join(data_dedup %>%
              select(dedup_key,Archetype) %>% 
              mutate(Archetype = as.character(Archetype)
              ) ,by = join_by(name  == dedup_key)) %>%
  filter(rowname != name) # %>%   select(-c(rowname,name))



rm(dist_test)
df_archetype_count <- data_wide %>%
  mutate(
    Archetype = as.character(Archetype)
    ) %>% 
  group_by(Archetype) %>% 
  summarise(
    count = n()
  )


rm(data_wide)
rm(data_dedup)

grouping_cards_recursive <- function(
    dist_mat_fun,
    grouping_df_base_fun = data.frame(
      Archetype_name = character(),
      value = character()
    ),
    Archetype_count_fun = df_archetype_count,
    n = 1
){


  print(n)

  if(n > 50) {system("shutdown -s")}

  summarise_dist_jaccard <- dist_mat_fun %>%
    group_by(Archetype.x ,Archetype.y) %>%
    summarize(
      count = n(),
      Q2 = quantile(value ,0.5),
      .groups = "drop"
    ) %>%
    left_join(
      dist_mat_fun %>%
                filter(Archetype.x == Archetype.y) %>%
                select(-Archetype.y) %>%
                group_by(Archetype.x) %>%
                summarize(
                  Q3 = quantile(value ,0.75),
                  .groups = "drop"
                ) %>%
                rename_all(~paste0("self_",.)) %>%
                rename(Archetype = self_Archetype.x) ,
              by = join_by(Archetype.x == Archetype)
      ) %>%
    mutate(
      Q3_compare_Median = self_Q3 - Q2
    ) %>%
    filter( Archetype.x != Archetype.y   ) %>%
    filter(Q3_compare_Median >= 0) %>%
    arrange(desc(Q3_compare_Median))



  if(nrow(summarise_dist_jaccard) > 0){
  print(paste0("Remaining archetype to proximity join : ",nrow(summarise_dist_jaccard)))

  archetype_to_group <- grouping_close_df(summarise_dist_jaccard) %>%
    left_join(Archetype_count_fun,by = join_by(Archetype.x == Archetype)) %>%
    rename(count.x = count) %>%
    left_join(Archetype_count_fun,by = join_by(Archetype.y == Archetype)) %>%
    rename(count.y = count) %>%
    mutate(Archetype_name = ifelse(count.x < count.y,
                                   Archetype.y,
                                   Archetype.x
    )) %>%
    select(-starts_with("count")) %>%
    pivot_longer(-Archetype_name) %>%
    select(-name) %>%
    filter(Archetype_name != value)

  grouping_df_res <- bind_rows(grouping_df_base_fun,archetype_to_group)



  dist_mat_fun_res <- dist_mat_fun %>%
    filter(
      Archetype.x %notin% grouping_df_res$value,
      Archetype.y %notin% grouping_df_res$value
           )
  # Nettoyage mémoire explicite
  rm(dist_mat_fun)
  rm(summarise_dist_jaccard)
  gc(verbose = FALSE)

    res <- grouping_cards_recursive(
      dist_mat_fun = dist_mat_fun_res ,
      grouping_df_base_fun = grouping_df_res,
      Archetype_count_fun = df_archetype_count,
      n = n + 1
      )
   } else{
    res <- grouping_df_base_fun

  }


return(res)


}

gc()

resulting_distance_mat_with_all_group <- grouping_cards_recursive(
  dist_mat_fun = long_dist_mat
)  %>% 
  rename(Archetype_proximity = Archetype_name )
gc()


res_proximity_joins <- df_export_pre_60_filter %>% 
  left_join(
    resulting_distance_mat_with_all_group,
            by = join_by(Archetype == value)
    ) %>% 
  mutate(
    Archetype = ifelse(!is.na(Archetype_proximity),Archetype_proximity,Archetype),
    Base_Archetype = ifelse(ReferenceArchetype_Archetype != "Unknown",
                            ReferenceArchetype_Archetype,Base_Archetype),
    Base_Archetype = ifelse(Base_Archetype == "Unknown" & Archetype !="Unknown",
                            Archetype,Base_Archetype
                            )
    
    ) %>% 
  group_by(Archetype) %>% 
  mutate(
    Archetype_count = n()) %>% 
  ungroup() 



Debug_resulting_distance_mat_with_all_group <- long_dist_mat %>% 
    filter(Archetype.x == Archetype.y) %>% 
    select(-Archetype.y) %>% 
    group_by(Archetype.x) %>% 
    summarize(
      n = n_distinct(rowname),
      self_value = median_quantile_paste(
        x = value,
        round_val = 1
        ),
      Q3 = quantile(value ,0.75),
      .groups = "drop"
    ) %>% 
    rename_all(~paste0("self_",.)) %>% 
  right_join(
    long_dist_mat %>%
      group_by(Archetype.y) %>% 
      mutate(n.y = n_distinct(name)) %>% 
      group_by(Archetype.x ,Archetype.y,n.y) %>% 
      summarize(
        Q2 = quantile(value ,0.5),
        .groups = "drop"
      ),
    by =  join_by(
      self_Archetype.x == Archetype.x
    )
  ) %>% 
  mutate(
    Q3_compare_Median = self_Q3 - Q2
  ) %>% 
  select(self_Archetype.x,Archetype.y ,everything()) %>% 
  rename(Archetype.x = self_Archetype.x) %>% 
  left_join(
    resulting_distance_mat_with_all_group %>% 
      mutate(groupe = TRUE),
    join_by(
      Archetype.x == Archetype_proximity,
      Archetype.y == value        
      )
             ) %>% 
  mutate(groupe = ifelse(is.na(groupe),FALSE,TRUE)) %>% 
  rename(n.x = self_n) %>% 
  relocate(n.y,.after = n.x ) %>% 
  select(-self_Q3)


write.csv(
  Debug_resulting_distance_mat_with_all_group,
  paste0("data/intermediate_result/",format_param,"_proxymity_archetype_group.csv"),
  row.names = FALSE)

write_rds(res_proximity_joins ,paste0("data/",format_param,"_data_meta_en_cours.rds"))



################################################################################


###################################################################################
# some random test of improving proximity aggregation 


# mean_distance_betwwen_arch <- long_dist_mat %>% 
#   group_by(Archetype.x , Archetype.y) %>% 
#   summarise(
#     sum_dist = mean(value),
#     n = n(),
#     .groups = "drop"
#   )
# 
# 
# a <- inner_join(
#   mean_distance_betwwen_arch,
#   mean_distance_betwwen_arch,
#   by = join_by(
#     Archetype.x == Archetype.y,
#     Archetype.y == Archetype.x      
#                )
#   
# ) %>% 
#   rowwise() %>% 
#   mutate(
#   test = (sum_dist.x + sum_dist.y)/(n.x * n.y)
# )

# https://en.wikipedia.org/wiki/Hausdorff_distance

# Hausdorff_distance <- long_dist_mat %>% 
#   # get previous aggregation 
#   left_join(
#     resulting_distance_mat_with_all_group ,
#     by = join_by(Archetype.x == value)
#             ) %>% 
#   mutate(
#     Archetype.x = ifelse(is.na(Archetype_proximity) , 
#                          Archetype.x, Archetype_proximity )
#     ) %>% 
#   select(-Archetype_proximity) %>% 
#   left_join(
#       resulting_distance_mat_with_all_group ,
#       by = join_by(Archetype.y == value)
#     ) %>% 
#   mutate(
#     Archetype.y = ifelse(is.na(Archetype_proximity) , 
#                          Archetype.y, Archetype_proximity )
#   ) %>% 
#   select(-Archetype_proximity) %>%
#   group_by(Archetype.y,rowname) %>% 
#   mutate(min_x_to_y = min(value ) ) %>% 
#   ungroup() %>% 
#   group_by(Archetype.x,name) %>% 
#   mutate(min_y_to_x = min(value ) ) %>% 
#   ungroup() %>% 
#   group_by(Archetype.x,Archetype.y) %>% 
#   summarise(
#     max_dist_x_to_y = max(min_x_to_y),
#     max_dist_y_to_x = max(min_y_to_x),
#     max = pmax(max_dist_x_to_y,max_dist_y_to_x),
#     .groups = "drop"
#   ) 
# 
# 
# a <- Hausdorff_distance %>% 
#   filter(Archetype.x != Archetype.y) %>% 
#   left_join(
# Hausdorff_distance %>% 
#   filter(Archetype.x == Archetype.y) %>% 
#   select(Archetype.x,max) %>% 
#   rename(self_Hausdorff = max),
# by = join_by(Archetype.x)
# ) %>% 
#   mutate(test = max  - self_Hausdorff) %>% 
#   left_join(
#     Debug_resulting_distance_mat_with_all_group %>% 
#       select(Archetype.x,Archetype.y, n.x ,  n.y),
#     by = join_by(Archetype.x, Archetype.y)
#   )


################################################################################
################################################################################
################################################################################
# 
# # core n % des decks qui contiennent n % des cartes 
# 
# core_base_fun <- data_wide %>%
#   filter(Archetype != "Unknown") %>%
#   pivot_longer(-c( id,Archetype)) %>% 
#   ungroup() %>% 
#   mutate(n_arch = n_distinct(id),
#          .by = Archetype) %>% 
#   filter(n_arch >= 50) %>%  
#   mutate(number_of_cards = sum(value),.by = id) %>% 
#   group_by(Archetype,name) %>% 
#   mutate(max_number_of_copie_in_arch = max(value)) %>% 
#   ungroup() %>% 
#    filter(max_number_of_copie_in_arch > 0) 
# 
# 
# first_core_cards <- core_base_fun %>%
#   group_by(Archetype,n_arch , name) %>%
#   # Pour chaque carte et archétype, calculer la proportion de decks pour chaque quantité possible
#   reframe(
#     max_value = max(value), # Valeur maximale pour itérer
#     value_ite = unique(value),
#     sum_copy =  map_dbl(value_ite, ~ sum(value >= .) ) ,
#     # Calculer proportion pour chaque niveau
#   )  %>% 
#   mutate(
#     proportions = sum_copy/n_arch
#   ) %>% 
#   filter(proportions == 1,
#          value_ite > 0
#          ) %>% 
#   group_by(Archetype , name ) %>% 
#   filter(value_ite  == max(value_ite )) %>% 
#   ungroup() %>% 
#   mutate(
#     iteration = 0
#   )
# 
# 
# 
# deck_without_core <- core_base_fun %>% 
#   left_join(first_core_cards %>%  
#               select(Archetype ,name,value_ite  ),
#             by = join_by(Archetype, name)
#             ) %>% 
#   replace_na(list(value_ite = 0)) %>% 
#   mutate(
#     include_in_core = value_ite > 0,
#     value = value - value_ite,
#     # aaa = !(include_in_core & value <= 0)
#   ) %>% 
#   filter(!(include_in_core & value <= 0)) %>% 
#   select(-c(value_ite ,include_in_core))
# 
# n_distinct(core_base_fun$id)
# n_distinct(deck_without_core$id)
# 
# 
# ite_other_cards_for_cards_in_core <- function(
#     deck_reamining_cards,
#     core_cards,
#     iteration = 1
# ){
#   print(paste0("defining core iteration : ",iteration))
#   # if(iteration == 2) browser()
#   core_ite <- deck_reamining_cards %>%
#     # {if(!is.null(id_core_par)) filter(
#     #   id  %in% id_core_par
#     # ) else .} %>% 
#     group_by(Archetype,n_arch , name) %>%
#     # Pour chaque carte et archétype, calculer la proportion de decks pour chaque quantité possible
#     reframe(
#       max_value = max(value), # Valeur maximale pour itérer
#       value_ite = unique(value),
#       sum_copy =  map_dbl(value_ite, ~ sum(value >= .) ) ,
#       # Calculer proportion pour chaque niveau
#     )  %>% 
#     mutate(
#       proportions = sum_copy/n_arch
#     ) %>% 
#     filter(value_ite > 0) %>% 
#     group_by(Archetype) %>% 
#     slice_max(proportions, n = 1, with_ties = FALSE) %>% 
#     mutate(iteration = iteration)
#   
#   # browser()
#   
#   deck_with_core_removing_core_cards_ite <- deck_reamining_cards %>% 
#     left_join(core_ite %>%  
#                 select(Archetype ,name,value_ite  ),
#               by = join_by(Archetype, name)
#     ) %>% 
#     replace_na(list(value_ite = 0)) %>% 
#     mutate(
#       include_in_core = value_ite > 0,
#       value = value - value_ite
#     ) %>% 
#     mutate(contains_core_cards = any(include_in_core),.by = id) %>% 
#     filter(!(include_in_core & value <= 0)) %>% 
#     filter(contains_core_cards) %>% 
#     select(-c(value_ite ,include_in_core,contains_core_cards))
#   
#   
#   ##############################################################################
#   ######## possible opti directement inclure quand il ne reste qu'un deck dans le coeur
#   Archetype_with_single_deck <- deck_with_core_removing_core_cards_ite %>%
#     group_by(Archetype) %>%
#     mutate(
#       unique_deck_in_arch = n_distinct(id) == 1
#     ) %>%
#     filter(unique_deck_in_arch,
#            value > 0)
# 
# 
#   # print(n_distinct(deck_with_core_removing_core_cards_ite$id))
#   if(nrow(Archetype_with_single_deck) > 0){
#     core_ite <- rbind(core_ite,  Archetype_with_single_deck %>%
#       select(Archetype,n_arch ,name,value ) %>% 
#       rename(value_ite = value) %>% 
#       mutate(
#         max_value = value_ite,
#         sum_copy = 1,
#         iteration = iteration,
#         proportions = sum_copy/n_arch 
#              )
#     )
#     
#     deck_with_core_removing_core_cards_ite <- deck_with_core_removing_core_cards_ite  %>%
#       group_by(Archetype) %>%
#       mutate(
#         unique_deck_in_arch = n_distinct(id) == 1
#       ) %>%
#       filter(!unique_deck_in_arch,
#              ) %>% 
#       ungroup() %>% 
#       select(-unique_deck_in_arch)
#     
#   }
#   
#   
#  core_res <-  rbind(core_cards,core_ite)
# 
# 
#  if(nrow(deck_with_core_removing_core_cards_ite) > 0){
#    res <- ite_other_cards_for_cards_in_core(
#     deck_reamining_cards = deck_with_core_removing_core_cards_ite,
#     core_cards = core_res,
#     iteration = iteration + 1
#    )
#  } else {
#    res <- core_res
#  }
#  return(res)
# }
# 
# 
# core_cards <- ite_other_cards_for_cards_in_core(
#   deck_reamining_cards = deck_without_core,
#   core_cards = first_core_cards
# ) 
# 
# cards_by_iteration <- core_cards %>% 
#   group_by(Archetype,iteration,proportions ) %>% 
#   summarise(Number_of_cards_in_core = sum(value_ite ),
#             .groups = "drop") %>% 
#   group_by(Archetype) %>% 
#   mutate(Number_of_cards_in_core = cumsum(Number_of_cards_in_core)) %>% 
#   left_join(
#   core_base_fun %>% group_by(Archetype) %>% 
#   summarise(number_of_cards = quantile(number_of_cards,0.9),.groups = "drop"),
#   by = join_by(Archetype)
# ) %>% 
#   ungroup() %>% 
#   rowwise() %>% 
#   mutate(percent_of_the_card_in_deck = Number_of_cards_in_core/number_of_cards,
#          result = min(c(percent_of_the_card_in_deck,proportions ))
#          ) 
# 
# Best_number_of_core_iteration <- cards_by_iteration %>% 
#   ungroup() %>% 
#   group_by(Archetype) %>% 
#   slice_max(result, n = 1, with_ties = TRUE) %>% 
#   filter(iteration == min(iteration)) %>% 
#   ungroup()
# 
# 
# ################################################################################
# ######################### to return in debug ###################################
# final_core <- core_cards %>% 
#   left_join(
#   Best_number_of_core_iteration %>% 
#     select(Archetype,iteration,Number_of_cards_in_core ,result) %>% 
#     rename(iteration_cut_of = iteration,
#           core_result  = result
#            ),
#   by = join_by(Archetype)
#   ) %>% 
#   filter(iteration <= iteration_cut_of) 
# 
# 
# Core_final_deck_list <- final_core %>% 
#   group_by(Archetype ,n_arch, name , Number_of_cards_in_core ,core_result ) %>% 
#   summarise(
#     max_value = max(max_value),
#     value_ite = sum(value_ite),
#     sum_copy = min(sum_copy),
#     .groups = "drop"
#     )
# 
# 
# ################################################################################
# 
# core_card_df_for_dist <- Core_final_deck_list %>% 
#   select(Archetype  ,name,value_ite ) %>% 
#   # fin de la gestion
#   # cards matrix wild
#   pivot_wider(
#     names_from = name,
#     # names_prefix = "Mainboard_",
#     values_from = value_ite,
#     values_fill = 0,
#     names_repair = "universal_quiet"
#   ) %>% 
#   mutate(
#     Archetype = as.factor(Archetype)
#   )
# 
# 
# missing_col_in_core <- setdiff(colnames(Dist_matrice_basale_from_wide),colnames(core_card_df_for_dist))
# 
# 
# 
# Dist_matrice_core_cards <- cbind(
#   as.matrix(core_card_df_for_dist %>%
#     select(-Archetype)
#     ),
# matrix(0,
#        nrow = nrow(core_card_df_for_dist), ncol = length(missing_col_in_core),
#        dimnames = list(
#          x = c(1: nrow(core_card_df_for_dist)),
#          y = missing_col_in_core
#        )
# )
#   )
# 
# 
# dist_test_with_core <- proxyC::simil(
#  x = Dist_matrice_basale_from_wide,
#   y = Dist_matrice_core_cards,
# method = "ejaccard"
# )
# 
# 
# long_dist_core_mat <- (
#   1 - as.matrix(dist_test_with_core)
# ) %>%
#   as.data.frame() %>%
#   rownames_to_column() %>%
#   pivot_longer(-rowname) %>%
#   left_join(data_wide %>%
#               select(id,Archetype) %>% 
#               mutate(Archetype = as.character(Archetype)
#               ),by = join_by(rowname  == id)
#   ) %>%
#   left_join(core_card_df_for_dist %>%
#               select(Archetype) %>% 
#               rownames_to_column() %>% 
#               mutate(Archetype = as.character(Archetype)
#               ) ,by = join_by(name  == rowname)) 
# 
# # summarise_long_dist_core_mat <- long_dist_core_mat %>% 
# #   group_by(Archetype.x,Archetype.y) %>% 
# #   summarise(
# #     moyenne = mean(value, na.rm = TRUE),
# #     ecart_type = sd(value, na.rm = TRUE),
# #     decile_0 = quantile(value, probs = 0, na.rm = TRUE),
# #     decile_1 = quantile(value, probs = 0.1, na.rm = TRUE),
# #     decile_2 = quantile(value, probs = 0.2, na.rm = TRUE),
# #     decile_3 = quantile(value, probs = 0.3, na.rm = TRUE),
# #     decile_4 = quantile(value, probs = 0.4, na.rm = TRUE),
# #     decile_5 = quantile(value, probs = 0.5, na.rm = TRUE),
# #     decile_6 = quantile(value, probs = 0.6, na.rm = TRUE),
# #     decile_7 = quantile(value, probs = 0.7, na.rm = TRUE),
# #     decile_8 = quantile(value, probs = 0.8, na.rm = TRUE),
# #     decile_9 = quantile(value, probs = 0.9, na.rm = TRUE),
# #     decile_10 = quantile(value, probs = 1, na.rm = TRUE)
# #   )
# 
# 
# 
# 
# mean_dist_value_from_core_tot <- long_dist_core_mat %>% 
#     group_by(Archetype.x,Archetype.y) %>% 
#   summarise(
#     mean = mean(value),
#     .groups = "drop"
#       )
# 
# 
# 
# mean_dist_value_from_core <- mean_dist_value_from_core_tot %>% 
#   filter(Archetype.x != Archetype.y) %>% 
#   left_join(
#     mean_dist_value_from_core_tot %>% 
#       filter(Archetype.x == Archetype.y) %>% 
#       select(-Archetype.y) %>% 
#       rename(self_mean = mean),
#     by = join_by(Archetype.x)
#   ) %>% 
#   mutate(test = mean <= self_mean)
#   
#   
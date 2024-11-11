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
source("sources/S2_Source_mtg_new_card.R",local = TRUE)
source("data/mtg_data/sources/global_mtg_sources.R")
# library("ggdendro")

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
    
    already_select_arch <- rbind(
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

data_wide <- df_export  %>%
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


tictoc::tic("test")
dist_test <- proxyC::simil(as.matrix(
  data_wide %>%
    filter(Archetype != "Unknown") %>% 
    column_to_rownames("id") %>%
    select(-Archetype)
),
method = "ejaccard"
)


long_dist_mat <- (
  1 - as.matrix(dist_test)
                  ) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  left_join(data_wide %>%
              select(id,Archetype) %>% 
              mutate(Archetype = as.character(Archetype)
                     ),by = join_by(rowname  == id)
            ) %>%
  left_join(data_wide %>%
              select(id,Archetype) %>% 
              mutate(Archetype = as.character(Archetype)
              ) ,by = join_by(name  == id)) %>%
  filter(rowname != name) # %>%   select(-c(rowname,name))




df_archetype_count <- data_wide %>%
  mutate(    Archetype = as.character(Archetype)) %>% 
  group_by(Archetype) %>% 
  summarise(
    count = n()
  )



grouping_cards_recursive <- function(
    dist_mat_fun,
    grouping_df_base_fun = data.frame(
      Archetype_name = character(),
      value = character()
    ),
    Archetype_count_fun = df_archetype_count,
    n = 1
){
  
  
  # print(n)

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
  
  grouping_df_res <- rbind(grouping_df_base_fun,archetype_to_group)
  
  
  
  dist_mat_fun_res <- dist_mat_fun %>% 
    filter(
      Archetype.x %notin% grouping_df_res$value,
      Archetype.y %notin% grouping_df_res$value
           )
    
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


resulting_distance_mat_with_all_group <- grouping_cards_recursive(
  dist_mat_fun = long_dist_mat
)  %>% 
  rename(Archetype_proximity = Archetype_name )



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





# core n % des decks qui contiennent n % des cartes 

# a <- data_wide %>%
#   filter(Archetype != "Unknown") %>%
#   column_to_rownames("id") %>% 
#   group_by(Archetype) %>% 
#   summarise_all(
#     
#     
#   )










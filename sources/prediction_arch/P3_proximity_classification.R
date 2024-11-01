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
# library("ggdendro")



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

# format_param <- format_date_en_cours$format_param
# date_cut <- format_date_en_cours$date_cutoff



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
  left_join(resulting_distance_mat_with_all_group,by = join_by(Archetype == value)) %>% 
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
  group_by(Archetype.x ,Archetype.y) %>% 
  summarize(
    count = n(),
    Q2 = quantile(value ,0.5),
    .groups = "drop"
  ) %>% 
  left_join(
    long_dist_mat %>% 
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







Debug_resulting_distance_mat_with_all_group <- resulting_distance_mat_with_all_group %>% 
  left_join(
    long_dist_mat %>% 
      filter(Archetype.x == Archetype.y) %>% 
      select(-Archetype.y) %>% 
      group_by(Archetype.x) %>% 
      summarize(
        Q3 = quantile(value ,0.75),
        .groups = "drop"
      ) %>% 
      rename_all(~paste0("self_",.)) %>% 
      rename(Archetype = self_Archetype.x),
            by =  join_by(
              Archetype_proximity == Archetype )
            ) %>% 
  left_join(
    long_dist_mat %>%
              group_by(Archetype.x ,Archetype.y) %>% 
              summarize(
                # count = n(),
                Q2 = quantile(value ,0.5),
                .groups = "drop"
              ),
    by =  join_by(
      Archetype_proximity == Archetype.x,
      value == Archetype.y
      )
    
            ) %>% 
  mutate(
    Q3_compare_Median = self_Q3 - Q2
  )
  



write.csv(
  Debug_resulting_distance_mat_with_all_group,
  paste0("data/intermediate_result/",format_param,"_proxymity_archetype_group.csv"),
  row.names = FALSE)

write_rds(res_proximity_joins ,paste0("data/",format_param,"_data_meta_en_cours.rds"))



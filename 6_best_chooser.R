library("rjson")
library(lubridate)
# library(kableExtra)
library(tidyverse)



source("S2_Source_mtg_new_card.R")
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(plotly::layout)
loaded_base_data <- readRDS("data/data_meta_en_cours.rds")



Archetype_cut_of <- 50

df_base <- loaded_base_data %>%
  filter(Tournament != "Modern League") %>%
  group_by(Archetype) %>%
  mutate(
    Archetype_count = n()
  ) %>%
  arrange(Archetype_count) 




Other_Archetype <- df_base %>% 
  filter(Archetype_count < Archetype_cut_of | Archetype == "Unknown") %>% 
  pull(Archetype) %>% unique()


df_base_all_data <- df_base %>%
  mutate(
    Archetype = ifelse(Archetype %in% Other_Archetype,
                       "Other",
                       Archetype
    )
  ) %>% 
  mutate(
    Archetype =
      factor(Archetype,
             level = unique(.$Archetype)
      ),
    Rank = as.numeric(
      factor(Archetype,
             level = rev(unique(.$Archetype))
      )
    ),
    Base_Archetype = 
      
      factor( Base_Archetype,
              level = unique(.$Base_Archetype)
      )
  )



Df_win_rate_matrix_base <- df_base_all_data %>% 
  rowwise() %>% 
  filter(!is.null(Matchups)) %>% 
  unnest_longer(Matchups) %>%
  unnest_wider(Matchups,names_sep = "_") %>% 
  select(
    id,Meta,Week,Date,Archetype,Player,
    Matchups_Opponent,Matchups_OpponentArchetype,Matchups_Wins,
    Matchups_Losses,Matchups_Draws,matches) %>% 
  mutate(
    Matchups_Opponent_basedArchetype = Matchups_OpponentArchetype,
    Matchups_OpponentArchetype = Archetype_agreger(Matchups_OpponentArchetype),
    Matchups_OpponentArchetype = ifelse(Matchups_OpponentArchetype %in% Other_Archetype,
                                        "Other",
                                        Matchups_OpponentArchetype
    ),
    Matchups_OpponentArchetype = factor(Matchups_OpponentArchetype,levels = rev(levels(Archetype)))
  ) 





Df_win_rate_matrix_summarise <- Df_win_rate_matrix_base %>% 
  select(
    Archetype,Matchups_OpponentArchetype,
    Matchups_Wins,Matchups_Losses
  ) %>% 
  mutate(
    Result = Matchups_Wins > Matchups_Losses,
    Draw = Matchups_Wins == Matchups_Losses
  ) %>% 
  group_by(
    Archetype,
    Matchups_OpponentArchetype
  ) %>%
  summarise(
    number_of_matches = n() - sum(Draw),
    Win_matches = sum(Result),
    number_of_games = sum(Matchups_Wins) + sum(Matchups_Losses),
    Matchups_Wins = sum(Matchups_Wins),
    Matchups_Losses = sum(Matchups_Losses),
    .groups = "drop"
  ) %>%
  mutate(
    WR_games = winrate_1_data(Matchups_Wins,Matchups_Losses),
    CI_WR_games = CI_prop(WR_games,number_of_games),
    CI_WR_sign_games = factor(
      ifelse(CI_WR_games == 0,"0",
             ifelse(
               ((WR_games - 0.5) + (CI_WR_games)) > 0,
               "+",
               ifelse(
                 ((WR_games - 0.5) - (CI_WR_games)) < 0,
                 "-","0"
               ))),
      levels = c("+","0","-")
    ),
    
    WR_matches = winrate_1_data(Win_matches,(number_of_matches - Win_matches)),
    CI_WR_matches = CI_prop(WR_matches,number_of_matches),
    CI_WR_sign_matches = factor(
      ifelse(CI_WR_matches == 0,"0",
             ifelse(
               ((WR_matches - 0.5) + (CI_WR_matches)) > 0,
               "+",
               ifelse(
                 ((WR_matches - 0.5) - (CI_WR_matches)) < 0,
                 "-","0"
               ))),levels = c("+","0","-") ),
    
  ) 







 

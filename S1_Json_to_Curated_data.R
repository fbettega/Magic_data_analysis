library("rjson")
library(tidyverse)


source("S2_Source_mtg_new_card.R")



json_parsing <- fromJSON(file = "MTGOArchetypeParser_20231227/data_FB_test.json")

df_export <- json_parsing %>%
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
    Week = as.integer(ceiling(
      difftime(
        Date,
        (min(Date) - as.difftime(1, unit = "days")),
        units = "weeks"
      )
    ))
  )


df_export_remove_bann <- Ban_patch(
  df = df_export,
  vec_of_ban_cards = c("Violent Outburst")
)


write_rds(df_export_remove_bann, "data/data_meta_en_cours.rds")



















#

#
#
# df <- df_export %>%
#   filter(Tournament != "Modern League") %>%
#   group_by(Archetype) %>%
#   mutate(
#     Archetype_count = n()
#   )
#
#
# df_encours <- df %>% filter(
#   Archetype_count < 100,
#                              # Archetype_count > 100
#                             )
#
#
# sort(table(df_encours$Archetype), decreasing = TRUE)
#
#
#
#
# df_export %>% filter(id == 27181 ) %>% unnest_longer(Mainboard)  %>%
#   unnest_wider(Mainboard,names_sep = "_") %>% view()
#
#
# # Bordel d'exploration
# #
# #
# Count_cards_in_decklist <- function(df,colname_deck_list){
#
#   df_new_card_base <- df %>%
#     unnest_longer(!!colname_deck_list) %>%
#     unnest_wider(!!colname_deck_list,names_sep = "_")
#
#
#
#   df_new_card <- df_new_card_base %>%
#     mutate( !!rlang::sym(paste0(colname_deck_list,"_CardName")) := Card_agregueur(
#       !!rlang::sym(paste0(colname_deck_list,"_CardName")))
#            ) %>%
#     group_by(
#       Archetype,
#       Archetype_count,
#       !!rlang::sym(paste0(colname_deck_list,"_CardName")),
#       !!rlang::sym(paste0(colname_deck_list,"_Count"))
#     ) %>%
#     summarise(
#       count = n(),
#       .groups = "drop"
#     ) %>%
#     mutate(!!rlang::sym(paste0(colname_deck_list,"_Count")) := as.character(!!rlang::sym(paste0(colname_deck_list,"_Count"))))
#
#
#   df_new_card_total <- df_new_card %>%
#     group_by(
#       Archetype,
#       !!rlang::sym(paste0(colname_deck_list,"_CardName"))) %>%
#     filter(n() > 1) %>%
#     group_by(
#       Archetype,
#       Archetype_count,
#       !!rlang::sym(paste0(colname_deck_list,"_CardName"))
#     ) %>%
#     summarise(
#       !!rlang::sym(paste0(colname_deck_list,"_Count")) := "Any",
#       count = sum(count),
#       .groups = "drop"
#     ) %>%
#     rbind(df_new_card) %>%
#     arrange(desc(Archetype_count),Archetype,    !!rlang::sym(paste0(colname_deck_list,"_CardName")),
#             !!rlang::sym(paste0(colname_deck_list,"_Count")))
# }
#
#
#
#
# #
# #
# # sort(unique(df_export$Archetype))
# #
# #
# #
# # # handle UWx
# # # Handdle UBx
# # # RAkdos midrange
# #
# # # Check jund
# # # check omnath C
# #
# #
# fall_back <- df_export %>%
#   filter(str_detect(Archetype,
#                     "Midrange _fallback"
#   ) ) #%>%
#   # ungroup() %>%
#   # mutate(Archetype = "a",
#   #        Archetype_count = n())
# #
# #
# # unique(fall_back$Archetype)
# #
# fall_back_control <- df_export %>%
#   filter(str_detect(Archetype,
#                     "Skelementals"
#                     #"Rakdos Midrange"
#   ) )
#
# fall_back_control <- df_export %>%
#   filter(Archetype %in% c(
#     "Naya Midrange _fallback",
#     "Boros Midrange _fallback",
#     "Mono Red Aggro")
#                     #"Rakdos Midrange"
#   )
#
#
#
# #
# # "Grixis Aggro",
# #
# #
# # test_absence <-df_export  %>%
# #   filter(Archetype == "Goryo Reanimator") %>%
# #   filter(ReferenceArchetype_Archetype != "Reanimator",
# #          ReferenceArchetype_Archetype != "Goryo Reanimator"
# #          )
# #
# #
# card_count_en_cours <- Count_cards_in_decklist(fall_back_control %>% mutate(Archetype = "a"),"Mainboard")
#
# count_card_absence <- Count_cards_in_decklist(test_absence,"Mainboard")
#
# df_export %>% group_by(Archetype) %>%
#   summarise(n()) %>% view()
#
# fall_back %>% group_by(Archetype) %>%
#   summarise(n()) %>% view()
#
#
#
#
#
#
#

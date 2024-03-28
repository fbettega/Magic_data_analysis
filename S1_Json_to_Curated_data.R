library("rjson")
library(tidyverse)


# prévoir un regroupement automatique au dela de ce que j'ai fait



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
sort(table(df_export$Archetype))

df_export_remove_bann <- Ban_patch(
  df = df_export,
  vec_of_ban_cards = c("Violent Outburst")
)


write_rds(df_export_remove_bann, "data/data_meta_en_cours.rds")



# 
# 
# df_export <- json_parsing %>%
#   as_tibble() %>%
#   unnest_wider(Data) %>%
#   unnest_wider(Archetype) %>%
#   unnest_wider(ReferenceArchetype,
#                names_sep = "_"
#   ) %>%
#   mutate(across(c(Wins, Losses, Draws), ~ as.numeric(.))) %>%
#   mutate(matches = Wins + Losses + Draws) %>%
#   mutate(Base_Archetype = Archetype) %>%
#   mutate(Archetype = Archetype_agreger2(Base_Archetype, Color)) %>%
#   rownames_to_column(var = "id") %>%
#   group_by(Archetype) %>%
#   mutate(
#     Archetype_count = n()
#   ) %>%
#   ungroup() %>%
#   mutate(
#     Date = as_datetime(Date),
#     Week = as.integer(
#       ceiling(
#         difftime(
#           Date,
#           (min(Date) - as.difftime(1, unit = "days")),
#           units = "weeks"
#         )
#       )
#     )
#   )
# 
# 
# 
# 
# 
# Archetype_agreger2 <- function(Archetype_to_agreg, color_agreg = NULL) {
# 
#   # Regroupement du split de creativity sur persist
#   Archetype_to_agreg <- ifelse(str_detect(
#     Archetype_to_agreg,
#     "Creativity"
#   ),
#   "Creativity", Archetype_to_agreg
#   )
#   
#   # Groupe breach value and murktide
#   Archetype_to_agreg <- ifelse(Archetype_to_agreg == "Breach Value",
#                                "Murktide", Archetype_to_agreg
#   )
#   
#   
#   # Pack rhinos
#   Archetype_to_agreg <- ifelse(Archetype_to_agreg == "Footfalls 4 C",
#                                "Footfalls", Archetype_to_agreg
#   )
#   # Pack tron
#   Archetype_to_agreg <- ifelse(str_detect(Archetype_to_agreg, "Tron$"),
#                                "Tron", Archetype_to_agreg
#   )
#   
#   # Regroupement de tout les rakdos midrange et scam
#   Archetype_to_agreg <- ifelse(Archetype_to_agreg %in%
#                                  c(
#                                    # "Rakdos Midrange _fallback",
#                                    # "Mardu Midrange _fallback",
#                                    "Skelementals", "Scam"
#                                  ),
#                               "BR midrange", Archetype_to_agreg
#   )
#   
#   # Merge the two combo breach potentiellement breach storm groupable avec les autres storms
#   Archetype_to_agreg <- ifelse(Archetype_to_agreg %in% c(
#     "Breach Storm", "Grinding Breach"
#   ),
#   "Breach combo", Archetype_to_agreg
#   )
#   
#   # Disctuable merge goryo et reanimator
#   Archetype_to_agreg <- ifelse(Archetype_to_agreg == "Goryo Reanimator",
#                                "Reanimator", Archetype_to_agreg
#   )
#   
#   # Regroupement de toutes les version tuant avec vaalakut, gros doutes sur l'inclusion de titanshift
#   Archetype_to_agreg <- ifelse(Archetype_to_agreg %in% c(
#     "Guildpact Valakut", "Blue Scapeshift"
#   ),
#   "Scapeshift", Archetype_to_agreg
#   )
#   
#   # Merge titan shift avec scapshift
#   Archetype_to_agreg <- ifelse(Archetype_to_agreg == "Titan Shift",
#                                "Scapeshift", Archetype_to_agreg
#   )
# 
#   # Merge tout les titan sauf titan shift
#   Archetype_to_agreg <- ifelse(str_detect(Archetype_to_agreg, "Titan$"),
#                                "Amulet Titan", Archetype_to_agreg
#   )
#   Archetype_to_agreg <- ifelse(Archetype_to_agreg == "Timeless Lotus",
#                                "Amulet Titan", Archetype_to_agreg
#   )
# 
#   # Merge les 2 versions de gob
#   Archetype_to_agreg <- ifelse(Archetype_to_agreg == "Goblin Whack",
#                                "Goblins", Archetype_to_agreg
#   )
#   
#   # Groupement de tout les storms
#   Archetype_to_agreg <- ifelse(Archetype_to_agreg %in% c(
#     "Grixis Storm", "Boros Storm", "Mono Red Storm",
#     "Gifts Storm", "Twiddle Storm"
#   ),
#   "Storm", Archetype_to_agreg
#   )
# 
#   # Regroupement de toutes les 4/5C soupe avec des betes
#   Archetype_to_agreg <- ifelse(Archetype_to_agreg %in% c(
#     "Elementals", "Beans Cascade", "Saheeli Combo"
#   ),
#   "Omnath Control", Archetype_to_agreg
#   )
#   
#   # Regroupement de toutes les soupes sans lands
#   Archetype_to_agreg <- ifelse(Archetype_to_agreg %in% c("Oops All Spells"),
#                                "Belcher", Archetype_to_agreg
#   )
#   
#   # Groupement de tout les Burn quelquesois les couleurs
#   Archetype_to_agreg <- ifelse(str_detect(Archetype_to_agreg, "Burn"),
#                                "Burn", Archetype_to_agreg
#   )
#   
#   # Meta groupes avec les soupes foods
#   Archetype_to_agreg <- ifelse(Archetype_to_agreg %in% c(
#     "Asmo Food", "Manufactor Combo"
#   ),
#   "Food", Archetype_to_agreg
#   )
#   
#   return(Archetype_to_agreg)
# }
# 
# 
# 




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

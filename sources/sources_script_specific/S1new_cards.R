
# Fonction that count cards in given arhcetype
Count_cards_in_decklist <- function(df, Name_of_card_of_interest = NULL, colname_deck_list,
                                    # this paramater and following if statement are for overall card presence in presence script
                                    No_grouping_column = FALSE
) {
  if (colname_deck_list == "Mainboard") {
    expected_min_size <- 60
  } else {
    expected_min_size <- 0
  }
  
  if(colname_deck_list == "All"){
    
    df_unnest_main <- df %>%
      unnest_longer(Mainboard) %>%
      unnest_wider(Mainboard, names_sep = "_") %>%
      group_by(id) %>%
      # a filtrer mes uniquement pour le main deck
      mutate(Number_of_cards := sum(!!rlang::sym(paste0("Mainboard", "_Count")))) %>%
      filter(Number_of_cards >= 60) %>% 
      rename_all(~str_remove(.,"Mainboard"))
     
    df_unnest_side <- df %>%
      unnest_longer(Sideboard) %>%
      unnest_wider(Sideboard, names_sep = "_") %>%
      group_by(id) %>%
      # a filtrer mes uniquement pour le main deck
      mutate(Number_of_cards := sum(!!rlang::sym(paste0("Sideboard", "_Count")))) %>%
      filter(Number_of_cards <= 15) %>% 
      rename_all(~str_remove(.,"Sideboard"))
    
    
    
    df_unnest <- rbind(df_unnest_main,df_unnest_side)
    colname_deck_list <- ""
    
  } else {
    
    df_unnest <- df %>%
      unnest_longer(!!colname_deck_list) %>%
      unnest_wider(!!colname_deck_list, names_sep = "_") %>%
      group_by(id) %>%
      # a filtrer mes uniquement pour le main deck
      mutate(Number_of_cards := sum(!!rlang::sym(paste0(colname_deck_list, "_Count")))) %>%
      {if(colname_deck_list == "Mainboard") filter(
        .,Number_of_cards >= 60
        ) else if(colname_deck_list == "Sideboard") filter(.,
          Number_of_cards <= 15
          ) else filter(.,
                        Number_of_cards <= 0
                        )} 
      
  }
  
  
  

  df_new_card_base <- df_unnest  %>%
    {if(is.null(Name_of_card_of_interest)) . else filter(.,!!rlang::sym(paste0(colname_deck_list, "_CardName")) %in% Name_of_card_of_interest)} %>%
    mutate(!!rlang::sym(paste0(colname_deck_list, "_CardName")) := Card_agregueur(
      !!rlang::sym(paste0(colname_deck_list, "_CardName")),
      ALL_mod = TRUE
    )) %>%
    group_by(id, !!rlang::sym(paste0(colname_deck_list, "_CardName"))) %>%
    mutate(
      !!rlang::sym(paste0(colname_deck_list, "_Count")) :=
        sum(!!rlang::sym(paste0(colname_deck_list, "_Count")))
    ) %>%
    distinct()
  
  
  df_new_card <- df_new_card_base %>%
    {if(No_grouping_column) group_by(.,
                                     !!rlang::sym(paste0(colname_deck_list, "_CardName")),
                                     !!rlang::sym(paste0(colname_deck_list, "_Count"))
    ) else  group_by(.,
      Archetype,
      Archetype_count,
      !!rlang::sym(paste0(colname_deck_list, "_CardName")),
      !!rlang::sym(paste0(colname_deck_list, "_Count"))
    ) } %>%
    summarise(
      count = n(),
      .groups = "drop"
    ) %>%
    mutate(!!rlang::sym(paste0(colname_deck_list, "_Count")) := as.character(!!rlang::sym(paste0(colname_deck_list, "_Count"))))
  
  
  df_new_card_total <- df_new_card %>%
    {if(No_grouping_column) group_by(
      .,
      !!rlang::sym(paste0(colname_deck_list, "_CardName"))
    )
     else  group_by(
      .,
      Archetype,
      !!rlang::sym(paste0(colname_deck_list, "_CardName"))
    ) } %>%
    filter(n() > 1) %>%
    {if(No_grouping_column) group_by(
      .,
      !!rlang::sym(paste0(colname_deck_list, "_CardName"))
    )
      else  group_by(
        .,
        Archetype,
        Archetype_count,
        !!rlang::sym(paste0(colname_deck_list, "_CardName"))
      ) } %>%
    summarise(
      !!rlang::sym(paste0(colname_deck_list, "_Count")) := "Any",
      count = sum(count),
      .groups = "drop"
    ) %>%
    rbind(df_new_card) %>%
    {if(No_grouping_column) arrange(
      .,
      !!rlang::sym(paste0(colname_deck_list, "_CardName")),
      !!rlang::sym(paste0(colname_deck_list, "_Count"))
    )
      else  arrange(.,
        desc(Archetype_count), Archetype, !!rlang::sym(paste0(colname_deck_list, "_CardName")),
        !!rlang::sym(paste0(colname_deck_list, "_Count"))
      ) }  %>%
    select(-any_of("Archetype_count")) %>% 
    rename_all(~str_remove(.,"^_"))
  
  return(df_new_card_total)
}







# Count_and_winrates_cards_in_decklist(
#   df_export,
#   card_of_interest$name, "Mainboard"
# )

# Fonction that count + win rates cards in given arhcetype
Count_and_winrates_cards_in_decklist <- function(df,
                                                 Name_of_card_of_interest = NULL,
                                                 colname_deck_list, 
                                                 filter_archetype_count = 10,
                                                 # this paramater and following if statement are for overall card presence in presence script
                                                 No_grouping_column = FALSE
                                                 
                                                 
                                                 ) {
  winrate_by_archetype <- df %>%
    filter(!is.na(Wins)) %>%
    filter(!is.null(!!colname_deck_list)) %>%
    {if(No_grouping_column) . else filter(
      .,Archetype_count > filter_archetype_count) %>%
        group_by(Archetype) } %>%
    
    summarise(
      Wins_total = sum(Wins),
      Losses_total = sum(Losses),
      Draws_total = sum(Draws)
    )
   # browser()
  
  if (colname_deck_list == "Mainboard") {
    expected_min_size <- 60
  } else {
    expected_min_size <- 0
  }
  
  if(colname_deck_list == "All"){
    
    df_unnest_main <- df %>%
      filter(!is.na(Wins)) %>%
      filter(!is.null(Mainboard)) %>%
      {if(is.null(Name_of_card_of_interest)) . else filter(.,Archetype_count > filter_archetype_count)} %>%
      unnest_longer(Mainboard) %>%
      unnest_wider(Mainboard, names_sep = "_") %>%
      group_by(id) %>%
      # a filtrer mes uniquement pour le main deck
      mutate(Number_of_cards := sum(!!rlang::sym(paste0("Mainboard", "_Count")))) %>%
      filter(Number_of_cards >= 60) %>% 
      rename_all(~str_remove(.,"Mainboard"))
    
    df_unnest_side <- df %>%
      filter(!is.na(Wins)) %>%
      filter(!is.null(Sideboard)) %>%
      {if(is.null(Name_of_card_of_interest)) . else filter(.,Archetype_count > filter_archetype_count)} %>%
      unnest_longer(Sideboard) %>%
      unnest_wider(Sideboard, names_sep = "_") %>%
      group_by(id) %>%
      # a filtrer mes uniquement pour le main deck
      mutate(Number_of_cards := sum(!!rlang::sym(paste0("Sideboard", "_Count")))) %>%
      filter(Number_of_cards <= 15) %>% 
      rename_all(~str_remove(.,"Sideboard"))
    
    # browser()
    
    df_unnest <- rbind(df_unnest_main,df_unnest_side) 
    # %>%  
    #   group_by(id,`_CardName`) %>% 
    #   mutate(`_Count` = sum(`_Count`)) %>%
    #   distinct()
    colname_deck_list <- ""
    
  } else {
    df_unnest <- df %>%
      filter(!is.na(Wins)) %>%
      filter(!is.null(!!colname_deck_list)) %>%
      {if(is.null(Name_of_card_of_interest)) . else filter(.,Archetype_count > filter_archetype_count)} %>%
      unnest_longer(!!colname_deck_list) %>%
      unnest_wider(!!colname_deck_list, names_sep = "_") %>%
      group_by(id) %>%
      # a filtrer mes uniquement pour le main deck
      mutate(Number_of_cards := sum(!!rlang::sym(paste0(colname_deck_list, "_Count")))) %>%
      filter(Number_of_cards >= expected_min_size)
  }
  

  df_new_card_base <- df_unnest %>%
    {if(is.null(Name_of_card_of_interest)) . else filter(.,!!rlang::sym(paste0(colname_deck_list, "_CardName")) %in% Name_of_card_of_interest)} %>%
    mutate(!!rlang::sym(paste0(colname_deck_list, "_CardName")) := Card_agregueur(
      !!rlang::sym(paste0(colname_deck_list, "_CardName")),
      ALL_mod = TRUE
    )) %>%
    group_by(id, !!rlang::sym(paste0(colname_deck_list, "_CardName"))) %>%
    mutate(
      !!rlang::sym(paste0(colname_deck_list, "_Count")) :=
        sum(!!rlang::sym(paste0(colname_deck_list, "_Count")))
    ) %>%
    distinct() %>%
    {if(No_grouping_column) cbind(.,winrate_by_archetype) else inner_join(
      .,winrate_by_archetype, by = "Archetype") }
    
  
  
  
  
  df_new_card <- df_new_card_base %>%
    
    {if(No_grouping_column) group_by(
      .,
                                     !!rlang::sym(paste0(colname_deck_list, "_CardName")),
                                     !!rlang::sym(paste0(colname_deck_list, "_Count")),
                                     Wins_total,
                                     Losses_total,
                                     Draws_total # ,
                                     # .drop = FALSE
    ) else  group_by(.,
                     Archetype,
                     Archetype_count,
                     !!rlang::sym(paste0(colname_deck_list, "_CardName")),
                     !!rlang::sym(paste0(colname_deck_list, "_Count")),
                     Wins_total,
                     Losses_total,
                     Draws_total # ,
                     # .drop = FALSE
    ) } %>%
    summarise(
      count = n(),
      Wins_card = sum(Wins),
      Losses_card = sum(Losses),
      Draws_card = sum(Draws),
      .groups = "drop"
    ) %>%
    mutate(!!rlang::sym(paste0(colname_deck_list, "_Count")) := as.character(!!rlang::sym(paste0(colname_deck_list, "_Count"))))
  
  
  df_new_card_total <- df_new_card %>%
    # je laisse le doublon dans le cas d'une seul carte pour faciliter les filters
    # group_by(
    #   Archetype,
    #   !!rlang::sym(paste0(colname_deck_list, "_CardName"))
    # ) %>%
    # filter(n() > 1) %>%
    {if(No_grouping_column) group_by(.,
                                     !!rlang::sym(paste0(colname_deck_list, "_CardName")),
                                     Wins_total,
                                     Losses_total,
                                     Draws_total
    ) else  group_by(.,
                     Archetype,
                     Archetype_count,
                     !!rlang::sym(paste0(colname_deck_list, "_CardName")),
                     Wins_total,
                     Losses_total,
                     Draws_total
    ) } %>%
    summarise(
      !!rlang::sym(paste0(colname_deck_list, "_Count")) := "Any",
      count = sum(count,na.rm = TRUE),
      Wins_card = sum(Wins_card),
      Losses_card = sum(Losses_card),
      Draws_card = sum(Draws_card),
      .groups = "drop"
    ) %>%
    rbind(df_new_card) %>%
    filter((Wins_card + Losses_card) > 10) %>%
    {if(No_grouping_column) arrange(
      .,
      !!rlang::sym(paste0(colname_deck_list, "_CardName")),
      !!rlang::sym(paste0(colname_deck_list, "_Count"))
    )
      else  arrange(.,
                    desc(Archetype_count), Archetype, !!rlang::sym(paste0(colname_deck_list, "_CardName")),
                    !!rlang::sym(paste0(colname_deck_list, "_Count"))
      ) } # %>% select(-Archetype_count)
  
  
  Win_rate_df <- df_new_card_total %>%
    {if(No_grouping_column) group_by(.,
                                     !!rlang::sym(paste0(colname_deck_list, "_CardName")),
                                     !!rlang::sym(paste0(colname_deck_list, "_Count")),
                                     count
    ) else  group_by(.,
                     Archetype,
                     Archetype_count,
                     !!rlang::sym(paste0(colname_deck_list, "_CardName")),
                     !!rlang::sym(paste0(colname_deck_list, "_Count")),
                     count
    ) } %>%
    summarise(
      Wins_card = Wins_card,
      Losses_card = Losses_card,
      card_win_rate = winrate_1_data(Wins_card, Losses_card),
      CI_card_win_rate = CI_prop(card_win_rate, (Wins_card + Losses_card)),
      not_card_win_rate = winrate_2_data(
        Wins_total, Losses_total,
        Wins_card, Losses_card
      ),
      delta_winrate_card = card_win_rate - not_card_win_rate,
      CI_delta_winrate_card = CI_2_prop(
        card_win_rate,
        not_card_win_rate,
        (Wins_card + Losses_card),
        (
          (Wins_total - Wins_card) + (Losses_total - Losses_card)
        )
      ),
      card_draw_diff = Draw_diff_2_data(
        Wins_card, Draws_card, Losses_card,
        Wins_total, Draws_total, Losses_total
      ),
      .groups = "drop"
    )
  
  
  Win_rate_df_final <- Win_rate_df %>%
    mutate(
      Sign_delta_wr = factor(ifelse(
        (delta_winrate_card + CI_delta_winrate_card) > 0,
        "+",
        ifelse(
          (delta_winrate_card - CI_delta_winrate_card) < 0,
          "-", "0"
        )
      ), levels = c("+", "0", "-")),
      CI_card_win_rate = formating_CI(card_win_rate, CI_card_win_rate),
      # card_win_rate = round(card_win_rate * 100, 2),
      CI_delta_winrate_card = formating_CI(delta_winrate_card, CI_delta_winrate_card),
      # delta_winrate_card = round(delta_winrate_card * 100, 2),
      # card_draw_diff = round(card_draw_diff * 100, 2),
      count_winrate = count
    ) %>%
    select(
      any_of("Archetype"), count_winrate,
      !!rlang::sym(paste0(colname_deck_list, "_CardName")),
      !!rlang::sym(paste0(colname_deck_list, "_Count")),
      Wins_card,Losses_card,
      card_win_rate, CI_card_win_rate,
      delta_winrate_card, CI_delta_winrate_card, Sign_delta_wr,
      card_draw_diff
    ) %>% 
    rename_all(~str_remove(.,"^_"))
  
  
  return(Win_rate_df_final)
}


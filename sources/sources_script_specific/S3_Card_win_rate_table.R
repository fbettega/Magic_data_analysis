Count_and_winrates_cards_in_decklist_total <- function(df,
                                                       group_var,
                                                       colname_deck_list # don't remove use for unnest
) {
  
  
  if (group_var == "Base_Archetype") {
    select_group_var <- c(group_var, "Archetype")
  } else {
    select_group_var <- group_var
  }
  
  
  if (colname_deck_list == "Mainboard") {
    expected_min_size <- 60
  } else {
    expected_min_size <- 0
  }
  
  winrate_by_archetype <- df %>%
    group_by(across(all_of(select_group_var))) %>%
    summarise(
      Wins_total = sum(Wins),
      Losses_total = sum(Losses),
      Draws_total = sum(Draws),
      .groups = "drop"
    )
  
  
  df_new_card_base <- df %>%
    unnest_longer(!!colname_deck_list) %>%
    unnest_wider(!!colname_deck_list, names_sep = "_") %>%
    group_by(id) %>%
    # a filtrer mes uniquement pour le main deck
    mutate(Number_of_cards := sum(!!rlang::sym(paste0(colname_deck_list, "_Count")))) %>%
    filter(Number_of_cards >= expected_min_size) %>%
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
    inner_join(winrate_by_archetype, by = select_group_var) %>%
    ungroup()
  
  
  
  
  df_new_card_before_filter <- df_new_card_base %>%
    group_by(
      across(all_of(select_group_var)),
      Archetype_count,
      !!rlang::sym(paste0(colname_deck_list, "_CardName")),
      !!rlang::sym(paste0(colname_deck_list, "_Count")),
      Wins_total,
      Losses_total,
      Draws_total
    ) %>%
    summarise(
      count = n(),
      Wins_card = sum(Wins),
      Losses_card = sum(Losses),
      Draws_card = sum(Draws),
      .groups = "drop"
    ) %>%
    mutate(!!rlang::sym(paste0(colname_deck_list, "_Count")) :=
             as.character(!!rlang::sym(paste0(colname_deck_list, "_Count"))))
  
  
  
  
  
  Card_always_in_deck <- df_new_card_before_filter %>%
    group_by(
      across(all_of(select_group_var)),
      Archetype_count,
      !!rlang::sym(paste0(colname_deck_list, "_CardName")),
      # !!rlang::sym(paste0(colname_deck_list,"_Count"))
    ) %>%
    mutate(
      temp_filter_count = Archetype_count - max(count),
      percent_card = (1 - (count / Archetype_count)) * 100
    ) %>%
    filter(temp_filter_count <= 10 |
             percent_card <= 2) %>%
    select(-temp_filter_count, -percent_card) %>%
    ungroup()
  
  
  
  
  df_new_card <- df_new_card_before_filter %>%
    group_by(
      across(all_of(select_group_var)),
      Archetype_count,
      !!rlang::sym(paste0(colname_deck_list, "_CardName")),
      # !!rlang::sym(paste0(colname_deck_list,"_Count"))
    ) %>%
    mutate(
      temp_filter_count = Archetype_count - max(count)
    ) %>%
    mutate(
      temp_filter_count = Archetype_count - max(count),
      percent_card = (1 - (count / Archetype_count)) * 100
    ) %>%
    filter(temp_filter_count > 10 &
             percent_card > 2) %>%
    select(-temp_filter_count, -percent_card) %>%
    ungroup()
  
  
  
  df_new_card_total <- df_new_card %>%
    group_by(
      across(all_of(select_group_var)),
      Archetype_count,
      !!rlang::sym(paste0(colname_deck_list, "_CardName")),
      Wins_total,
      Losses_total,
      Draws_total
    ) %>%
    summarise(
      !!rlang::sym(paste0(colname_deck_list, "_Count")) := "Any",
      count = sum(count),
      Wins_card = sum(Wins_card),
      Losses_card = sum(Losses_card),
      Draws_card = sum(Draws_card),
      .groups = "drop"
    ) %>%
    rbind(df_new_card) %>%
    # filter((Wins_card + Losses_card) > 10) %>%
    arrange(
      desc(Archetype_count), !!rlang::sym(group_var),
      !!rlang::sym(paste0(colname_deck_list, "_CardName")),
      !!rlang::sym(paste0(colname_deck_list, "_Count"))
    )
  
  
  Win_rate_df <- df_new_card_total %>%
    group_by(
      across(all_of(select_group_var)),
      Archetype_count,
      !!rlang::sym(paste0(colname_deck_list, "_CardName")),
      !!rlang::sym(paste0(colname_deck_list, "_Count")),
      count
    ) %>%
    summarise(
      Wins_card = Wins_card,
      Losses_card = Losses_card,
      card_WR = winrate_1_data(Wins_card, Losses_card),
      CI_card_WR = CI_prop(card_WR, (Wins_card + Losses_card)),
      not_card_WR = winrate_2_data(
        Wins_total, Losses_total,
        Wins_card, Losses_card
      ),
      Total_WR = winrate_1_data(Wins_total, Losses_total),
      delta_WR_card = card_WR - not_card_WR,
      CI_delta_WR_card = CI_2_prop(
        card_WR,
        not_card_WR,
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
      CI_signe_vs_Total = 
        ifelse(
        ((card_WR - Total_WR) + (CI_card_WR)) > 0,
        "+",
        ifelse(
          ((card_WR - Total_WR) - (CI_card_WR)) < 0,
          "-", "0"
        )
      ),
      card_WR_vs_Total =
        (card_WR - Total_WR) 
      ,
      CI_card_WR_vs_Total = formating_CI((card_WR - Total_WR), CI_card_WR),
      CI_card_WR = formating_CI(card_WR, CI_card_WR),
      card_WR = 
        card_WR ,
      CI_signe_vs_other = ifelse(
        (delta_WR_card + CI_delta_WR_card) > 0,
        "+",
        ifelse(
          (delta_WR_card - CI_delta_WR_card) < 0,
          "-", "0"
        )
      ),
      CI_delta_WR_card = formating_CI(delta_WR_card, CI_delta_WR_card),
      delta_WR_card =
        delta_WR_card,
      card_draw_diff = 
        card_draw_diff,
      count_WR = count,
      Count_WR_percent = 
        (count / Archetype_count),
      CI_signe_vs_Total = factor(
        ifelse(str_detect(CI_card_WR_vs_Total,"No data"),"0", CI_signe_vs_Total), 
                                 levels = c("+", "0", "-")
      ),
      CI_signe_vs_other = factor(
        ifelse(str_detect(CI_delta_WR_card,"No data"),"0", CI_signe_vs_other), 
        levels = c("+", "0", "-")
        )
    ) %>%
    select(
      all_of(select_group_var),
      count_WR, Count_WR_percent,
      Wins_card,
      Losses_card,
      !!rlang::sym(paste0(colname_deck_list, "_CardName")),
      !!rlang::sym(paste0(colname_deck_list, "_Count")),
      card_WR, CI_card_WR,
      delta_WR_card, CI_delta_WR_card, CI_signe_vs_other,
      card_WR_vs_Total, CI_card_WR_vs_Total, CI_signe_vs_Total # ,card_draw_diff
    )
  
  Card_always_in_deck_final <- Card_always_in_deck %>%
    group_by(
      across(all_of(select_group_var)),
      Archetype_count,
      !!rlang::sym(paste0(colname_deck_list, "_CardName")),
      # !!rlang::sym(paste0(colname_deck_list,"_Count"))
    ) %>%
    summarise(
      Wins_card = sum(Wins_card),
      Losses_card = sum(Losses_card),
      !!rlang::sym(paste0("Most_present_", colname_deck_list, "_Count")) := as.numeric(last(!!rlang::sym(paste0(colname_deck_list, "_Count")), count)),
      N_most_prez = max(count),
      most_prez_perc = N_most_prez / unique(Archetype_count),
      !!rlang::sym(paste0(colname_deck_list, "_Count")) := paste0(!!rlang::sym(paste0(colname_deck_list, "_Count")), collapse = "/")
    ) %>%
    mutate(
      !!rlang::sym(paste0("Not_most_present_", colname_deck_list, "_percent")) :=
        paste0("n = ", Archetype_count - N_most_prez, " (", round(most_prez_perc * 100, 2), " %)")
    ) %>%
    select(-N_most_prez, -most_prez_perc)
  
  return(
    list(
      Win_rate_df_final = Win_rate_df_final,
      Card_always_in_deck_final = Card_always_in_deck_final
    )
  )
}

################################################################################
# Group linear combination of cards 

group_linear_comb_cards <- function(df_long) {
  colname_deck_list <- df_long %>%
    select(ends_with("_CardName")) %>%
    colnames() %>%
    str_remove("_CardName")
  
  df_wide_linear_comb <- df_long %>%
    ungroup() %>%
    pivot_wider(
      names_from = !!rlang::sym(paste0(colname_deck_list, "_CardName")),
      values_from = !!rlang::sym(paste0(colname_deck_list, "_Count")),
      values_fill = "0",
      id_cols = c(Archetype, id)
    ) %>%
    group_split(Archetype)
  
  # x <- df_wide_linear_comb[[2]]
  res_group_df_to_join <- lapply(df_wide_linear_comb, function(x) {
    # print(unique(x$Archetype))
    df_linear_comb_apply_encours <- x %>%
      select(where(~ n_distinct(.) > 1)) %>%
      mutate(
        across(
          -c(id),
          ~ as.numeric(
            factor(.,
                   levels = gtools::mixedsort(unique(.))
            )
          )
        )
      ) %>%
      select(-id)
    linear_combo <- caret::findLinearCombos(
      df_linear_comb_apply_encours
    )$linearCombos
    
    linear_combo_definition <- function(
    fun_list_of_linear_comb,
    n = 1) {
      # print(n)
      
      res <- fun_list_of_linear_comb %>%
        # check whether any numbers of an element are in any of the elements
        map(~ map_lgl(fun_list_of_linear_comb, purrr::compose(any, `%in%`), .x)) %>%
        unique() %>% # drop duplicated groups
        map(~ reduce(fun_list_of_linear_comb[.x], union))
      
      if (length(res) != length(fun_list_of_linear_comb)) {
        res <- linear_combo_definition(
          fun_list_of_linear_comb = res,
          n = n + 1
        )
      }
      
      return(res)
    }
    named_and_groups_combo <-
      linear_combo_definition(fun_list_of_linear_comb = linear_combo) %>%
      
      lapply(., function(x) colnames(df_linear_comb_apply_encours)[x])
    
    df_res <- x
    if (length(named_and_groups_combo) > 0) {
      for (i in seq_along(named_and_groups_combo)) {
        df_res <- df_res %>%
          rowwise() %>%
          mutate(
            !!rlang::sym(paste0(named_and_groups_combo[[i]], collapse = " ; ")) :=
              # ifelse(
              paste(!!!rlang::syms(named_and_groups_combo[[i]]), sep = " ; ")
          ) %>%
          select(-all_of(named_and_groups_combo[[i]]))
        
        # )
      }
    }
    
    res <- df_res %>%
      pivot_longer(-c(Archetype, id),
                   names_to = paste0(colname_deck_list, "_CardName"),
                   values_to = paste0(colname_deck_list, "_Count"),
      ) %>%
      filter(!str_detect(!!rlang::sym(paste0(colname_deck_list, "_Count")), "^0"))
    return(res)
  }) %>%
    bind_rows()
  
  res <- res_group_df_to_join %>%
    left_join(
      df_long %>%
        select(
          -c(Join_main_count),
          -all_of(c(
            paste0(colname_deck_list, "_CardName"),
            paste0(colname_deck_list, "_Count")
          ))
        ) %>%
        distinct(),
      by = join_by(Archetype, id)
    )
}



################################################################################
# Base df preparation from nested df
Prepare_df_for_long_for_model <- function(
    df_base_fun,
    min_arch_presence_fun = filter_archetype_count_5,
    deck_or_side,
    type_of_archetype,
    land_name_fun,
    min_number_of_cards = min_sample_size_5) {

  if (deck_or_side == "All") {
  
    df_Archetype_unnest_without_selection <- rbind(
      df_base_fun %>%
        mutate(Archetype = !!rlang::sym(type_of_archetype)) %>%
        filter(!is.na(Wins)) %>%
        filter(Losses + Wins > 0) %>%
        rowwise() %>%
        filter(!is.null(Mainboard)) %>%
        filter(!is.null(Sideboard)) %>%
        ungroup() %>%
        group_by(Archetype) %>%
        mutate(
          Archetype_count = n()
        ) %>%
        filter(Valide_deck) %>%
        filter(Archetype_count > min_arch_presence_fun) %>%
        arrange(desc(Archetype_count)) %>%
        unnest_longer(Mainboard) %>%
        unnest_wider(Mainboard, names_sep = "_") %>%
        mutate(
          Mainboard_CardName =  paste0(Mainboard_CardName,"_main")
               ) %>% 
        rename(
          `_CardName` = Mainboard_CardName,
          `_Count` = Mainboard_Count
        ),
      df_base_fun %>%
        mutate(Archetype = !!rlang::sym(type_of_archetype)) %>%
        filter(!is.na(Wins)) %>%
        filter(Losses + Wins > 0) %>%
        rowwise() %>%
        filter(!is.null(Mainboard)) %>%
        filter(!is.null(Sideboard)) %>%
        ungroup() %>%
        group_by(Archetype) %>%
        mutate(
          Archetype_count = n()
        ) %>%
        filter(Valide_deck) %>%
        filter(Archetype_count > min_arch_presence_fun) %>%
        arrange(desc(Archetype_count)) %>%
        unnest_longer(Sideboard) %>%
        unnest_wider(Sideboard, names_sep = "_") %>%
        mutate(
          Sideboard_CardName = paste0(
            Sideboard_CardName,"_side")
        ) %>% 
        rename(
          `_CardName` = Sideboard_CardName,
          `_Count` = Sideboard_Count
        )
    )
    deck_or_side <- ""
  } else {
    df_Archetype_unnest_without_selection <- df_base_fun %>%
      mutate(Archetype = !!rlang::sym(type_of_archetype)) %>%
      filter(!is.na(Wins)) %>%
      filter(Losses + Wins > 0) %>%
      rowwise() %>%
      filter(!is.null(Mainboard)) %>%
      filter(!is.null(Sideboard)) %>%
      ungroup() %>%
      group_by(Archetype) %>%
      mutate(
        Archetype_count = n()
      ) %>%
      filter(Valide_deck) %>%
      filter(Archetype_count > min_arch_presence_fun) %>%
      arrange(desc(Archetype_count)) %>%
      unnest_longer(!!deck_or_side) %>%
      unnest_wider(!!deck_or_side, names_sep = "_")
  }
  
  df_Archetype_long <- df_Archetype_unnest_without_selection %>%
    # mutate(Mainboard_CardName = Card_agregueur(Mainboard_CardName)) %>%
    select(
      id, Player, Wins, Losses, Draws, Archetype, Color,
      all_of(
        c(
          paste0(deck_or_side, "_CardName"), paste0(deck_or_side, "_Count")
        )
      )
    ) %>%
    group_by(id) %>%
    mutate(
      Number_of_cards = sum(!!rlang::sym(paste0(deck_or_side, "_Count")))
    ) %>%
    ungroup() %>%
    mutate(
      Archetype =
        factor(Archetype,
               level = unique(.$Archetype)
        )
    )
  
  
  
  Df_archetype_cards_land_name_agreg <- rbind(
    df_Archetype_long %>%
      group_by(Archetype) %>%
      mutate(Archetype_count = n_distinct(id)) %>%
      mutate(
        !!rlang::sym(paste0(deck_or_side, "_CardName")) :=
          Card_agregueur(
            !!rlang::sym(paste0(deck_or_side, "_CardName")),
            ALL_mod = TRUE
          )
      ) %>%
      ungroup() %>%
      group_by(
        id, !!rlang::sym(paste0(deck_or_side, "_CardName"))
      ) %>%
      mutate(
        !!rlang::sym(paste0(deck_or_side, "_Count")) := sum(
          !!rlang::sym(paste0(deck_or_side, "_Count"))
        )
      ) %>%
      distinct(id, !!rlang::sym(paste0(deck_or_side, "_CardName")), .keep_all = TRUE),
    df_Archetype_long %>%
      mutate(
        !!rlang::sym(paste0(deck_or_side, "_Count")) := if_else(
          !!rlang::sym(paste0(deck_or_side, "_CardName")) %in% land_name_fun$name,
          !!rlang::sym(paste0(deck_or_side, "_Count")), 0
        ),
        !!rlang::sym(paste0(deck_or_side, "_CardName")) := "Land_tot"
      ) %>%
      group_by(id) %>%
      mutate(
        !!rlang::sym(paste0(deck_or_side, "_Count")) := sum(!!rlang::sym(paste0(deck_or_side, "_Count")))
      ) %>%
      ungroup() %>%
      group_by(Archetype) %>%
      mutate(Archetype_count = n_distinct(id)) %>%
      ungroup() %>%
      distinct()
  ) %>%
    arrange(desc(Archetype_count), id)
  
  
  
  Df_archetype_cards_agreg <- Df_archetype_cards_land_name_agreg %>%
    group_by(
      Archetype, Archetype_count,
      !!rlang::sym(paste0(deck_or_side, "_CardName")),
      !!rlang::sym(paste0(deck_or_side, "_Count"))
    ) %>%
    summarise(
      Wins = sum(Wins),
      Losses = sum(Losses),
      count_iteration_cards = n(),
      .groups = "drop"
    ) %>%
    group_by(
      Archetype, !!rlang::sym(paste0(deck_or_side, "_CardName"))
    ) %>%
    mutate(
      Wins = sum(Wins),
      Losses = sum(Losses),
      total_number_of_copie = sum(count_iteration_cards),
      most_common_count = max(count_iteration_cards)
    ) %>%
    filter(total_number_of_copie >= min_number_of_cards)
  
  
  Deck_win_rate_join <- df_Archetype_long %>%
    ungroup() %>%
    distinct(id, .keep_all = TRUE) %>%
    group_by(Archetype) %>%
    summarise(Archetype_winrate = sum(Wins) / (sum(Wins) + sum(Losses))) %>%
    ungroup()
  
  
  return(
    list(
      df_agreg = Df_archetype_cards_agreg,
      df_land_agreg = Df_archetype_cards_land_name_agreg,
      deck_winrate = Deck_win_rate_join,
      Archetype_list = sort(levels(df_Archetype_long$Archetype))
    )
  )
}


################################################################################
# Groupe cards and final preparation for model


model_preparation_df <- function(
    df_prett_fun,
    min_arch_presence_fun,
    deck_or_side,
    min_number_of_cards) {
  
  Uncommon_cards_pre_process <- df_prett_fun$df_agreg %>%
    # filter(count_iteration_cards == most_common_count) %>%
    filter(
      (Archetype_count - most_common_count) >= min_number_of_cards,
      # most_common_count >= min_number_of_cards
      # (Archetype_count - total_number_of_copie) >= min_number_of_cards,
      total_number_of_copie >= min_number_of_cards
    ) %>%
    group_by(
      Archetype, !!rlang::sym(paste0(deck_or_side, "_CardName"))
    ) %>%
    mutate(
      # Choix de prendre la plus faible quantité en cas d'éaglité pour le plus commun
      # most_common_quantity = Mainboard_Count[count_iteration_cards == max(count_iteration_cards)][1],
      most_common_quantity = {
        column_name <- !!rlang::sym(paste0(deck_or_side, "_Count"))
        column_name[count_iteration_cards == max(count_iteration_cards)][1]
      },
      min_count_group = !!rlang::sym(paste0(deck_or_side, "_Count"))
    )
  
  
  if(nrow(Uncommon_cards_pre_process) == 0 ){
    Uncommon_cards <- NULL
    }else{
  Uncommon_cards_agreg_out <- Agreg_count_by_cards(
    Uncommon_cards_pre_process,
    deck_or_side,
    min_number_of_cards
  )
  
  Uncommon_cards <- Uncommon_cards_agreg_out %>%
    group_by(
      Archetype, !!rlang::sym(paste0(deck_or_side, "_CardName"))
    )}
  
  # Récupération des cartes a 1 niveaux après agreg
  Base_cards_and_base_count <-
    df_prett_fun$df_agreg %>%
    mutate(
      min_count_group = !!rlang::sym(paste0(deck_or_side, "_Count")),
      most_common_quantity = !!rlang::sym(paste0(deck_or_side, "_Count"))
    ) %>%
    group_by(
      Archetype, !!rlang::sym(paste0(deck_or_side, "_CardName"))
    ) %>%
    mutate(!!rlang::sym(paste0(deck_or_side, "_Count")) :=
             paste0(!!rlang::sym(paste0(deck_or_side, "_Count")),
                    collapse = "/"
             )) %>%
    ungroup() %>%
    filter(count_iteration_cards == most_common_count) %>%
    filter((Archetype_count - most_common_count) < min_number_of_cards) %>%
    rowwise() %>%
    mutate(!!rlang::sym(paste0(deck_or_side, "_Count")) :=
             paste0(
               findIntRuns(
                 as.numeric(unlist(str_split(!!rlang::sym(paste0(deck_or_side, "_Count")), "/")))
               ),
               collapse = "/"
             )) %>%
    ungroup() %>%
    left_join(df_prett_fun$deck_winrate, by = "Archetype") %>%
    group_split(Archetype) %>%
    name_list_of_df_with_arch()
  
  
  if(!is.null(Uncommon_cards)){
  Model_data_Uncommon_cards <- prepare_df_for_model(
    df_fun = Uncommon_cards,
    base_df = df_prett_fun$df_land_agreg,
    cols_fun = deck_or_side
  )
  
  groupe_cards_uncommon_data <- group_linear_comb_cards(
    df_long = Model_data_Uncommon_cards
    )
  }else{
    groupe_cards_uncommon_data <- NULL
  }
  return(
    list(
      group_com_unco_cards_res = groupe_cards_uncommon_data,
      Base_cards_and_base_count_post_format_model = Base_cards_and_base_count
    )
  )
}









# simple function extracting name from a specific list of df 
# and using Archetype column to name the list
name_list_of_df_with_arch <- function(list) {
  names(list) <- lapply(list, function(x) {
    as.character(x$Archetype[1])
  }) %>% unlist()
  
  return(list)
}
#################################################################################
# function that create all models from a prepare df


model_unco_cards_fun <- function(
    df_fun
    ) {
  cols_fun <- df_fun %>%
    select(ends_with("_CardName")) %>%
    colnames() %>%
    str_remove("_CardName")
  # unique(df_fun$Archetype)
  # x <- 'Storm'
  # x <- "Delver"
  # x <- "Tron : Eldrazi Tron"
  # x <- "Boros Energy" 
  model_unco_fun <- lapply(
    unique(df_fun$Archetype),
    function(x) {
      # print(as.character(x))
      # if(x == "Scam") browser()
      df_model <- df_fun %>%
        # select(-Join_main_count) %>%
        filter(Archetype == x) %>%
        ungroup() %>%
        filter(!is.na(!!rlang::sym(paste0(cols_fun, "_CardName")))) %>%
        rowwise() %>%
        mutate(
          !!rlang::sym(paste0(cols_fun, "_Count")) :=
            ifelse(
              str_detect(!!rlang::sym(paste0(cols_fun, "_Count")), " ; "),
              !!rlang::sym(paste0(cols_fun, "_Count")),
              paste0(
                findIntRuns(
                  as.numeric(
                    unlist(str_split(!!rlang::sym(paste0(cols_fun, "_Count")), "/"))
                  )
                ),
                collapse = "/"
              )
            )
        ) %>%
        ungroup() %>%
        group_by(Color) %>%
        mutate(
          Color = ifelse(n() < min_sample_size_5, "Other", Color)
        ) %>%
        ungroup() %>%
        pivot_wider(
          names_from = !!rlang::sym(paste0(cols_fun, "_CardName")),
          values_from = !!rlang::sym(paste0(cols_fun, "_Count")),
          values_fill = "0"
        ) %>%
        column_to_rownames("id") %>% 
        select(-Archetype, -Player,# -id,
               -Archetype_count, -Draws) %>%
        mutate(
          across(
            where(is.character),
            ~ fct_infreq(as.factor(.))
          )
        ) %>%
        select(where(~ n_distinct(.) > 1)) %>% 
        rownames_to_column("id") %>% 
        group_by(across(-c( Wins ,Losses))) %>% 
        summarise(
          id = list(id),
          Wins = sum(Wins),
          Losses = sum(Losses),
          .groups = "drop"
        ) %>% 
        relocate(
          Wins,Losses,.before = 1
        )
      
      model_unco_tot_fun <- df_fun %>%
        filter(Archetype == x) %>%
        ungroup() %>%
        filter(!is.na(!!rlang::sym(paste0(cols_fun, "_CardName")))) %>%
        rowwise() %>%
        mutate(!!rlang::sym(paste0(cols_fun, "_Count")) :=
                 ifelse(
                   str_detect(!!rlang::sym(paste0(cols_fun, "_Count")), " ; "),
                   !!rlang::sym(paste0(cols_fun, "_Count")),
                   paste0(
                     findIntRuns(
                       as.numeric(
                         unlist(str_split(!!rlang::sym(paste0(cols_fun, "_Count")), "/"))
                       )
                     ),
                     collapse = "/"
                   )
                 )) %>%
        ungroup() %>%
        pivot_wider(
          names_from = !!rlang::sym(paste0(cols_fun, "_CardName")),
          values_from = !!rlang::sym(paste0(cols_fun, "_Count")),
          values_fill = "0"
        ) %>%
        column_to_rownames("id") %>% 
        select(-Archetype, -Player,# -id,
               -Archetype_count, -Draws) %>%
        mutate(
          # Choose most common level as references
          across(
            where(is.character),
            ~ factor(
              if_else(. == names(sort(table(.),
                                      decreasing = TRUE
              ))[1],
              names(sort(table(.),
                         decreasing = TRUE
              ))[1], "Other"
              ),
              levels = c(names(sort(table(.),
                                    decreasing = TRUE
              ))[1], "Other")
            )
          )
        ) %>%
        # remove card with only 1+ like basic land fetch ....
        select(where(~ n_distinct(.) > 1)) %>% 
        rownames_to_column("id") %>% 
        group_by(across(-c( Wins ,Losses))) %>% 
        summarise(
          id = list(id),
          Wins = sum(Wins),
          Losses = sum(Losses),
          .groups = "drop"
        ) %>% 
        relocate(
          Wins,Losses,.before = 1
        )
      
      if (nrow(df_model) == 0) {
        model_res <- NULL
        model_res_any <- NULL
        model_res_ridge <- NULL
      } else {
        model_res_any <- model_removing_alias_var(
          df = model_unco_tot_fun # , interaction_fun = interaction_term[x]
        )
        model_res <- model_removing_alias_var(
          df = df_model # , interaction_fun = interaction_term[x]
        )
        
        model_res_any$Archetype <- x
        model_res$Archetype <- x
        model_res_any$id <- tibble(
          rownames = row.names(model_unco_tot_fun),
          id = model_unco_tot_fun$id
          )
        model_res$id <- tibble(
          rownames = row.names(df_model),
          id = df_model$id
        )
        
      }
      return(
        list(
          Model_any = model_res_any,
          Model_count = model_res #,
          # model_ridgge = fit_ridge
        )
      )
    }
  ) %>%
    discard(is.null)
  
  return(model_unco_fun)
}

################################################################################
# 
model_removing_alias_var <- function(
    df) {
  formula_model <- as.formula(
    paste0(
      "cbind(Wins, Losses) ~."
    )
  )
  
  res <- glm(formula_model,
             data = df %>% select(-id),
             family = quasibinomial # binomial
  )
  
  # remove the linearly dependent variables variables
  return(res)
}

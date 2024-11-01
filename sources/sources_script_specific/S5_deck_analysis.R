format_model_list <- function(model_list) {
  model_clean <- lapply(model_list, function(x) {
    # print(as.character(x$Model_any$Archetype))
    
    
    if (is.null(x$Model_any)) {
      Model_any_encours <- NULL
    } else if (
      length(x$Model_any$coefficients) == 1
    ) {
      Model_any_encours <- NULL
    } else {
      Model_any_encours <- x$Model_any %>%
        gtsummary::tbl_regression(exponentiate = TRUE) %>%
        gtsummary::bold_labels() %>%
        gtsummary::add_n(location = "level") %>%
        gtsummary::modify_spanning_header(
          c(
            stat_n, estimate,
            # ci
            conf.low, p.value
          ) ~
            paste0(
              "**", x$Model_any$Archetype, " N :",
              sum(x$Model_any$data$Wins + x$Model_any$data$Losses),
              "**"
            )
        )
    }
    if (is.null(x$Model_count)) {
      Model_count_encours <- NULL
    } else if (
      length(x$Model_count$coefficients) == 1
    ) {
      Model_count_encours <- NULL
    } else {
      Model_count_encours <- x$Model_count %>%
        gtsummary::tbl_regression(exponentiate = TRUE) %>%
        gtsummary::bold_labels() %>%
        gtsummary::add_n(location = "level") %>%
        gtsummary::modify_spanning_header(
          c(
            stat_n, estimate,
            # ci
            conf.low, p.value
          ) ~
            paste0(
              "**", x$Model_count$Archetype, " N :",
              sum(x$Model_count$data$Wins + x$Model_count$data$Losses),
              "**"
            )
        )
    }
    if (is.null(x$model_ridgge)) {
      Model_ridge_encours <- NULL
    } else {
      
      model_format_table_ridge <- cbind(
        bhat = x$model_ridgge$bhat,
        se = x$model_ridgge$se,
        confint = confint(x$model_ridgge),
        pval = x$model_ridgge$pval
      ) %>%
        as.data.frame() %>%
        rename(
          OR = V1
        ) %>%
        mutate(
          OR = exp(OR),
          lower = exp(lower),
          upper = exp(upper)
        ) %>%
        rownames_to_column("Card_name") %>%
        mutate(
          Card_name = str_remove_all(Card_name, "`"),
          Card_name = sub("(\\D(?=\\d))", "\\1:", Card_name, perl = TRUE)
        ) %>%
        right_join(
          x$model_ridgge$data %>%
            select(where(is.factor)) %>%
            pivot_longer(everything()) %>%
            group_by(name, value) %>%
            summarise(
              N = n(),
              .groups = "drop"
            ) %>%
            rowwise() %>%
            mutate(Card_name = paste0(name, ":", value), .before = 1),
          by = join_by(Card_name)
        )  %>%
        mutate(
          sort_col = paste0(
            str_extract(Card_name, "[:alpha:]+(?=:)"),
            ifelse(is.na(OR), "0",
                   as.numeric(str_extract(Card_name, "(?<=:)\\d{1}")) + 1
            )
          )
        ) %>%
        arrange(sort_col) %>%
        select(-sort_col) %>%
        mutate(
          Archetype = as.character(x$model_ridgge$Archetype), .before = 1
        )
      
      
      
      
      
      DF_Model_ridge_encours <- model_format_table_ridge %>%
        separate_wider_delim(
          Card_name,
          delim = ":",
          names = c("Card_name", "quantity")
        ) %>%
        mutate(
          `95% CI` = ifelse(is.na(lower) & is.na(upper), NA, paste0(
            # round(value * (100 * percent),round_val)," ",
            
            round(lower, 2),
            "; ",
            round(upper, 2)
          )), .before = pval
        ) %>%
        select(-c(se, Archetype, lower, upper, name, value)) %>%
        relocate(N, .before = OR) %>%
        group_by(Card_name) %>%
        rename(` ` = quantity)
      
      
      
      Model_ridge_encours <- gt::gt(DF_Model_ridge_encours) %>%
        gt::sub_missing() %>%
        gt::fmt_number(
          columns = -N
        ) %>%
        gt::text_transform(
          locations = gt::cells_row_groups(),
          fn = function(x) {
            lapply(x, function(x) {
              gt::md(paste0("**", x, "**"))
            })
          }
        ) %>%
        gt::cols_align(
          align = c("center"),
          columns = everything()
        ) %>%
        gt::tab_spanner(
          label = gt::md(paste0(
            "**", unique(model_format_table_ridge$Archetype), " N :",
            unique((DF_Model_ridge_encours %>%
                      summarise(n = sum(N)))$n),
            "**"
          )),
          columns = everything()
        )
    }
    
    
    return(
      list(
        Model_any = Model_any_encours,
        Model_count = Model_count_encours,
        model_ridge = Model_ridge_encours
      )
    )
  })
  
  return(model_clean)
}





compute_pairwise_table <- function(df_fun_cor_cat, seuil_common_sup = 0.9) {
  list_of_pairwise_table <- lapply(
    apply(
      combn(names(df_fun_cor_cat), 2), 2,
      function(i) df_fun_cor_cat[i]
    ),
    function(x) {
      card_name <- colnames(x)
      df_en_cours <- x %>%
        rename(
          card1 = 1,
          card2 = 2,
        ) %>%
        filter(!(card1 == "0" & card2 == "0"))
      
      count_card1 <- df_en_cours %>%
        count(card1) %>%
        rename(
          count_card1 = 1,
          n_card1 = 2
        ) %>%
        mutate(
          card1 = card_name[1],
          total_card1 = sum(df_en_cours$card1 != 0)
        )
      
      count_card2 <- df_en_cours %>%
        count(card2) %>%
        rename(
          count_card2 = 1,
          n_card2 = 2
        ) %>%
        mutate(
          card2 = card_name[2],
          total_card2 = sum(df_en_cours$card2 != 0)
        )
      res <- as.data.frame(table(x)) %>%
        mutate(
          card1 = card_name[1],
          .before = 1
        ) %>%
        mutate(
          card2 = card_name[2],
          .before = 3
        ) %>%
        rename(
          count_card1 = 2,
          count_card2 = 4,
        ) %>%
        left_join(count_card1, by = join_by(card1, count_card1)) %>%
        left_join(count_card2, by = join_by(card2, count_card2))
      
      return(res)
    }
    # )
  ) %>%
    bind_rows() %>%
    filter(!(count_card1 == "0" | count_card2 == "0")) %>%
    drop_na() %>%
    rowwise() %>%
    mutate(
      MIN_COL_name = c("card1", "card2")[which.min(c_across(c(n_card1, n_card2)))],
      perc1 = Freq / total_card1,
      perc2 = Freq / total_card2,
      test = perc1 + perc2,
      remaining_card1 = total_card1 - Freq,
      remaining_card2 = total_card2 - Freq,
      max_overlap = max(c(perc1, perc2)),
      percent_support = Freq / min(total_card1, total_card2)
    ) %>%
    group_by(card1, card2) %>%
    summarise(percent_support = max(max_overlap), .groups = "drop") %>%
    ungroup() %>%
    filter(
      percent_support > (seuil_common_sup)
    ) %>%
    arrange(desc(percent_support))
  
  return(list_of_pairwise_table)
}

##########################################################################################################################################################################################################

group_variable_function <- function(df_arch_init, seuil_common_sup) {
  Archetype_en_cours <- unique(df_arch_init$Archetype)
  df_fun_cor_cat_arch <- df_arch_init %>%
    select(-c(
      id
    )) %>%
    select(-ends_with(" land"), -any_of("Land_tot")) %>%
    select(where(~ n_distinct(.) > 1))
  if (ncol(df_fun_cor_cat_arch) <= 1) {
    res <- ""
  } else {
    list_of_pairwise_table_en_cours <- compute_pairwise_table(
      df_fun_cor_cat = df_fun_cor_cat_arch,
      seuil_common_sup = seuil_common_sup
    ) %>%
      rowwise() %>%
      mutate(result = paste0("`", card1, "`", ":", "`", card2, "`"))
    res <- paste0(list_of_pairwise_table_en_cours$result, collapse = "+")
  }
  return(res)
}

most_common_association <- function(table_res) {
  card_to_choose <- table_res
  select_row <- table_res %>%
    slice_max(perc, n = 1, with_ties = FALSE)
  while (nrow(card_to_choose) != 0) {
    card_to_choose <- card_to_choose %>%
      filter(
        card1 %notin% c(
          select_row$card1,
          select_row$card2
        ),
        card2 %notin% c(
          select_row$card1,
          select_row$card2
        )
      )
    if (nrow(card_to_choose) != 0) {
      select_row <- rbind(
        select_row,
        card_to_choose %>%
          slice_max(perc, n = 1, with_ties = FALSE)
      )
    }
    res <- select_row %>%
      select(-c(
        card2_agreg,
        card1_agreg,
        Freq,
        MIN_COL_name,
        min_common_support
      ))
  }
}


compute_vif_for_model <- function(res_model) {
  res_fun <- lapply(res_model, function(x) {
    res_per_arch <- lapply(seq_along(x), function(y) {
      model_vif_fun <- x[[y]]
      
      print(as.character(x$Model_any$Archetype))
      if (length(coefficients(model_vif_fun)) < 3) {
        res <- data.frame(
          model = names(x[y]),
          name = "a",
          Df = 1,
          vif = 0
        ) %>% as_tibble()
      } else {
        vif_res <- car::vif(model_vif_fun)
        
        if (is.null(ncol(vif_res))) {
          res <- data.frame(
            model = names(x[y]),
            name = names(vif_res),
            Df = 1,
            vif = vif_res
          ) %>%
            as_tibble() %>%
            arrange(desc(vif))
        } else {
          res <- as.data.frame(vif_res) %>%
            rownames_to_column("name") %>%
            as_tibble() %>%
            mutate(
              model = names(x[y]),
              .before = 1
            ) %>%
            select(-GVIF) %>%
            rename(vif = `GVIF^(1/(2*Df))`) %>%
            arrange(desc(vif))
        }
      }
    }) %>%
      bind_rows() %>%
      mutate(
        Archetype = x$Model_any$Archetype,
        .before = 1
      ) %>%
      filter(vif > 3)
  }) %>%
    bind_rows()
  return(res_fun)
}


Generate_and_format_model_result <-
  function(
    df_base_fun,
    min_arch_presence_fun = filter_archetype_count_5,
    deck_or_side,
    type_of_archetype,
    land_name_fun,
    min_number_of_cards = min_sample_size_5) {
    result_pre_treatement <- Prepare_df_for_long_for_model(
      df_base_fun = df_base_fun,
      min_arch_presence_fun = filter_archetype_count_5,
      deck_or_side = deck_or_side,
      type_of_archetype = type_of_archetype,
      land_name_fun = land_name_fun,
      min_number_of_cards = min_sample_size_5
    )
    
    DF_prepare_for_model <- model_preparation_df(
      df_prett_fun = result_pre_treatement,
      min_arch_presence_fun = filter_archetype_count_5,
      deck_or_side = deck_or_side,
      min_number_of_cards = min_sample_size_5
    )
    
    # Projet avec les images des cartes pour arborescence deck list penser a mettre en gras le compte le plus rreprésenter pour base card and base count
    result_models_Uncommon_cards_all_arch <- model_unco_cards_fun(
      df_fun = DF_prepare_for_model$group_com_unco_cards_res
    ) %>%
      name_list_of_model_with_string(unique(DF_prepare_for_model$group_com_unco_cards_res$Archetype))
    
    
    uncomon_card_format_model <- format_model_list(result_models_Uncommon_cards_all_arch) %>%
      name_list_of_model_with_string(unique(DF_prepare_for_model$group_com_unco_cards_res$Archetype))
    
    
    Df_base_number_to_print <- result_pre_treatement$df_land_agreg %>%
      ungroup() %>%
      distinct(id, .keep_all = TRUE) %>%
      group_by(Archetype, Archetype_count) %>%
      summarise(
        Wins = sum(Wins),
        Losses = sum(Losses), .groups = "drop"
      )
    
    return(
      list(
        Base_cards_and_base_count_res = DF_prepare_for_model$Base_cards_and_base_count_post_format_model,
        uncomon_card_format_model_res = uncomon_card_format_model,
        list_archetype = result_pre_treatement$Archetype_list,
        Df_base_number_to_print_res = Df_base_number_to_print
      )
    )
  }


print_result_total_script_deck_ana <- function(
    res_main,
    res_side,
    iteration) {
  temp_df_to_print <- res_main$Df_base_number_to_print_res %>%
    filter(Archetype == iteration)
  
  
  print_count_string <- paste0(
    "Number of deck : ", temp_df_to_print %>%
      pull(Archetype_count), " for ", temp_df_to_print$Wins,
    " wins over ", temp_df_to_print$Wins + temp_df_to_print$Losses, " rounds"
  )
  
  # Inserts Month titles
  pander::pandoc.header(iteration, level = 2)
  # Section contents
  pander::pandoc.header("Main deck", level = 3)
  pander::pandoc.p("")
  
  
  if (iteration %in% names(res_main$Base_cards_and_base_count_res)) {
    pander::pandoc.header("Base Cards", level = 4)
    pander::pandoc.p("")
    pander::pandoc.p("Cards Always in deck with nearly fix count")
    pander::pandoc.p(print_count_string)
    
    pander::pandoc.p("")
    flextable::flextable_to_rmd(
      flextable::flextable(
        res_main$Base_cards_and_base_count_res[[iteration]] %>%
          mutate(
            WR = paste0(round(((Wins / (Wins + Losses)) - Archetype_winrate) * 100, 2), " %"),
            Not_most_common_count = total_number_of_copie - most_common_count,
            Card_not_in_deck = Archetype_count - total_number_of_copie
          ) %>%
          select(
            Mainboard_CardName, WR, Mainboard_Count,
            most_common_quantity, Card_not_in_deck, Not_most_common_count
          )
      ) %>% flextable::align(align = "center", part = "all")
    )
  }
  
  
  if (iteration %in% names(res_main$uncomon_card_format_model_res)) {
    pander::pandoc.header("Variable Cards", level = 4)
    pander::pandoc.p("")
    pander::pandoc.p("Cards not always in deck using binomial regression for WR")
    pander::pandoc.p("")
    pander::pandoc.p("::: {.panel-tabset .nav-pills}")
    if (!is.null(res_main$uncomon_card_format_model_res[[iteration]]$Model_any)) {
      pander::pandoc.header("Any", level = 5)
      flextable::flextable_to_rmd(
        res_main$uncomon_card_format_model_res[[iteration]]$Model_any %>%
          gtsummary::as_flex_table()
      )
    }
    pander::pandoc.p("")
    pander::pandoc.p("")
    if (!is.null(res_main$uncomon_card_format_model_res[[iteration]]$Model_count)) {
      pander::pandoc.header("Count", level = 5)
      flextable::flextable_to_rmd(
        res_main$uncomon_card_format_model_res[[iteration]]$Model_count %>%
          gtsummary::as_flex_table()
      )
    }
    pander::pandoc.p("")
    if (!is.null(res_main$uncomon_card_format_model_res[[iteration]]$model_ridge)) {
      pander::pandoc.header("Ridge", level = 5)
      print(
        htmltools::tagList(res_main$uncomon_card_format_model_res[[iteration]]$model_ridge))
      
    }
    pander::pandoc.p(":::")
  }
  # adding also empty lines, to be sure that this is valid Markdown
  pander::pandoc.p("")
  pander::pandoc.p("")
  
  pander::pandoc.header("Side Board", level = 3)
  pander::pandoc.p("")
  
  
  if (iteration %in% names(res_side$Base_cards_and_base_count_res)) {
    pander::pandoc.header("Base Cards", level = 4)
    pander::pandoc.p("")
    pander::pandoc.p("Cards Always in deck with nearly fix count")
    pander::pandoc.p(print_count_string)
    pander::pandoc.p("")
    flextable::flextable_to_rmd(
      flextable::flextable(
        res_side$Base_cards_and_base_count_res[[iteration]] %>%
          mutate(
            WR = paste0(
              round(
                ((Wins / (Wins + Losses)) - Archetype_winrate) * 100, 2),
              " %"),
            Not_most_common_count = total_number_of_copie - most_common_count,
            Card_not_in_deck = Archetype_count - total_number_of_copie
          ) %>%
          select(
            Sideboard_CardName, WR, Sideboard_Count,
            most_common_quantity, Card_not_in_deck, Not_most_common_count
          )
      ) %>%
        flextable::align(align = "center", part = "all")
    )
  }
  
  if (iteration %in% names(res_side$uncomon_card_format_model_res)) {
    pander::pandoc.header("Variable Cards", level = 4)
    pander::pandoc.p("")
    pander::pandoc.p("Cards not always in deck using binomial regression for WR")
    pander::pandoc.p("")
    #     pander::pandoc.p(':::::::::::::: {.columns}
    # ::: {.column width="50%"}')
    
    pander::pandoc.p("::: {.panel-tabset .nav-pills}")
    if (!is.null(
      res_side$uncomon_card_format_model_res[[iteration]]$Model_any
    )) {
      pander::pandoc.header("Any", level = 5)
      flextable::flextable_to_rmd(
        res_side$uncomon_card_format_model_res[[iteration]]$Model_any %>%
          gtsummary::as_flex_table()
      )
    }
    # pander::pandoc.p(":::")
    #
    # pander::pandoc.p('::: {.column width="50%"}')
    pander::pandoc.p("")
    if (!is.null(
      res_side$uncomon_card_format_model_res[[iteration]]$Model_count
    )) {
      pander::pandoc.header("Count", level = 5)
      flextable::flextable_to_rmd(
        res_side$uncomon_card_format_model_res[[iteration]]$Model_count %>%
          gtsummary::as_flex_table()
      )
    }
    pander::pandoc.p("")
    if (!is.null(
      res_side$uncomon_card_format_model_res[[iteration]]$model_ridge
    )
    ) {
      pander::pandoc.header("Ridge", level = 5)
      # flextable::flextable_to_rmd(
      print(htmltools::tagList(res_side$uncomon_card_format_model_res[[iteration]]$model_ridge)) # %>%        gt::as_flex_table()
      # )
    }
    #     pander::pandoc.p(":::
    # ::::::::::::::")
    pander::pandoc.p(":::")
  }
  # adding also empty lines, to be sure that this is valid Markdown
  pander::pandoc.p("")
  pander::pandoc.p("")
  
  
  
  pander::pandoc.p("")
  pander::pandoc.p("")
}
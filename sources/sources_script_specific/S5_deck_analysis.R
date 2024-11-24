flextable_hyper_link_from_scry_fall <- function(
    text,
    scry_fall_df = df_with_link_any
){
  out <- lapply(text, function(x){
    if(x %in% scry_fall_df$CardName){
      flextable::hyperlink_text(
        x ,
        url = scry_fall_df$scryfall_uri[scry_fall_df$CardName == x]
      )
      
    } else flextable::as_chunk(x)
  })
  return(out)
}


format_model_list <- function(
    model_list,
    scry_fall_db_format_par = NULL) {
  # x <- model_list[[1]]
  model_clean <- lapply(model_list, function(x) {
    # print(as.character(x$Model_any$Archetype))
    
    
    if (is.null(x$Model_any)) {
      Model_any_encours <- NULL
    } else if (
      length(x$Model_any$coefficients) == 1
    ) {
      Model_any_encours <- NULL
    } else {
      
      df_with_link_any <-
        data.frame(
          CardName = colnames(
            x$Model_any$data %>%
              select(
                -any_of(c("Wins", "Losses", "Color", "Number_of_cards"))
              )
          )
        ) %>%
        left_join(
          join_with_scryfall(
            Df_with_cardname =   .,
            cardname_col = "CardName",
            scry_fall_df = scry_fall_db_format_par
          ),
          by = c("CardName" = "CardName")
        ) %>%
        left_join(
          scry_fall_db_format_par %>%
            select(id, scryfall_uri),
          by = join_by(
            scry_fall_id == id
          )
        ) %>% 
        left_join(
          agregate_land_link(),
          by = join_by("CardName" == join_name)
        ) %>%
        mutate(
          scryfall_uri = ifelse(
            is.na(scryfall_uri)
            & !is.na(search_Link ) ,search_Link ,scryfall_uri
          )
        ) %>%
        select(-scry_fall_id ,- search_Link) %>% 
        filter(!is.na(scryfall_uri))
      
      
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
        ) %>%
        gtsummary::as_flex_table()  %>% 
        flextable::compose(
          j = "label",
          value = flextable_hyper_link_from_scry_fall(
            label,
            scry_fall_df = df_with_link_any
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
      df_with_link_count <-
        data.frame(
          CardName = colnames(
            x$Model_count$data %>%
              select(
                -any_of(c("Wins", "Losses", "Color", "Number_of_cards"))
              )
          )
        ) %>%
        left_join(
          join_with_scryfall(
            Df_with_cardname =   .,
            cardname_col = "CardName",
            scry_fall_df = scry_fall_db_format_par
          ),
          by = c("CardName" = "CardName")
        ) %>%
        left_join(
          scry_fall_db_format_par %>%
            select(id, scryfall_uri),
          by = join_by(
            scry_fall_id == id
          )
        ) %>% 
        left_join(
          agregate_land_link(),
          by = join_by("CardName" == join_name)
        ) %>%
        mutate(
          scryfall_uri = ifelse(
            is.na(scryfall_uri)
            & !is.na(search_Link ) ,search_Link ,scryfall_uri
          )
        ) %>%
        select(-scry_fall_id ,- search_Link) %>% 
        filter(!is.na(scryfall_uri))
      
   
      
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
        ) %>%
        gtsummary::as_flex_table()  %>% 
        flextable::compose(
          j = "label",
          value = flextable_hyper_link_from_scry_fall(
            label,
            scry_fall_df = df_with_link_count
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
        ) %>%
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
      
      
      df_with_link_ridge <-
        data.frame(
          CardName = DF_Model_ridge_encours$Card_name
        ) %>%
        left_join(
          join_with_scryfall(
            Df_with_cardname =   .,
            cardname_col = "CardName",
            scry_fall_df = scry_fall_db_format_par
          ),
          by = c("CardName" = "CardName")
        ) %>%
        inner_join(
          scry_fall_db_format_par %>%
            select(id, scryfall_uri),
          by = join_by(
            scry_fall_id == id
          )
        ) %>%
        select(-scry_fall_id) %>%
        distinct()
      
      
      Model_ridge_encours <- gt::gt(DF_Model_ridge_encours) %>%
        gt::sub_missing() %>%
        gt::fmt_number(
          columns = -N
        ) %>%
        gt::cols_align(
          align = c("center"),
          columns = everything()
        ) %>%
        gt::tab_spanner(
          label = gt::html(paste0(
            "<b>", unique(model_format_table_ridge$Archetype), " N :",
            unique((DF_Model_ridge_encours %>%
                      summarise(n = sum(N)))$n),
            "</b>"
          )),
          columns = everything()
        ) %>%
        gt::text_transform(
          locations = gt::cells_row_groups(),
          fn = function(x) {
            # print(x)
            lapply(x, function(y) {
              tibble(
                base_name = y
              ) %>%
                left_join(
                  df_with_link_ridge,
                  by = join_by(base_name == CardName)
                ) %>%
                mutate(
                  final_name = gt::html(ifelse(
                    !is.na(scryfall_uri),
                    paste0(
                      '<a href=\"', scryfall_uri, '"><b>', base_name, "</b></a>"
                    ),
                    paste0("<b>", base_name, "</b>")
                  ))
                ) %>%
                pull(final_name)
            })
          }
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
    min_number_of_cards = min_sample_size_5,
    db_scryfall_fun_par) {
    
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
      name_list_of_model_with_string(
        unique(DF_prepare_for_model$group_com_unco_cards_res$Archetype)
        )
    
    uncomon_card_format_model <- format_model_list(
      model_list = result_models_Uncommon_cards_all_arch,
      scry_fall_db_format_par = db_scryfall_fun_par
    ) %>%
      name_list_of_model_with_string(
        unique(
          DF_prepare_for_model$group_com_unco_cards_res$Archetype
          )
        )
    
    
    Df_base_number_to_print <- result_pre_treatement$df_land_agreg %>%
      ungroup() %>%
      distinct(id, .keep_all = TRUE) %>%
      group_by(Archetype, Archetype_count) %>%
      summarise(
        Wins = sum(Wins),
        Losses = sum(Losses), 
        .groups = "drop"
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
################################################################################


################################################################################
############## function that print result for main side or all  ################
print_main_side <- function(
    res_encours_fun,
    iteration_print ,
    # print_count_string,
    type,
    Df_with_cardname
){
  
  
  
  # Inserts Month titles
  
  # Section contents
  pander::pandoc.header(type, level = 3)
  pander::pandoc.p("")
  # pander::pandoc.p(print_count_string)
  # pander::pandoc.p("")
  
  if (iteration_print %in% names(res_encours_fun$Base_cards_and_base_count_res)) {
    
    colname_deck_list <- res_encours_fun$Base_cards_and_base_count_res[[iteration_print]] %>% 
      select(ends_with("_CardName")) %>% 
      colnames() %>% 
      str_remove("_CardName")
    
    pander::pandoc.header("Base Cards", level = 4)
    pander::pandoc.p("")
    pander::pandoc.p("Cards Always in deck with nearly fix count")
    pander::pandoc.p("")
    pander::pandoc.p(
      paste0("Total number of most common count : ",
             sum(res_encours_fun$Base_cards_and_base_count_res[[iteration_print]]$most_common_quantity)
                            )
      )
    pander::pandoc.p("")
    flextable::flextable_to_rmd(
      flextable::flextable(
        res_encours_fun$Base_cards_and_base_count_res[[iteration_print]] %>%
          mutate(
            WR = paste0(round(((Wins / (Wins + Losses)) - Archetype_winrate) * 100, 2), " %"),
            Not_most_common_count = total_number_of_copie - most_common_count,
            Card_not_in_deck = Archetype_count - total_number_of_copie
          ) %>%
          select(
            !!rlang::sym(paste0(colname_deck_list, "_CardName")), WR, !!rlang::sym(paste0(colname_deck_list, "_Count")),
            most_common_quantity, Card_not_in_deck, Not_most_common_count
          )  %>%
          left_join(
            join_with_scryfall(
              Df_with_cardname =   .,
              cardname_col = paste0(colname_deck_list, "_CardName"),
              scry_fall_df = Df_with_cardname
            ),
            by = join_by(!!rlang::sym(paste0(colname_deck_list, "_CardName")) == CardName)
          ) %>%
          left_join(
            Df_with_cardname %>%
              select(id, scryfall_uri),
            by = join_by(
              scry_fall_id == id
            )
          ) %>% 
          left_join(
            agregate_land_link(),
            by = join_by(!!rlang::sym(paste0(colname_deck_list, "_CardName")) == join_name)
          ) %>%
          mutate(
            scryfall_uri = ifelse(
              is.na(scryfall_uri)
              & !is.na(search_Link ) ,search_Link ,scryfall_uri
            )
          ) %>%
          select(-scry_fall_id,-search_Link)
      ) %>%
        flextable::compose(j = paste0(colname_deck_list, "_CardName"),
                           value = flextable::as_paragraph(
                             flextable::hyperlink_text(x = !!rlang::sym(paste0(colname_deck_list, "_CardName")), url = scryfall_uri)
                           )
        ) %>%
        flextable::delete_columns( j = "scryfall_uri") %>%
        flextable::align(align = "center", part = "all")
    )
  }
  if (iteration_print %in% names(res_encours_fun$uncomon_card_format_model_res)) {
    pander::pandoc.header("Variable Cards", level = 4)
    pander::pandoc.p("")
    pander::pandoc.p("Cards not always in deck using binomial regression for WR")
    pander::pandoc.p("")
    # open navpills
    pander::pandoc.p("::: {.panel-tabset .nav-pills}")
    if (!is.null(res_encours_fun$uncomon_card_format_model_res[[iteration_print]]$Model_any)) {
      pander::pandoc.header("Any", level = 5)
      
      flextable::flextable_to_rmd(
        res_encours_fun$uncomon_card_format_model_res[[iteration_print]]$Model_any
      )
    }
    pander::pandoc.p("")
    pander::pandoc.p("")
    if (!is.null(res_encours_fun$uncomon_card_format_model_res[[iteration_print]]$Model_count)) {
      pander::pandoc.header("Count", level = 5)
      flextable::flextable_to_rmd(
        res_encours_fun$uncomon_card_format_model_res[[iteration_print]]$Model_count
      )
    }
    pander::pandoc.p("")
    if (!is.null(res_encours_fun$uncomon_card_format_model_res[[iteration_print]]$model_ridge)) {
      pander::pandoc.header("Ridge", level = 5)
      print(
        htmltools::tagList(
          res_encours_fun$uncomon_card_format_model_res[[iteration_print]]$model_ridge
        )
      )
    }
    # close nav pill
    pander::pandoc.p(":::")
  }
  # adding also empty lines, to be sure that this is valid Markdown
  pander::pandoc.p("")
  pander::pandoc.p("")
  
}







print_result_total_script_deck_ana <- function(
    res_main,
    res_side,
    res_75,
    scryfall_db,
    iteration) {
  temp_df_to_print <- res_main$Df_base_number_to_print_res %>%
    filter(Archetype == iteration)
  
  print_count_string <- paste0(
    "Number of deck : ", temp_df_to_print %>%
      pull(Archetype_count), " for ", temp_df_to_print$Wins,
    " wins over ", temp_df_to_print$Wins + temp_df_to_print$Losses, " rounds"
  )
  
  pander::pandoc.header(iteration, level = 2)
  pander::pandoc.p("")
  pander::pandoc.p(print_count_string)
  pander::pandoc.p("")  
  
  print_main_side(
    res_encours_fun = res_main ,
    iteration_print = iteration,
    type = "Main deck",
    Df_with_cardname = scryfall_db
  )
  
  print_main_side(
    res_encours_fun = res_side ,
    iteration_print = iteration,
    type = "Side Board",
    Df_with_cardname = scryfall_db
  )
  
  print_main_side(
    res_encours_fun = res_75 ,
    iteration_print = iteration,
    type = "Total 75",
    Df_with_cardname = scryfall_db
  )
}
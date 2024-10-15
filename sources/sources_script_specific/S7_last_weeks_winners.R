top_8_table_and_plot_prez_generator <- function(df_fun,current_tournament){
  Tournament_of_interest_en_cours <-
    df_fun  %>%
    filter(TournamentFile == current_tournament) %>%
    group_by(Archetype) %>%
    mutate(
      Archetype_count = n()
    ) %>%
    arrange(Archetype_count) %>% 
    ungroup() %>% 
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
        
        factor(Base_Archetype,
               level = unique(.$Base_Archetype)
        ),
      # A reflechir ou tu mets ça selon les filters
      Archetype_percent = Archetype_count / nrow(.)
    )
  
  colors_scales_presence_en_cours <- scheme$hex(
    length(levels(Tournament_of_interest_en_cours$Base_Archetype))
  )
  
  
  plot_tournament_presence <- 
    plot_presence_fun(
      df_base = Tournament_of_interest_en_cours,
      color_scheme = colors_scales_presence_en_cours,
      time_limit = Inf,
      compare_time_limit = NULL
    )
  
  unlist_side_or_main_deck_winner <- function(df,top_x_rank,cols_fun){
    not_colfuns <- ifelse(cols_fun == "Sideboard","Mainboard","Sideboard")
    
    Unnest_filter_table <- df %>%  
      select(
        all_of(cols_fun),all_of(not_colfuns),Place,Player,AnchorUri, Archetype,Base_Archetype
      ) %>% 
      select(-all_of(not_colfuns)) %>% 
      filter(Place <= max(sort(Place)[1:top_x_rank])) %>% 
      arrange(Place) %>% 
      # mutate(rank = 1:nrow(.)) %>% 
      # mutate(Player = paste0('[',Place," : ",Player,'](',AnchorUri,')'))%>% 
      # select(-AnchorUri#,-rank
      #      ) %>%
      unnest_longer(!!rlang::sym(cols_fun)) %>%
      unnest_wider(!!rlang::sym(cols_fun), names_sep = "_") %>% 
      mutate(Main_or_side = cols_fun) %>% 
      rename(Count = paste0(cols_fun, "_Count") ,CardName = paste0(cols_fun, "_CardName"))
    return(Unnest_filter_table)
  }
  
  
  Df_combine <- rbind(
    unlist_side_or_main_deck_winner(
      Tournament_of_interest_en_cours, 
      top_x_rank = 8,
      "Mainboard"
    ),
    unlist_side_or_main_deck_winner(
      Tournament_of_interest_en_cours,
      top_x_rank =8,
      "Sideboard"
    )
  ) %>% 
    group_by(Place,Player) %>% 
    group_split()
  
  
  
  list_of_table_to_plot <- lapply(Df_combine, function(y){
    
    table_format <- y %>% 
      select(-Place,-Player,-AnchorUri,-Archetype,-Base_Archetype) %>% 
      group_by(Main_or_side) %>%
      gt::gt(rowname_col = "CardName") %>%  
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_row_groups()
      ) %>%
      gt::tab_options(column_labels.hidden = TRUE,
                      table.layout = "Auto") %>%
      gt::as_raw_html()
    
    return(table_format)
  }) %>% 
    bind_cols()
  
  # manque archetype et base archetype
  title_table <- lapply(Df_combine, function(y){ title_table <- gt::md(paste0(
    "Place : ", unique(y$Place)," Archetype : ",unique(y$Archetype),"/",unique(y$Base_Archetype), "<br>",
    "[Player : ",unique(y$Player),"](",unique(y$AnchorUri),")")
  )
  }) %>%
    purrr::set_names(colnames(list_of_table_to_plot))
  
  
  
  final_table <- list_of_table_to_plot %>% 
    gt::gt() %>% 
    gt::fmt_markdown(columns = everything()) %>% #render cell contents as html
    gt::cols_label(
      .list = title_table
    ) %>% 
    gt::opt_stylize(style = 5, color = "cyan", add_row_striping = TRUE) %>%
    gt::cols_align(
      align = c("center"),
      columns = everything()
    ) %>% gt::tab_style( style = "vertical-align:top", locations = gt::cells_body() ) %>% 
    gt::tab_style( style = "vertical-align:top", locations = gt::cells_column_labels() )
  
  
  
  return(list(plot_prez = plot_tournament_presence,
              top8_table = final_table))
}


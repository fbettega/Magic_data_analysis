top_8_table_and_plot_prez_generator <- function(
    df_fun,
    current_tournament,
    top_8_table_and_plot_scry_fall_db
    ){
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
      filter(Place <= max(sort(Place)[1:top_x_rank],na.rm = TRUE)) %>% 
      arrange(Place) %>% 
      unnest_longer(!!rlang::sym(cols_fun)) %>%
      unnest_wider(!!rlang::sym(cols_fun), names_sep = "_") %>% 
      mutate(Main_or_side = cols_fun) %>% 
      rename(Count = paste0(cols_fun, "_Count") ,CardName = paste0(cols_fun, "_CardName"))
    return(Unnest_filter_table)
  }
  
    
  Df_combine_base <- rbind(
    unlist_side_or_main_deck_winner(
      Tournament_of_interest_en_cours, 
      top_x_rank = 8,
      "Mainboard"
    ),
    unlist_side_or_main_deck_winner(
      Tournament_of_interest_en_cours,
      top_x_rank = 8,
      "Sideboard"
    )
  )  
    
    Df_combine <- Df_combine_base %>% 
    group_by(Place,Player) %>%
    group_split()
  
    
    Df_combine_join_with_scryfall <- Df_combine_base %>% 
      left_join(
        join_with_scryfall(
          Df_with_cardname =   .,
          cardname_col = "CardName" ,
          scry_fall_df = top_8_table_and_plot_scry_fall_db
        ),
        by = c("CardName" = "CardName")
      ) %>%
      left_join(
        top_8_table_and_plot_scry_fall_db %>%
          select(id,scryfall_uri),
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
      select(CardName,
             scryfall_uri
             ) %>% distinct()
      
  list_of_table_to_plot <- lapply(Df_combine, function(y){
    table_format <- y %>% 
      select(-Place,-Player,-AnchorUri,-Archetype,-Base_Archetype) %>% 
      group_by(Main_or_side) %>%
      gt::gt(
           rowname_col = "CardName"
        ) %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_row_groups()
      ) %>%
      gt::text_transform(
        locations = 
          gt::cells_stub(),
          # gt::cells_body(columns = CardName),
        
        # ICI
        fn = function(x) {
          tibble(
            base_name = x
          ) %>% 
            left_join(
              Df_combine_join_with_scryfall,
              by = join_by(base_name == CardName)
            ) %>% 
            
            
            
            mutate(
              final_name = ifelse(
                !is.na(scryfall_uri),
                paste0(
                  '<a href=\"',scryfall_uri,'">',base_name,'</a>'
                ),
                base_name
              )
            ) %>% pull(final_name)
        }
      )  %>% 
      gt::tab_options(
        column_labels.hidden = TRUE,
                      table.layout = "Auto"
        )  %>%
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
    ) %>% 
    gt::tab_style( 
      style = "vertical-align:top", locations = gt::cells_body() 
      ) %>% 
    gt::tab_style(
      style = "vertical-align:top", locations = gt::cells_column_labels() 
      ) 
  
  
  
  return(list(plot_prez = plot_tournament_presence,
              top8_table = final_table))
}


fun_print_tournament <- function(
    tournament_file_fun,
    total_result_fun,
    iteration){
  
  pander::pandoc.header(paste0("Week : ",names(tournament_file_fun)[iteration]), level = 1)
  pander::pandoc.p("")
  pander::pandoc.p("")
  res <- lapply(seq_along(total_result_fun), function(u){
    
    
    
    # for (u in seq_along(total_result_fun)){
    if(names(total_result_fun)[u] %in% tournament_file_fun[[iteration]]){
      
      pander::pandoc.header(total_result_fun[[u]]$Title,
                            level = 2)
      pander::pandoc.p("")
      pander::pandoc.p("")
      pander::pandoc.p("::: {.panel-tabset .nav-pills}")
      pander::pandoc.p("")
      pander::pandoc.p("")
      
      # pander::pandoc.header(total_result_fun[[u]]$Title, level = 3)
      # pander::pandoc.p("")
      # pander::pandoc.p("")
      pander::pandoc.header("Presence plot", level = 3)
      pander::pandoc.p("")
      pander::pandoc.p("")
      
      
      print(
      htmltools::tagList(
        ggplotly(total_result_fun[[u]]$plot$plot_prez)
         )
         )

      
      # solution to force inclusion of plotly in loop but lead to really large files
      # htmlwidgets::saveWidget(
      #   total_result_fun[[u]]$plot$plot_prez,
      #   paste0("../data/intermediate_result/temp_html_outpout/temp7plot/",
      #          iteration,
      #          tournament_file_fun[[iteration]][u],".html"),
      #   selfcontained = TRUE)

      # print(
      #   htmltools::includeHTML(
      #     paste0("../data/intermediate_result/temp_html_outpout/temp7plot/",
      #            iteration,
      #            tournament_file_fun[[iteration]][u],".html"))
      # )
      
      pander::pandoc.p("")
      pander::pandoc.p("")
      pander::pandoc.header("Top 8 list", level = 3)
      print(htmltools::tagList(total_result_fun[[u]]$plot$top8_table))
      pander::pandoc.p("")
      pander::pandoc.p("")
      pander::pandoc.p(":::")
      pander::pandoc.p("")
      pander::pandoc.p("")
    }
  }
  )
  
  
}

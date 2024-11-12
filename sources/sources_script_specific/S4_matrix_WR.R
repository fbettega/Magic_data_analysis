


################################################################################


################################################################################
######  Function that plot presence of archetype used in 2 and 7  ##############

plot_presence_modify_for_matchup_matrix_fun <- function(
    df_base ,
    color_scheme ,
    plot_scaling = 2.25
    
) {
  df_plot_presence <- df_base %>% 
    mutate(
      win = as.numeric(Matchups_Wins > Matchups_Losses),
      loose = as.numeric(Matchups_Wins < Matchups_Losses)
      ) %>% 
    group_by(id,Archetype ,Base_Archetype ,Player) %>% 
    summarise(
      win = sum(win),
      loose = sum(loose),.groups = "drop"
    ) %>% 
    group_by(Archetype) %>% 
    mutate(
      Archetype_count = n(),
      Archetype_matches = sum(win + loose)
    ) %>% 
    ungroup() %>% 
    group_by(Base_Archetype) %>% 
    mutate(
      Base_Archetype_count = n(),
      Base_Archetype_matches = sum(win + loose)
    ) %>% 
    ungroup()
  
  

    Plot_presence <- (
      ggplot(
        df_plot_presence,
        aes(
          x = Archetype,
          y = prop.table(stat(count)),
          fill = Base_Archetype,
          label = scales::percent(prop.table(stat(count))),
          # Delta_rank = Delta_rank,
          # Delta_percent_arch = Delta_percent_arch,
          text = paste(
            "Archetype: ", Archetype, "<br>", # Archetype name
            "Base Archetype: ", Base_Archetype, "<br>", # Base Archetype name
            "Archetype n : ", "n = ", Archetype_count, " (Matches : ",Archetype_matches,")", "<br>",
            # "Archetype Win rate: ", Arch_winrate_format, " ", " (", Delta_Arch_win_rate, ")", "<br>",
            # "Base Archetype Win rate: ", Based_Arch_winrate_format, " ", " (", Delta_based_Arch_win_rate, ")", "<br>",
            # "Delta Archetype percent: ", Delta_percent_arch, " %", "<br>",
            "Base Archetype count: ", Base_Archetype_count, " (Matches : ",Base_Archetype_matches, ")", "<br>",
            sep = ""
          )
        )
      ) +
        geom_bar() +
        geom_text(
          aes(
            label = paste0(
              round(prop.table(stat(count)) * 100, 1),
              " % "
            ),
            y = prop.table(stat(count)) + 0.008,
            group = 1
          ),
          stat = "count",
          position = position_dodge2(width = 0.9),
          size = 5
        ) +
        # geom_text(
        #   aes(
        #     label = paste0(Delta_rank," ",Delta_percent_arch," %"),
        #     y = ,
        #     group = 1
        #       ),
        #   stat = "identity",
        #   position = position_dodge2(width = 0.9),
        #   size = 3
        #   ) +
        scale_y_continuous(labels = scales::percent) +
        coord_flip() +
        theme(
          legend.position = "none",
          axis.title.x = element_blank()
        ) +
        scale_fill_manual(
          values = color_scheme[
            levels(
              as.factor(df_base$Base_Archetype)
            ) %in%
              levels(df_plot_presence$Base_Archetype)
          ]
        )
     )  %>%
      ggplotly(
        tooltip = c("text"), height = (480 * plot_scaling), width = (820 * plot_scaling)
      )
  
  
  # Truc compliqué pour enlever l'overlay du texte
  Plot_presence$x$data[[which(
    sapply(Plot_presence$x$data, function(x) {
      x$mode == "text"
    }) == TRUE
  )]]$hoverinfo <- "none"
  
  return(Plot_presence)
}















plot_win_rate_mat <- function(
    Df_winrate_format_fun,
    group_column,
    Cut_of_number_of_data = 0,
    simplify_tab_ratio = 0,
    only_signif = FALSE,
    tiles_size = 1) {
  marges <- list(
    l = 0, # marge gauche
    r = 50, # marge droite (par exemple)
    b = 50, # marge basse (par exemple)
    t = 50 # marge haute (par exemple)
  )

  Df_winrate_format_filter_base_fun <- Df_winrate_format_fun %>%
    filter(number_of_games > 0) %>%
    # filtre surement a déplacé pour opti
    filter(Archetype != Matchups_OpponentArchetype) %>%
    filter(!!rlang::sym(paste0("number_of_", group_column)) > Cut_of_number_of_data)
  
  if (only_signif) {
    Df_winrate_format_filter_base_fun <- Df_winrate_format_filter_base_fun %>%
      filter(!!rlang::sym(paste0("CI_WR_sign_diff_0_", group_column)))
  }
  
  # Calculer le nombre de lignes et colonnes
  num_rows <- length(unique(Df_winrate_format_filter_base_fun$Archetype)) # MODIFICATION
  num_cols <- length(unique(Df_winrate_format_filter_base_fun$Matchups_OpponentArchetype)) # MODIFICATION
  
  # Augmenter le facteur pour des dimensions plus grandes
  plot_height <- max(600, num_rows * 80)   # MODIFICATION
  plot_width <- max(800, num_cols * 80)    # MODIFICATION
  
  # Garder une taille de texte suffisamment lisible
  text_size <- min(12, 17 - 0.055 * num_rows) # MODIFICATION
  
  # Ajustement dynamique de la taille des tuiles en fonction de la densité
  tile_scale <- min(0.5, 1 - 0.02 * num_rows) # MODIFICATION
  
  
  
  # browser()
  
  Df_winrate_format_filter_fun <- Df_winrate_format_filter_base_fun %>%
    group_by(Archetype) %>%
    mutate(
      ratio_matchup_arch = n() / (length(unique(Df_winrate_format_filter_base_fun$Archetype)) - 1)
    ) %>%
    ungroup() %>%
    group_by(Matchups_OpponentArchetype) %>%
    mutate(
      ratio_matchup_oppoarch = n() / (length(unique(Df_winrate_format_filter_base_fun$Matchups_OpponentArchetype)) - 1)
    ) %>%
    ungroup() %>%
    filter(ratio_matchup_arch >= simplify_tab_ratio) %>%
    filter(ratio_matchup_oppoarch >= simplify_tab_ratio) %>%
    select(-ratio_matchup_arch, -ratio_matchup_oppoarch) %>%
    group_by(Archetype) %>%
    mutate(
      Archetype_presence_matches = sum(number_of_matches),
      Archetype_presence_games = sum(number_of_games),
      Archetype_WR_matches = winrate_1_data(sum(Win_matches), sum(number_of_matches - Win_matches)),
      Archetype_CI_WR_matches = CI_prop(Archetype_WR_matches, Archetype_presence_matches),
      Archetype_CI_WR_format_matches = paste0(
        round(Archetype_WR_matches * 100, 1),
        "%",
        formating_CI(
          Archetype_WR_matches,
          Archetype_CI_WR_matches,
          round_val = 1, limit = c(0, 1)
        )
      ),
      Archetype_WR_games = winrate_1_data(sum(Matchups_Wins), sum(Matchups_Losses)),
      Archetype_CI_WR_games = CI_prop(Archetype_WR_games, Archetype_presence_games),
      Archetype_CI_WR_format_games = paste0(
        round(Archetype_WR_games * 100, 1),
        "%",
        formating_CI(
          Archetype_WR_games,
          Archetype_CI_WR_games,
          round_val = 1, limit = c(0, 1)
        )
      )
    ) %>%
    select(
      -Archetype_WR_games, -Archetype_CI_WR_games,
      -Archetype_WR_matches, -Archetype_CI_WR_matches
    ) %>%
    ungroup() %>%
    group_by(Matchups_OpponentArchetype) %>%
    mutate(
      oppo_Archetype_presence_matches = sum(number_of_matches),
      oppo_Archetype_presence_games = sum(number_of_games),
      oppo_Archetype_WR_matches = winrate_1_data(sum(number_of_matches - Win_matches), sum(Win_matches)),
      oppo_Archetype_CI_WR_matches = CI_prop(oppo_Archetype_WR_matches, oppo_Archetype_presence_matches),
      oppo_Archetype_CI_WR_format_matches = paste0(
        round(oppo_Archetype_WR_matches * 100, 1),
        "%",
        formating_CI(
          oppo_Archetype_WR_matches,
          oppo_Archetype_CI_WR_matches,
          round_val = 1, limit = c(0, 1)
        )
      ),
      oppo_Archetype_WR_games = winrate_1_data(sum(Matchups_Wins), sum(Matchups_Losses)),
      oppo_Archetype_CI_WR_games = CI_prop(oppo_Archetype_WR_games, oppo_Archetype_presence_games),
      oppo_Archetype_CI_WR_format_games = paste0(
        round(oppo_Archetype_WR_games * 100, 1),
        "%",
        formating_CI(
          oppo_Archetype_WR_games,
          oppo_Archetype_CI_WR_games,
          round_val = 1, limit = c(0, 1)
        )
      )
    ) %>%
    select(
      -oppo_Archetype_WR_games, -oppo_Archetype_CI_WR_games,
      -oppo_Archetype_WR_matches, -oppo_Archetype_CI_WR_matches
    ) %>%
    ungroup()
  
  
  
  label_x <- paste0(
    "<span style='font-size:",1+text_size, "px;'> <b>",
    Df_winrate_format_filter_fun %>%
      pull(Archetype) %>%
      unique(),
    "</b> </span>",
    "<br /> n : ", Df_winrate_format_filter_fun %>%
      select(Archetype, all_of(paste0("Archetype_presence_", group_column))) %>%
      distinct() %>%
      pull(
        !!rlang::sym(paste0("Archetype_presence_", group_column))
      ),
    "<br /> ", Df_winrate_format_filter_fun %>%
      pull(
        !!rlang::sym(paste0("Archetype_CI_WR_format_", group_column))
      ) %>%
      unique()
  )
  
  
  label_y <- paste0(
    "<span style='font-size:",1+text_size, "px;'> <b>",
    Df_winrate_format_filter_fun %>%
      pull(Archetype) %>%
      unique(),
    "</b> </span>",
    "<br /> n : ", Df_winrate_format_filter_fun %>%
      select(Archetype, all_of(paste0("Archetype_presence_", group_column))) %>%
      distinct() %>%
      pull(
        !!rlang::sym(paste0("Archetype_presence_", group_column))
      ),
    "<br /> ", Df_winrate_format_filter_fun %>%
      pull(
        !!rlang::sym(paste0("Archetype_CI_WR_format_", group_column))
      ) %>%
      unique()
  )

  
  
  
  if (nrow(Df_winrate_format_filter_fun) > 0){
    plot_base_en_cours <- 
      ggplot(
        Df_winrate_format_filter_fun,
        aes(
          Matchups_OpponentArchetype,
          Archetype,
          fill = !!rlang::sym(paste0("WR_", group_column)),
          text = paste(
            "Win rate of ", Archetype, " vs ", Matchups_OpponentArchetype, "<br>", # Archetype name
            !!rlang::sym(paste0("number_format_", group_column)), "<br>",
            "(",
            !!rlang::sym(paste0("CI_WR_sign_", group_column)), ") ",
            !!rlang::sym(paste0("CI_WR_format_", group_column)), "<br>",
            sep = ""
          )
        )
      ) +
      geom_tile(
        color = "white",
        stat = "identity",
        height = tiles_size * tile_scale, 
        width = tiles_size * tile_scale
      ) +
      scale_fill_gradient2(
        midpoint = 0.5, low = "red", mid = "white",
        high = "green", space = "Lab"
      ) +
      geom_text(
        aes(
          Matchups_OpponentArchetype,
          Archetype,
          label = ifelse(
            !!rlang::sym(paste0("CI_WR_sign_diff_0_", group_column)),
            paste0(
              "<b>",
              round(
                !!rlang::sym(paste0("WR_", group_column)) * 100,
                1
              ),
              "</b>"
            ),
            round(!!rlang::sym(paste0("WR_", group_column)) * 100, 1)
          )
        ),
        size = text_size / 3  #  Ajuste la taille du texte
      ) +
      scale_x_discrete(
        label = rev(label_x),
        # guide = guide_axis(n.dodge=3)
      ) +
      scale_y_discrete(
        label = label_y,
        # sec.axis = dup_axis()
        # guide = guide_axis(n.dodge = 2)
      ) 
    
    
    signif_dataframe <- Df_winrate_format_filter_fun %>%
      filter(
        !!rlang::sym(paste0("CI_WR_sign_diff_0_", group_column))
      )
    
    
    
    if (nrow(signif_dataframe) > 0){
      plot_signif_en_cours <- plot_base_en_cours +
        geom_tile(
          data =
            signif_dataframe,
          aes(
            Matchups_OpponentArchetype,
            Archetype
          ),
          fill = "transparent",
          colour = "black",
          size = 1
        )
    } else {
      plot_signif_en_cours <- plot_base_en_cours 
    }
    
    plot_en_cours <- (
      plot_signif_en_cours  +
        theme(
          axis.title = element_blank(),
          legend.position = "none",
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1), # MODIFICATION,
          axis.text = element_text(size = text_size/1.8) #  Ajuste la taille des étiquettes de l'axe
        ) 
    ) %>%
      ggplotly(
        tooltip = c("text"),
        # height = (480 * size_multiplier), width = (640 * size_multiplier)
        height = plot_height,  #  Utilisation de la hauteur calculée dynamiquement
        width = plot_width     #  Utilisation de la largeur calculée dynamiquement
      ) %>%
      plotly::layout(margin = marges)
  } else {
    plot_en_cours <- NULL
  }
  
  
  
  return(plot_en_cours)
}




win_rate_matrix_maker <- function(
    df_fun,
    Archetype_type # "Archetype" , "Base_Archetype"
) {
  if (Archetype_type == "Archetype") {
    select_col <- c("Archetype", "Matchups_OpponentArchetype")
  } else if (Archetype_type == "Base_Archetype") {
    select_col <- c("Base_Archetype", "Matchups_OpponentBase_Archetype")
  }
  
  
  win_rate_matrix_fun_res <- df_fun %>%
    select(
      all_of(select_col),
      Matchups_Wins, Matchups_Losses
    ) %>%
    mutate(
      Result = Matchups_Wins > Matchups_Losses,
      Draw = Matchups_Wins == Matchups_Losses
    ) %>%
    group_by(across(all_of(select_col))) %>%
    summarise(
      number_of_matches = n() - sum(Draw),
      Win_matches = sum(Result),
      number_of_games = sum(Matchups_Wins) + sum(Matchups_Losses),
      Matchups_Wins = sum(Matchups_Wins),
      Matchups_Losses = sum(Matchups_Losses),
      .groups = "drop"
    ) %>%
    mutate(
      WR_games = winrate_1_data(Matchups_Wins, Matchups_Losses),
      CI_WR_games = CI_prop(WR_games, number_of_games),
      CI_WR_sign_games = factor(
        ifelse(CI_WR_games == 0, "0",
               ifelse(
                 ((WR_games - 0.5) + (CI_WR_games)) > 0,
                 "+",
                 ifelse(
                   ((WR_games - 0.5) - (CI_WR_games)) < 0,
                   "-", "0"
                 )
               )
        ),
        levels = c("+", "0", "-")
      ),
      WR_matches = winrate_1_data(Win_matches, (number_of_matches - Win_matches)),
      CI_WR_matches = CI_prop(WR_matches, number_of_matches),
      CI_WR_sign_matches = factor(
        ifelse(CI_WR_matches == 0, "0",
               ifelse(
                 ((WR_matches - 0.5) + (CI_WR_matches)) > 0,
                 "+",
                 ifelse(
                   ((WR_matches - 0.5) - (CI_WR_matches)) < 0,
                   "-", "0"
                 )
               )
        ),
        levels = c("+", "0", "-")
      ),
    )
  
  
  return(win_rate_matrix_fun_res)
}




CI_plot_prepare_df_fun <- function(df_fun) {
  res <- df_fun %>%
    group_by(Archetype) %>%
    summarise(
      number_of_matches = sum(number_of_matches),
      Win_matches = sum(Win_matches),
      number_of_games = sum(number_of_games),
      Matchups_Wins = sum(Matchups_Wins),
      .groups = "keep"
    ) %>%
    mutate(
      Loss_matches = number_of_matches - Win_matches,
      Matchups_Loss = number_of_games - Matchups_Wins
    ) %>%
    summarise(
      number_of_matches = number_of_matches,
      number_of_games = number_of_games, 
      Arch_winrate_matches = winrate_1_data(
        sum(Win_matches, na.rm = TRUE), sum(Loss_matches, na.rm = TRUE)
      ),
      CI_Arch_winrate_matches = CI_prop(
        Arch_winrate_matches, number_of_matches
      ),
      Arch_winrate_games = winrate_1_data(
        sum(Matchups_Wins, na.rm = TRUE), sum(Matchups_Loss, na.rm = TRUE)
      ),
      CI_Arch_winrate_games = CI_prop(
        Arch_winrate_games, number_of_games
      )
    )
  
  return(res)
}









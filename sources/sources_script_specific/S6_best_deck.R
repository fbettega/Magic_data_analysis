lapply_around_table_list <- function(
    df_main_fun,
    df_side_fun,
    Number_of_deck,
    observ_duration,
    Scry_fall_df_fun_around_list
    ){
  Archetype_fun <- unique(c(df_main_fun$Archetype,df_side_fun$Archetype))
  
  # x <- Archetype_fun[1]
  res_lapply <- lapply(Archetype_fun, function(x){
    res_count_sep <- Best_deck_get_table_function(
      res_fun_init_main = df_main_fun,
      res_fun_init_side = df_side_fun,
      Archetype_fun = x,
      Model = "cout",
      top_x_rank = Number_of_deck,
      Week_fun = observ_duration,
      db_format_best_deck_table_fun = Scry_fall_df_fun_around_list
    )
    res_any <- Best_deck_get_table_function(
      res_fun_init_main = df_main_fun,
      res_fun_init_side = df_side_fun,
      Archetype_fun = x,
      Model = "Any",
      top_x_rank = Number_of_deck,
      Week_fun = observ_duration,
      db_format_best_deck_table_fun = Scry_fall_df_fun_around_list
    )
    
    
    list(count = res_count_sep,
         any = res_any)
  })
  
  
  names(res_lapply) <- Archetype_fun
  
  return(res_lapply)
}


unlist_side_or_main <- function(df,Archetype_fun,Model,top_x_rank,Week_fun,cols_fun){
  not_colfuns <- ifelse(cols_fun == "Sideboard","Mainboard","Sideboard")
  # if(Archetype_fun == "Footfalls") browser()
  if(nrow(df %>% 
          filter(Archetype == Archetype_fun) ) == 0 |
     nrow(df %>%  
          select(
            all_of(
              c("rank","AnchorUri","type_of_model","Tournament","Week","Date","Player","Archetype",paste0(cols_fun),paste0(not_colfuns), "Deck_winrate")
            )
          ) %>% 
          mutate(Date = lubridate::date(Date))%>% 
          select(-all_of(not_colfuns)) %>% 
          filter(Week >= (max(Week) - Week_fun)) %>% 
          filter(Archetype == Archetype_fun) %>%
          filter(type_of_model == Model) %>% 
          filter(rank <= max(sort(rank)[1:top_x_rank]))) ==0
     
  ){
    Unnest_filter_table <- NULL
    
  }else {
    
    Unnest_filter_table <- df %>%  
      select(
        all_of(
          c("rank","AnchorUri","type_of_model","Tournament","Week","Date","Player","Archetype",paste0(cols_fun),paste0(not_colfuns), "Deck_winrate")
        )
      ) %>% 
      mutate(Date = lubridate::date(Date))%>% 
      select(-all_of(not_colfuns)) %>% 
      filter(Week >= (max(Week) - Week_fun)) %>% 
      filter(Archetype == Archetype_fun) %>%
      filter(type_of_model == Model) %>% 
      filter(rank <= max(sort(rank)[1:top_x_rank])) %>% 
      arrange(rank) %>% 
      mutate(rank = 1:nrow(.)) %>% 
      mutate(Player = paste0('[',rank,'](',AnchorUri,')'))%>% 
      select(
        -AnchorUri#,-rank
      ) %>%
      unnest_longer(!!rlang::sym(cols_fun)) %>%
      unnest_wider(!!rlang::sym(cols_fun), names_sep = "_") %>% 
      mutate(Main_or_side = cols_fun) %>% 
      rename(
        Count = paste0(cols_fun, "_Count") ,
        CardName = paste0(cols_fun, "_CardName")
      )
  }
  
  return(Unnest_filter_table)
}





# Week_fun <- Inf
# top_x_rank <- 7
# Archetype_fun <- "Scam"
# Model <- "Any"#"cout"#
# res_fun_init_main <- res_main# res_global
# res_fun_init_side <- res_side #res_global

table_generator_sub_fun <- function(
    df_fun,
    Archetype_fun, 
    Model, 
    top_x_rank, 
    Week_fun,
    maind_and_side = "All", #"Main" "Side"
    db_scry_fall_generator_sub_dun
){
  
  if(is.null(df_fun)){
    Table_result <- NULL
  }else{
    Table_main_side_before_base_cards <- df_fun %>%
      mutate(
        Tournament = Tournament_agreger(Tournament),
        Player = paste0(Player, "<br>", Tournament, "<br>Week : ", Week),
      ) %>%
      select(
        
        -any_of(c(
          "rank",  "Tournament",
          "Week",  "type_of_model",  "Date",  "Archetype",  "Deck_winrate"
        ))
        
      ) %>%
      pivot_wider(
        names_from = Player,
        values_from = c(Count),
        values_fill = 0
      ) %>% 
      left_join(
        join_with_scryfall(
          Df_with_cardname =   .,
          cardname_col = "CardName" ,
          scry_fall_df = db_scry_fall_generator_sub_dun
        ),
        by = c("CardName" = "CardName")
      ) %>%
      left_join(
        db_scry_fall_generator_sub_dun %>% 
          select(id,scryfall_uri),
        by = join_by(
          scry_fall_id == id
        )
      ) %>% 
      add_link_to_a_column(
        df_add_link_fun =   .,
        column_where_is_add = "CardName", #"link",
        link_column = "scryfall_uri",
        mode = "md",
        Card_name_col = "CardName"  
      ) %>%
      select(-scry_fall_id,-search_Link) %>%
      rowwise()  %>%
  # Récupération des divers deck pour obtenir les stat desc par cartes
  mutate(
    mean_number = round(mean(c_across(starts_with(
      "[" # "<a href=https://"
    )
    )
    ), 1),
    min_number = min(c_across(starts_with(
      "[" # "<a href=https://"
    )
    )
    ),
    max_number = max(c_across(starts_with(
      "[" # "<a href=https://"
    )
    )
    ),
    base_deck_cards = min_number > 0,
    # base_deck_cards = min_number == max_number,
    .before = 3
  ) 
    
    base_cards_table <-   Table_main_side_before_base_cards  %>%
      filter(base_deck_cards ) %>% 
      select(-base_deck_cards) %>% 
      ungroup() %>%
      mutate(
        across(!c(CardName, Main_or_side , mean_number ,min_number ,max_number ),
        ~ min_number
      )) %>% 
      mutate(
        CardName = 
            paste0(
              CardName, " ",
              min_number
            ),
        base_deck_cards = TRUE
      ) 
      
      
      
    variables_cards_table <-   Table_main_side_before_base_cards  %>%
      select(-base_deck_cards) %>% 
      ungroup() %>%
      mutate(
        across(!c(CardName, Main_or_side , mean_number ,min_number ,max_number ),
               ~ . -min_number
        )) %>% 
      mutate(base_number = min_number) %>% 
      rowwise()  %>%
      # Récupération des divers deck pour obtenir les stat desc par cartes
      mutate(
        mean_number = round(mean(c_across(starts_with(
          "[" # "<a href=https://"
        )
        )
        ), 1),
        min_number = min(c_across(starts_with(
          "[" # "<a href=https://"
        )
        )
        ),
        max_number = max(c_across(starts_with(
          "[" # "<a href=https://"
        )
        )
        )
      ) %>% 
      ungroup() %>% 
      filter(max_number > 0) %>% 
      mutate(
        CardName = 
            paste0(
              CardName, "<br>",
              mean_number, "[",
              min_number, ";",
              max_number, "]",
              ifelse(base_number > 0,
                     paste0(" ",
              base_number,"*"),""
              )
            ),
        base_deck_cards = FALSE
      ) %>% 
      select(-base_number)
    
      Table_main_side <- rbind(base_cards_table,variables_cards_table) %>% 
        group_by(Main_or_side) %>% 
        mutate(number_of_base_cards = sum(min_number * as.numeric(base_deck_cards)),
               Main_or_side = paste0(Main_or_side, " : ",number_of_base_cards)) %>% 
        ungroup() %>% 
        select(-c(mean_number, min_number, max_number,number_of_base_cards))
        
    title_table <- paste0(
      "Top ", top_x_rank,
      " best performing ",ifelse(
        maind_and_side == "All","list",
        ifelse(
          maind_and_side == "Main","Mainboard","Sideboard"
        )),
      " in ",
      ifelse(Week_fun == Inf, "all data ",
             paste0("last ", Week_fun, " weeks ")
      ),
      "**Archetype : ", Archetype_fun, "**"
    )
    
    Subtitle <- paste0(
      "`Using quasibinomial regression models ",
      ifelse(Model == "cout", "with each possible number of cards",
             "with absence vs presence for each cards"
      ), "`"
    )
    
    
    Table_result <- Table_main_side %>%
      dplyr::arrange(Main_or_side,desc(base_deck_cards)) %>% 
      select(-Main_or_side,-base_deck_cards) %>% 
      rename(` ` = CardName) %>% 
      knitr::kable(
        format = "html",escape = FALSE,
        caption = paste0(title_table,"<br>", #"\\\\",
                         "\\scriptsize ",Subtitle
        )
      ) %>% 
      kableExtra::kable_styling(
        bootstrap_options = "striped",  position = "center"
      ) %>% 
      kableExtra::pack_rows(
        index = table(Table_main_side$Main_or_side)
      )
    
    
  }

  return(Table_result)
}








# Best_deck_get_table_function_separate_main_and_side

Best_deck_get_table_function <- function(res_fun_init_main,
                                         res_fun_init_side, 
                                         Archetype_fun, 
                                         Model, 
                                         top_x_rank, 
                                         Week_fun,
                                         db_format_best_deck_table_fun
) {
  
  if (identical(res_fun_init_main , res_fun_init_side)) {
    Df_combine <- rbind(
      unlist_side_or_main(
        res_fun_init_main, Archetype_fun, Model,
        top_x_rank,
        Week_fun,
        "Mainboard"
      ),
      unlist_side_or_main(
        res_fun_init_side, Archetype_fun, Model,
        top_x_rank,
        Week_fun, 
        "Sideboard"
      )
    )
    
    format_table <- table_generator_sub_fun(
      df_fun = Df_combine,
      Archetype_fun = Archetype_fun, 
      Model = Model, 
      top_x_rank = top_x_rank, 
      Week_fun = Week_fun,
      maind_and_side = "All", #"Main" "Side"
      db_scry_fall_generator_sub_dun = db_format_best_deck_table_fun
    )
  } else {
    format_table_main <- table_generator_sub_fun(
      df_fun =  unlist_side_or_main(
        res_fun_init_main, Archetype_fun, Model,
        top_x_rank,
        Week_fun,
        "Mainboard"
      ),
      Archetype_fun = Archetype_fun, 
      Model = Model, 
      top_x_rank = top_x_rank, 
      Week_fun = Week_fun,
      maind_and_side = "Main" , #"Main" "Side"
      db_scry_fall_generator_sub_dun = db_format_best_deck_table_fun
    )
    format_table_side <- table_generator_sub_fun(
      df_fun = unlist_side_or_main(
        res_fun_init_side, Archetype_fun, Model,
        top_x_rank,
        Week_fun, 
        "Sideboard"
      ),
      Archetype_fun = Archetype_fun, 
      Model = Model, 
      top_x_rank = top_x_rank, 
      Week_fun = Week_fun,
      maind_and_side = "Side", #"Main" "Side"
      db_scry_fall_generator_sub_dun = db_format_best_deck_table_fun
    )
    
    format_table <- list(
      main = format_table_main,
      side = format_table_side
    )
  }
  return(format_table)
}




Get_best_deck_from_model <- function(
    model_list,
    base_df,
    number
){
  result_best_deck <- lapply(model_list, function(x){
    best_any <- get_best_deck_sub_fun(
      x$Model_any,
      base_df,
      number) %>% mutate(type_of_model = "Any",.before = 4)
    
    best_count <- get_best_deck_sub_fun(
      x$Model_count,
      base_df,
      number) %>% mutate(type_of_model = "cout",.before = 4)
    
    
    
    return(rbind(best_any,best_count))
  }
  ) %>%
    bind_rows()
  
}

get_best_deck_sub_fun <-  function(model_list_spe,base_df,number){
  if (number == Inf){number <- nrow(model_list_spe$data)}
  proba <- (
    data.frame(
      id = rownames(model_list_spe$data),
      proba = model_list_spe$fitted.values 
    ) %>% 
      arrange(desc(proba))
  )[1:number,]
  
  res <- proba %>% 
    left_join(base_df,by = "id") %>% 
    mutate(
      rank = 1:nrow(.),
      .before =2
    )
  
}


# specific 6
get_models_for_best_deck <- function(
    get_models_fun_par_df ,
    get_models_fun_parmin_arch_presence_fun ,
    get_models_fun_pardeck_or_side,
    get_models_fun_partype_of_archetype,
    get_models_fun_parland_name_fun,
    get_models_fun_parmin_number_of_cards
){
  
  init_treatment <- Prepare_df_for_long_for_model(
    df_base_fun = get_models_fun_par_df ,
    min_arch_presence_fun = get_models_fun_parmin_arch_presence_fun,
    deck_or_side = get_models_fun_pardeck_or_side,#"Mainboard" ,
    type_of_archetype = get_models_fun_partype_of_archetype,# "Archetype" "Base_Archetype"
    land_name_fun = get_models_fun_parland_name_fun,
    min_number_of_cards = get_models_fun_parmin_number_of_cards
  )
  
  if(get_models_fun_pardeck_or_side == "All"){
    get_models_fun_pardeck_or_side <- ""
  }
  
  df_after_model_preparation <- model_preparation_df(
    df_prett_fun = init_treatment,
    min_arch_presence_fun = get_models_fun_parmin_arch_presence_fun,
    deck_or_side =  get_models_fun_pardeck_or_side,
    min_number_of_cards = get_models_fun_parmin_number_of_cards
  )
  
  result_models_Uncommon_cards_all_arch <- model_unco_cards_fun(
    df_fun = df_after_model_preparation$group_com_unco_cards_res
  ) %>%
    name_list_of_model_with_string(
      unique(
        df_after_model_preparation$group_com_unco_cards_res$Archetype
        )
      )
  
  
  return(result_models_Uncommon_cards_all_arch)
  
}




generate_total_result_of_best_deck <- function(
    fun_par_df, #= df_export,
    fun_parmin_arch_presence_fun = filter_archetype_count_6,
    fun_pardeck_or_side,# = c("All","Mainboard" ,"Sideboard")
    fun_partype_of_archetype,# = "Archetype"# "Archetype" "Base_Archetype"
    fun_parland_name_fun = Land_modern,
    fun_parmin_number_of_cards = min_sample_size_6,
    fun_parnumber_of_week = c(Inf,2),
    fun_parNumber_of_deck_print = Number_of_deck,
    db_scryfall_fun_par
){
  
  # all_main_side_fun <- fun_pardeck_or_side[1]
  list_of_Model_result_total_fun <- lapply(fun_pardeck_or_side, 
                                           function(all_main_side_fun){
    # print(all_main_side_fun)
    
    fun_par_df <- fun_par_df %>%
      mutate(Archetype = !!rlang::sym(fun_partype_of_archetype))
    
    
    res_prett <-   get_models_for_best_deck(
      get_models_fun_par_df = fun_par_df,
      get_models_fun_parmin_arch_presence_fun = fun_parmin_arch_presence_fun,
      get_models_fun_pardeck_or_side = all_main_side_fun,
      get_models_fun_partype_of_archetype = fun_partype_of_archetype,
      get_models_fun_parland_name_fun = fun_parland_name_fun,
      get_models_fun_parmin_number_of_cards = fun_parmin_number_of_cards
    )
    
    res_best_deck_fun <- Get_best_deck_from_model(
      model_list = res_prett,
      base_df = fun_par_df,
      number = Inf 
    ) 
    
    return(
      
      res_best_deck_fun   
    )
    
  }
  )  %>%
    name_list_of_model_with_string(unique(fun_pardeck_or_side))
  
  
  
  # duration_en_cours <- fun_parnumber_of_week[1]
  list_of_GT_table_for_all_duration <- lapply(
    fun_parnumber_of_week,
    function(duration_en_cours){
      
      global_res <- lapply_around_table_list(
        df_main_fun = list_of_Model_result_total_fun$All,
        df_side_fun = list_of_Model_result_total_fun$All,
        Number_of_deck = fun_parNumber_of_deck_print,
        observ_duration = duration_en_cours,
        Scry_fall_df_fun_around_list = db_scryfall_fun_par
      )
      
      main_and_side_combine_res <- lapply_around_table_list(
        df_main_fun = list_of_Model_result_total_fun$Mainboard,
        df_side_fun = list_of_Model_result_total_fun$Sideboard,
        Number_of_deck = fun_parNumber_of_deck_print,
        observ_duration = duration_en_cours,
        Scry_fall_df_fun_around_list = db_scryfall_fun_par
      )
      return( 
        list(
          global = global_res,
          main_side = main_and_side_combine_res
        )
      )
    }
  ) %>%
    name_list_of_model_with_string(paste0("number_of_week_",fun_parnumber_of_week))
  
  
  
  
  return(
    list(
      Archetype_consider = sort(unique(list_of_Model_result_total_fun$All$Archetype)),
      resulting_gt_table = list_of_GT_table_for_all_duration
    )
  )
}



print_result_total_script_best_deck <- function(
    res_tot,
    res_main_side,
    iteration) {
  #, out.width="100%"
  
  pander::pandoc.header(iteration, level = 3)
  pander::pandoc.p("")
  
  pander::pandoc.header("Global deck models", level = 4)
  pander::pandoc.p("")
  pander::pandoc.p("")
  
  if (iteration %in% names(res_tot)) {
    pander::pandoc.p('::: {.panel-tabset .nav-pills}')
    pander::pandoc.header("Any", level = 5)
    
    pander::pandoc.p("")
    print(htmltools::tagList(htmltools::HTML(res_tot[[iteration]]$any)))
    
    pander::pandoc.p("")
    pander::pandoc.p("")
    
    pander::pandoc.header("Count", level = 5)
    pander::pandoc.p("")
    print(htmltools::tagList(htmltools::HTML(res_tot[[iteration]]$count)))
    
    pander::pandoc.p(":::")
  }
  
  pander::pandoc.header("Separate model for main and side", level = 4)
  pander::pandoc.p("")
  pander::pandoc.p("")
  
  if (iteration %in% names(res_main_side)) {
    
    
    pander::pandoc.header("Mainboard", level = 5)
    pander::pandoc.p('::: {.panel-tabset .nav-pills}')
    
    pander::pandoc.header("Any", level = 6)
    
    pander::pandoc.p("")
    print(htmltools::tagList(htmltools::HTML(res_main_side[[iteration]]$any$main)))
    
    pander::pandoc.p("")
    pander::pandoc.p("")
    
    pander::pandoc.header("Count", level = 6)
    pander::pandoc.p("")
    print(htmltools::tagList(htmltools::HTML(res_main_side[[iteration]]$count$main)))
    
    pander::pandoc.p(":::")
    pander::pandoc.p("")
    
    pander::pandoc.header("Sideboard", level = 5)
    
    pander::pandoc.p('::: {.panel-tabset .nav-pills}')
    
    pander::pandoc.header("Any", level = 6)
    
    pander::pandoc.p("")
    print(htmltools::tagList(htmltools::HTML(res_main_side[[iteration]]$any$side)))
    
    pander::pandoc.p("")
    pander::pandoc.p("")
    
    pander::pandoc.header("Count", level = 6)
    pander::pandoc.p("")
    print(htmltools::tagList(htmltools::HTML(res_main_side[[iteration]]$count$side)))
    
    
    pander::pandoc.p(":::")
  }
  # adding also empty lines, to be sure that this is valid Markdown
  pander::pandoc.p("")
  pander::pandoc.p("")
  
}

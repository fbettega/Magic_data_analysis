
################################################################################
new_cards_number_of_month_for_new_set_1 <- 5
top_n_player <- 20
Archetype_cut_of_4 <- 50
min_sample_size_5 <- 50
filter_archetype_count_5 <- 50
min_sample_size_6 <- 25
filter_archetype_count_6 <- 50
min_tournament_size_7 <- 64
last_week_number_7 <- 3



################################################################################
######################## group common elements in list #########################
# commonElements <-  function(l,o=list(l[[1]])){
#   if(length(l) == 0){return(o)}
#   match = which(unlist(lapply(lapply(o,intersect,l[[1]]),any)))
#   if(length(match) == 0) o[[length(o)+1]] = l[[1]]
#   if(length(match) == 1) o[[match]] = unique(c(o[[match]],l[[1]]))
#   if(length(match) > 1){
#     o[[match[1]]] = unique(unlist(o[match]))
#     p[rev(match)[-1]] = NULL
#   }
#   l[[1]] = NULL
#   commonElements(l,o)
# }

################################################################################
######################## Format proportion   ###################################
proportion_format_fun <- function(nb,tot){
  percent = round(nb*100/tot,1)
  res <- paste0(nb,"/",tot,"(",percent,"%)")
  return(res)
}



################################################################################
######################## collapse following number separate by / ###############
findIntRuns <- function(run) {
  run <- sort(run)
  rundiff <- c(1, diff(run))
  difflist <- split(run, cumsum(rundiff != 1))
  unlist(lapply(difflist, function(x) {
    if (length(x) %in% 1:2) as.character(x) else paste0(x[1], "-", x[length(x)])
  }), use.names = FALSE)
}
################################################################################
########################### is rendering fun ###################################

is_inside_knitr <-  function() {
  !is.null(knitr::opts_knit$get("out.format"))
}

################################################################################
########################### Pre TT function for archetype classif ##############
prett_fun_classif <- function(df, colname_deck_list) {
  start.time <- Sys.time()

   pre_tt_dataframe <-
    df %>%
    select(
      id, Archetype, Color, any_of(colname_deck_list) # ,Sideboard
    ) %>%
    unnest_longer(!!colname_deck_list) %>%
    unnest_wider(!!colname_deck_list, names_sep = "_") %>%
    mutate(
      color_W = as.numeric(str_detect(Color, "W")),
      color_B = as.numeric(str_detect(Color, "B")),
      color_U = as.numeric(str_detect(Color, "U")),
      color_R = as.numeric(str_detect(Color, "R")),
      color_G = as.numeric(str_detect(Color, "G"))
    ) %>%
    select(-Color) %>%
    mutate(!!rlang::sym(paste0(colname_deck_list, "_CardName")) := 
             Card_agregueur(!!rlang::sym(paste0(colname_deck_list, "_CardName")), ALL_mod = TRUE)) %>%
    # Gestion des aggregation (eg fetch)
    group_by(id, !!rlang::sym(paste0(colname_deck_list, "_CardName"))) %>%
    mutate(!!rlang::sym(paste0(colname_deck_list, "_Count")) := sum(!!rlang::sym(paste0(colname_deck_list, "_Count")))) %>%
    distinct()  %>%
     mutate(!!rlang::sym(paste0(colname_deck_list, "_CardName")) := 
              sanitize_string(!!rlang::sym(paste0(colname_deck_list, "_CardName"))                                                                 )
            ) %>% 
    # fin de la gestion
    # cards matrix wild
    pivot_wider(
      names_from = !!rlang::sym(paste0(colname_deck_list, "_CardName")),
      # names_prefix = "Mainboard_",
      values_from = !!rlang::sym(paste0(colname_deck_list, "_Count")),
      values_fill = 0,
      names_repair = "universal_quiet"
    ) 
  
   
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  return(pre_tt_dataframe)
}

####################### print_quantile #################################
median_quantile_paste <- function(x, Q1 = 0.25,Q2 = 0.75, round_val = Inf) {
  paste0(
 round( median(x),round_val)," [",round(quantile(x,Q1),round_val),";",round(quantile(x,Q2),round_val),"]"
  )
  }


value_to_string_with_mean_min_max <- function(x){
  paste0(
    round(mean(x),1) , " [",
    min(x) ," ; ", max(x),"]"
  )
}

####################### sanitize string #################################
sanitize_string <- function(text) {
  # Supprimer les caractères spéciaux et les chiffres
  cleaned_text <- gsub("[^a-zA-Z\\s]", "", text)
  # Convertir en minuscules
  cleaned_text <- tolower(cleaned_text)
  return(cleaned_text)
}

####################### Patch foireux pour ban #################################

search_for_illegal_cards <- function(
  df_illeg,
  cards_db,
  Format_fun_par
  ){
  # conflicted::conflicts_prefer(dplyr::lag)
  
  Base_df <- rbind(df_illeg %>%
    unnest_longer(Mainboard) %>%
    unnest_wider(Mainboard,
                 names_sep = "_"
    ) %>% 
    rename(
      CardName = Mainboard_CardName,
      Count = Mainboard_Count
            ) %>% 
      select(-Sideboard ),
    df_illeg %>%
      unnest_longer(Sideboard ) %>%
      unnest_wider(Sideboard ,
                   names_sep = "_"
      ) %>% 
      rename(
        CardName = Sideboard_CardName,
        Count = Sideboard_Count
      ) %>% 
      select(-Mainboard)
    )
  
  Troll_deck <- Base_df %>% 
    group_by(id,CardName,AnchorUri ) %>% 
    summarise(
      Count = sum(Count),
      .groups = "drop"
    ) %>% 
    filter( Count > 40) %>% 
    select(-AnchorUri,-Count) %>% 
    mutate(
      scry_fall_id   = NA,                      
      !!sym(paste0("legalities.",tolower(Format_fun_par))) := "Troll deck"
    )
  
  
  join_with_scryfall_df_res <- Base_df %>% 
    select(id,CardName) %>% 
    left_join(
      join_with_scryfall(
        Df_with_cardname =   .,
        cardname_col = "CardName" ,
        scry_fall_df = cards_db
      ),
      by = c("CardName" = "CardName")
    ) %>%
    left_join(
      cards_db %>% 
        select(id ,!!sym(paste0("legalities.",tolower(Format_fun_par))) ),
      by = join_by(
        scry_fall_id == id
      )
    ) %>% 
    filter(
      !!sym(paste0("legalities.",tolower(Format_fun_par))) != "legal" &
        !!sym(paste0("legalities.",tolower(Format_fun_par))) !=  "restricted"
    ) 
    
  res <- rbind(
    Troll_deck,
    join_with_scryfall_df_res
    )
  
  
  return(res)

}
# prévoir exact résultat en supprimant les résultats obtenue contre des deck ban mais douteux risque de biais ++
Ban_patch <- function(
    df ,
    scryfall_db,
    Format_fun_par,
    Date_cutoff
) {
  # Search deck id with ban cards in side or deck
  illegal_cards_id <- search_for_illegal_cards(
    df_illeg = df,
    cards_db =  scryfall_db,
    Format_fun_par = Format_fun_par
  )
  
  

  # Get unique id when ban cards is in both side and main
  Remove_id <- unique(illegal_cards_id$id)



  # search for remove player
  # Create a df of with a list of key by pasting player and archetype use after for remove player in matchup list
  Remove_id_temp <- df %>%
    filter(id %in% Remove_id) %>%
    select(TournamentFile, Player,
           Base_Archetype
           # ReferenceArchetype_Archetype
           
           ) %>%
    group_by(TournamentFile) %>%
    summarise(
      remove_matchup = list(
        paste0(Player, "_", 
               Base_Archetype
               # ReferenceArchetype_Archetype
               )
      )
    )

  # Remove remove player from each matchup by unlisting matchup and remove player using Remove_id_temp
  Matchup_issue <- df %>%
    # In order to speed up remove already remove deck because of ban cards
    filter(!(id %in% Remove_id)) %>%
    # joining with list of remove id present in tournament
    left_join(Remove_id_temp, by = "TournamentFile") %>%
    select(-Mainboard, -Sideboard, -Week, -Meta) %>%
    # unnest the col
    unnest_longer(
      Matchups
    ) %>%
    unnest_wider(
      Matchups,
      names_sep = "_"
    ) %>%
    rowwise() %>%
    # make a col for each oppo and checking if this var is in remove matchup
    mutate(
      id_player_tournament = paste0(TournamentFile, "_", Player),
      remove_match_cuz_of_oppo = paste0(Matchups_Opponent, "_", Matchups_OpponentArchetype) %in% remove_matchup
    ) %>%
    filter(!remove_match_cuz_of_oppo) %>%
    # group_by(id_player_tournament) %>%

    # Recreate matchup by renesting
    rowwise() %>%
    mutate(
      Matchups = list(
        list(
          Opponent = Matchups_Opponent,
          OpponentArchetype = Matchups_OpponentArchetype,
          Wins = Matchups_Wins,
          Losses = Matchups_Losses,
          Draws = Matchups_Draws
        )
      )
    ) %>%
    # remove temp column created in process
    select(-c(
      Matchups_Opponent, Matchups_OpponentArchetype, Matchups_Wins,
      Matchups_Losses, Matchups_Draws, remove_match_cuz_of_oppo, remove_matchup
    )) %>%
    group_by(id_player_tournament) %>%
    mutate(
      Matchups = list(
        Matchups
      )
    ) %>%
    ungroup() %>%
    # rename(Matchups = Matchups) %>%
    distinct() %>%
    select(id, Matchups)

  # ADD new matchup column with a join in order to get null in empty matchups
  Df_final <- df %>%
    select(-Matchups) %>%
    filter(!(id %in% Remove_id)) %>%
    left_join(Matchup_issue, by = "id")

  
  debug_remove_cards <- df %>% 
    filter(
      Date >= as.Date(
        Date_cutoff,
        tryFormats = c("%Y-%m-%d", "%d/%m/%Y")
        )
      ) %>% 
    select(id,Date,AnchorUri ,Archetype ,Color) %>% 
    right_join(
      illegal_cards_id,
      by = join_by(id)
    ) %>% 
    filter(!is.na(AnchorUri))
    
    
  saveRDS(debug_remove_cards, paste0("data/intermediate_result/",Format_fun_par,"_debug_remove_cards.rds"))  

  return(Df_final)
}
##################################################################################






################# Json function to auto update filter ##########################
return_filter_js <- function() {
    onfly_filter_js <- c(r"{
    function onlyUnique(value, index, self) {
    return self.indexOf(value) === index;
    };
    var table_header = table.table().header();
    var column_nodes = $(table_header).find('tr:nth-child(2) > td');
    var input_nodes = $(column_nodes).find('input.form-control');
    for (let i = 0; i < input_nodes.length; i++){
    data_type_attr = $(input_nodes[i]).closest('td').attr('data-type');
    if (data_type_attr == 'factor'){
    $(input_nodes[i]).on('input propertychange', function(){
    if (typeof unique_values !== 'undefined'){
    selection_content = $(input_nodes[i]).closest('td').find('div.selectize-dropdown-content');
    var content_str = '';
    for (let j = 0; j < unique_values.length; j++){
    content_str = content_str.concat('<div data-value="', unique_values[j],'" data-selectable="" class="option">', unique_values[j], '</div>')
    }
    selection_content[0].innerHTML = content_str;
    }
    })
    }
    }
    column_nodes.on('click', function(){
    setTimeout(function(){
    for (let i = 0; i < column_nodes.length; i++){
    data_type_attr = $(column_nodes[i]).attr('data-type');
    if (data_type_attr == 'factor'){
    selection_div = $(column_nodes[i]).find('div.selectize-input');
    if($(selection_div).hasClass('dropdown-active')){
    values = table.column(i, {pages: 'all', search: 'applied'}).data();
    unique_values = Array.from(values.filter(onlyUnique));
    selection_content = $(column_nodes[i]).find('div.selectize-dropdown-content');
    var content_str = '';
    for (let j = 0; j < unique_values.length; j++){
    content_str = content_str.concat('<div data-value="', unique_values[j],'" data-selectable="" class="option">', unique_values[j], '</div>')
    }
    selection_content[0].innerHTML = content_str;
    }
    }
    }
    }, 50);
    })
    }")
    return(onfly_filter_js)
}
################################################################################




############################## Format table  ###################################
# Format table résult removing space and shorten name make some variables factor
# Used in 3_Card_win_rate_table

format_df_result_card_table <- function(
    df_base_fun, # data frame from xx
    df_Archetyp_fun, # df Used to put corect order for archetype or based archetype
    Based_Archetyp_fun = FALSE # function use deagregeted archetype
    ) {
  
  colname_deck_list <- df_base_fun %>% 
    ungroup() %>% 
    select(ends_with("_CardName")) %>% 
    colnames() %>% 
    str_remove("_CardName")
  
  
  
  if (Based_Archetyp_fun) {
    # juste reordor Based archetype level and make cardname and count factor
    df_temp_fun <- df_base_fun %>%
      mutate(
        Base_Archetype = factor(
          Base_Archetype,
          levels = unique(df_Archetyp_fun$Base_Archetype)
        ),
        Archetype = factor(
          Archetype,
          levels = unique(df_Archetyp_fun$Archetype)
        ),
        !!rlang::sym(paste0(colname_deck_list, "_CardName")) := as.factor(
          !!rlang::sym(paste0(colname_deck_list, "_CardName"))
        ),
        !!rlang::sym(paste0(colname_deck_list, "_Count")) := as.factor(
          !!rlang::sym(paste0(colname_deck_list, "_Count"))
        )
      )
  } else {
    # juste  make archetype level, cardname and count factor
    df_temp_fun <- df_base_fun %>%
      mutate(
        Archetype = factor(
          Archetype,
          levels = unique(df_Archetyp_fun$Archetype)
        ),
        !!rlang::sym(paste0(colname_deck_list, "_CardName")) := as.factor(!!rlang::sym(paste0(colname_deck_list, "_CardName"))),
        !!rlang::sym(paste0(colname_deck_list, "_Count")) := as.factor(!!rlang::sym(paste0(colname_deck_list, "_Count")))
      )
  }

  # Remove _ and shorten some column
  df_res_fun <- df_temp_fun %>%
    rename_with(
      ~ str_replace(
        str_replace(
          str_replace(
            str_replace_all(., "_", " "),
            "Mainboard", "Main"
          ),
          "Sideboard", "Side"
        ),
        "CardName", "Card"
      )
    ) 
    


  return(df_res_fun)
}
################################################################################





##
## HANDLE lot of copies off one cards ex slime against humanity
################################################################################
# A reflechir grouping conditionnel basé sur la quantité des arch cumulé
Archetype_agreger <- function(Archetype_to_agreg, color_agreg = NULL) {
  
  # color_group <- list(
  #   Delver = list(
  #     color = c("UR","UBR"),
  #     groupe = c("Murktide","UBlackX Control"),
  #     fallback = c("URedX Control")
  #     )
  #   )
  
  name_group <- list(
    # here because of debug grouping with self
    Yawgmoth = c("Yawgmoth"),
    `Hardened Scales` = c("Hardened Scales"),
    Prowess = c("Prowess"),
    Dredge = c("Dredge"),
    `Heliod Combo` = c("Heliod Combo","Soul Sisters"),
    `Glimpse Combo` = c("Glimpse Combo"),
    `Land Destruction` = c("Ensoul","Land Destruction"),
    `Living End` = c("Living End"),
    `Hammer Time` = c("Hammer Time"),
    Mill = c("Mill"),
    Shadow = c("Shadow"),
    Merfolk = c("Merfolk"),
    
    
    Devotion = c("Nykthos Leyline"),
    # ajouter point d'accroche pour red aggro
    `UWhiteX Control` = c(
      "Azorius Control",
      "Jeskai Energy _fallback",
      "Azorius Control _fallback",
      "Taking Turns",
      "Miracle"
    ),
    # Group UB base control and midrange in maccro archetype deck share with UW are in the UW groups
    
    `UBlackX Control` = c(
      "Dimir Control _fallback", 
      "Rogues"
    ),
    # Not murktide UR control
    `URedX Control` = c(
      "Izzet Control _fallback",
      "Temur Energy _fallback",
      "Wizard Control",
      "Reclamation",
      "Delver",
      "Izzet Energy _fallback",
      "Faeries"
    ),
    
    # `The Rock Midrange` = 
    `Black Or Red Midrange` = 
      c("Saga Party"),
    
    `RWx aggro` = c(
      "Mardu Energy _fallback",
      "Naya Energy _fallback",
      "Mono White Energy _fallback",
      "Mono Red Aggro _fallback",
      "Mono Red Midrange _fallback",
      "Boros Aggro _fallback",
      "Boros Midrange _fallback",
      "Mono White Midrange _fallback",
      "Obosh Red"
    ),
    
    
    
    # groupe eldra avec eldra tron
    Eldrazi = c("Eldrazi","Eldrazi Tron","Breach Eldrazi"),
    ############################ Réfléxion a mener #################################
    # groupe all deck blade a reflechir sur le fait de grouper avec blink
    Stoneblade = c(
      "Grief Blade",
      "Stoneblade",
      "Emeria Control"
    ),
    # Groupe breach value and murktide
    Murktide = c(
      "Murktide",
      "Breach Value"
    ),
    
    # Pack rhinos
    Footfalls = c("Footfalls 4 C", "Footfalls"),
    # Regroupement de tout les rakdos midrange et scam
    Scam = c(
      "Scam", 
      #"Rakdos Midrange _fallback",
      # "Mardu Midrange _fallback",
      "Skelementals"
    ),
    # Regroupement de mono B midrange et coffer
    `Coffers Control` = c(
      "Mono Black Midrange",
      "Mono Black Scam",
      "The Rack",
      "Coffers Control" # ,"Mono Black Midrange _fallback"
    ),
    Enchantress = c("Enchantress","Enigmatic Incarnation"),
    
    
    # Merge the two combo breach potentiellement breach storm groupable avec les autres storms
    
    # `Breach combo` = c(
    #   "Breach Storm", "Grinding Breach"
    # ),
    # Disctuable merge goryo et reanimator
    Reanimator = c(
      "Reanimator",
      "Goryo Reanimator"
    ),
    # Regroupement de toutes les version tuant avec vaalakut, gros doutes sur l'inclusion de titanshift
    Scapeshift = c(
      "Scapeshift", "Guildpact Valakut", "Blue Scapeshift", "Titan Shift",
      "Niv To Light"
    ),
    `Amulet Titan` = c(
      "Timeless Lotus"
    ),
    # Merge les 2 versions de gob
    Goblins = c(
      "Goblin Whack",
      "Goblins"
    ),
    # Merge the two combo breach potentiellement breach with storm groupable
    # Ascendancy Combo et adnauseam groupe avec le reste même si pas vraiment storm
    Storm = c(
      
      "Breach Storm", "Grinding Breach",
      "Ad Nauseam", "Ascendancy Combo",
      "Grixis Storm", "Boros Storm", "Mono Red Storm",
      "Gifts Storm", "Twiddle Storm","Ruby Storm"
    ),
    # Regroupement de toutes les 4/5C soupe avec des betes
    `Omnath Control` = c(
      "Omnath Control", "Elementals", "Beans Cascade", "Saheeli Combo","Tameshi Bloom"
    ),
    # Regroupement de toutes les soupes sans lands
    Belcher = c("Belcher", "Oops All Spells"),
    # Meta groupes avec les soupes foods
    Food = c(
      "Asmo Food", "Manufactor Combo",
      "Crabvine","Hollow One"
    ),
    Zoo = c(
      "Blue Zoo", "Black Zoo", "Bushwhacker Zoo", "Domain Zoo"
    ),
    
    Convoke = c("Kuldotha Aggro"),
    `Free Spells` = c("Electro End","Free Spells"),
    `Combo Artifact` = c("Lantern","Combo Artifact","Affinity"),
    
    Creativity = c("Izzet Through Breach"),
    ############################################################################
    # `Kiki Jiki` = c("Kiki Jiki", "Kiki Chord"),
    `Creature combo` = c(
      "Soultrader Combo",
      "Neobrand",
      "Vivien Combo",
      "Sacrifice",
      "Kiki Jiki", "Kiki Chord",
      "Neobrand",
      "Devoted Combo",
      "Kethis Combo",
      "Discover Combo"
    )
  )
  
  
  regex_group <- list(
    Creativity = "Creativity",
    # Pack tron
    Tron = "Tron$",
    Scam = "Scam$",
    # Merge tout les titan sauf titan shift
    `Amulet Titan` = "Titan$",
    Devotion = "Devotion$",
    `Creature combo` = "Creature Combo _fallback$",
    # Groupement de tout les Burn quelquesois les couleurs
    Burn = "Burn",
    Blink = "Blink$",
    Convoke = "Convoke$",
    `Combo Artifact` = "Combo Artifact$",
    `Black Or Red Midrange` = "The Rock Midrange$"
  )

  
  nested_if_else <- "case_when("
  ##############################################################################
  
  # Delver Gestion
  # Gestion du problème de l'absence des couleurs dans les archetypes des mathcups
  # for (i in seq_along(color_group)){
  #   nested_if_else <- paste0(
  #     nested_if_else,'if_else(Archetype_to_agreg == "',paste0(names(color_group[i])) ,'" & is.null(color_agreg[1]),"',
  #     color_group[[i]]$fallback,'",'
  #   )
  #   for (u in seq_along(color_group[[i]]$color )){
  #     nested_if_else <- paste0(
  #       nested_if_else,"if_else(Archetype_to_agreg == ",'"',paste0(names(color_group[i])),'"',
  #       " & color_agreg ==",'"', color_group[[i]]$color[[u]],'"',",",'"',
  #       color_group[[i]]$groupe[[u]],'",'
  #     )
  #   }
  #
  # }
  
  

  for (i in seq_along(name_group)) {
    nested_if_else <- paste0(
      nested_if_else, "Archetype_to_agreg %in% ", "c(", paste0('"', name_group[[i]], '"', collapse = ","), ")", '~"',
      names(name_group[i]), '",'
    )
  }
  
  
  
  for (i in seq_along(regex_group)) {
    nested_if_else <- paste0(
      nested_if_else, 'str_detect(Archetype_to_agreg,"',
      regex_group[[i]], '"', ")", '~"',
      names(regex_group[i]), '",'
    )
  }
  
  
  nested_if_else_final <- paste0(nested_if_else,".default = Archetype_to_agreg)")

  res <- eval(parse(text = nested_if_else_final))


  return(res)
}








################################################################################
############# Simple function that agregate some land together #################

Card_agregueur <- function(
    string,
    ALL_mod = FALSE,
    fetch = TRUE,
    Tron = TRUE,
    snow = TRUE,
    surveil = FALSE,
    shock = FALSE,
    triome = FALSE,
    filter_land = FALSE,
    fast_land = FALSE,
    bounce_land = FALSE,
    horizon_land = FALSE,
    gates_land = FALSE,
    dual_arto_land =FALSE,
    Mono_colo_arto_land =FALSE,
    pain_land  =FALSE,
    reveal_land =FALSE,
    pathway_land =FALSE,
    unlucky_land= FALSE,
    real_dual= FALSE,
    slow_land = FALSE,
    check_land = FALSE,
    basic_land = FALSE) {
  # A ajouté pain utilitaire cycle land


  if(ALL_mod){
    fetch = TRUE
    Tron = TRUE
    snow = TRUE
    surveil = TRUE
    shock = TRUE
    triome = TRUE
    filter_land = TRUE
    fast_land = TRUE
    bounce_land = TRUE
    horizon_land = TRUE
    slow_land = TRUE
    gates_land = TRUE
    dual_arto_land =TRUE
    Mono_colo_arto_land =TRUE
    pain_land  =TRUE
    reveal_land =TRUE
    pathway_land =TRUE
    unlucky_land= TRUE
    real_dual= TRUE
    check_land = TRUE
    basic_land = TRUE
  }
  
  fetech_list <- c(
    "flooded strand", "polluted delta", "wooded foothills", "verdant catacombs",
    "arid mesa", "windswept heath", "scalding tarn", "prismatic vista",
    "misty rainforest", "bloodstained mire", "marsh flats"
  )

  Tron_land <- c("Urza's Mine", "Urza's Power Plant", "Urza's Tower")

  
  real_dual_list <- c(
    "Tundra",
    "Underground Sea",
    "Badlands",
    "Taiga",
    "Savannah", 
    "Scrubland",
    "Volcanic Island",
    "Bayou",
    "Plateau",
    "Tropical Island"
  )
  
  unlucky_land_list  <- c(
    "Abandoned Campground",
    "Murky Sewer",
    "Razortrap Gorge",
  "Bleeding Woods",
  "Etched Cornfield",
  "Neglected Manor",
  "Peculiar Lighthouse" ,
 "Strangled Cemetery",
  "Raucous Carnival" ,
  "Lakeside Shack"
 ) 
  
  pathway_land_list   <- c(  
  "Barkchannel Pathway // Tidechannel Pathway",
  "Barkchannel Pathway",
  "Blightstep Pathway // Searstep Pathway",
"Blightstep Pathway",
  "Branchloft Pathway // Boulderloft Pathway",
"Branchloft Pathway",
  "Brightclimb Pathway // Grimclimb Pathway",
"Brightclimb Pathway",
  "Clearwater Pathway // Murkwater Pathway",
"Clearwater Pathway",
  "Cragcrown Pathway // Timbercrown Pathway",
"Cragcrown Pathway",
  "Darkbore Pathway // Slitherbore Pathway",
"Darkbore Pathway",
  "Hengegate Pathway // Mistgate Pathway",
"Hengegate Pathway",
  "Needleverge Pathway // Pillarverge Pathway",
"Needleverge Pathway",
  "Riverglide Pathway // Lavaglide Pathway",
"Riverglide Pathway" 

  )
  
  
  reveal_land_list   <- c(   
  "Port Town" ,
  "Choked Estuary" ,
  "Foreboding Ruins" ,
  "Game Trail" ,
  "Fortified Village",
  "Shineshadow Snarl",
  "Frostboil Snarl",
  "Necroblossom Snarl",
  "Furycalm Snarl",
  "Vineglimmer Snarl" 
  )
  
  pain_land_list   <- c(  
    "Adarkar Wastes",
    "Underground River",
    "Sulfurous Springs",
    "Karplusan Forest",
    "Brushland",
    "Caves of Koilos" ,
    "Shivan Reef" ,
    "Llanowar Wastes" ,
    "Battlefield Forge" ,
    "Yavimaya Coast"
  )
  
Mono_colo_arto_land_list  <- c(
  "Ancient Den" ,
  "Seat of the Synod" ,
  "Vault of Whispers" ,
  "Great Furnace",
  "Tree of Tales"
  
)
  
dual_arto_land_list  <- c(
  "Razortide Bridge" ,
  "Mistvault Bridge" ,
  "Drossforge Bridge" ,
  "Slagwoods Bridge" ,
  "Thornglint Bridge" ,
  "Goldmire Bridge" ,
  "Silverbluff Bridge",
  "Darkmoss Bridge",
  "Rustvale Bridge",
  "Tanglepool Bridge" 
)
  
gates_land_list <- c(
  "Azorius Guildgate" ,
  "Dimir Guildgate",
  "Rakdos Guildgate",
  "Gruul Guildgate",
  "Selesnya Guildgate" ,
  "Orzhov Guildgate",
  "Izzet Guildgate",
  "Golgari Guildgate" ,
  "Boros Guildgate" ,
  "Simic Guildgate"
)



  surveil_land <- c(
    "Meticulous Archive",
    "Undercity Sewers",
    "Raucous Theater",
    "Commercial District",
    "Lush Portico",
    "Shadowy Backstreet",
    "Thundering Falls",
    "Underground Mortuary",
    "Elegant Parlor",
    "Hedge Maze"
  )


  shock_land <-
    c(
      "Hallowed Fountain",
      "Watery Grave",
      "Blood Crypt",
      "Stomping Ground",
      "Temple Garden",
      "Godless Shrine",
      "Overgrown Tomb",
      "Breeding Pool",
      "Steam Vents",
      "Sacred Foundry"
    )


  Triome_land <-
    c(
      "Savai Triome",
      "Indatha Triome",
      "Zagoth Triome",
      "Ketria Triome",
      "Raugrin Triome",
      "Spara's Headquarters",
      "Raffine's Tower",
      "Xander's Lounge",
      "Ziatora's Proving Ground",
      "Jetmir's Garden"
    )

  filter_land_list <- c(
    "Mystic Gate",
    "Sunken Ruins",
    "Graven Cairns",
    "Fire-Lit Thicket",
    "Wooded Bastion",
    "Fetid Heath",
    "Cascade Bluffs",
    "Twilight Mire",
    "Rugged Prairie",
    "Flooded Grove"
  )

  fast_land_list <- c(
    "Seachrome Coast",
    "Darkslick Shores",
    "Blackcleave Cliffs",
    "Copperline Gorge",
    "Razorverge Thicket",
    "Concealed Courtyard",
    "Spirebluff Canal",
    "Blooming Marsh",
    "Inspiring Vantage",
    "Botanical Sanctum"
  )



  bounce_land_list <- c(
    "Azorius Chancery",
    "Dimir Aqueduct",
    "Rakdos Carnarium",
    "Gruul Turf",
    "Selesnya Sanctuary",
    "Orzhov Basilica",
    "Izzet Boilerworks",
    "Golgari Rot Farm",
    "Boros Garrison",
    "Simic Growth Chamber"
  )

  horizon_land_list <- c(
    "Silent Clearing",
    "Fiery Islet",
    "Nurturing Peatland",
    "Sunbaked Canyon",
    "Waterlogged Grove",
    "Horizon Canopy"
  )
  slow_land_list <- c(
    "Deserted Beach", 
    "Shipwreck Marsh", 
    "Haunted Ridge" ,
    "Rockfall Vale" ,
    "Overgrown Farmland" ,
    "Shattered Sanctum" ,
    "Stormcarved Coast" ,
    "Deathcap Glade",
    "Sundown Pass",
    "Dreamroot Cascade" 
  )
  check_land_list <- c(
  "Glacial Fortress" ,
  "Drowned Catacomb",
  "Dragonskull Summit" ,
  "Rootbound Crag" ,
  "Sunpetal Grove" ,
  "Isolated Chapel" ,
  "Sulfur Falls" ,
  "Woodland Cemetery" ,
  "Clifftop Retreat" ,
  "Hinterland Harbor"
  )
  
  
  
  
  basic_land_list <- c(
    "Island",
    "Plains",
    "Swamp",
    "Mountain",
    "Forest"
  )
  base_string <- trimws(string)

  
  
  modify_land <- function(
    base_string_fun = base_string,
    list_name_fun,
    name_fun
  ){
    ifelse(tolower(trimws(base_string_fun)) %in% tolower(trimws(list_name_fun)),
           name_fun, base_string
    )
  }
  
  if (fetch) {
    base_string <- modify_land(
    base_string_fun = base_string,
    fetech_list,
    "Fetch land"
    )
    #   ifelse(tolower(base_string) %in% fetech_list,
    #   "Fetch land", base_string
    # )
  }
  if (Tron) {
    base_string <- modify_land(
      base_string_fun = base_string,
      Tron_land,
      "Tron land"
    )
    #   ifelse(base_string %in% Tron_land,
    #   "Tron land", base_string
    # )
  }
  if (snow) {
    base_string <- str_remove(
      base_string,
      "Snow-Covered "
    )
  }
  if (surveil) {
    base_string <- 
      base_string <- modify_land(
        base_string_fun = base_string,
        surveil_land,
        "Surveil land"
      )
    #   ifelse(base_string %in% surveil_land,
    #   "Surveil land", base_string
    # )
  }
  if (shock) {
    base_string <- 
      base_string <- modify_land(
        base_string_fun = base_string,
        shock_land,
        "Shock land"
      ) 
      
    #   ifelse(base_string %in% shock_land,
    #   "Shock land", base_string
    # )
  }
  if (triome) {
    base_string <- 
      base_string <- modify_land(
        base_string_fun = base_string,
        Triome_land,
        "Triome land"
      ) 
    #   ifelse(base_string %in% Triome_land,
    #   "Triome land", base_string
    # )
  }
  if (filter_land) {
    base_string <- 
      base_string <- modify_land(
        base_string_fun = base_string,
        filter_land_list,
        "Filter land"
      ) 
    #   ifelse(base_string %in% filter_land_list,
    #   "Filtre land", base_string
    # )
  }

  if (fast_land) {
    base_string <- 
      base_string <- modify_land(
        base_string_fun = base_string,
        fast_land_list,
        "Fast land"
      ) 
    #   ifelse(base_string %in% fast_land_list,
    #   "Fast land", base_string
    # )
  }

  if (bounce_land) {
    base_string <- 
      base_string <- modify_land(
        base_string_fun = base_string,
        bounce_land_list,
        "Bounce land"
      ) 
    #   ifelse(base_string %in% bounce_land_list,
    #   "Bounce land", base_string
    # )
  }

  if (horizon_land) {
    base_string <-
      base_string <- modify_land(
        base_string_fun = base_string,
        horizon_land_list,
        "Horizon land"
      ) 
    #   ifelse(base_string %in% horizon_land_list,
    #   "Horizon land", base_string
    # )
  }
  
  if (gates_land) {
    base_string <-
      base_string <- modify_land(
        base_string_fun = base_string,
        gates_land_list,
        "Gates land"
      ) 
    #   ifelse(base_string %in% gates_land_list,
    #                       "Gates land", base_string
    # )
  }
  if (dual_arto_land) {
    base_string <- 
      base_string <- modify_land(
        base_string_fun = base_string,
        dual_arto_land_list,
        "Dual arto land"
      ) 
    #   ifelse(base_string %in% dual_arto_land_list,
    #                       "Dual arto land", base_string
    # )
  }
  if (Mono_colo_arto_land) {
    base_string <- 
      base_string <- modify_land(
        base_string_fun = base_string,
        Mono_colo_arto_land_list,
        "Mono arto land"
      ) 
    #   ifelse(base_string %in% Mono_colo_arto_land_list,
    #                       "Mono arto land", base_string
    # )
  }
  if (pain_land) {
    base_string <- 
      base_string <- modify_land(
        base_string_fun = base_string,
        pain_land_list,
        "Pain land"
      ) 
    #   ifelse(base_string %in% pain_land_list,
    #                       "Pain land", base_string
    # )
  }
  if (reveal_land) {
    base_string <- 
      base_string <- modify_land(
        base_string_fun = base_string,
        reveal_land_list,
        "Reveal land"
      ) 
    #   ifelse(base_string %in% reveal_land_list,
    #                       "Reveal land", base_string
    # )
  }
  if (pathway_land) {
    base_string <- 
      base_string <- modify_land(
        base_string_fun = base_string,
        pathway_land_list,
        "Pathway land"
      ) 
    #   ifelse(base_string %in% pathway_land_list,
    #                       "Pathway land", base_string
    # )
  }
  if (unlucky_land) {
    base_string <-
      base_string <- modify_land(
        base_string_fun = base_string,
        unlucky_land_list,
        "Unlucky land"
      ) 
    #   ifelse(base_string %in% unlucky_land_list,
    #                       "Unlucky land", base_string
    # )
  }
  if (real_dual) {
    base_string <-
      base_string <- modify_land(
        base_string_fun = base_string,
        real_dual_list,
        "Dual land"
      ) 
    #   ifelse(base_string %in% real_dual_list,
    #                       "Dual land", base_string
    # )
  }
  if (slow_land) {
    base_string <-
      base_string <- modify_land(
        base_string_fun = base_string,
        slow_land_list,
        "Slow land"
      ) 
    #   ifelse(base_string %in% slow_land_list,
    #                       "Slow land", base_string
    # )
  }
  
  if (check_land) {
    base_string <- 
      base_string <- modify_land(
        base_string_fun = base_string,
        check_land_list,
        "Check land"
      ) 
    #   ifelse(base_string %in% check_land_list,
    #                       "Check land", base_string
    # )
  }
  
  # A laisser après snow qui permet de regrouper les snow land avec les basic au besoin
  if (basic_land) {
    base_string <-
      base_string <- modify_land(
        base_string_fun = base_string,
        basic_land_list,
        "Basic land"
      ) 
    #   ifelse(base_string %in% basic_land_list,
    #   "Basic land", base_string
    # )
  }

  return(base_string)
}
################################################################################

agregate_land_link <- function() {
  result <- tibble(
    # Name = c(
    #   "Fetch Lands", "Tron Lands", "Real Dual Lands", "Unlucky Lands", "Pathway Lands",
    #   "Reveal Lands", "Pain Lands", "Mono-Color Artifact Lands", "Dual Artifact Lands",
    #   "Gates", "Surveil Lands", "Shock Lands", "Triomes", "Filter Lands", "Fast Lands",
    #   "Bounce Lands", "Horizon Lands", "Slow Lands", "Check Lands", "Basic Lands"
    # ),
    search_Link = c(
      "https://scryfall.com/search?q=flooded+strand+OR+polluted+delta+OR+wooded+foothills+OR+verdant+catacombs+OR+arid+mesa+OR+windswept+heath+OR+scalding+tarn+OR+prismatic+vista+OR+misty+rainforest+OR+bloodstained+mire+OR+marsh+flats",
      "https://scryfall.com/search?q=urza%27s+mine+OR+urza%27s+power+plant+OR+urza%27s+tower",
      "https://scryfall.com/search?q=tundra+OR+underground+sea+OR+badlands+OR+taiga+OR+savannah+OR+scrubland+OR+volcanic+island+OR+bayou+OR+plateau+OR+tropical+island",
      "https://scryfall.com/search?q=abandoned+campground+OR+murky+sewer+OR+razortrap+gorge+OR+bleeding+woods+OR+etched+cornfield+OR+neglected+manor+OR+peculiar+lighthouse+OR+strangled+cemetery+OR+raucous+carnival+OR+lakeside+shack",
      "https://scryfall.com/search?q=barkchannel+pathway+OR+blightstep+pathway+OR+branchloft+pathway+OR+brightclimb+pathway+OR+clearwater+pathway+OR+cragcrown+pathway+OR+darkbore+pathway+OR+hengegate+pathway+OR+needleverge+pathway+OR+riverglide+pathway",
      "https://scryfall.com/search?q=port+town+OR+choked+estuary+OR+foreboding+ruins+OR+game+trail+OR+fortified+village+OR+shineshadow+snarl+OR+frostboil+snarl+OR+necroblossom+snarl+OR+furycalm+snarl+OR+vineglimmer+snarl",
      "https://scryfall.com/search?q=adarkar+wastes+OR+underground+river+OR+sulfurous+springs+OR+karplusan+forest+OR+brushland+OR+caves+of+koilos+OR+shivan+reef+OR+llanowar+wastes+OR+battlefield+forge+OR+yavimaya+coast",
      "https://scryfall.com/search?q=ancient+den+OR+seat+of+the+synod+OR+vault+of+whispers+OR+great+furnace+OR+tree+of+tales",
      "https://scryfall.com/search?q=razortide+bridge+OR+mistvault+bridge+OR+drossforge+bridge+OR+slagwoods+bridge+OR+thornglint+bridge+OR+goldmire+bridge+OR+silverbluff+bridge+OR+darkmoss+bridge+OR+rustvale+bridge+OR+tanglepool+bridge",
      "https://scryfall.com/search?q=azorius+guildgate+OR+dimir+guildgate+OR+rakdos+guildgate+OR+gruul+guildgate+OR+selesnya+guildgate+OR+orzhov+guildgate+OR+izzet+guildgate+OR+golgari+guildgate+OR+boros+guildgate+OR+simic+guildgate",
      "https://scryfall.com/search?q=meticulous+archive+OR+undercity+sewers+OR+raucous+theater+OR+commercial+district+OR+lush+portico+OR+shadowy+backstreet+OR+thundering+falls+OR+underground+mortuary+OR+elegant+parlor+OR+hedge+maze",
      "https://scryfall.com/search?q=hallowed+fountain+OR+watery+grave+OR+blood+crypt+OR+stomping+ground+OR+temple+garden+OR+godless+shrine+OR+overgrown+tomb+OR+breeding+pool+OR+steam+vents+OR+sacred+foundry",
      "https://scryfall.com/search?q=savai+triome+OR+indatha+triome+OR+zagoth+triome+OR+ketria+triome+OR+raugrin+triome+OR+spara%27s+headquarters+OR+raffine%27s+tower+OR+xander%27s+lounge+OR+ziatora%27s+proving+ground+OR+jetmir%27s+garden",
      "https://scryfall.com/search?q=mystic+gate+OR+sunken+ruins+OR+graven+cairns+OR+fire-lit+thicket+OR+wooded+bastion+OR+fetid+heath+OR+cascade+bluffs+OR+twilight+mire+OR+rugged+prairie+OR+flooded+grove",
      "https://scryfall.com/search?q=seachrome+coast+OR+darkslick+shores+OR+blackcleave+cliffs+OR+copperline+gorge+OR+razorverge+thicket+OR+concealed+courtyard+OR+spirebluff+canal+OR+blooming+marsh+OR+inspiring+vantage+OR+botanical+sanctum",
      "https://scryfall.com/search?q=azorius+chancery+OR+dimir+aqueduct+OR+rakdos+carnarium+OR+gruul+turf+OR+selesnya+sanctuary+OR+orzhov+basilica+OR+izzet+boilerworks+OR+golgari+rot+farm+OR+boros+garrison+OR+simic+growth+chamber",
      "https://scryfall.com/search?q=silent+clearing+OR+fiery+islet+OR+nurturing+peatland+OR+sunbaked+canyon+OR+waterlogged+grove+OR+horizon+canopy",
      "https://scryfall.com/search?q=deserted+beach+OR+shipwreck+marsh+OR+haunted+ridge+OR+rockfall+vale+OR+overgrown+farmland+OR+shattered+sanctum+OR+stormcarved+coast+OR+deathcap+glade+OR+sundown+pass+OR+dreamroot+cascade",
      "https://scryfall.com/search?q=glacial+fortress+OR+drowned+catacomb+OR+dragonskull+summit+OR+rootbound+crag+OR+sunpetal+grove+OR+isolated+chapel+OR+sulfur+falls+OR+woodland+cemetery+OR+clifftop+retreat+OR+hinterland+harbor",
      "https://scryfall.com/search?q=t%3Abasic+t%3Aland"
    ),
    join_name = c(
      "Fetch land", "Tron land", "Dual land", "Unlucky land", "Pathway land",
      "Reveal land", "Pain land", "Mono arto land", "Dual arto land",
      "Gate", "Surveil land", "Shock land", "Triome land", "Filter land", "Fast land",
      "Bounce land", "Horizon land", "Slow land", "Check land", "Basic land"
    )
  )
  return(result)
}


################################################################################
#######################  Remove NULL from nested list  #########################
## a list of NULLs
is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

## Recursively step down into list, removing all such objects 
rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}



################################################################################
####################### Ci functionusing agresti-coull #########################

# Réflechir a un merge entre les function selon les paramètres

CI_2_prop <- function(p1, p2, n1, n2, alpha = 0.95) {
  # browser()
  if(
    any(
      is.na(c(p1, p2, n1, n2))
      )
  ){
    result <-  NA
  }else{
  
  # Compute number of win from P and n
  Wins1 <- round(p1 * n1, 0)
  Wins2 <- round(p2 * n2, 0)
  # using agresti-coull CI
  ##############################################
  ########## Agresti coulli formula ############
  z <- qnorm((1 - alpha) / 2) # qt((1 - alpha)/2,n-1)
  z_square <- z^2
  n1_tide <- n1 + z_square
  n2_tide <- n2 + z_square

  x1_tide <- Wins1 + 0.5 * z_square
  p1_tide <- x1_tide / n1_tide
  x2_tide <- Wins2 + 0.5 * z_square
  p2_tide <- x2_tide / n2_tide


  # if(any(
  #   is.na(
  #     sqrt(
  #     
  #   (p1_tide * (1 - p1_tide) / n1_tide) +
  #   (p2_tide * (1 - p2_tide) / n2_tide)
  # )
  # )) ) browser()
  upper_bound <- (p1_tide - p2_tide) - z * sqrt(
    abs((p1_tide * (1 - p1_tide) / n1_tide)) +
      abs( (p2_tide * (1 - p2_tide) / n2_tide))
  )
  ###################################################

  # Gestion des cas ou le sample size est trop faible
  result <- ifelse(
    (n1 < 5 | n2 < 5),
    0,
    p1 - p2 - upper_bound
  )
  }
  
  return(
    result
  )
}





CI_prop <- function(p, n, alpha = 0.95) {
  # using agresti-coull CI
  Wins <- round(p * n, 0)

  z <- qnorm((1 - alpha) / 2) # qt((1 - alpha)/2,n-1)
  z_square <- z^2
  n_tide <- n + z_square
  x_tide <- Wins + 0.5 * z_square
  p_tide <- x_tide / n_tide
  upper_bound <- p_tide - z * sqrt(
    p_tide * (1 - p_tide) / n_tide
  )


  result <- ifelse(n < 5,
    0,
    # je retire le vai win rate identique a celui du tableaux afin de conserver seulement le CI
    p - upper_bound
  )
  return(
    result
    # upper_bound
  )
}
################################################################################

################################################################################
###########################  Compute WR  #######################################
# merge les 2 selon les paramètres
winrate_1_data <- function(win, loose) {
  win / (win + loose)
}
winrate_2_data <- function(win_base, loose_base, win_diff, loose_diff) {
  (
    win_base - win_diff
  ) / (
    (win_base - win_diff) +
      (loose_base - loose_diff)
  )
}

################################################################################

Draw_diff_2_data <- function(
    win_base, draw_base, loose_base,
    win_diff, draw_diff, loose_diff) {
  (
    draw_base / (
      win_base + loose_base + draw_base)
  ) -
    (
      (
        draw_diff - draw_base
      ) / (
        (win_diff - win_base) +
          (loose_diff - loose_base) +
          (draw_diff - draw_base)
      )
    )
}

################################################################################
###########################  Format and CI   ###################################

# transforming a bound in CI like that [x ; y]
formating_CI <- function(value,
                         CI, round_val = 1,
                         percent = TRUE,
                         limit = c(-Inf, Inf) # c(min,max)
) {
  low_bound <- ifelse(
    (value + CI) < limit[1],
    limit[1],
    (value + CI)
  )

  upper_bound <- ifelse(
    (value - CI) > limit[2],
    limit[2],
    (value - CI)
  )



  ifelse(CI == 0,
    paste0(
      "[No data]"
    ),
    paste0(
      # round(value * (100 * percent),round_val)," ",
      "[",
      round(low_bound * (100 * percent), round_val),
      ";",
      round(upper_bound * (100 * percent), round_val),
      "]"
    )
  )
}
################################################################################

################################################################################
##################  plot number of deck by color_combination  #######################
color_presence_plot_fun <- function(
    df_fun_base_color,
    column_presence # "Presence_count_castable" ,"winrate_count_castable"
){
  
  combinations_palette <- c(
    # Couleurs de base
    Colorless = "#FFFFFF", # Incolore
    W = "#FFFF00", # Blanc
    U = "#0000FF", # Bleu
    B = "#000000", # Noir
    R = "#FF0000", # Rouge
    G = "#00FF00", # Vert
    
    # Combinaisons bicolores
    WU = "#7F7FFF",
    WB = "#6C4E12",
    WR = "#FF7F7F",
    WG = "#7FFF7F",
    BU = "#00007F",
    UR = "#7F007F",
    UG = "#007F7F",
    BR = "#7F0000",
    BG = "#007F00",
    RG = "#7F7F00",
    
    # Combinaisons tricolores
    WBU = "#7F7FBF", # Blanc, Bleu, Noir
    WUR = "#BF7FBF", # Blanc, Bleu, Rouge
    WUG = "#7FBF7F",  # Blanc, Bleu, Vert
    WBR = "#BF5F5F",  # Blanc, Noir, Rouge
    WBG = "#5FBF5F", # Blanc, Noir, Vert
    WRG = "#BF9F5F", # Blanc, Rouge, Vert
    BUR = "#3F003F", # Bleu, Noir, Rouge
   # UBG 
    BUG = "#003F3F",  # Bleu, Noir, Vert
    URG = "#3F3F00", # Bleu, Rouge, Vert
    BRG = "#5F3F00",  # Noir, Rouge, Vert
    
    # Combinaisons quadricolores
    WUBR = "#9F5FAF",
    WBUG = "#5FAF7F",
    WURG = "#AF9F5F",
    WBRG = "#9F7F5F",
    WBUR = "#8F6F8F", 
    BURG = "#5F2F1F",
    
    # Toutes les couleurs
    WBURG = "#9F9F9F" 
  )
  
  color_combination_plot_df_base <- df_fun_base_color %>% 
    select(-Color) %>% 
    group_by(
      across(starts_with("color_"))
    ) %>% 
    summarise(
      Presence_count_castable = n(),
      winrate_count_castable = sum(!only_presence),
      .groups = "drop"
    )  %>%
    mutate(
      combination = paste0(
        ifelse(color_W == 1, "W", ""),
        ifelse(color_B == 1, "B", ""),
        ifelse(color_U == 1, "U", ""),
        ifelse(color_R == 1, "R", ""),
        ifelse(color_G == 1, "G", ""),
        ifelse(color_W + color_B + color_U + color_R + color_G == 0, "Colorless", "")
      )
    )

  color_combination_plot_df <- color_combination_plot_df_base  %>%
    rowwise() %>%
    mutate(
      included_in = list(
        color_combination_plot_df_base$combination[
          sapply(color_combination_plot_df_base$combination, function(x) all(strsplit(x, "")[[1]] %in% strsplit(combination, "")[[1]]))
        ]
      )
    ) %>%
    unnest(included_in) %>%
    group_by(included_in) %>%
    select(-starts_with("color_")) %>% 
    mutate(
      proportion = !!rlang::sym(column_presence)   / sum(
        color_combination_plot_df_base %>% 
          pull(!!rlang::sym(column_presence))
      )
    ) %>%
    rename(Color = included_in ) %>% 
    group_by(Color) %>%
    mutate(total_proportion = sum(proportion),
           total_count = sum(!!rlang::sym(column_presence))
           ) %>%
    ungroup() %>% 
    arrange(total_proportion) %>%
    mutate(
      combination = factor(combination, levels = names(combinations_palette)),
      Color = factor(Color,levels = names(combinations_palette))
    )
  
  

  color_combination_plot <- ( 
    ggplot(color_combination_plot_df,
           aes(
             x = Color,
             y = proportion, 
             fill = combination,
             text = paste0("Combinaison : ", 
                           combination, "<br>N(%) : ",
                           !!rlang::sym(column_presence)," (",scales::percent(proportion, accuracy = 0.1),")",
                           "<br>Total N(%): ",
                           total_count," (",scales::percent(total_proportion, accuracy = 0.1),")"
             )
           )
    ) +
      geom_bar(stat = "identity", position = "stack", width = 0.8, color = "black") +
      scale_fill_manual(values = combinations_palette) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        title = "Cumulative Proportions of Color Combinations in Decks",
        x = "Combinations",
        y = "Proportion",
        fill = "Combination"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ) %>% 
    ggplotly(
      height = 800,
      tooltip = "text" #c("x", "y", "fill")
    ) %>% 
    bslib::card(full_screen = TRUE)
  
  return(color_combination_plot)
}

################################################################################

################################################################################
##################  Function that prepare df in 5 and 6  #######################

prepare_df_for_model <- function(df_fun,base_df,cols_fun){
  card_present <- df_fun %>%
    rowwise() %>%
    mutate(Join_main_count = list(
      as.numeric(unlist(strsplit(!!rlang::sym(paste0(cols_fun, "_Count")), "/")))
    )) %>%
    ungroup() %>%
    unnest_longer(Join_main_count) %>%
    select(
      Archetype, Archetype_count, all_of(
        c(paste0(cols_fun, "_CardName"),
          paste0(cols_fun, "_Count")
        )
      ), Join_main_count # ,most_common_quantity
    ) %>%
    right_join(
      base_df,
      by = c("Archetype", "Archetype_count", paste0(cols_fun, "_CardName"), "Join_main_count" = paste0(cols_fun, "_Count"))
    ) %>%
    ungroup() %>%
    filter(!is.na(!!rlang::sym(paste0(cols_fun, "_Count")))) 
  
  
  
  
  Deck_without_select_cards <- base_df %>% 
    ungroup() %>%
    anti_join(card_present , by = c("id","Archetype")) %>% 
    distinct(id,.keep_all = TRUE) %>% 
    select(
      -all_of(
        c(paste0(cols_fun, "_CardName"),
          paste0(cols_fun, "_Count")
        )
      )
    ) %>%
    full_join(card_present %>% 
                      distinct(Archetype,!!rlang::sym(paste0(cols_fun, "_CardName"))),
                    relationship = "many-to-many",by = "Archetype") %>% 
    filter(!is.na(id)) %>% 
    mutate(
      !!rlang::sym(paste0(cols_fun, "_Count")) := 0,
      Join_main_count = 0 ) %>% drop_na() 
  
  
  res <- rbind(
    card_present,
    Deck_without_select_cards
  )
  
  return(res)
}

################################################################################
###  Function that compute presence of archetype for plot used in 2 and 7  #####
# plot_presence_fun
#   color_scheme,
DF_presence_fun <- function(
    df_base,
    time_limit = Inf,
    compare_time_limit = NULL) {

  Presence_df_base <- df_base %>%
    ungroup() %>%
    filter(
      Week > (max(Week) - time_limit)
    ) %>%
    group_by(Archetype) %>%
    mutate(
      Archetype_count = n(),
      Arch_winrate = winrate_1_data(sum(Wins, na.rm = TRUE) , sum(Losses, na.rm = TRUE)),
      CI_Arch_winrate = CI_prop(Arch_winrate, sum(Losses + Wins, na.rm = TRUE)),
    ) %>%
     ungroup() %>% 
    {. ->> intermediateResult} %>%
    mutate(Global_winrate = mean(intermediateResult %>%
                                   distinct(Archetype,.keep_all = TRUE) %>% 
                                 pull(Arch_winrate)
                                 )
           ) %>% 
    group_by(Archetype) %>% 
    mutate(
      Arch_winrate = Arch_winrate - Global_winrate
      ) %>% 
    arrange(Archetype_count) %>%
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
      Base_Archetype = as.factor(Base_Archetype),
      # A reflechir ou tu mets ça selon les filters
      Archetype_percent = Archetype_count / nrow(.)
    ) %>%
    group_by(Archetype,Base_Archetype) %>%
    mutate(
      Based_Archetype_count = n(),
      Based_Arch_winrate = winrate_1_data(sum(Wins, na.rm = TRUE) , sum(Losses, na.rm = TRUE)),
      CI_Based_Arch_winrate = CI_prop(Based_Arch_winrate, sum(Losses + Wins, na.rm = TRUE))
    ) %>%
    ungroup() %>% 
    {. ->> intermediateResult} %>%
    mutate(
      Global_winrate_base_arch = 
        mean(
          intermediateResult %>%
            distinct(Base_Archetype,.keep_all = TRUE) %>% 
            pull(Based_Arch_winrate), na.rm = TRUE
          )
      ) %>% 
    group_by(Base_Archetype) %>% 
    mutate(  
      Based_Arch_winrate = Based_Arch_winrate - Global_winrate_base_arch,
      Based_Archetype_percent = #round((
        Based_Archetype_count / nrow(.),#) * 100, 1),
      Based_Archetype_inside_main_percent = round(
        (Based_Archetype_count / Archetype_count) * 100, 1
      ),
      Arch_winrate_format = paste0(
        round(Arch_winrate * 100, 1),
        formating_CI(Arch_winrate, CI_Arch_winrate)
      ),
      Based_Arch_winrate_format = paste0(
        round(Based_Arch_winrate * 100, 1),
        formating_CI(Based_Arch_winrate, CI_Based_Arch_winrate)
      )
    ) %>% 
    ungroup()
    
  # browser()
  if (!is.null(compare_time_limit)) {
    df_comparisson <- DF_presence_fun(df_base, time_limit = compare_time_limit)
    
    
    df_presence_res <- Presence_df_base %>%
      inner_join(
        df_comparisson %>%
          select(
            id, Rank, Archetype_count,
            Archetype_percent, Based_Archetype_count,
            Based_Archetype_inside_main_percent,
            Arch_winrate, CI_Arch_winrate,
            Based_Arch_winrate, CI_Based_Arch_winrate
          ),
        by = "id",
        suffix = c("", paste0("_", compare_time_limit - 1))
      ) %>%
      mutate(
        Delta_rank = if_else(
          Rank - !!rlang::sym(paste0("Rank_", compare_time_limit - 1)) == 0,
          "",
          ifelse(Rank - !!rlang::sym(paste0("Rank_", compare_time_limit - 1)) < 0,
                 paste0("+ ", abs(Rank - !!rlang::sym(paste0("Rank_", compare_time_limit - 1)))),
                 paste0("- ", Rank - !!rlang::sym(paste0("Rank_", compare_time_limit - 1)))
          )
        ),
        Num_delta_perc_arch = ifelse(
          is.na(Archetype_percent) |
            is.na(!!rlang::sym(paste0("Archetype_percent_", compare_time_limit - 1))),0,
                                     Archetype_percent - !!rlang::sym(paste0("Archetype_percent_", compare_time_limit - 1))),
        Num_Delta_Arch_win_rate = ifelse(
          is.na(Arch_winrate) |
            is.na(!!rlang::sym(paste0("Arch_winrate_", compare_time_limit - 1))),0,
          Arch_winrate - !!rlang::sym(paste0("Arch_winrate_", compare_time_limit - 1))),
        Num_Delta_based_Arch_win_rate = ifelse(
          is.na(Based_Arch_winrate) |
            is.na( !!rlang::sym(paste0("Based_Arch_winrate_", compare_time_limit - 1))
                  ),0,
          Based_Arch_winrate - !!rlang::sym(paste0("Based_Arch_winrate_", compare_time_limit - 1))
        ),
        Delta_percent_arch = ifelse(Num_delta_perc_arch > 0,
                                    paste0("+ ", round(Num_delta_perc_arch * 100, 1)),
                                    paste0(round(Num_delta_perc_arch * 100, 1))
        ),
        Delta_Arch_count = ifelse(
          is.na(Archetype_count), 0,Archetype_count
          ) - ifelse(
          is.na(!!rlang::sym(paste0("Archetype_count_", compare_time_limit - 1))),0,
          !!rlang::sym(paste0("Archetype_count_", compare_time_limit - 1))
          ),
        
        CI_Delta_Arch_win_rate = CI_2_prop(
          Arch_winrate,
          !!rlang::sym(paste0("Arch_winrate_", compare_time_limit - 1)),
          Archetype_count,
          !!rlang::sym(paste0("Archetype_count_", compare_time_limit - 1))
        ),
        CI_Delta_based_Arch_win_rate = CI_2_prop(
          Based_Arch_winrate,
          !!rlang::sym(paste0("Based_Arch_winrate_", compare_time_limit - 1)),
          Based_Archetype_count,
          !!rlang::sym(paste0("Based_Archetype_count_", compare_time_limit - 1))
        ),
        Delta_Arch_win_rate = ifelse(
          Num_Delta_Arch_win_rate > 0,
          paste0(
            "+ ", round(Num_Delta_Arch_win_rate * 100, 1),
            formating_CI(Num_Delta_Arch_win_rate, CI_Delta_Arch_win_rate)
          ),
          paste0(
            round(Num_Delta_Arch_win_rate * 100, 1),
            formating_CI(Num_Delta_Arch_win_rate, CI_Delta_Arch_win_rate)
          )
        ),
        Delta_based_Arch_win_rate = ifelse(Num_Delta_based_Arch_win_rate > 0,
                                           paste0(
                                             "+ ", round(Num_Delta_based_Arch_win_rate * 100, 1),
                                             formating_CI(Num_Delta_based_Arch_win_rate, CI_Delta_based_Arch_win_rate)
                                           ),
                                           paste0(
                                             round(Num_Delta_based_Arch_win_rate * 100, 1),
                                             formating_CI(Num_Delta_based_Arch_win_rate, CI_Delta_based_Arch_win_rate)
                                           )
        )
      )
  } else {
    df_presence_res <- Presence_df_base
  }
  return(df_presence_res)
}





################################################################################

################################################################################
######  Function that plot presence of over week as line #######################
function_plot_spaghetti_plot <- function(
    df_fun_spaghetti_plot,
    scheme_color_sheme_fun  = scheme,
    Arch_or_base_arch , #"Base_Archetype" "Archetype"
    week_var = "Week",
    count_arch_var,
    Number_deck_by_week_var = "Week_deck_number",
    hide_treshold = 0.025 #,     ratio_plot_fun = ratio_plot
){
  # Archetype Count_arch Archetype_percent
  num_levels_in_fun <- length(levels(df_fun_spaghetti_plot[[Arch_or_base_arch]]))
  # Associer chaque Archetype avec une couleur et un style de ligne
  Color_sheme_fun_df <- tibble(
    archetype_levels = levels(df_fun_spaghetti_plot[[Arch_or_base_arch]]),
    type_styles = rep(c("solid", "dashed"), length.out = num_levels_in_fun),
    colors_for_archetypes = rep(
      scheme_color_sheme_fun$hex(ceiling(num_levels_in_fun / 2)) , 
      each = 2, length.out = num_levels_in_fun)
  )
  
  # Calcul dynamique de la taille du plot
  num_weeks <- length(unique(df_fun_spaghetti_plot[[week_var]]))
  percent_range <- max(df_fun_spaghetti_plot[[paste0(Arch_or_base_arch, "_percent")]], na.rm = TRUE)
  plot_width <- max(1600,(90 * num_weeks ))
  
  
  Low_spaghetti <- as.character(
    df_fun_spaghetti_plot %>%
      distinct(
        !!rlang::sym(week_var),
        !!rlang::sym(Arch_or_base_arch),
        !!rlang::sym(paste0(Arch_or_base_arch,"_percent"))
      ) %>%
      group_by(!!rlang::sym(Arch_or_base_arch)) %>%
      summarise(
        max_percent = 
          max(
            !!rlang::sym(paste0(Arch_or_base_arch,"_percent"))
          )
      ) %>%
      filter(max_percent < hide_treshold) %>% 
      pull(!!rlang::sym(Arch_or_base_arch))
  )
  
  plot_base_spaghetti <- 
    ggplot(
      df_fun_spaghetti_plot,
      aes(
        x = !!rlang::sym(week_var),
        y = !!rlang::sym(paste0(Arch_or_base_arch,"_percent")),
        color = !!rlang::sym(Arch_or_base_arch),
        linetype = !!rlang::sym(Arch_or_base_arch),
        text = paste(
          "Archetype: ", !!rlang::sym(Arch_or_base_arch)," (", !!rlang::sym(count_arch_var),")", "<br>", # Archetype name
          "Archetype percent: ", round(!!rlang::sym(paste0(Arch_or_base_arch,"_percent")) * 100, 2), " %", "<br>",
          sep = ""
        ),
        group = 1
      )
    )
  
  
  plot_spaghetti_res <- 
    (plot_base_spaghetti +
       geom_line(linewidth = 1) +  # Ajuster la taille des lignes pour une meilleure visibilité
       geom_point(size = 2) +  # Ajuster la taille des points
       scale_x_discrete(
         week_var,
         # breaks = levels(Presence_for_best_deck_plot$Week),
         labels = paste0(
           "Week: ", as.character(unique(df_fun_spaghetti_plot[[week_var]])), "<br>",
           # unique(Presence_Base_Archetype_for_best_deck_plot$Date), "<br>",
           "N decks:<br>", df_fun_spaghetti_plot %>%
             arrange(!!rlang::sym(week_var)) %>%  
             select(!!rlang::sym(week_var),!!rlang::sym(Number_deck_by_week_var)) %>% 
             distinct(!!rlang::sym(week_var),.keep_all = TRUE) %>% 
             pull(!!rlang::sym(Number_deck_by_week_var))
           
         )
       ) +
       scale_color_manual(
         values = setNames(
           Color_sheme_fun_df$colors_for_archetypes, Color_sheme_fun_df$archetype_levels),
         guide = guide_legend(ncol = 3)
       ) +
       scale_linetype_manual(
         values = setNames(Color_sheme_fun_df$type_styles, Color_sheme_fun_df$archetype_levels),  # Appliquer le style de ligne
         guide = guide_legend(ncol = 3)
         ) +
       theme(
         axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
         legend.position = "top",            # Position en haut
         legend.justification = c(0, 1),    # Alignement à droite
         # legend.direction = "horizontal",   # Organisation sur plusieurs lignes
         legend.box = "vertical",        # Ajustement horizontal pour plusieurs lignes
         legend.text = element_text(size = 8),  # Taille des textes de la légende
         legend.title = element_text(size = 10), # Taille du titre de la légende
         axis.title.x=element_blank()
       ) +
       ylab(
         paste0(str_replace(Arch_or_base_arch,"_"," "),
                   " presence")
         ) +
       scale_y_continuous(labels = function(x) scales::percent(x))
    ) %>%
    ggplotly(
      tooltip = c("text"), 
      # height = (480 * ratio_plot_fun), width = (850 * ratio_plot_fun)
      height = 480 * 2.5, 
      width = plot_width
      ) %>%
    plotly::layout(legend = list(
      orientation = "h",
      y = -0.1,
      title = list(text = "")
    ))

  
  plot_spaghetti_res$x$data <- lapply(
    plot_spaghetti_res$x$data, function(y) {
      if (y$name %in% Low_spaghetti) {
        y$visible <- "legendonly"
      }
      
      
      return(y)
    }
  )
  
  return(
    plot_spaghetti_res 
  )
}
################################################################################
######  Function that plot presence of archetype used in 2 and 7  ##############
plot_presence_fun <- function(
    df_base,
    color_scheme,
    time_limit = Inf,
    compare_time_limit = NULL,
    plot_scaling = 2.25
    ) {
  # browser()
  
  df_plot_presence <- DF_presence_fun(
    df_base = df_base,
    time_limit = time_limit,
    compare_time_limit = compare_time_limit
  ) %>% 
    select(
      any_of(
        c(
          "Archetype","Base_Archetype","Rank","Delta_rank","Archetype_count",
           "Arch_winrate_format","Delta_based_Arch_win_rate","Archetype_percent",
           "Delta_percent_arch","Delta_Arch_win_rate","Based_Archetype_count","Based_Archetype_percent",
          "Based_Arch_winrate_format"
          )
        )
      ) %>% 
    distinct()
  
  df_labels <- df_plot_presence %>%
    group_by(Archetype) %>%
    summarise(
      sum_prop = unique(Archetype_percent) # Toujours égal à 1 ou 100%
    ) %>%
    ungroup()
  
  # Nombre d'archetypes pour ajuster la hauteur
  n_archetypes <- length(unique(df_plot_presence$Archetype))
  
  # Largeur maximum des proportions pour ajuster la largeur
  max_plot_width <- max(df_labels$sum_prop, na.rm = TRUE)
  
  # Calcul dynamique des dimensions
  plot_height <- min(max(480, n_archetypes * 50), 1100) # Entre 480px et 1200px
  plot_width <- min(2400, 1200 * (1 + max_plot_width))
  
  if (!is.null(compare_time_limit)) {
    base_Plot_presence  <- 
      ggplot(
        df_plot_presence,
        aes(
          x = Archetype,
          y = Based_Archetype_percent,#prop.table(stat(count)),
          fill = Base_Archetype,
          # label = scales::percent(prop.table(stat(count))),
          text = paste(
            "Archetype: ", Archetype, "<br>", # Archetype name
            "Base Archetype: ", Base_Archetype, "<br>", # Base Archetype name
            "Rank: ", Rank, if_else(Delta_rank == "", "", paste0(" (", Delta_rank, ")")), " [n = ", Archetype_count, "]", "<br>",
            "Archetype Win rate: ", Arch_winrate_format, " ", " (", Delta_Arch_win_rate, ")", "<br>",
            "Base Archetype Win rate: ", Based_Arch_winrate_format, " ", " (", Delta_based_Arch_win_rate, ")", "<br>",
            "Delta Archetype percent: ", Delta_percent_arch, " %", "<br>",
            "Base Archetype count: ", Based_Archetype_count, " (", round(Based_Archetype_percent*100,1), " %", ")", "<br>",
            sep = ""
          )
        )
      )
  } else {
    base_Plot_presence <- 
      ggplot(
        df_plot_presence,
        aes(
          x = Archetype,
          y = Based_Archetype_percent,#prop.table(stat(count)),
          fill = Base_Archetype,
          # label = scales::percent(prop.table(stat(count))),
          text = paste(
            "Archetype: ", Archetype, "<br>",
            "Base Archetype: ", Base_Archetype, "<br>",
            "Rank: ", Rank, " [n = ", Archetype_count, "]", "<br>",
            "Archetype Win rate: ", Arch_winrate_format, " ", "<br>",
            "Base Archetype Win rate: ", Based_Arch_winrate_format, " ", "<br>",
            "Archetype: ", round(Based_Archetype_percent*100,1), "<br>",
            "Base Archetype count: ", Based_Archetype_count, "<br>",
            sep = ""
          )
        )
      )
  }
  
  
  Plot_presence <- 
    (base_Plot_presence
  +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(
      data = df_labels,
      aes(
        x = Archetype,
        label = paste0(round(sum_prop * 100, 1), " %"),
        y = sum_prop + 0.008 # Place les labels au-dessus de la pile
      ),
      size = 5,
      inherit.aes = FALSE
    ) +
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
    tooltip = c("text"), 
    height = plot_height,#(480 * plot_scaling),  # 1440
    width = plot_width  #(820 * plot_scaling) # 2460
  )
  
  
  # Truc compliqué pour enlever l'overlay du texte
  Plot_presence$x$data[[which(
    sapply(Plot_presence$x$data, function(x) {
      x$mode == "text"
    }) == TRUE
  )]]$hoverinfo <- "none"
  
  return(Plot_presence)
}


################################################################################

################################################################################
################   Compute winrate and CI for top n player  ####################

best_player_result_fun <- function(
    top_n_player_fun,
    Arch_or_base_arch,
    df_fun_top_player
) {
  
  top_player_df <- df_fun_top_player %>%
    group_by(!!rlang::sym(Arch_or_base_arch),Player) %>% 
    mutate(n_deck_player = sum(Wins + Losses),.before = 1) %>% 
    filter(n_deck_player >= top_n_player_fun) %>% 
    summarise( 
      Arch_winrate = winrate_1_data(
        sum(Wins, na.rm = TRUE) , sum(Losses, na.rm = TRUE)
      ),
      player_lower_bound  = Arch_winrate + CI_prop(
        Arch_winrate, sum(Losses + Wins, na.rm = TRUE)
      ),
      .groups = "drop" # "keep"
    ) %>% 
    group_by(!!rlang::sym(Arch_or_base_arch)) %>%
    slice_max(
      player_lower_bound,
      n = 10 ,
      with_ties = FALSE
    ) %>% 
    select(-Arch_winrate) %>%
    ungroup()
  
  best_player_res <- df_fun_top_player %>% 
    left_join(
      top_player_df,
      by = join_by(Player, !!rlang::sym(Arch_or_base_arch))
    ) %>% 
    filter(!is.na(player_lower_bound)) %>%
    group_by(!!rlang::sym(Arch_or_base_arch)) %>% 
    summarise( 
      Number_of_best_player = n_distinct(Player),
      Number_of_match_best_player = sum(Wins, na.rm = TRUE) + sum(Losses, na.rm = TRUE),
      best_player_Arch_winrate = winrate_1_data(
        sum(Wins, na.rm = TRUE) , sum(Losses, na.rm = TRUE)
      ),
      best_player_CI_Arch_winrate = CI_prop(
        best_player_Arch_winrate, sum(Losses + Wins, na.rm = TRUE)
      )
    )
  
  return(best_player_res)
}

################################################################################

################################################################################
##################      Function that Generate CI plot  ######################## 
Generate_CI_plot_fun <- function(
    df_ci_fun_param ,
    win_rate_fun_par ,
    CI_fun_par,
    Arch_or_base_arch ,
    best_player_df_par_fun = NULL
){

  ci_df_plot_lines_df <- data.frame(
    yintercept = c(
      mean(df_ci_fun_param[[win_rate_fun_par]]),
      mean(df_ci_fun_param[[win_rate_fun_par]] - df_ci_fun_param[[CI_fun_par]]),
      mean(df_ci_fun_param[[win_rate_fun_par]] + df_ci_fun_param[[CI_fun_par]])
    ),
    line_type = c("Average Winrate",  "Lower CI", "Upper CI")
  )
  
  if(is.null(best_player_df_par_fun) | nrow(best_player_df_par_fun) == 0){
  df_plot_with_best_player <- df_ci_fun_param %>% 
    mutate(
      Number_of_best_player = NA,
      Number_of_match_best_player = NA,
      best_player_Arch_winrate = NA,
      best_player_CI_Arch_winrate = NA
    )
  
  additional_layers <- NULL
  } else {
    df_plot_with_best_player <- df_ci_fun_param %>% 
      mutate(join_arch = as.character(!!rlang::sym(Arch_or_base_arch))) %>% 
      left_join(
        best_player_df_par_fun,
        by = join_by(join_arch == !!rlang::sym(Arch_or_base_arch))
        ) %>% 
      select(-join_arch)
    additional_layers <- list(
      geom_point(
        data = df_plot_with_best_player %>%
          filter(!is.na(best_player_Arch_winrate)),
        aes(
          y = !!rlang::sym("best_player_Arch_winrate"),
          x = !!rlang::sym(Arch_or_base_arch),
          color = "Best Player Winrate"
        ),
        shape = 17, # Forme différente pour distinguer
        position = position_nudge(x = 0.3)
      ),
      geom_errorbar(
        data = df_plot_with_best_player %>%
          filter(!is.na(best_player_Arch_winrate)),
        aes(
          x = !!rlang::sym(Arch_or_base_arch),
          ymin = !!rlang::sym("best_player_Arch_winrate") + !!rlang::sym("best_player_CI_Arch_winrate"),
          ymax = !!rlang::sym("best_player_Arch_winrate") - !!rlang::sym("best_player_CI_Arch_winrate"),
          color = "Best Player Winrate"
        ),
        width = 0.01,
        position = position_nudge(x = 0.3)
      )
    )
  }

  resulting_plot <- (
    ggplot(data = df_plot_with_best_player,
           aes(text = paste(
             "Archetype: ", !!rlang::sym(Arch_or_base_arch)," ",!!rlang::sym(paste0(Arch_or_base_arch,"_count"))," (",Number_of_match ,")" ,"<br>", # Archetype name
             "Winrate: ",
             round(!!rlang::sym(win_rate_fun_par) * 100, 1), " %",
             "[", round((!!rlang::sym(win_rate_fun_par) + !!rlang::sym(CI_fun_par)) * 100, 2), ";",
             round((!!rlang::sym(win_rate_fun_par) - !!rlang::sym(CI_fun_par)) * 100, 2), "]", "<br>",
             "Best Player Winrate: ",
             ifelse(is.na(!!rlang::sym("best_player_Arch_winrate")), "NA",
                    paste0(Number_of_best_player ," players (matches :",Number_of_match_best_player,")<br>",
                           round(!!rlang::sym("best_player_Arch_winrate") * 100, 1), " %",
                           "[", round((!!rlang::sym("best_player_Arch_winrate") + !!rlang::sym("best_player_CI_Arch_winrate")) * 100, 2), ";",
                           round((!!rlang::sym("best_player_Arch_winrate") - !!rlang::sym("best_player_CI_Arch_winrate")) * 100, 2), "]"
                           )

                    ), "<br>",
             sep = ""
           ))
    ) +
      geom_point(
        aes(
          y = !!rlang::sym(win_rate_fun_par),
          x = !!rlang::sym(Arch_or_base_arch)
        ),
        position = position_dodge(0.75)
      ) +
      geom_errorbar(aes(
        x = !!rlang::sym(Arch_or_base_arch),
        ymin = !!rlang::sym(win_rate_fun_par) + !!rlang::sym(CI_fun_par),
        ymax = !!rlang::sym(win_rate_fun_par) - !!rlang::sym(CI_fun_par)
      ),
      position = position_dodge(width = .75),
      width = .01
      ) +
      # Points et barres d'erreur pour les meilleurs joueurs
      additional_layers + 
      geom_hline(
        data = ci_df_plot_lines_df,
        aes(yintercept = yintercept, color = line_type),
        linetype = "dashed"
      ) +
      scale_color_manual(
        name = "Legend",
        values = c("Average Winrate" = "red",
                   "Lower CI" = "darkgreen",
                   "Upper CI" = "blue",
                   "Best Player Winrate" = "purple" #,"Best Player Winrate CI" = "orange"
                   ),
      ) +
      scale_x_discrete(
        label = paste0(
          "<span style='font-size:", 17 , "px;'> <b>",
          df_ci_fun_param %>%
            pull(!!rlang::sym(Arch_or_base_arch)) %>%
            unique(),
          "</b> </span>",
          "<br /> n : ", df_ci_fun_param %>%
            pull(
              !!rlang::sym(paste0(Arch_or_base_arch,"_count")
              ))," (",df_ci_fun_param %>%
            pull(
              Number_of_match
              ) ,
          ")" ,
          "<br /> ", (df_ci_fun_param %>%
                        mutate(temp_ci = paste0(
                          round(!!rlang::sym(win_rate_fun_par) * 100, 1), " %",
                          "[", round((!!rlang::sym(win_rate_fun_par) + !!rlang::sym(CI_fun_par)) * 100, 2), ";",
                          round((!!rlang::sym(win_rate_fun_par) - !!rlang::sym(CI_fun_par)) * 100, 2), "]")
                        ) %>%
                        pull(
                          temp_ci
                        )
          )
        )
      ) +
      coord_flip() +
      theme(
        legend.position = c(0.0, 0.85),
        legend.background = element_rect(fill = "white", color = "black")
      )
  )  %>%
    ggplotly(
      tooltip = c("text"),
      height = max(650,50 * nrow(df_ci_fun_param)),
      ################### ajout temp
      width = NULL, # Permet à Plotly de calculer automatiquement la largeur
      autosize = TRUE
    )  %>%
    plotly::layout(
      legend = list(x = 0.0, y = 1), # Position précise de la légende dans le graphique Plotly
      showlegend = TRUE ,
      ################### ajout temp
      margin = list(l = 1, r = 1, t = 10, b = 10) # Réduction des marges gauche, droite, haut et bas
    )  %>%
    bslib::card(full_screen = TRUE)
  return(resulting_plot)
}

################################################################################




################################################################################
######  Function that agreg cards with not enought count df in 5 and 6  ########
Agreg_count_by_cards <- function(df,
                                 cols_choice,
                                 min_sample_size_fun ) {
  # !!rlang::sym(paste0(cols_choice,"_CardName"))
  
  df_result <- df %>%
    
    # Sens du groupe afin de grouper 0 avec 1 et 4 avec 3 
    
    ungroup() %>% 
    mutate(
      grouping_sens = if_else(
        (min_count_group - most_common_quantity) < 0,
        "up",
        # if_else((min_count_group - most_common_quantity) > 0,
        "botom"
        # ,"most_common")
      )
    ) %>%
    
    group_by(Archetype, !!rlang::sym(paste0(cols_choice, "_CardName"))) %>%
    mutate(
      grouping_sens = if_else(
        grouping_sens == "up" & min_count_group == max(min_count_group),
        "botom",if_else(
          grouping_sens == "botom" & min_count_group == min(min_count_group),
          "up",grouping_sens)
      ),
      
      
      
      # Regarde le nombres de d'itération de chaque groupe
      need_group = count_iteration_cards < min_sample_size_fun,
      no_group_needed = any(need_group),
      grouping_this_iter =
        (min_count_group == suppressWarnings(min(min_count_group[need_group])) |
           min_count_group == suppressWarnings(max(min_count_group[need_group]))
         
        ),
      
      
      # Gestion compliqué des cas ou les les 2 groupes sont contigu ce qui conduisait a des doubles groupement
      grouping_this_iter = ifelse(
        grouping_this_iter & if_else(
          is.na(lag(grouping_this_iter)),
          FALSE, lag(grouping_this_iter)
        ),
        FALSE,
        grouping_this_iter
      ),
      # Gestion compliqué des cas ou les les 2 groupes sont séparé par un groupe ce qui conduisait a des doubles groupement
      grouping_this_iter = ifelse(
        grouping_this_iter & if_else(
          is.na(lag(grouping_this_iter,2)),
          FALSE, lag(grouping_this_iter,2)
        ),
        FALSE,
        grouping_this_iter
      ),
      
    ) %>%
    group_by(
      Archetype, Archetype_count, !!rlang::sym(paste0(cols_choice, "_CardName")), total_number_of_copie,
      most_common_count, most_common_quantity
    ) %>% 
    mutate(
      
      # new variable to handle case when grouping as same count as not grouping var
      as_been_group = 
        ifelse(
          (
            count_iteration_cards < min_sample_size_fun & 
              grouping_sens == "up" & grouping_this_iter &
              !is.na(lead(count_iteration_cards))
            
          ) | (lag(count_iteration_cards) < min_sample_size_fun &
                 lag(grouping_sens) == "up" &
                 !is.na(lag(count_iteration_cards)) &
                 lag(grouping_this_iter)
               
          ),"up",ifelse(
            (count_iteration_cards < min_sample_size_fun & grouping_sens == "botom" & grouping_this_iter&
               !is.na(lag(count_iteration_cards))
            )  | (
              lead(count_iteration_cards) < min_sample_size_fun &
                lead(grouping_sens) == "botom" &
                !is.na(lead(count_iteration_cards)) &
                lead(grouping_this_iter)
            ),"botom","no group"
          )
        ),
      # create incremental name for no groups to prevent grouping
      as_been_group = str_replace(as_been_group,"no group",paste0("no group",cumsum(as_been_group == "no group"))),
      
      # reflechir a gérer most common pour le moment non fonctionnel
      new_count =
        if_else(
          (
            count_iteration_cards < min_sample_size_fun & 
              grouping_sens == "up" & grouping_this_iter &
              !is.na(lead(count_iteration_cards))
            
          ),
          count_iteration_cards + lead(count_iteration_cards),
          if_else(
            (count_iteration_cards < min_sample_size_fun & grouping_sens == "botom" & grouping_this_iter&
               !is.na(lag(count_iteration_cards))
            ),
            count_iteration_cards + lag(count_iteration_cards),
            count_iteration_cards
          )
        ),
      new_count = if_else(
        (lag(count_iteration_cards) < min_sample_size_fun &
           lag(grouping_sens) == "up" &
           !is.na(lag(count_iteration_cards)) &
           lag(grouping_this_iter)
         
        ),
        lag(new_count),
        if_else(
          (
            lead(count_iteration_cards) < min_sample_size_fun &
              lead(grouping_sens) == "botom" &
              !is.na(lead(count_iteration_cards)) &
              lead(grouping_this_iter)
            
          ),
          lead(new_count),
          new_count
        )
      )
      
      
    ) %>%
    group_by(
      Archetype, Archetype_count, !!rlang::sym(paste0(cols_choice, "_CardName")), total_number_of_copie,
      most_common_count, most_common_quantity, new_count,as_been_group
    ) %>%
    summarise(
      Wins = sum(Wins),
      Losses = sum(Losses),
      min_count_group = min(min_count_group),
      !!rlang::sym(paste0(cols_choice, "_Count")) := paste0(!!rlang::sym(paste0(cols_choice, "_Count")), collapse = "/"),
      .groups = "drop"
    ) %>%
    group_by(
      Archetype, Archetype_count, !!rlang::sym(paste0(cols_choice, "_CardName")), total_number_of_copie,
      most_common_quantity
    ) %>% 
    mutate(
      most_common_count = max(new_count) 
    ) %>% 
    ungroup() %>% 
    rename(count_iteration_cards = new_count) %>%
    arrange(Archetype, !!rlang::sym(paste0(cols_choice, "_CardName")), min_count_group) %>%
    select(
      Archetype, Archetype_count, !!rlang::sym(paste0(cols_choice, "_CardName")), !!rlang::sym(paste0(cols_choice, "_Count")), Wins, Losses,
      count_iteration_cards, total_number_of_copie, most_common_count, most_common_quantity,
      min_count_group
    )
  
  
  df_temp_check <- df_result %>%
    mutate(
      need_group = count_iteration_cards < min_sample_size_fun,
    ) 
  
  
  # Testing 
  df_check_tot <-
    df_temp_check %>%
    group_by(
      Archetype, Archetype_count, !!rlang::sym(paste0(cols_choice, "_CardName")), total_number_of_copie,
      most_common_quantity
    ) %>%
    summarise(Check_total_count_iteration = sum(count_iteration_cards),.groups = "drop") %>%
    mutate(
      check_tot = total_number_of_copie == Check_total_count_iteration
      
    )
  
  if(any(is.na(df_temp_check$need_group))){stop("any na")}
  if(any(df_temp_check$count_iteration_cards > df_temp_check$most_common_count)){stop("max count not max")}
  if(!all(df_check_tot$check_tot)){stop("quantité total")}
  
  
  
  if (
    all(!df_temp_check$need_group)
  ) {
    
    df_final <- df_result
  } else {
    df_final <- 
      # df <- 
      Agreg_count_by_cards(
        df_result,
        cols_choice,
        min_sample_size_fun = min_sample_size_fun
      )
  }
  
  
  return(df_final)
}




################################################################################

################################################################################
#######################  Agreg tournament  #####################################
Tournament_agreger <- function(tournament_string){
  
  # reflechir_mtgo vs paper
  res <- if_else(
    str_detect(tournament_string, "Modern Qualifier"), "Modern Qualifier",
    if_else(
      str_detect(tournament_string, "Last Chance Qualifier"), "Last Chance Qualifier",
      if_else(
        str_detect(tournament_string, "Modern Preliminary"), "Modern Preliminary",
        if_else(
          str_detect(tournament_string, "Modern Super Qualifier"), "Modern Super Qualifier",
          if_else(
            str_detect(tournament_string, "Modern Challenge"), "Modern Challenge",
            if_else(
              str_detect(tournament_string, "Modern \\$\\d+K Trial"), "Modern X K dollars Trial",
              if_else(
                str_detect(tournament_string, "Modern Showcase Challenge"), "Modern Showcase Challenge",
                if_else(
                  str_detect(tournament_string, "\\$\\d+K RCQ - Modern -|\\$\\d+K RCQ \\(Top \\d+\\) - Modern"), "X K dollars RCQ - Modern",
                  if_else(
                    str_detect(tournament_string, "Regional Championship|European Championship"), "Regional Championship", "other"
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  return(res)
  
}




################################################################################
################################################################################
#######################  Simple flaten cor matrix ##############################
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  = (cormat)[ut]#,
    # p = pmat[ut]
  )
}



################################################################################
# simple function that a link to a column or create column with link if not exists
add_link_to_a_column <- function(
    df_add_link_fun ,# =   a,
    column_where_is_add, #= "link", #"link",
    link_column, #= "scryfall_uri",  , 
    mode = "html", # type of link not use now
    Card_name_col =   NULL #  "Main Card"
){
  result_with_link <- df_add_link_fun %>% 
    mutate(!!rlang::sym(paste0(link_column)) := 
             as.character(!!rlang::sym(paste0(link_column)))
           
    ) %>% 
    {if (!column_where_is_add %in% colnames(.)) mutate(.,
                                                       !!rlang::sym(paste0(column_where_is_add)) := "link",
                                                       .before = 1
    ) else .} 
  
  if(!is.null(Card_name_col)){
    result_with_link <- result_with_link %>% 
      left_join(
        agregate_land_link(),
        by = join_by(!!rlang::sym(paste0(Card_name_col)) == join_name)
      ) %>%
      mutate(
        scryfall_uri = ifelse(
          is.na(scryfall_uri)
         & !is.na(search_Link ) ,search_Link ,scryfall_uri
      )
      )

  }
  # create link
  if(mode == "html"){
    res <-  result_with_link  %>% 
      mutate(
        
        !!rlang::sym(paste0(column_where_is_add)) := ifelse(
          is.na(!!rlang::sym(paste0(link_column))),
          paste0(!!rlang::sym(paste0(column_where_is_add))),
          
          paste0(
            '<a href=\"',!!rlang::sym(paste0(link_column)),'">',!!rlang::sym(paste0(column_where_is_add)),'</a>'
          ))
      ) %>% 
      select(-any_of(link_column))
  } else if(mode == "md"){
    res <-  result_with_link  %>% 
      mutate(
        
        !!rlang::sym(paste0(column_where_is_add)) := ifelse(
          is.na(!!rlang::sym(paste0(link_column))),
          paste0(!!rlang::sym(paste0(column_where_is_add))),
          
          paste0(
            "[",!!rlang::sym(paste0(column_where_is_add)),'](',!!rlang::sym(paste0(link_column)),')'
          ))
      ) %>% 
      select(-any_of(link_column))
    
    
    
  }
  

  return(res)
  
}



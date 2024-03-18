################################################################################
####################### Patch foireux pour ban #################################


# Take simple df from json curated and search for a vector of ban cards
Search_for_ban_cards <- function(vec_of_ban_cards, df, colname_deck_list) {
  main_remove_id <- df %>%
    unnest_longer(!!colname_deck_list) %>%
    unnest_wider(!!colname_deck_list,
      names_sep = "_"
    ) %>%
    filter(!!rlang::sym(paste0(colname_deck_list, "_CardName")) %in% vec_of_ban_cards) %>%
    distinct(id) %>%
    unlist()
  return(main_remove_id)
}


# prévoir exact résultat en supprimant les résultats obtenue contre des deck ban mais douteux risque de biais ++
Ban_patch <- function(vec_of_ban_cards, df # ,exact_result = FALSE
) {
  # Search deck id with ban cards in side or deck
  Main_board_remove_id <- Search_for_ban_cards(vec_of_ban_cards, df, "Mainboard")
  Side_board_remove_id <- Search_for_ban_cards(vec_of_ban_cards, df, "Sideboard")


  # Get unique id when ban cards is in both side and main
  Remove_id <- unique(c(Main_board_remove_id, Side_board_remove_id))



  # search for remove player
  # Create a df of with a list of key by pasting player and archetype use after for remove player in matchup list
  Remove_id_temp <- df %>%
    filter(id %in% Remove_id) %>%
    select(TournamentFile, Player, ReferenceArchetype_Archetype) %>%
    group_by(TournamentFile) %>%
    summarise(
      remove_matchup = list(
        paste0(Player, "_", ReferenceArchetype_Archetype)
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




  return(Df_final)
}
##################################################################################















################################################################################
################# Json function to auto update filter ##########################

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
################################################################################





################################################################################
############################## Format table  ###################################
# Format table résult removing space and shorten name make some variables factor
# Used in 3_Card_win_rate_table

format_df_result_card_table <- function(
    df_base_fun, # data frame from xx
    colname_deck_list, # "Mainboard" or "Sideboard"
    df_Archetyp_fun, # df Used to put corect order for archetype or based archetype
    Based_Archetyp_fun = FALSE # function use deagregeted archetype
    ) {
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









# IZET mid = deck de nassif a voir pour parser

# Aggregation a regarder

# [1] "Beans Cascade"                 "Simic Midrange _fallback"      "Eldrazi"                       "Grixis Midrange"
# [5] "Jund Midrange"                 "Phoenix"                       "Discover Combo"                "Jund Aggro"
# [9] "Mono Blue Midrange"            "Free Spells"                   "Electro End"                   "Mono Red Aggro"
# [13] "Izzet Aggro"                   "Boros Blink"                   "Zombies"                       "Green Devotion"
# [17] "Grixis Kiki Jiki"              "Mono Green Midrange _fallback" "Azorius Midrange"              "Boros Midrange _fallback"
# [21] "Bant Midrange _fallback"       "5 Color Midrange _fallback"    "Temur Midrange _fallback"      "Mono White Midrange _fallback"
# [25] "Enduring Ideal"                "Midrange _fallback"            "Ensoul"
# [29] "Crabvine"                      "Reclamation"                   "Neobrand"                      "WUBG Midrange _fallback"
# [33] "Ascendancy Combo"              "Grixis Aggro"                  "Slivers"                       "Skelementals"
# [37] "Naya Midrange _fallback"       "Mono Blue Control _fallback"   "Rakdos Sacrifice"              "Tameshi Bloom"
# [41] "WBRG Midrange _fallback"       "Hollow One"                    "Devoted Combo"                 "Spirits"
# [45] "Izzet Midrange _fallback"      "Soul Sisters"                  "Land Destruction"              "Glimpse Combo"
# [49] "Manufactor Combo"              "Lantern"                       "Boros Aggro"                   "Enchantress"
# [53] "The Rack"                      "Calibrated Blast"              "              "Infect"
# [57] "Elves"                         "Blink"                         "Kuldotha Aggro"                "Bogles"
# [61] "Faeries"                       "Taxes"                         "Goblins"
#             "Ad Nauseam"                                     "Affinity"
#               "Orzhov Midrange"               "Scapeshift"

################################################################################









################################################################################
# A evaluer
# "Mono Green Midrange _fallback"
# "Green Devotion"



Archetype_agreger <- function(Archetype_to_agreg, color_agreg = NULL) {
  # Group UW base control and midrange in maccro archetype also contain archetype share with dimir
  UW_controle_group <- c(
    "Azorius Control _fallback",
    "Bant Control _fallback", "Jeskai Control _fallback",
    "Taking Turns",
    # soupe liée a dimir
    "Esper Control _fallback", "Esper Midrange _fallback", "WUBG Control _fallback",
    "WUBR Control _fallback", "WURG Control _fallback",
    "5 Color Control _fallback"
  )


  # Group UB base control and midrange in maccro archetype deck share with UW are in the UW groups
  UB_controle_group <- c(
    "Dimir Control _fallback", "Dimir Midrange _fallback",
    "Sultai Control _fallback", "Sultai Midrange _fallback",
    "Grixis Control _fallback", "Grixis Midrange _fallback",
    # ajout discutable
    "Rogues"
  )


  # Not murktide UR control
  UR_controle_group <- c(
    "Izzet Control _fallback", "Temur Control _fallback",
    "Temur Midrange _fallback", "Grixis Aggro",
    "Faeries"
  )






  # Gestion des controles en classe large définis au dessus
  Archetype_to_agreg <- ifelse(Archetype_to_agreg %in% UW_controle_group,
    "UWhiteX Control",
    ifelse(Archetype_to_agreg %in% UB_controle_group,
      "UBlackX Control",
      ifelse(Archetype_to_agreg %in% UR_controle_group,
        "URedX Control", Archetype_to_agreg
      )
    )
  )

  # Regroupement du split de creativity sur persist
  Archetype_to_agreg <- ifelse(str_detect(
    Archetype_to_agreg,
    "Creativity"
  ),
  "Creativity", Archetype_to_agreg
  )


  ################################################################################
  ############################ Réfléxion a mener #################################
  # All Blink decks (weak groups because deck can be really differents)
  Archetype_to_agreg <- ifelse(Archetype_to_agreg %in%
    c(
      "Azorius Blink", "Bant Blink", "WURG Blink",
      "Jeskai Blink", "Esper Blink", "Boros Blink"
    ),
  "Blink", Archetype_to_agreg
  )
  # groupe all deck blade a reflechir sur le fait de grouper avec blink
  Archetype_to_agreg <- ifelse(Archetype_to_agreg == "Grief Blade",
    "Stoneblade", Archetype_to_agreg
  )

  # A reflechir sur groupement avec blink ou taxes
  Archetype_to_agreg <- ifelse(
    Archetype_to_agreg %in% c(
      "Orzhov Midrange _fallback",
      "Orzhov Blink", "Mono White Blink",
      "Abzan Blink"
    ),
    "Orzhov Midrange", Archetype_to_agreg
  )
  ##############################################################################

  # Delver Gestion
  # Gestion du problème de l'absence des couleurs dans les archetypes des mathcups
  if (is.null(color_agreg[1])) {
    Archetype_to_agreg <- ifelse(Archetype_to_agreg == "Delver",
      "URedX Control", Archetype_to_agreg
    )
  } else {
    # split des 2 types de dever dans leur archetype par couleurs
    Archetype_to_agreg <- ifelse(Archetype_to_agreg == "Delver" & color_agreg == "UR",
      "Murktide", Archetype_to_agreg
    )
    Archetype_to_agreg <- ifelse(Archetype_to_agreg == "Delver" & color_agreg == "UBR",
      "UBlackX Control", Archetype_to_agreg
    )
  }

  # Groupe breach value and murktide
  Archetype_to_agreg <- ifelse(Archetype_to_agreg == "Breach Value",
    "Murktide", Archetype_to_agreg
  )


  # Pack rhinos
  Archetype_to_agreg <- ifelse(Archetype_to_agreg == "Footfalls 4 C",
    "Footfalls", Archetype_to_agreg
  )
  # Pack tron
  Archetype_to_agreg <- ifelse(str_detect(Archetype_to_agreg, "Tron$"),
    "Tron", Archetype_to_agreg
  )

  # Regroupement de tout les rakdos midrange et scam
  Archetype_to_agreg <- ifelse(Archetype_to_agreg %in%
    c(
      "Rakdos Midrange _fallback",
      "Mardu Midrange _fallback",
      "Skelementals"
    ),
  "Scam", Archetype_to_agreg
  )

  # Regroupement de mono B midrange et coffer
  Archetype_to_agreg <- ifelse(Archetype_to_agreg == "Mono Black Midrange _fallback",
    "Coffers Control", Archetype_to_agreg
  )




  # Groupement des différentes version R midrange ou aggros en 1 maccro arc
  Archetype_to_agreg <- ifelse(
    Archetype_to_agreg %in% c(
      "Gruul Midrange _fallback", "Mono Red Midrange _fallback",
      "Mardu Midrange _fallback", "Boros Aggro", "Naya Midrange _fallback",
      "Boros Midrange _fallback",
      "Mono Red Aggro"
    ),
    "REdx Midrange", Archetype_to_agreg
  )

  # Merge all rock soupes together
  Archetype_to_agreg <- ifelse(Archetype_to_agreg %in% c(
    "Golgari Midrange _fallback", "Jund Midrange _fallback",
    "Abzan Midrange _fallback", "Jund Aggro", "Jund Midrange"
  ),
  "Golgarix Midrange", Archetype_to_agreg
  )

  # Merge the two combo breach potentiellement breach storm groupable avec les autres storms
  Archetype_to_agreg <- ifelse(Archetype_to_agreg %in% c(
    "Breach Storm", "Grinding Breach"
  ),
  "Breach combo", Archetype_to_agreg
  )

  # Disctuable merge goryo et reanimator
  Archetype_to_agreg <- ifelse(Archetype_to_agreg == "Reanimator",
    "Goryo Reanimator", Archetype_to_agreg
  )

  # Regroupement de toutes les version tuant avec vaalakut, gros doutes sur l'inclusion de titanshift
  Archetype_to_agreg <- ifelse(Archetype_to_agreg %in% c(
    "Guildpact Valakut", "Blue Scapeshift"
  ),
  "Scapeshift", Archetype_to_agreg
  )
  # Merge titan shift avec scapshift
  Archetype_to_agreg <- ifelse(Archetype_to_agreg == "Titan Shift",
    "Scapeshift", Archetype_to_agreg
  )

  # Merge tout les titan sauf titan shift
  Archetype_to_agreg <- ifelse(str_detect(Archetype_to_agreg, "Titan$"),
    "Amulet Titan", Archetype_to_agreg
  )
  Archetype_to_agreg <- ifelse(Archetype_to_agreg == "Timeless Lotus",
    "Amulet Titan", Archetype_to_agreg
  )


  # Merge les 2 versions de gob
  Archetype_to_agreg <- ifelse(Archetype_to_agreg == "Goblin Whack",
    "Goblins", Archetype_to_agreg
  )

  # Groupement de tout les storms
  Archetype_to_agreg <- ifelse(Archetype_to_agreg %in% c(
    "Grixis Storm", "Boros Storm", "Mono Red Storm",
    "Gifts Storm", "Twiddle Storm"
  ),
  "Storm", Archetype_to_agreg
  )




  # Regroupement de toutes les 4/5C soupe avec des betes
  Archetype_to_agreg <- ifelse(Archetype_to_agreg %in% c(
    "Elementals", "Beans Cascade", "Saheeli Combo"
  ),
  "Omnath Control", Archetype_to_agreg
  )

  # Regroupement de toutes les soupes sans lands
  Archetype_to_agreg <- ifelse(Archetype_to_agreg %in% c("Oops All Spells"),
    "Belcher", Archetype_to_agreg
  )

  # Groupement de tout les Burn quelquesois les couleurs
  Archetype_to_agreg <- ifelse(str_detect(Archetype_to_agreg, "Burn"),
    "Burn", Archetype_to_agreg
  )

  # Meta groupes avec les soupes foods
  Archetype_to_agreg <- ifelse(Archetype_to_agreg %in% c(
    "Asmo Food", "Manufactor Combo"
  ),
  "Food", Archetype_to_agreg
  )

  return(Archetype_to_agreg)
}











################################################################################
############# Simple function that agregate some land together #################

Card_agregueur <- function(
    string,
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
    basic_land = FALSE) {
  # A ajouté pain utilitaire cycle land


  fetech_list <- c(
    "flooded strand", "polluted delta", "wooded foothills", "verdant catacombs",
    "arid mesa", "windswept heath", "scalding tarn", "prismatic vista",
    "misty rainforest", "bloodstained mire", "marsh flats"
  )

  Tron_land <- c("Urza's Mine", "Urza's Power Plant", "Urza's Tower")


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

  basic_land_list <- c(
    "Island",
    "Plains",
    "Swamp",
    "Mountain",
    "Forest"
  )



  base_string <- trimws(string)

  if (fetch) {
    base_string <- ifelse(tolower(base_string) %in% fetech_list,
      "Fetch land", base_string
    )
  }
  if (Tron) {
    base_string <- ifelse(base_string %in% Tron_land,
      "Tron land", base_string
    )
  }
  if (snow) {
    base_string <- str_remove(
      base_string,
      "Snow-Covered "
    )
  }
  if (surveil) {
    base_string <- ifelse(base_string %in% surveil_land,
      "Surveil land", base_string
    )
  }
  if (shock) {
    base_string <- ifelse(base_string %in% shock_land,
      "Shock land", base_string
    )
  }
  if (triome) {
    base_string <- ifelse(base_string %in% Triome_land,
      "Triome land", base_string
    )
  }
  if (filter_land) {
    base_string <- ifelse(base_string %in% filter_land_list,
      "Filtre land", base_string
    )
  }

  if (fast_land) {
    base_string <- ifelse(base_string %in% fast_land_list,
      "Fast land", base_string
    )
  }

  if (bounce_land) {
    base_string <- ifelse(base_string %in% bounce_land_list,
      "Bounce land", base_string
    )
  }

  if (horizon_land) {
    base_string <- ifelse(base_string %in% horizon_land_list,
      "Horizon land", base_string
    )
  }


  # A laisser après snow qui permet de regrouper les snow land avec les basic au besoin
  if (basic_land) {
    base_string <- ifelse(base_string %in% basic_land_list,
      "Basic land", base_string
    )
  }

  return(base_string)
}
################################################################################

################################################################################
#######################  Not in function  ######################################
`%notin%` <- Negate(`%in%`)
################################################################################



################################################################################
####################### Ci functionusing agresti-coull #########################

# Réflechir a un merge entre les function selon les paramètres

CI_2_prop <- function(p1, p2, n1, n2, alpha = 0.95) {
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


  upper_bound <- (p1_tide - p2_tide) - z * sqrt(
    (p1_tide * (1 - p1_tide) / n1_tide) +
      (p2_tide * (1 - p2_tide) / n2_tide)
  )
  ###################################################

  # Gestion des cas ou le sample size est trop faible
  result <- ifelse(
    (n1 < 5 | n2 < 5),
    0,
    p1 - p2 - upper_bound
  )
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
                         CI, round_val = 2,
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


onfly_filter_js <- c( r"{
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
# Format table résult removing space and shorten name make some variables factor
format_df_result_card_table <- function(
    df_base_fun,
    colname_deck_list,
    df_Archetyp_fun,
    Based_Archetyp_fun = FALSE
){
  if(Based_Archetyp_fun){
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
        !!rlang::sym(paste0(colname_deck_list,"_CardName")) := as.factor(!!rlang::sym(paste0(colname_deck_list,"_CardName"))),
        !!rlang::sym(paste0(colname_deck_list,"_Count")) := as.factor(!!rlang::sym(paste0(colname_deck_list,"_Count")))
      ) 
  }else{
    df_temp_fun <-  df_base_fun %>%
      mutate(
        Archetype = factor(
          Archetype,
          levels = unique(df_Archetyp_fun$Archetype)
        ),
        !!rlang::sym(paste0(colname_deck_list,"_CardName")) := as.factor(!!rlang::sym(paste0(colname_deck_list,"_CardName"))),
        !!rlang::sym(paste0(colname_deck_list,"_Count")) := as.factor(!!rlang::sym(paste0(colname_deck_list,"_Count")))  
      ) 
  }
  
  df_res_fun <- df_temp_fun %>% 
    rename_with(
      ~str_replace(
        str_replace(
          str_replace(
            str_replace_all(.,"_"," "),
            "Mainboard","Main"),
          "Sideboard","Side"),
        "CardName","Card")
    )
  
  
  return(df_res_fun)
}



















# IZET mid = deck de nassif a voir pour parser

# Aggregation a regarder  

# [1] "Beans Cascade"                 "Simic Midrange _fallback"      "Eldrazi"                       "Grixis Midrange"              
# [5] "Jund Midrange"                 "Phoenix"                       "Discover Combo"                "Jund Aggro"                   
# [9] "Mono Blue Midrange"            "Free Spells"                   "Electro End"                   "Mono Red Aggro"               
# [13] "Izzet Aggro"                   "Boros Blink"                   "Zombies"                       "Green Devotion"               
# [17] "Grixis Kiki Jiki"              "Mono Green Midrange _fallback" "Azorius Midrange"              "Boros Midrange _fallback"     
# [21] "Bant Midrange _fallback"       "5 Color Midrange _fallback"    "Temur Midrange _fallback"      "Mono White Midrange _fallback"
# [25] "Enduring Ideal"                "Midrange _fallback"            "Ensoul"                        "Oops All Spells"              
# [29] "Crabvine"                      "Reclamation"                   "Neobrand"                      "WUBG Midrange _fallback"      
# [33] "Ascendancy Combo"              "Grixis Aggro"                  "Slivers"                       "Skelementals"                 
# [37] "Naya Midrange _fallback"       "Mono Blue Control _fallback"   "Rakdos Sacrifice"              "Tameshi Bloom"                
# [41] "WBRG Midrange _fallback"       "Hollow One"                    "Devoted Combo"                 "Spirits"                      
# [45] "Izzet Midrange _fallback"      "Soul Sisters"                  "Land Destruction"              "Glimpse Combo"                
# [49] "Manufactor Combo"              "Lantern"                       "Boros Aggro"                   "Enchantress"                  
# [53] "The Rack"                      "Calibrated Blast"              "Timeless Lotus"                "Infect"                       
# [57] "Elves"                         "Blink"                         "Kuldotha Aggro"                "Bogles"                       
# [61] "Faeries"                       "Taxes"                         "Goblins"                       "Belcher"                      
# [65] "Blue Scapeshift"               "Ad Nauseam"                    "Elementals"                    "Affinity"                     
# [69] "REdx Midrange"                 "Orzhov Midrange"               "Scapeshift"                       

################################################################################







Archetype_agreger <- function(Archetype_to_agreg,color_agreg = NULL){
  
  
  
  
  UW_controle_group <- c("Azorius Control _fallback",
                         "Bant Control _fallback","Jeskai Control _fallback",
                         "Taking Turns",
                         # soupe liée a dimir
                         "Esper Control _fallback","Esper Midrange _fallback","WUBG Control _fallback",
                         "WUBR Control _fallback","WURG Control _fallback",
                         "5 Color Control _fallback") 
  
  UB_controle_group <- c(
    
    "Dimir Control _fallback","Dimir Midrange _fallback",
    "Sultai Control _fallback","Sultai Midrange _fallback",
    
    "Grixis Control _fallback","Grixis Midrange _fallback",
    # ajout discutable 
    "Rogues"
  )
  
  
  UR_controle_group <- c("Izzet Control _fallback", "Temur Control _fallback")
  
  
  
  
  
  
  # Gestion des controles en classe large
  Archetype_to_agreg = ifelse(Archetype_to_agreg %in% UW_controle_group,
                              "UWhiteX Control",
                              ifelse(Archetype_to_agreg %in% UB_controle_group,
                                     "UBlackX Control",
                                     ifelse(Archetype_to_agreg %in% UR_controle_group,
                                            "URedX Control",Archetype_to_agreg
                                            
                                            
                                     )
                                     
                              )
                              
  )
  
  
  # Regroupement du split de creativity sur persist
  Archetype_to_agreg = ifelse(str_detect(Archetype_to_agreg,
                                         "Creativity"
  ),
  "Creativity",Archetype_to_agreg)
  
  # Blink except boros
  Archetype_to_agreg = ifelse(Archetype_to_agreg %in% 
                                c( "Azorius Blink","Bant Blink" ,"WURG Blink",
                                   "Jeskai Blink",  "Esper Blink"),
                              "Blink",Archetype_to_agreg)
  
  
  # Delver Gestion
  # Gestion du problème de l'absence des couleurs dans les archetypes des mathcups
  if (is.null(color_agreg[1])){
    Archetype_to_agreg = ifelse(Archetype_to_agreg =="Delver",
                                "URedX Control",Archetype_to_agreg)
  } else{
  Archetype_to_agreg = ifelse(Archetype_to_agreg =="Delver" & color_agreg == "UR",
                              "Murktide",Archetype_to_agreg)
  Archetype_to_agreg = ifelse(Archetype_to_agreg =="Delver" & color_agreg == "UBR",
                              "UBlackX Control",Archetype_to_agreg)
  
  }
  
  Archetype_to_agreg = ifelse(Archetype_to_agreg =="Breach Value",
                              "Murktide",Archetype_to_agreg)
  
  
  Archetype_to_agreg = ifelse(Archetype_to_agreg =="Grief Blade",
                              "Stoneblade",Archetype_to_agreg)
  # Pack rhinos
  Archetype_to_agreg = ifelse(Archetype_to_agreg =="Footfalls 4 C",
                              "Footfalls",Archetype_to_agreg)
  # Pack tron
  Archetype_to_agreg = ifelse(str_detect(Archetype_to_agreg,"Tron$"),
                              "Tron",Archetype_to_agreg)
  # Regroupement de rakdos midrange et scam
  Archetype_to_agreg = ifelse(Archetype_to_agreg %in% 
                                c("Rakdos Midrange _fallback",
                                  "Mardu Midrange _fallback"),
                              "Scam",Archetype_to_agreg)
  
  # Regroupement de mono B midrange et coffer
  Archetype_to_agreg = ifelse(Archetype_to_agreg =="Mono Black Midrange _fallback",
                              "Coffers Control",Archetype_to_agreg)
  # Gestion des 2 fallback qui n'en sont pas vraiment
  Archetype_to_agreg = ifelse(Archetype_to_agreg %in% c("Gruul Midrange _fallback",
                                                        "Mono Red Midrange _fallback",
                                                        "Mardu Midrange _fallback"
                                                        
  ),
  "REdx Midrange",Archetype_to_agreg)
  
  Archetype_to_agreg = ifelse(Archetype_to_agreg %in% c("Orzhov Midrange _fallback",
                                                        "Orzhov Blink","Mono White Blink",
                                                        "Abzan Blink"),
                              "Orzhov Midrange",Archetype_to_agreg)
  
  Archetype_to_agreg = ifelse(Archetype_to_agreg %in% c(
    "Golgari Midrange _fallback","Jund Midrange _fallback",
    "Abzan Midrange _fallback"),
    "Golgarix Midrange",Archetype_to_agreg)
  
  # Merge the two combo breach 
  Archetype_to_agreg = ifelse(Archetype_to_agreg %in% c(
    "Breach Storm","Grinding Breach"),
    "Breach combo",Archetype_to_agreg)
  
  # Disctuable merge goryo et reanimator
  Archetype_to_agreg = ifelse(Archetype_to_agreg =="Reanimator",
                              "Goryo Reanimator",Archetype_to_agreg)
  
  # Merge titan shift avec scapshift
  Archetype_to_agreg = ifelse(Archetype_to_agreg =="Titan Shift",
                              "Scapeshift",Archetype_to_agreg)
  
  # Merge tout les titan sauf titan shift
  Archetype_to_agreg = ifelse(str_detect(Archetype_to_agreg,"Titan$"),
                              "Amulet Titan",Archetype_to_agreg)
  
  #Merge les 2 version de gob 
  Archetype_to_agreg = ifelse(Archetype_to_agreg =="Goblin Whack",
                              "Goblins",Archetype_to_agreg)
  
  Archetype_to_agreg = ifelse(Archetype_to_agreg %in% c(
    "Grixis Storm","Boros Storm","Gifts Storm","Twiddle Storm"),
    "Storm",Archetype_to_agreg)
  
  
  
  return(Archetype_to_agreg)
}













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
    basic_land = FALSE
    ) {
  
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
  if (triome){
    
    base_string <- ifelse(base_string %in% Triome_land,
                          "Triome land", base_string
    )
    
  }
  if (filter_land){
    
    base_string <- ifelse(base_string %in% filter_land_list,
                          "Filtre land", base_string
    )
  }
  
  if (fast_land){
    
    base_string <- ifelse(base_string %in% fast_land_list,
                          "Fast land", base_string
    )
  
  }
  
  if (bounce_land){
    
    base_string <- ifelse(base_string %in% bounce_land_list,
                          "Bounce land", base_string
    )
  }
  
  if (horizon_land){
    
    base_string <- ifelse(base_string %in% horizon_land_list,
                          "Horizon land", base_string
    )
    
  }
   
  
  if (basic_land){
    
    base_string <- ifelse(base_string %in% basic_land_list,
                          "Basic land", base_string
    )
    
  }
  
  return(base_string)
    
}

`%notin%` <- Negate(`%in%`)

CI_2_prop <- function(p1,p2,n1,n2,alpha =0.95){
  Wins1 = round(p1*n1,0)
  Wins2 = round(p2*n2,0)
  # using agresti-coull CI
  z = qnorm((1 - alpha)/2)#qt((1 - alpha)/2,n-1)
  z_square = z^2
  n1_tide = n1 + z_square
  n2_tide = n2 + z_square
  
  x1_tide = Wins1 + 0.5 * z_square
  p1_tide = x1_tide/n1_tide   
  x2_tide = Wins2 + 0.5 * z_square
  p2_tide = x2_tide/n2_tide
  
  
  q1 <- (1-p1_tide)
  q2 <- (1-p2_tide)
  
  
  upper_bound <- (p1_tide - p2_tide) - z * sqrt(
    (p1_tide*(1-p1_tide)/n1_tide) +
      (p2_tide*(1-p2_tide)/n2_tide)
  )
  
  result <- ifelse(
    (n1 < 5 | n2 <5),
    0,
    # je retire le vai win rate identique a celui du tableaux afin de conserver seulement le CI
    p1 - p2 - upper_bound
    
  )
  return(
    result
  )
}





CI_prop <- function(p,n,alpha =0.95){
  # using agresti-coull CI
  Wins = round(p*n,0)
  
  z = qnorm((1 - alpha)/2)#qt((1 - alpha)/2,n-1)
  z_square = z^2
  n_tide = n + z_square
  x_tide = Wins + 0.5 * z_square
  p_tide = x_tide/n_tide
  upper_bound <- p_tide - z * sqrt(
    p_tide*(1-p_tide)/n_tide
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


winrate_1_data <- function(win,loose){win/(win + loose)}
winrate_2_data <- function(win_base,loose_base,win_diff,loose_diff){
  (
    win_base - win_diff
  )/(
    (win_base - win_diff) + 
      (loose_base - loose_diff) 
  )
}

Draw_diff_2_data <- function(
    win_base,draw_base,loose_base,
    win_diff,draw_diff,loose_diff){
  (
    draw_base/(
      win_base + loose_base + draw_base)
  ) -
    (
      (
        draw_diff - draw_base
      )/(
        (win_diff - win_base) + 
          (loose_diff - loose_base) +
          (draw_diff - draw_base)
      )
    )
}


formating_CI <- function(value,
                         CI,round_val = 2,
                         percent = TRUE,
                         limit = c(-Inf,Inf) # c(min,max)
                         ){


  low_bound <- ifelse(
    (value+CI) < limit[1],
    limit[1],
    (value+CI) 
    )
  
  upper_bound <- ifelse(
    (value-CI) > limit[2],
    limit[2],
    (value-CI) 
  )
  
  
  
  ifelse(CI == 0,
         paste0(
           "[No data]"
         ),
         paste0(
           # round(value * (100 * percent),round_val)," ",
           "[",
           round(low_bound * (100 * percent),round_val),
           ";",
           round(upper_bound * (100 * percent),round_val),
           "]"
           
         )
         )

  
}








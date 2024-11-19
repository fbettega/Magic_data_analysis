################################################################################
################################################################################
#######################  Intro of script 2  ####################################
## ---- Introduction_chunk_1_newcards

Introduction_char_vec_par_1_1 <-
  paste0(
  "This chapter focuses on the cards that have recently entered the format (the latest ",new_cards_number_of_month_for_new_set_1  ," months). The aim is to present the number of times they have been included in decks and their winrates. 
The file is split into 3 parts: \n\n  
- A first part aggregating all the cards whether they are maindeck or sideboard and whatever the archetypes.\n
- The second part is stratified by archetype and shows the presence and winrate of new cards when they are present in the main deck. \n
- The third part is stratified by archetype and shows the presence and winrate of new cards when they are present in the sideboard.\n
\n\n
For parts 2 and 3, the win rates of the cards are only described in situations with a number of wins and losses (excluding 5-0 leagues), but the presence of a card also includes 5-0 leagues.
\n"
)
pander::pandoc.p(Introduction_char_vec_par_1_1)
pander::pandoc.p("")
pander::pandoc.p("")


################################################################################
################################################################################
#######################  Intro of script 2  ####################################
## ---- Introduction_chunk_2_Deck_analysis

Introduction_char_vec_par_2_1 <-  paste0(
  "This chapter shows the representation of differences over time. Leagues are excluded from this analysis.\n\n
  File in 3 parts :\n
The first shows the presence curves over time for each archetype or archetype base. Archetypes with too low a presence are deactivated by default but can be reactivated by clicking on the desired decks. By default, certain Archetypes are hidden if their number is less than 0.25%.\n
Leagues are includes in this part\n\n
The second part shows the presence barchart of the different archetypes (archetype is define as other if their number is less than the minimum of 50 or 1%) and base archetypes for different time intervals: \n\n
- all data\n
- one moth\n
- two weeks\n
- one week\n
Additional information is available in tooltip (for Archetype and base archetype): \n\n
- Number of copies of the deck\n
- The delta in percent compared to the upper time interval\n
- Deck rank and its evolution compared to the previous time interval\n
- Win rates and confidence interval\n
The confidence interval graphs show the averages and 95% confidence intervals (calculated using the [Agresti-Coull method](https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Agresti%E2%80%93Coull_interval)). The vertical red line represents the mean of the winrates and the dotted blue lines represent the mean of the upper and lower bounds of the confidence interval.",
"Top player 10 average win rate and CI (A player need at least", top_n_player, " rounds for Archetype and",top_n_player/2," for base archetype) result are show above error bar",
"In particular, the publication of the top32 only for results from MTGO led to an overestimation of the winrates, the winrates were centred.\n\n
The last part present : \n
- The représentation of each colors combinations in the format\n
- The presence of different cards in the format.\n
Leagues are includes in this part\n\n
"
)
pander::pandoc.p(Introduction_char_vec_par_2_1)
pander::pandoc.p("")
pander::pandoc.p("")


################################################################################
################################################################################
#######################  Intro of script 3  ####################################
## ---- Introduction_chunk_3_Deck_analysis

Introduction_char_vec_par_3_1 <- "Presents the win rate of each card in each archetype in the form of multiple tables. \n
The definition of each column is given in a tooltip accessible by passing the cursor over the column names.\n
This file is split into 2 parts : the first concentrates on the aggregated maccro archetypes and the second presents the sub-archetypes. \n
Each part is organised in the same way, repeated 2 times, one for the maindeck and one for the sideboard.\n\n
- **Base cards**: These are the cards present in decks almost exclusively in a given number of copies (deck numbers without the most common count less than 10).\n
- **Side/Mainboard cards**: Cards present in variable numbers in the decks"

pander::pandoc.p(Introduction_char_vec_par_3_1)
pander::pandoc.p("")
pander::pandoc.p("")


################################################################################
################################################################################
#######################  Intro of script 4  ####################################
## ---- Introduction_chunk_4_Deck_analysis



Introduction_char_vec_par_4_1 <- paste0(
  "This chapter focuses on the data for which we know the result of each match and the Archetype of the opponent.\n
In order to be included, an archetype must be represented more than ", Archetype_cut_of_4, " times in the dataset.\n
- Matrix considers the matches as a whole (for example, a 2-1 score counts as 1 game won).\n
- 

Part one focus on Archetype (aggragated) and part two on base archetype (parser archetype).\n\n

They are built on the following model (additionnal information in tooltip) one tab for all data and one tab for 1 month data: \n\n
- Summary of data.\n\n
- Bar chart shows the presence of each archetype and base archetype, as well as their win rate and some additional information in tooltips.\n\n
- The confidence interval graphs show the averages winrates (without miror matchs) and 95% confidence intervals (calculated using the [Agresti-Coull method](https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Agresti%E2%80%93Coull_interval)). The vertical red line represents the mean of the winrates and the dotted blue lines represent the mean of the upper and lower bounds of the confidence interval.\n
- A complete matrix with all the information.\n
The third part explores the notion of the best deck according to a given metagame using the winrates obtained using the complete games obtained on the data set and the presence of each archetype over time.\n
In order to determine an expected number of victories 2 criteria are used the average winrate and the lower bounds of the confidence interval.**Please note that this part is still under construction as some decks with too few matchups are included**.\n"
)

pander::pandoc.p(Introduction_char_vec_par_4_1)
pander::pandoc.p("")
pander::pandoc.p("")


################################################################################
################################################################################
#######################  Intro of script 5  ####################################
## ---- Introduction_chunk_5_Deck_analysis

Introduction_char_vec_par_5_1 <- paste0(
  "This analysis attempts to use regression to determine the cards with the best performance inside archetype or base archetype.\n
A binomial regression is initially trained on a set of decks. In order to be included in this analysis the archetype must be present at least ", filter_archetype_count_5, " times in the dataset.\n
In order to be considered a card must be included at least ", min_sample_size_5, " times in either the main deck or the sideboards, one or the other being considered separately.
  In models comparing the number of copies of each card, when a number of copies is less than ", min_sample_size_5, " it is grouped with an adjacent number of copies. 
  For example, a card that is present 32 times in 1 copy 200 times in 2 copies, 15 times in 3 copies and 47 times in 4 copies would lead to the following result 1/2 : 232 and 3/4 : 62. 
  The formulation 2-4 indicates that the numbers of copies 2, 3 and 4 have been grouped together.\n
**Be careful, this part leads to results that I'm not really sure of. The interpretation of the regression coefficients seems really questionable, particularly in relation to the collinearity problem and the very large number of variables with sometimes small sample sizes. I would therefore encourage you to be very careful.**\n
"
)


Introduction_char_vec_par_5_2 <-
  paste0(
    "**Templates are created separately for the maindeck and the sideboard and maindeck and side board pull together (Total 75) according to the following scheme :**\n\n",
    "- **Base Cards** cards systematically present in decks with an almost fixed number of copies less than ", min_sample_size_5,
    " decks that do not have the most common number of copies. \n",
    " decks with zero copies are grouped with the majority class)  contained in the decks, for which the number of copies varies, quasibinomial regression models are created using the wins and losses of each deck : \n
\t- Comparing for each card presence *Most common count* vs absence *Other*\n
\t- Comparing each card count with a sufficient sample size *Most common count* vs *1* vs *3-4* for example\n
- **Uncommon Cards**, These cards are not always included in decks, quasibinomial regression models are created using the wins and losses of each deck : \n
\t- Comparing for each card presence *+1* vs absence *0*\n
\t- Comparing each card count with a sufficient sample size *0* vs *1* vs *3-4* for example.\n
\t- In an attempt to take account of potential collinearity between the variables, a penalised ridge regression model was also implemented for the counts of cards.\n    
    "

  )
pander::pandoc.p(Introduction_char_vec_par_5_1)
pander::pandoc.p("")
pander::pandoc.p(Introduction_char_vec_par_5_2)
pander::pandoc.p("")
pander::pandoc.p("")

################################################################################
#######################  Intro of script 6  #####################################
## ---- Introduction_chunk_6_best_deck



Introduction_char_vec_par_6_1 <- paste0("This analysis attempts to use regression to determine the decks with the best performance inside archetype or base archetype.\n
A binomial regression is initially trained on a set of decks.
In order to be included in this analysis the archetype must be present at least ", filter_archetype_count_6, " times in the dataset.\n
In order to be considered a card must be included at least ", min_sample_size_6, " times in either the main deck or the sideboards, one or the other being considered separately.
In models comparing the number of copies of each card, when a number of copies is less than ", min_sample_size_6, " it is grouped with an adjacent number of copies.
For example, a card that is present 32 times in 1 copy 200 times in 2 copies, 15 times in 3 copies and 47 times in 4 copies would lead to the following result 1/2 : 232 and 3/4 : 62.
The formulation 2-4 indicates that the numbers of copies 2, 3 and 4 have been grouped together. 
**Be careful, this part leads to results that I'm not really sure of. The interpretation of the regression coefficients seems really questionable, particularly in relation to the collinearity problem and the very large number of variables with sometimes small sample sizes. I would therefore encourage you to be very careful.**")


# see above for part 2
Introduction_char_vec_par_6_2 <- "**A total of 6 quasibinomial regression models are created using the wins and losses of each deck:**.\n\n
- Two models using the deck as a whole (maindeck and sideboard)\n
\t- Comparing for each card presence *+1* vs absence *0*.\n
\t- Comparing each card count with a sufficient sample size *0* vs *1* vs *3-4* for example\n
- Four separate models 2 for maindeck and 2 for sideboard\n
\t- Comparing for each card presence *+1* vs absence *0*\n
\t- Comparing each card count with a sufficient sample size *0* vs *1* vs *3-4* for example\n"


Introduction_char_vec_par_6_3 <- "These different models are then used to determine the 7 complete decks (maindeck and sideboard) with the highest probability of victory for each archetype.\n
As well as the 7 maindecks and 7 sideboards with the highest probability of victory are presented for each archetype.**Warning: this second part can lead to inconsistent combinations.** It seemed useful if you want explore the maindecks and sides separately.\n\n
Table shows the top7 decks: \n\n
- Firstly base cards (present in all decklist).\n  
- Variables cards are present as card name average number of cards[minimum; maximum number of cards]number of base cards* (if this card is also in base cards) \n\n  
"

pander::pandoc.p(Introduction_char_vec_par_6_1)
pander::pandoc.p("")
pander::pandoc.p(Introduction_char_vec_par_6_2)
pander::pandoc.p("")
pander::pandoc.p(Introduction_char_vec_par_6_3)
pander::pandoc.p("")
pander::pandoc.p("")



################################################################################
#######################  Intro of script 7  #####################################
## ---- Introduction_chunk_7_top8



Introduction_char_vec_par_7_1 <- paste0(
  "This chapter is divided by week over the last ", last_week_number_7, " weeks.
  For each week the different tournaments with more than", min_tournament_size_7, "players.",
  "-For each tournament, a bar graph shows the presence of each archetype and base archetype, as well as their win rate and some additional information in tooltips.\n
- A table shows the top8 decks, their basic archetype Archetype the player (which is a link to the decklist), and the decklist itself.\n"
)

pander::pandoc.p(Introduction_char_vec_par_7_1)
pander::pandoc.p("")
pander::pandoc.p("")

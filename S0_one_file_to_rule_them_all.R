run_command_from_dir <- function(cmd, dir_cmd) {
  # Current working directory
  cur <- getwd()
  # On exit, come back
  on.exit(setwd(cur))
  # Change directory
  setwd(dir_cmd)
  # Run the command
  system(cmd)
  # Return
  NULL
}

# conflicted::conflicts_prefer(dplyr::filter)
# library(git2r)
# conflicted::conflict_prefer("pull", "dplyr")
#
#  git2r::config( global = TRUE, user.name = "fbettega", user.email = "francois.bettega@gmail.com")
#
# a <- git2r::pull(repo = "MTGOArchetypeParser_20231227/MTGODecklistCache/")
#
# a <- git2r::pull(repo = "MTGOArchetypeParser_20231227/MTGOFormatData/")

tictoc::tic("total")
run_command_from_dir("./MTGOArchetypeParser.App.exe json detect", "MTGOArchetypeParser_20231227/")

# run_command_from_dir("git pull","MTGOArchetypeParser_20231227/MTGODecklistCache/")

# replace by ml predictions
# source("S1_Json_to_Curated_data.R", local = calledProgram <- new.env(), echo = TRUE)
source("S3_Archetype_classif_models.R", local = calledProgram <- new.env(), echo = TRUE)

rm(calledProgram)
source("S4_predict_arch_with_ML.R", local = calledProgram <- new.env(), echo = TRUE)
rm(calledProgram)

fichiers_rmd <- c(
  "1_new_card.Rmd", "2_presence_archetype.Rmd",
  "3_Card_win_rate_table.Rmd", "4_matrix_WR.Rmd",
  "5_Deck_analysis.Rmd","6_best_deck.Rmd"
)


rmarkdown::render(
  input = "index.Rmd",
  envir = new.env(),
  output_dir = "outpout/"
)


for (fichier in fichiers_rmd) {
  tictoc::tic(paste0(fichier))
  rmarkdown::render(
    input = fichier,
    envir = new.env(),
    output_dir = "outpout/result"
  )
  tictoc::toc()
}
tictoc::toc()

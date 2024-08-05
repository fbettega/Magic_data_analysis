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

tictoc::tic("total")
# conflicted::conflicts_prefer(dplyr::filter)
# library(git2r)
# conflicted::conflict_prefer("pull", "dplyr")
git2r::config(
  global = TRUE, 
  user.name = "fbettega",
  user.email = "francois.bettega@gmail.com",
  core.longpaths = "TRUE"
  )
# deck_list_repo <- git2r::clone(
#   "https://github.com/Badaro/MTGODecklistCache.git",
#   "ArchetypeParser/MTGODecklistCache/"
#   )


deck_list_repo <- git2r::repository("ArchetypeParser/MTGODecklistCache/")


pull_deck_list_repo <- git2r::pull(repo = deck_list_repo)



run_command_from_dir(
  "./MTGOArchetypeParser.App.exe json detect",
  "ArchetypeParser/"
  )

# run_command_from_dir("git pull","MTGOArchetypeParser_20231227/MTGODecklistCache/")

# replace by ml predictions
# source("S1_Json_to_Curated_data.R", local = calledProgram <- new.env(), echo = TRUE)




calledProgram <- new.env()

sys.source(
  "S3_Archetype_classif_models.R", 
  envir = calledProgram,
  toplevel.env = calledProgram
  )

rm(calledProgram)


calledProgram <- new.env()

sys.source(
  "S4_predict_arch_with_ML.R",
  envir = calledProgram,
  # keep.source = FALSE,
  toplevel.env = calledProgram
)
rm(calledProgram)


if(
  length(
    list.files(path = "data/",
               pattern = "^archidekt-collection-export-.*\\.csv")
    ) == 1){
withr::with_envvar(new = c("QUARTO_PROFILE" = "collection_tracking"), {
  quarto::quarto_render("rmd_files/", output_format = "html")
})

} else{
  quarto::quarto_render("rmd_files/", output_format = "html")
}


tictoc::toc()

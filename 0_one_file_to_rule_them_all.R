
run_command_from_dir <- function(cmd,dir_cmd){
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


git2r::config( global = TRUE, user.name = "fbettega", user.email = "francois.bettega@gmail.com")


git2r::pull(repo = "MTGOArchetypeParser_20231227/MTGODecklistCache/")


run_command_from_dir("./MTGOArchetypeParser.App.exe json detect","MTGOArchetypeParser_20231227/")



# run_command_from_dir("git pull","MTGOArchetypeParser_20231227/MTGODecklistCache/")





source("1_Json_to_Curated_data.R", local = calledProgram <- new.env(), echo=TRUE)
rm(calledProgram)

fichiers_rmd <- c(
  "2_new_card.Rmd", "3_presence_archetype.Rmd", 
  "4_Card_win_rate_table.Rmd","5_matrix_WR.Rmd"
                  )



for (fichier in fichiers_rmd) {
  rmarkdown::render(input = fichier,
                    envir =  new.env(),
                    output_dir = "outpout/")
}

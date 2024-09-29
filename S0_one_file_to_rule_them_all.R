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

# deck_list_format <- git2r::clone(
#   "https://github.com/Badaro/MTGOFormatData.git",
#   "ArchetypeParser/MTGOFormatData/"
#   )


# au moment du switch pour multiple format penser a créer un fichier format : start_date

# rajouter un fichier de qmd de debug associé au collection tracker avec les combinaisons base archetype / archetype et les cartes 



deck_list_repo <- git2r::repository("ArchetypeParser/MTGODecklistCache/")
deck_list_format <- git2r::repository("ArchetypeParser/MTGOFormatData/")

# Sys.sleep(7200)
pull_deck_list_repo <- git2r::pull(repo = deck_list_repo)
pull_format_repo <- git2r::pull(repo = deck_list_format)

calledProgram <- new.env()
sys.source(
  "sources/json_modifier.R",
  envir = calledProgram,
  toplevel.env = calledProgram
)



run_command_from_dir(
  "./MTGOArchetypeParser.App.exe json detect",
  "ArchetypeParser/"
)

calledProgram <- new.env()

sys.source(
  "sources/S3_Archetype_classif_models.R",
  envir = calledProgram,
  toplevel.env = calledProgram
)




calledProgram <- new.env()

sys.source(
  "sources/S4_predict_arch_with_ML.R",
  envir = calledProgram,
  # keep.source = FALSE,
  toplevel.env = calledProgram
)
rm(calledProgram)


# quarto::quarto_render("rmd_files/", output_format = "html", as_job = FALSE)

if(
  length(
    list.files(path = "data/mtg_data/",
               pattern = "^archidekt-collection-export-.*\\.csv")
  ) == 1) {
    quarto::quarto_render("rmd_files/", 
                          output_format = "html", 
                          profile = "fb",# c("fb","basic"),
                          as_job = FALSE)

} 

quarto::quarto_render("rmd_files/",
                          output_format = "html", 
                          profile ="basic",
                          as_job = FALSE)



if(file.exists("ssh_key/id_rsa")){
session <- ssh::ssh_connect("francois@176.31.183.129",keyfile = "ssh_key/id_rsa")
ssh::scp_upload(session,files = list.files("outpout/",full.names = TRUE),to = "/home/francois/docker/magic/magic_modern_meta/data/")
ssh::ssh_disconnect(session)
}

tictoc::toc()

system("shutdown -s")


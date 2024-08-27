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


deck_list_repo <- git2r::repository("ArchetypeParser/MTGODecklistCache/")

# Sys.sleep(7200)
pull_deck_list_repo <- git2r::pull(repo = deck_list_repo)



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

rm(calledProgram)


calledProgram <- new.env()

sys.source(
  "sources/S4_predict_arch_with_ML.R",
  envir = calledProgram,
  # keep.source = FALSE,
  toplevel.env = calledProgram
)
rm(calledProgram)


quarto::quarto_render("rmd_files/", output_format = "html", as_job = FALSE)


# quarto::quarto_render("rmd_files/2_presence_archetype.qmd", output_format = "html")




if(file.exists("ssh_key/id_rsa")){
session <- ssh::ssh_connect("francois@176.31.183.129",keyfile = "ssh_key/id_rsa")
ssh::scp_upload(session,files = list.files("outpout/",full.names = TRUE),to = "/home/francois/docker/magic/magic_modern_meta/data/")
ssh::ssh_disconnect(session)
}
system("shutdown -s")

tictoc::toc()
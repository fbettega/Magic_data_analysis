library(yaml)
source("sources/S0_source_init.R")
Reupdate_scryfall_db <- FALSE #TRUE FALSE
# suppressPackageStartupMessages()
reparse_arch <- TRUE  #TRUE FALSE
not_all_arch_filter_S0 <- c(
  # "Modern",
  # "Legacy",
  # "Pauper",
  # "Pioneer",
  # "Vintage",
  "All"
)
# Try catch because script is long allow shutdown if error
tryCatch(
{

format_date_en_cours_fulltable <- dplyr::filter(
  read.csv("other_file/format_handle.csv"),
  !format_param %in% not_all_arch_filter_S0)


log_df <- read.csv("other_file/log_run.csv")


tictoc::tic("total")


# install.packages if some missing
sys.source(
  "sources/S3_install_packages.R",
  envir = define_env(NULL),
  toplevel.env = define_env(NULL)
)


# git2r::config(
#   global = TRUE,
#   user.name = "fbettega",
#   user.email = "francois.bettega@gmail.com",
#   core.longpaths = "TRUE"
# )

# deck_list_repo <- git2r::clone(
#   "https://github.com/Badaro/MTGODecklistCache.git",
#   "ArchetypeParser/MTGODecklistCache_B/"
#   )

# deck_list_format <- git2r::clone(
#   "https://github.com/Badaro/MTGOFormatData.git",
#   "ArchetypeParser/MTGOFormatData/"
#   )

# # jillac fork with more data to test
# deck_list_repo <- git2r::clone(
#   "https://github.com/Jiliac/MTGODecklistCache.git",
#   "ArchetypeParser/MTGODecklistCache_J/"
#   )

################################################################################
# test automatise github
# deck_list_repo_base <-  git2r::repository("ArchetypeParser/MTGODecklistCache_B/")
path_decklist_repo <- "ArchetypeParser/MTGODecklistCache/"
jiliac_decklist_repo <- "ArchetypeParser/MTGODecklistCache_J/"
Format_path_repo <- "ArchetypeParser/MTGOFormatData/"
deck_list_repo <- git2r::repository(jiliac_decklist_repo)
deck_list_format <- git2r::repository(Format_path_repo)


# Sys.sleep(7200)
# pull_deck_list_repo_base <- git2r::pull(repo = deck_list_repo_base)
pull_deck_list_repo <- git2r::pull(repo = deck_list_repo)
pull_format_repo <- git2r::pull(repo = deck_list_format)

# system('git config --global user.email "francois.bettega@gmail.com"', intern = TRUE)
# system('git config --global user.name "fbettega"', intern = TRUE)
system(paste("cmd /c \"cd /d", path_decklist_repo, "&& git fetch jiliac\""), intern = TRUE)
system(paste("cmd /c \"cd /d", path_decklist_repo, "&& git  merge -X theirs jiliac/master --no-edit\""), intern = TRUE)
system(paste("cmd /c \"cd /d", path_decklist_repo, "&& git pull --no-edit\""), intern = TRUE)

tictoc::tic("Scryfall update data")
 

scryfall_update <- update_scryfall_data(
  "../scry_fall_to_csv/",
  reupdate = Reupdate_scryfall_db
)




tictoc::toc(log = TRUE, quiet = TRUE)

log_df <- log_df_fun(
  log_df_fun = log_df,
  format_fun = "Global",
  tictoc_res = paste0(tictoc::tic.log(format = TRUE))
)

tictoc::tic.clearlog()



for (i in 1:nrow(format_date_en_cours_fulltable)) {
  format_date_en_cours <- format_date_en_cours_fulltable[i, ]
  print(format_date_en_cours$format_param)
  readr::write_rds(
    format_date_en_cours, 
    "data/intermediate_result/temp_format_encours_for_param.rds"
  )

  tictoc::tic(paste0(format_date_en_cours$format_param))


  sys.source(
    "sources/prediction_arch/P0_json_modifier.R",
    envir = define_env(format_date_en_cours),
    toplevel.env = define_env(format_date_en_cours)
  )


  if (!dir.exists(paste0("outpout/", format_date_en_cours$format_param))) {
    dir.create(
      paste0("outpout/", format_date_en_cours$format_param),
      recursive = TRUE
      )
  }
  if (!dir.exists(paste0("outpout/", format_date_en_cours$format_param,"/debug"))) {
    dir.create(
      paste0("outpout/", format_date_en_cours$format_param,"/debug"),
      recursive = TRUE
    )
  }
  
  
  eddit_yaml(format_date_en_cours)


  tictoc::tic(paste0("Archetype parser : ", format_date_en_cours$format_param))
  run_command_from_dir(
    "./MTGOArchetypeParser.App.exe json detect",
    "ArchetypeParser/"
  )
  
  
  file.rename(from = paste0("ArchetypeParser/",format_date_en_cours$format_param,"_","data.json"),
              to = paste0("data/intermediate_result/parser_outpout/",format_date_en_cours$format_param,"_","data.json")
              )
  
  

  tictoc::toc(log = TRUE, quiet = TRUE)
  
  
  
  log_df <- log_df_fun(
    log_df_fun = log_df,
    format_fun = format_date_en_cours$format_param,
    tictoc_res = paste0(tictoc::tic.log(format = TRUE))
  )
  tictoc::tic.clearlog()

if(reparse_arch){
  tictoc::tic(paste0("Training archetype : ", format_date_en_cours$format_param))

  sys.source(
    "sources/prediction_arch/P1_Archetype_classif_models.R",
    envir = define_env(format_date_en_cours),
    toplevel.env = define_env(format_date_en_cours)
  )
  tictoc::toc(log = TRUE, quiet = TRUE)
  log_df <- log_df_fun(
    log_df_fun = log_df,
    format_fun = format_date_en_cours$format_param,
    tictoc_res = paste0(tictoc::tic.log(format = TRUE))
  )
  
  tictoc::tic.clearlog()

  tictoc::tic(paste0("Prediction archetype : ", format_date_en_cours$format_param))
  # calledProgram <- define_env(format_date_en_cours)
  sys.source(
    "sources/prediction_arch/P2_predict_arch_with_ML.R",
    envir = define_env(format_date_en_cours),
    # keep.source = FALSE,
    toplevel.env = define_env(format_date_en_cours)
  )
  # rm(calledProgram)
  tictoc::toc(log = TRUE, quiet = TRUE)
  log_df <- log_df_fun(
    log_df_fun = log_df,
    format_fun = format_date_en_cours$format_param,
    tictoc_res = paste0(tictoc::tic.log(format = TRUE))
  )
  tictoc::tic.clearlog()


  tictoc::tic(paste0("Proximity aggregation : ", format_date_en_cours$format_param))
  # calledProgram <- define_env(format_date_en_cours)
  sys.source(
    "sources/prediction_arch/P3_proximity_classification.R",
    envir = define_env(format_date_en_cours),
    # keep.source = FALSE,
    toplevel.env = define_env(format_date_en_cours)
  )
  # rm(calledProgram)
  tictoc::toc(log = TRUE, quiet = TRUE)
  log_df <- log_df_fun(
    log_df_fun = log_df,
    format_fun = format_date_en_cours$format_param,
    tictoc_res = paste0(tictoc::tic.log(format = TRUE))
  )
  tictoc::tic.clearlog()
}  
  
  # quarto::quarto_render("rmd_files/", output_format = "html", as_job = FALSE)
  

  if(
    length(
      list.files(
        path = "data/mtg_data/",
        pattern = "^archidekt-collection-export-.*\\.csv"
      )
    ) == 1) {
    quarto::quarto_render(
      "rmd_files/",
      output_format = "html",
      profile = "fb", # c("fb","basic"),
      as_job = FALSE
    )
  }
  
  

  quarto::quarto_render(
    "rmd_files/",
    output_format = "html",
    profile = "basic",
    as_job = FALSE
  )

  unlink(
    c(
      "rmd_files/_quarto-basic.yml",
      "rmd_files/_quarto.yml",
      "rmd_files/_quarto-fb.yml"
    )
  )
  tictoc::toc(log = TRUE, quiet = TRUE)
  log_df <- log_df_fun(
    log_df_fun = log_df,
    format_fun = format_date_en_cours$format_param,
    tictoc_res = paste0(tictoc::tic.log(format = TRUE))
  )
  
  tictoc::tic.clearlog()
}


rmarkdown::render(
  input = "sources/index.Rmd",
  envir = new.env(),
  output_dir = "outpout/"
)

rmarkdown::render(
  input = "sources/README.rmd",
  envir = new.env(),
  output_dir = "."
)



if (file.exists("other_file/ssh_key/id_rsa")) {
  session <- ssh::ssh_connect(
    "francois@51.159.194.184",
    keyfile = "other_file/ssh_key/id_rsa"
    )
  
  ssh::scp_upload(
    session, files = list.files("outpout/", full.names = TRUE),
    to = "/home/francois/docker/magic/magic_modern_meta/data/"
    )
  ssh::ssh_disconnect(session)
}

tictoc::toc(log = TRUE, quiet = TRUE)
log_df <- log_df_fun(
  log_df_fun = log_df,
  format_fun = format_date_en_cours$format_param,
  tictoc_res = paste0(tictoc::tic.log(format = TRUE))
)

tictoc::tic.clearlog()
# cancel shutdown use : system("shutdown -a")
system("shutdown /s /t 30")
},
error = function(e) {
  error_message <- capture.output(print(e))
  error_message <- paste("Une erreur s'est produite :",
                         paste(
                           error_message,
                               collapse = "\n"))
  
  # Écrire l'erreur dans le fichier
  cat(error_message, file = "outpout/erreur_log.txt")
  
  
  # cancel shutdown use : system("shutdown -a")
  print(paste0("shutdown cause of error"))
  system("shutdown /s /t 60")
}
)





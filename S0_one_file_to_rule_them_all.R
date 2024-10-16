library(yaml)
source("sources/S0_source_init.R")




format_date_en_cours_fulltable <- data.frame(
  format_param = c(
    "Modern",
    "Legacy",
    "Pauper",
    "Pioneer",
    # "Standard",
    "Vintage"
  ),
  date_cutoff = c(
    "2024-08-26",
    "2024-08-26",
    "2024-05-13",
    "2024-08-26",
    # "2024-07-30",
    "2024-08-26"
  )
)




log_df <- data.frame(
  date_run = character(),
  format = character(),
  log_txt = character()
)#read.csv("other_file/log_run.csv")

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


# log_df <- data.frame(
#   date_run = as.character(as.Date(Sys.time())),
#   format = format_date_en_cours$format_param,
#   log.txt = tictoc::tic.log(format = TRUE)
# )

# i <- 1





for (i in 1:nrow(format_date_en_cours_fulltable)) {
  format_date_en_cours <- format_date_en_cours_fulltable[i, ]
  print(format_date_en_cours$format_param)
  readr::write_rds(
    format_date_en_cours, 
    "data/intermediate_result/temp_format_encours_for_param.rds"
  )
  
  
  
  tictoc::tic(paste0(format_date_en_cours$format_param))


  sys.source(
    "sources/json_modifier.R",
    envir = define_env(format_date_en_cours),
    toplevel.env = define_env(format_date_en_cours)
  )


  if (!dir.exists(paste0("outpout/", format_date_en_cours$format_param))) {
    dir.create(
      paste0("outpout/", format_date_en_cours$format_param),
      recursive = TRUE
      )
  }
  
  eddit_yaml(format_date_en_cours)


  tictoc::tic(paste0("Archetype parser : ", format_date_en_cours$format_param))
  run_command_from_dir(
    "./MTGOArchetypeParser.App.exe json detect",
    "ArchetypeParser/"
  )

  tictoc::toc(log = TRUE, quiet = TRUE)
  
  log_df <- log_df_fun(
    log_df_fun = log_df,
    format_fun = format_date_en_cours$format_param,
    tictoc_res = paste0(tictoc::tic.log(format = TRUE))
  )
    
  tictoc::tic.clearlog()



  tictoc::tic(paste0("Archetype parser : ", format_date_en_cours$format_param))

  sys.source(
    "sources/S3_Archetype_classif_models.R",
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

  tictoc::tic(paste0("Archetype parser : ", format_date_en_cours$format_param))
  # calledProgram <- define_env(format_date_en_cours)
  sys.source(
    "sources/S4_predict_arch_with_ML.R",
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


  # quarto::quarto_render("rmd_files/", output_format = "html", as_job = FALSE)
  

  if (
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

  # # debug purpose
  quarto::quarto_render(
    "rmd_files/7_last_weeks_winners.qmd",
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





if (file.exists("ssh_key/id_rsa")) {
  session <- ssh::ssh_connect(
    "francois@176.31.183.129", 
    keyfile = "ssh_key/id_rsa"
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
# system("shutdown -s")

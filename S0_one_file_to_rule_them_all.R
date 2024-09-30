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

define_env <- function(
    df_format_date
){
  env_fun <- new.env()
  env_fun$format_param <- df_format_date$format_param
  env_fun$date_cut <- df_format_date$date_cutoff
  return(env_fun)
}

library(yaml)

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

format_date_en_cours_fulltable <- data.frame(
  format_param = c(
    "Modern",
    "Legacy",
    "Pauper",
    "Pioneer"#,
    # "Standard",
    # "Vintage"
                   ),
  date_cutoff = c(
    "2024-08-26",
    "2024-08-26",
    "2024-05-13",
    "2024-08-26"#,
    # "2024-07-30",
    # "2024-08-26",
    
    )
)

# i <- 4


for(i in 1:nrow(format_date_en_cours_fulltable)){
  
  format_date_en_cours <- format_date_en_cours_fulltable[i,]

print(format_date_en_cours$format_param)
tictoc::tic(paste0(format_date_en_cours$format_param))

calledProgram <- define_env(format_date_en_cours)
sys.source(
  "sources/json_modifier.R",
  envir = calledProgram,
  toplevel.env = calledProgram
)







if (!dir.exists(paste0("outpout/",format_date_en_cours$format_param))) {
  dir.create(paste0("outpout/",format_date_en_cours$format_param), recursive = TRUE)
}


quarto_fun_basic <- read_yaml("other_file/_quarto-basic.yml")
quarto_fun_basic$project$`output-dir` <- paste0(quarto_fun_basic$project$`output-dir`,"/",format_date_en_cours$format_param)
quarto_fun_basic$book$chapters[[2]]$chapters <- as.list(quarto_fun_basic$book$chapters[[2]]$chapters)

quarto_fun_fb <- read_yaml("other_file/_quarto-fb.yml")
quarto_fun_fb$project$`output-dir` <- paste0(quarto_fun_fb$project$`output-dir`,"/",format_date_en_cours$format_param)



quarto_fun <- read_yaml("other_file/_quarto.yml")
quarto_fun$book$title <- paste0(format_date_en_cours$format_param," ",quarto_fun$book$title)
# quarto_fun$params$date_cut <- format_date_en_cours$date_cutoff
# quarto_fun$params$format_param <-   format_date_en_cours$format_param


write_yaml(quarto_fun_basic, "rmd_files/_quarto-basic.yml", fileEncoding = "UTF-8",
           handlers = list(
  logical = function(x) {
    result <- ifelse(x, "true", "false")
    class(result) <- "verbatim"
    return(result)
  }))
write_yaml(quarto_fun_fb, "rmd_files/_quarto-fb.yml", fileEncoding = "UTF-8",
           handlers = list(
             logical = function(x) {
               result <- ifelse(x, "true", "false")
               class(result) <- "verbatim"
               return(result)
             }))
write_yaml(quarto_fun, "rmd_files/_quarto.yml", fileEncoding = "UTF-8",
           handlers = list(
             logical = function(x) {
               result <- ifelse(x, "true", "false")
               class(result) <- "verbatim"
               return(result)
             }))


run_command_from_dir(
  "./MTGOArchetypeParser.App.exe json detect",
  "ArchetypeParser/"
)


calledProgram <- define_env(format_date_en_cours)
sys.source(
  "sources/S3_Archetype_classif_models.R",
  envir = calledProgram,
  toplevel.env = calledProgram
)


calledProgram <- define_env(format_date_en_cours)

sys.source(
  "sources/S4_predict_arch_with_ML.R",
  envir = calledProgram,
  # keep.source = FALSE,
  toplevel.env = calledProgram
)
rm(calledProgram)

# quarto::quarto_render("rmd_files/", output_format = "html", as_job = FALSE)
write_rds(format_date_en_cours,"data/intermediate_result/temp_format_encours_for_param.rds")

if(
  length(
    list.files(path = "data/mtg_data/",
               pattern = "^archidekt-collection-export-.*\\.csv")
  ) == 1) {
    quarto::quarto_render(
      "rmd_files/", 
      output_format = "html", 
      profile = "fb",# c("fb","basic"),
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
tictoc::toc()


}
tictoc::toc()

if(file.exists("ssh_key/id_rsa")){
  session <- ssh::ssh_connect("francois@176.31.183.129",keyfile = "ssh_key/id_rsa")
  ssh::scp_upload(session,files = list.files("outpout/",full.names = TRUE),to = "/home/francois/docker/magic/magic_modern_meta/data/")
  ssh::ssh_disconnect(session)
}

system("shutdown -s")


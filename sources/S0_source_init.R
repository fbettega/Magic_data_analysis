################################################################################
# Function to uptdate scryfall data
update_scryfall_data <- function(
    path  ,
    reupdate = FALSE
){
  
  
  if(dir.exists(path)&reupdate){
    res <- "scryfall update"
    print(res)
    sys.source(
      paste0(path,"genrating_data.R"),
      chdir = TRUE,
      envir = define_env(df_format_date = NULL),
      toplevel.env = define_env(df_format_date = NULL)
    )
    
  } else {
    res <- "No scryfall update"
    print(res)
  }
  
  return(res)
}



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


################################################################################

define_env <- function(
    df_format_date) {
  env_fun <- new.env()
  
  if(!is.null(df_format_date)){
  env_fun$format_param <- df_format_date$format_param
  env_fun$date_cut <- df_format_date$date_cutoff
  }
  
  
  return(env_fun)
}

################################################################################

log_df_fun <- function(
    log_df_fun,
    format_fun,
    tictoc_res,
    path = "other_file/log_run.csv"
){
  log_df_fun_en_cours <- rbind(
    log_df_fun,
    data.frame(
      date_run = as.character(
        Sys.time()
        ),
      format = format_fun,
      log_txt = tictoc_res
    )
  )
  
  
  write.csv(log_df_fun_en_cours,path , row.names = FALSE)
  
  return(log_df_fun_en_cours)
}


################################################################################

eddit_yaml <- function(format_date_en_cours_fun){
  quarto_fun_basic <- read_yaml("other_file/_quarto-basic.yml")
  quarto_fun_basic$project$`output-dir` <- paste0(
    quarto_fun_basic$project$`output-dir`,
    "/", format_date_en_cours_fun$format_param
  )
  quarto_fun_basic$book$chapters[[5]]$chapters <- as.list(
    quarto_fun_basic$book$chapters[[5]]$chapters
  )
  
  quarto_fun_fb <- read_yaml("other_file/_quarto-fb.yml")
  quarto_fun_fb$project$`output-dir` <- paste0(
    quarto_fun_fb$project$`output-dir`,
    "/", format_date_en_cours_fun$format_param,"/debug"
  )
  
  
  
  quarto_fun <- read_yaml("other_file/_quarto.yml")
  quarto_fun$book$title <- paste0(
    format_date_en_cours_fun$format_param,
    " ", quarto_fun$book$title
  )
  # quarto_fun$params$date_cut <- format_date_en_cours_fun$date_cutoff
  # quarto_fun$params$format_param <-   format_date_en_cours_fun$format_param
  
  
  write_yaml(quarto_fun_basic, "rmd_files/_quarto-basic.yml",
             fileEncoding = "UTF-8",
             handlers = list(
               logical = function(x) {
                 result <- ifelse(x, "true", "false")
                 class(result) <- "verbatim"
                 return(result)
               }
             )
  )
  write_yaml(quarto_fun_fb, "rmd_files/_quarto-fb.yml",
             fileEncoding = "UTF-8",
             handlers = list(
               logical = function(x) {
                 result <- ifelse(x, "true", "false")
                 class(result) <- "verbatim"
                 return(result)
               }
             )
  )
  write_yaml(quarto_fun, "rmd_files/_quarto.yml",
             fileEncoding = "UTF-8",
             handlers = list(
               logical = function(x) {
                 result <- ifelse(x, "true", "false")
                 class(result) <- "verbatim"
                 return(result)
               }
             )
  )
  return(NULL)
}

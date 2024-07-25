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



fichiers_rmd <- c(
  "rmd_files/1_new_card.Rmd",
  "rmd_files/2_presence_archetype.Rmd",
  "rmd_files/3_Card_win_rate_table.Rmd",
  "rmd_files/4_matrix_WR.Rmd",
  "rmd_files/5_Deck_analysis.Rmd",
   "rmd_files/6_best_deck.Rmd",
  "rmd_files/7_last_weeks_winners.Rmd"
)


rmarkdown::render(
  input = "rmd_files/index.Rmd",
  envir = new.env(),
  output_dir = "outpout/"
)


# rmarkdown::render(
#   input = "6_best_deck.Rmd",
#   envir = new.env(),
#   output_dir = "outpout/result"
# )


# rmarkdown::render('6_best_deck.knit.md')

# "C:/PROGRA~1/Pandoc/pandoc" +RTS -K512m -RTS 6_best_deck.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output pandoc625041177aee.html --lua-filter "C:\Users\Francois\AppData\Local\R\win-library\4.4\rmarkdown\rmarkdown\lua\pagebreak.lua" --lua-filter "C:\Users\Francois\AppData\Local\R\win-library\4.4\rmarkdown\rmarkdown\lua\latex-div.lua" --embed-resources --standalone --variable bs3=TRUE --section-divs --table-of-contents --toc-depth 3 --variable toc_float=1 --variable toc_selectors=h1,h2,h3 --variable toc_collapsed=1 --variable toc_smooth_scroll=1 --variable toc_print=1 --template "C:\Users\Francois\AppData\Local\R\win-library\4.4\rmarkdown\rmd\h\default.html" --no-highlight --variable highlightjs=1 --number-sections --variable theme=united --mathjax --variable "mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML" --include-in-header "C:\Users\Francois\AppData\Local\Temp\RtmpszKuiE\rmarkdown-str62506aa4145.html" 
# pandoc.exe: osCommitMemory: VirtualAlloc MEM_COMMIT failed to commit 12582912 bytes of memory  (error code: 1455): The paging file is too small for this operation to complete.
# Error: pandoc document conversion failed with error 251




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

library(rjson)
library(tidyverse)

# format_param <- format_date_en_cours$format_param
# date_cut <- format_date_en_cours$date_cutoff


unlink(
 c(
   paste0("ArchetypeParser/MTGOFormatData_FB/Formats/",format_param,"_FB"),
   paste0("ArchetypeParser/MTGOFormatData_FB/Formats/",format_param)
   ),
 recursive = TRUE)

if (!dir.exists("ArchetypeParser/MTGOFormatData_FB/")) {
  dir.create("ArchetypeParser/MTGOFormatData_FB/", recursive = TRUE)
}




file.copy(
  "ArchetypeParser/MTGOFormatData/Formats",
  "ArchetypeParser/MTGOFormatData_FB/",
  overwrite = TRUE,
  recursive = TRUE
)

# unlink("ArchetypeParser/MTGOFormatData_FB", recursive = TRUE)

unlink("ArchetypeParser/settings.json")


settings_parser <- fromJSON(
  file = "other_file/settings_base.json",
  simplify = FALSE
)
# format_param <- format_date_en_cours$format_param
# date_cut <- format_date_en_cours$date_cutoff

# keep based data to have a reference format and suffix data without color with FB
if(format_param != "Duel_commander"){
  
settings_parser$Format <- paste0(format_param,"_FB")
settings_parser$ReferenceFormat <- format_param
}

# if periode of interst greater than 6 month -> periode of interest 
# if not last 6 month
min_date <- min(
  as.Date(Sys.time()) %m-% months(6), 
  as.Date(date_cut,tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))
  )
settings_parser$startdate <- as.character(min_date)


settings_parser$outputfile <- paste0(format_param, "_", settings_parser$outputfile)


if(format_param != "Duel_commander"){
  settings_parser$Filter[[1]] <- format_param
}else{
  settings_parser$Filter[[1]] <- ""
}


write(toJSON(settings_parser, indent = 2), "ArchetypeParser/settings.json")



# x <-   list.files(
#   paste0( "ArchetypeParser/MTGOFormatData_FB/Formats/",format_param,"/Archetypes/"),
#   full.names = TRUE
# )[5]






# create a copy of format in order to have ref and modify
if (!dir.exists(
  paste0("ArchetypeParser/MTGOFormatData_FB/Formats/",format_param,"_FB/")
  )
  ) {
  dir.create(paste0("ArchetypeParser/MTGOFormatData_FB/Formats/",format_param,"_FB/"), recursive = TRUE)
}
file.copy(
  list.files(paste0("ArchetypeParser/MTGOFormatData_FB/Formats/",format_param),
             full.names = TRUE),
  paste0("ArchetypeParser/MTGOFormatData_FB/Formats/",format_param,"_FB/"),
  overwrite = TRUE,
  recursive = TRUE
)







# Remove color in name and change name using file name and add initial name to variant
modify_archetype_res <- lapply(
  list.files(
    paste0("ArchetypeParser/MTGOFormatData_FB/Formats/", format_param, "_FB/Archetypes/"),
    full.names = TRUE
  ), function(x) {
    # print(x)
# Try catch to handle poor coma making json invalid
    base_parsing_res <- tryCatch(
      fromJSON(
        file = x,
        simplify = FALSE
      ),
      error = function(e) {
        print(paste0('correction in : ', x))
        writeLines(
          readLines(
            x,
            warn = FALSE
          ) %>%
            paste0(
              collapse = "\n" 
            ) %>%
            str_remove("(?<=\\}),(?=\\n\\s*\\])"),
          x
        )
        tryCatch(
          fromJSON(
            file = x,
            simplify = FALSE
          ),
          error = function(e) {
            stop(x)
          }
        )
      }
    )

    
    
    
    base_parsing_res$Name <- str_replace(
      str_extract(x, "(?<=Archetypes/)(.+)(?=\\.json)"),
      "_"," ")
    base_parsing_res$IncludeColorInName <- FALSE

    
    # in order to have valid json i need tto convert single cards in list of cards
    base_parsing_res$Conditions <- lapply(
      base_parsing_res$Conditions,
      function(condition_en_cours) {
        if (length(condition_en_cours$Cards) == 1) {
          condition_en_cours$Cards <- as.list(condition_en_cours$Cards)
          }
        return(condition_en_cours)
        }
      )



    if (!is.null(base_parsing_res$Variants)) {
      base_parsing_res$Variants <- lapply(base_parsing_res$Variants, function(y) {
        y$Name <- paste0(str_extract(x, "(?<=Archetypes/)(.+)(?=\\.json)"), ":", y$Name)
        y$IncludeColorInName <- FALSE

        # in order to have valid json i need tto convert single cards in list of cards
        y$Conditions <- lapply(y$Conditions, function(condition_en_cours) {
          if (length(condition_en_cours$Cards) == 1) {
            condition_en_cours$Cards <- as.list(condition_en_cours$Cards)
          }
          return(condition_en_cours)
        })


        return(y)
      })
    }

    
    write(toJSON(base_parsing_res, indent = 2), x)

    return(base_parsing_res)
  }
)

library("rjson")
library(tidyverse)



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

settings_parser$Format <- format_param
settings_parser$ReferenceFormat <- format_param

min_date <- min(as.Date(Sys.time()) %m-% months(5), date_cut)

settings_parser$startdate <- as.character(min_date)
settings_parser$outputfile <- paste0(format_param, "_", settings_parser$outputfile)
settings_parser$Filter[[1]] <- format_param

write(toJSON(settings_parser, indent = 2), "ArchetypeParser/settings.json")



# x <-   list.files(
#   paste0( "ArchetypeParser/MTGOFormatData_FB/Formats/",format_param,"/Archetypes/"),
#   full.names = TRUE
# )[1]


# Remove color in name and change name using file name and add initial name to variant
modify_archetype_res <- lapply(
  list.files(
    paste0("ArchetypeParser/MTGOFormatData_FB/Formats/", format_param, "/Archetypes/"),
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


    base_parsing_res$Name <- str_extract(x, "(?<=Archetypes/)(.+)(?=\\.json)")
    base_parsing_res$IncludeColorInName <- FALSE

    # in order to have valid json i need tto convert single cards in list of cards
    base_parsing_res$Conditions <- lapply(base_parsing_res$Conditions, function(condition_en_cours) {
      if (length(condition_en_cours$Cards) == 1) {
        condition_en_cours$Cards <- as.list(condition_en_cours$Cards)
      }
      return(condition_en_cours)
    })



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

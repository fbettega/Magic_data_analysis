library("rjson")
library(tidyverse)




if (!dir.exists('ArchetypeParser/MTGOFormatData_FB/')) {
  dir.create('ArchetypeParser/MTGOFormatData_FB/', recursive = TRUE)
}

file.copy(
  "ArchetypeParser/MTGOFormatData/Formats",
  "ArchetypeParser/MTGOFormatData_FB/", 
  overwrite = TRUE,
  recursive = TRUE
)
# unlink("ArchetypeParser/MTGOFormatData_FB", recursive = TRUE)

settings_parser <- fromJSON(
  file = "ArchetypeParser/settings_base.json",
  simplify = FALSE
)

settings_parser$Format <- "Modern"
settings_parser$startdate <- as.character(as.Date(Sys.time()) %m-% months(5))

write(toJSON(settings_parser,indent =  2),"ArchetypeParser/settings.json")



# Remove color in name and change name using file name and add initial name to variant
modify_archetype_res <- lapply(
  list.files(
    "ArchetypeParser/MTGOFormatData_FB/Formats/Modern/Archetypes/",
    full.names = TRUE
  ),function(x){
base_parsing_res <- fromJSON(
  file = x,
  simplify = FALSE
)
base_parsing_res$Name <- str_extract(x,"(?<=/)(\\w+)(?=\\.json)")
base_parsing_res$IncludeColorInName <- FALSE

# in order to have valid json i need tto convert single cards in list of cards
base_parsing_res$Conditions <- lapply(base_parsing_res$Conditions, function(condition_en_cours){
  if(length(condition_en_cours$Cards) == 1) {
    condition_en_cours$Cards <- as.list(condition_en_cours$Cards)
    }
  return(condition_en_cours)
})



if (!is.null(base_parsing_res$Variants) ) {
  base_parsing_res$Variants <- lapply(base_parsing_res$Variants, function(y){
      y$Name <- paste0(str_extract(x,"(?<=/)(\\w+)(?=\\.json)"),":", y$Name)
      y$IncludeColorInName <- FALSE
      
      # in order to have valid json i need tto convert single cards in list of cards
      y$Conditions <- lapply(y$Conditions, function(condition_en_cours){
        if(length(condition_en_cours$Cards) == 1) {
          condition_en_cours$Cards <- as.list(condition_en_cours$Cards)
        }
        return(condition_en_cours)
      })
      
      
    return(y)
  }) 
  
  
}

write(toJSON(base_parsing_res,indent =  2),x)

return(base_parsing_res)
})






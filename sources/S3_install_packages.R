


list.of.packages <- readRDS("data/list_of_packages.rds")

# library(tidyverse)
# list.of.packages <- rbind(list.of.packages ,data.frame(
#   package = c("future", "tidymodels", "baguette", "xgboost")
#   ) %>% mutate(source = "Cran"))
# readr::write_rds(list.of.packages,"data/list_of_packages.rds")



new.packages <- list.of.packages$package[list.of.packages$source == "Cran"][!(list.of.packages$package[list.of.packages$source == "Cran"] %in% installed.packages()[,"Package"])]
  
  # (list.of.packages %>% filter(source == "Cran") %>% pull(package) )[!((list.of.packages %>% filter(source == "Cran") %>% pull(package)) %in% installed.packages()[,"Package"])]


if(length(new.packages) > 0){
  install.packages(new.packages)
}


# new_package_github <- (list.of.packages %>% filter(str_detect(source,"devtools::install_github")) %>% pull(package))[!((list.of.packages  %>% filter(str_detect(source,"devtools::install_github")) %>% pull(package)) %in% installed.packages()[,"Package"])]
# 
# if(length(new_package_github) > 0){
#   
#   install.packages(new.packages)
# }
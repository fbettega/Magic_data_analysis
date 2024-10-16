
model_unco_cards_fun <- function(df_fun) {
  cols_fun <- df_fun %>%
    select(ends_with("_CardName")) %>%
    colnames() %>%
    str_remove("_CardName")
  # unique(df_fun$Archetype)
  # x <- 'Storm'
  # x <- "Delver"
  
  
  model_unco_fun <- lapply(
    unique(df_fun$Archetype),
    function(x) {
      # print(as.character(x))
      # if(x == "Scam") browser()
      df_model <- df_fun %>%
        # select(-Join_main_count) %>%
        filter(Archetype == x) %>%
        ungroup() %>%
        filter(!is.na(!!rlang::sym(paste0(cols_fun, "_CardName")))) %>%
        rowwise() %>%
        mutate(!!rlang::sym(paste0(cols_fun, "_Count")) :=
                 ifelse(
                   str_detect(!!rlang::sym(paste0(cols_fun, "_Count")), " ; "),
                   !!rlang::sym(paste0(cols_fun, "_Count")),
                   paste0(
                     findIntRuns(
                       as.numeric(
                         unlist(str_split(!!rlang::sym(paste0(cols_fun, "_Count")), "/"))
                       )
                     ),
                     collapse = "/"
                   )
                 )) %>%
        ungroup() %>%
        pivot_wider(
          names_from = !!rlang::sym(paste0(cols_fun, "_CardName")),
          values_from = !!rlang::sym(paste0(cols_fun, "_Count")),
          values_fill = "0"
        ) %>%
        select(-Archetype, -Player, -id, -Archetype_count, -Draws) %>%
        mutate(
          across(
            where(is.character),
            ~ fct_infreq(as.factor(.))
          )
        ) %>%
        select(where(~ n_distinct(.) > 1))
      
      
      
      model_unco_tot_fun <- df_fun %>%
        filter(Archetype == x) %>%
        ungroup() %>%
        filter(!is.na(!!rlang::sym(paste0(cols_fun, "_CardName")))) %>%
        rowwise() %>%
        mutate(!!rlang::sym(paste0(cols_fun, "_Count")) :=
                 ifelse(
                   str_detect(!!rlang::sym(paste0(cols_fun, "_Count")), " ; "),
                   !!rlang::sym(paste0(cols_fun, "_Count")),
                   paste0(
                     findIntRuns(
                       as.numeric(
                         unlist(str_split(!!rlang::sym(paste0(cols_fun, "_Count")), "/"))
                       )
                     ),
                     collapse = "/"
                   )
                 )) %>%
        ungroup() %>%
        pivot_wider(
          names_from = !!rlang::sym(paste0(cols_fun, "_CardName")),
          values_from = !!rlang::sym(paste0(cols_fun, "_Count")),
          values_fill = "0"
        ) %>%
        select(-Archetype, -Player, -id, -Archetype_count, -Draws) %>%
        mutate(
          # Choose most common level as references
          across(
            where(is.character),
            ~ factor(
              if_else(. == names(sort(table(.),
                                      decreasing = TRUE
              ))[1],
              names(sort(table(.),
                         decreasing = TRUE
              ))[1], "Other"
              ),
              levels = c(names(sort(table(.),
                                    decreasing = TRUE
              ))[1], "Other")
            )
          )
        ) %>%
        # remove card with only 1+ like basic land fetch ....
        select(where(~ n_distinct(.) > 1))
      
      # browser()
      
      if (nrow(df_model) == 0) {
        model_res <- NULL
        model_res_any <- NULL
        model_res_ridge <- NULL
      } else {
        model_res_any <- model_removing_alias_var(
          df = model_unco_tot_fun # , interaction_fun = interaction_term[x]
        )
        
        model_res <- model_removing_alias_var(
          df = df_model # , interaction_fun = interaction_term[x]
        )
        
        
        if(ncol(df_model) <= 3){
          
          model_res_ridge <- NULL
        } else {
          
          df_mod <- rbind(
            df_model %>%
              select(-Losses) %>%
              uncount(Wins) %>%
              mutate(
                Y = 1,
                .before = 1
              ),
            df_model %>%
              select(-Wins) %>%
              uncount(Losses) %>%
              mutate(
                Y = 0,
                .before = 1
              )
          )  %>%
            # remove card with only 1+ like basic land fetch ....
            # here because some rare deck as W 0 and L 0 and can be the only one with distinct value
            select(where(~ n_distinct(.) > 1))
          
          
          x_mat_model <- model.matrix(
            ~., # (.)^2,
            dplyr::select(df_mod, -c(Y))
          )[, -1]
          
          y_count <- df_mod$Y
          
          
          fit_ridge <- suppressWarnings(
            ridge.proj(x_mat_model,
                       y_count,
                       family = "binomial",
                       suppress.grouptesting = TRUE
            )
          )
          
          model_res_ridge <- cbind(
            bhat = fit_ridge$bhat,
            se = fit_ridge$se,
            confint = confint(fit_ridge), 
            pval = fit_ridge$pval
          ) %>% 
            as.data.frame() %>% 
            rename(
              OR = V1
            )  %>% 
            mutate(
              OR = exp(OR),
              lower = exp(lower),
              upper = exp(upper)
            )  %>% 
            rownames_to_column("Card_name") %>% 
            mutate(
              Card_name = str_remove_all(Card_name ,"`"),
              Card_name = sub("(\\D(?=\\d))", "\\1:", Card_name,perl = TRUE)
            ) %>% 
            right_join(
              df_mod %>% 
                select(where(is.factor)) %>% 
                pivot_longer(everything()) %>% 
                group_by( name,value) %>% 
                summarise(
                  N = n() ,
                  .groups = "drop") %>% 
                rowwise() %>% 
                mutate(Card_name = paste0(name,":",value),.before =1 ),
              by = join_by(Card_name)
            ) %>% 
            mutate(sort_col = paste0(
              str_extract(Card_name,"[:alpha:]+(?=:)"),
              ifelse(is.na(OR),"0",
                     as.numeric(str_extract(Card_name,"(?<=:)\\d{1}") ) +1      
              )
            )
            ) %>% 
            arrange(sort_col) %>% 
            select(-sort_col) %>%  
            mutate(    
              Archetype = as.character(x), .before = 1)
        }
        
        
        model_res_any$Archetype <- x
        model_res$Archetype <- x
      }
      return(
        list(
          Model_any = model_res_any,
          Model_count = model_res,
          model_ridgge = model_res_ridge
        )
      )
    }
  ) %>%
    discard(is.null)
  
  return(model_unco_fun)
}


model_removing_alias_var <- function(
    df
){
  
  
  formula_model <- as.formula(
    paste0(
      "cbind(Wins, Losses) ~."
    )
  )
  
  res <- glm(formula_model,
             data = df,
             family = quasibinomial # binomial
  )
  
  #remove the linearly dependent variables variables
  return(res)
}
# result_models_Uncommon_cards_all_arch <- model_unco_cards_fun(
#   df_fun = groupe_cards_uncommon_data
# ) %>%
#   name_list_of_model_with_string(unique(groupe_cards_uncommon_data$Archetype))
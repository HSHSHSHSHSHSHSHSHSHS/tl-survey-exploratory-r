
# Library  -----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(stringr)

library(readxl)
library(writexl)
library(jsonlite)

library(coin) #exact

library(MRCV) #mcrv BELOVED

library(fastDummies) #tbh might be able to work around this one but it's single-basic -> 1/0

library(FSA) #correct kruskal-wallis (difficulty/importance)
library(rcompanion) #correct cohran-armitage (likert)

library(rstatix)

library(Partiallyoverlapping) # For props, multiple dep

library(multcompView) #For anova post hoc
library(lsmeans) # ^

# Setup -------------------------------------------------------------------

all_q <- na.omit(question_types$Shortq[question_types$ALL == "T"])

# Fisher Exact ------------------------------------------------------------

fishers_test <- function(mydf, myvar, my_qlist){
  test_store <- list()

  for (q in my_qlist){
    print(q)

    q_type <- question_types %>%
      filter(Shortq == q) %>%
      pull(Type)

    #Multi-write and single-write
    if (grepl("-write", q_type)) {
      q <- paste0(q, "_combined")
    }

    #Filter no NAs
    mynewdf <- mydf %>%
      filter(!is.na(.data[[q]]) & !is.na(.data[[myvar]]))

    unique_ind <- unique(mynewdf[[myvar]])
    print(paste("Unique vars:", unique_ind))
    ind_pairs <-combn(unique_ind, 2, simplify = FALSE)

    #Different tests depending on dep
    if (any(grepl("Single", q_type)) | any(grepl("Bool", q_type))) {
      test_results <- fisher_single_dependent(mynewdf, myvar, q, unique_ind, ind_pairs)
      print(test_results)
      if (length(test_results) > 0) {
        test_store[[q]] <- test_results
      }

      else if (grepl("Multi", q_type)) {
        test_results <- fisher_multi_dependent(mynewdf, myvar, q, unique_ind, ind_pairs)
        print(test_results)
        if (length(test_results) > 0) {
          test_store[[q]] <- test_results
        }
      }
    }
  }
  return (test_store)
}

fisher_single_dependent <- function(mydf, myvar, myq, unique_ind, ind_pairs){ #WIP see wip below, maybe dummycols?
  all_vars <- c(myvar, myq)

  mydf <- mydf %>%
    dplyr::select(all_of(all_vars))

  dummiedcols <- fastDummies::dummy_cols(mydf, select_columns = myq)

  single_q_list <- colnames(dummiedcols) %>%
    .[startsWith(., myq)] %>%
    .[. != myq]
  print(single_q_list)

  test_results <- list()
  adjusted_alpha <- 0.05 / (length(single_q_list) * length(unique_ind))

  for (q in single_q_list){
    extramydf <- dummiedcols %>%
      dplyr::select({{myvar}}, {{q}})

    for (pair in ind_pairs) {
      myvar <- sym(myvar)

      subset_df <- extramydf %>%
        filter(!!myvar %in% pair)

      contingency_table <- table(subset_df[[myvar]], subset_df[[q]])
      print(paste(q, ":", pair[1], "vs", pair[2]))

      if (ncol(contingency_table) > 1) {
        test <- fisher.test(contingency_table, simulate.p.value = TRUE)
        print(test$p.value)

        if (test$p.value < adjusted_alpha) {
          short_q <- gsub(paste0(myq, "_"), "", q)
          test_results[[paste(short_q, ":", pair[1], "vs", pair[2])]] <- test
        }
      }
    }
  }
  print(paste("Alpha:", adjusted_alpha))
  return(test_results)
}

fisher_multi_dependent <- function(mydf, myvar, myq, unique_ind, ind_pairs){
  multi_q_list <- colnames(mydf) %>%
    .[startsWith(., myq)] %>%
    .[. != myq]

  test_results <- list()
  adjusted_alpha <- 0.05 / (length(multi_q_list) * length(unique_ind))

  for (q in multi_q_list){
    extramydf <- mydf %>%
      select({{myvar}}, {{q}})

    for (pair in ind_pairs) {
      myvar <- sym(myvar)

      subset_df <- extramydf %>%
        filter(!!myvar %in% pair)

      contingency_table <- table(subset_df[[myvar]], subset_df[[q]])
      print(paste(q, ":", pair[1], "vs", pair[2]))

      if (ncol(contingency_table) > 1 && nrow(contingency_table) > 1) {
        test <- fisher.test(contingency_table, simulate.p.value = TRUE)
        print(test$p.value)

        if (test$p.value < adjusted_alpha) {
          short_q <- gsub(paste0(myq, "_"), "", q)
          test_results[[paste(short_q, ":", pair[1], "vs", pair[2])]] <- test #again i wanna strip the start kind of annoying
        }
      }
    }
  }
  print(paste("Alpha:", adjusted_alpha))
  return(test_results)
}

##
startyear_tests <- fishers_test(startyear_df_INC_EX, "start_year", all_q)
years_spent_tests <- fishers_test(years_spent_df, "years_spent", all_q)


# Kruskal-Wallis ----------------------------------------------------------

likert_order <- c("No, it has become significantly worse.",
                   "No, it has become slightly worse.",
                   "It has stayed around the same.",
                   "Yes, it has improved slightly.",
                   "Yes, it has improved significantly.")
likert_qs <- c("community_change", "quality_change")


#Between
BETWEEN_kruskal_wallis_plus_dunn_test<- function(mydf, myvar, qs_list){
  test_store <- list()

  for (col in qs_list){
    print(col)

    formula <- as.formula(paste(col, "~", myvar))
    krus <- kruskal.test(formula, data = mydf)
    print(krus)

    adjusted_alpha <- 0.05 / (length(qs_list) * length(unique(mydf[[myvar]])))

    if (krus["p.value"] <= adjusted_alpha) {
      dunn <- dunnTest(as.numeric(mydf[[col]]) ~ mydf[[myvar]],
                       data = mydf,
                       method="bonferroni")
      print(dunn)
      test_store[[paste(col, "krus")]] <- krus
      test_store[[paste(col, "dunn")]] <- dunn

    }
  }
  return (test_store)
}


startyear_tests_3 <- BETWEEN_kruskal_wallis_plus_dunn_test(startyear_df_INC_EX, "start_year", likert_qs)
years_spent_tests_3 <- BETWEEN_kruskal_wallis_plus_dunn_test(years_spent_df, "years_spent", likert_qs)

# Anova + T- Test -------------------------------------------------------------------

STRING_difficulty_cols <- cols <- names(df)[grepl("_difficulty$", names(df))]
STRING_importance_cols <- cols <- names(df)[grepl("_importance$", names(df))]

##Two inds, Anova

anova_func <- function(mydf, myvar, my_qlist) {

  a = 0.05
  teststore <- list()

  newdf <- mydf

  for (q in my_qlist){

    print(q)

    newdf <- newdf %>%
      filter(!is.na(newdf[[q]]))

    print(length(newdf[[q]]))

    formula <- as.formula(paste(q, "~", myvar))

    model = lm(formula, data = newdf)

    anova = Anova(model, type = "II")
    print(anova)

    if(anova[[1, "Pr(>F)"]] < a){

      var_formula <- as.formula(paste("~", myvar))

      marginal = lsmeans(model, var_formula)

      pairs <- pairs(marginal,
                     adjust="tukey")

      teststore[[q]] <- pairs

    }

  }

  return(teststore)
}

startyear_tests_5 <- anova_func(startyear_df_INC_EX, "start_year", STRING_difficulty_cols)
startyear_tests_6 <- anova_func(startyear_df_INC_EX, "start_year", STRING_importance_cols)
years_spent_tests_5 <- anova_func(years_spent_df, "years_spent", STRING_difficulty_cols)
years_spent_tests_6 <- anova_func(years_spent_df, "years_spent", STRING_importance_cols)



##One ind, paired T

t_func <- function(mydf, myvar, my_qlist) {

  a = 0.05
  teststore <- list()

  each_ind <- levels(mydf[[myvar]])
  pairs_dep <- combn(my_qlist, 2, simplify = FALSE)

  adjusted_alpha = a/length(each_ind)/length(my_qlist)

  for (ind in each_ind){

    print(ind)
    ind_list <- list()

    newdf <- mydf %>%
      filter(.data[[myvar]] == ind)

    for(pair in pairs_dep){
      print(pair)

      relevant_cols <- c(myvar, pair)

      a_newdf <- newdf %>%
        dplyr::select(all_of(relevant_cols))%>%
        na.omit()%>%
        pivot_longer(cols = pair)

      tee <- t.test(value ~ name,
                    data = a_newdf,
                    paired = TRUE)

      print(tee)

      if (tee$p.value < adjusted_alpha) {
        ind_list[[paste(pair[1], "vs", pair[2])]] <- tee
      }
    }

    if (length(ind_list) > 0){
      teststore[[ind]] <- ind_list
    }

  }

  print(adjusted_alpha)
  return(teststore)

}

startyear_tests_7 <- t_func(startyear_df_INC_EX, "start_year", STRING_importance_cols)
startyear_tests_8 <- t_func(startyear_df_INC_EX, "start_year", STRING_difficulty_cols)
years_spent_tests_7 <- t_func(years_spent_df, "years_spent", STRING_importance_cols)
years_spent_tests_8 <- t_func(years_spent_df, "years_spent", STRING_difficulty_cols)

# Proportions + Overlapping -----------------------------------------------

within_check <- function(mydf, myvar, myqlist){

  test_store <- list()

  unique_ind <- levels(mydf[[myvar]])
  print(paste("Unique vars:", unique_ind))

  for (ind in unique_ind){

    ind_store <- list()

    newdf <- mydf %>%
      filter(.data[[myvar]] == ind)

    for (q in myqlist){
      print(q)

      q_type <- question_types %>%
        filter(Shortq == q) %>%
        pull(Type)

      if (grepl("-write", q_type)) {
        q <- paste0(q, "_combined")
      }

      if (any(grepl("Single", q_type)) | any(grepl("Bool", q_type))) {
        test_results <- prop_func(newdf, ind, q)
        print(test_results)
        if (length(test_results) > 0) {
          ind_store[[q]] <- test_results
        }

      }

      else if (grepl("Multi", q_type)) {
          test_results <- stacked_prop_func(newdf, ind, q)
          print(test_results)
          if (length(test_results) > 0) {
            ind_store[[q]] <- test_results
          }
        }
      }

    test_store[[ind]] <- ind_store
  }

  return (test_store)
}


## Prop no overlap

prop_func <- function(mydf, myvar, myq){

  #chi square goodness of fit - doesn't go into individ lol
  tabe <- table(mydf[[myq]])
  print(tabe)

  if (length(tabe) < 2){
    return()
  }

  chi <- chisq.test(tabe, p = rep(1/length(tabe), length(tabe)))
  print(chi)

  if (chi$p.value < 0.05){
    return(chi)
  }
}

## Overlap

stacked_prop_func <-function(mydf, ind, myq){

  this_prop_store <- list()

  q_list <- colnames(mydf) %>%
    .[startsWith(., myq)] %>%
    .[. != myq]%>%
    .[!endsWith(., "yn_split")]

  q_pairs <-combn(q_list, 2, simplify = FALSE)

  a <- 0.05 / length(q_list)

  for (pair in q_pairs){

    print(pair)

    a_mydf <- mydf %>%
      dplyr::select(all_of(pair))%>%
      na.omit()

    print(table(a_mydf))

    if(length(a_mydf[[pair[1]]]) < 2 & length(a_mydf[[pair[2]]]) < 2){
      next
    }

    Pp <- Prop.test(
      as.numeric(as.character(a_mydf[[1]])),
      as.numeric(as.character(a_mydf[[2]])),
      stacked = TRUE
    )

    print(Pp)

    if(Pp$p.value <= a){
      name <- paste0(ind, ": ", pair[1], " vs ", pair[2])

      this_prop_store[[name]] = Pp
    }
  }

  print(paste("Alpha:", a))
  return(this_prop_store)

}


#startyear_tests_9 <- within_check(startyear_df_INC_EX, "start_year", all_q)
#years_spent_tests_9 <- within_check(years_spent_df, "years_spent", all_q)


# MRCV --------------------------------------------------------------------

#TO CHECK

MRCV_test <- function(mydf, myvar, my_qlist, test_store){
  test_store <- list()
  myvar_list <- colnames(mydf) %>%
    .[startsWith(., myvar)] %>%
    .[. != myvar]

  for (q in my_qlist){
    q_type <- question_types %>%
      filter(Shortq == q) %>%
      pull(Type)

    #Multi-write and single-write
    if (grepl("-write", q_type)) {
      q <- paste0(q, "_combined")
    }

    print(q)

    #Filter no NAs
    mynewdf <- mydf %>%
      filter(!is.na(.data[[q]]) & !is.na(.data[[myvar]]))

    #single has to do some dum dum
    #NOTE ok so it compares within group ie girls more likely to use x than y NOT
    #between group like guys vs girls. fix for later i guess
    if (any(grepl("Single", q_type)) | any(grepl("Bool", q_type))) {

      mynewdf <- mynewdf %>%
        select({{q}}, {{myvar_list}})

      temp_store <- MI.test(data = mynewdf, summary.data = FALSE, I = 1, J = length(myvar_list), type = "bon")
      print(temp_store$bon$p.value.bon)

      if (temp_store$bon$p.value.bon < 0.05){

        unique_dep <- unique(mynewdf[[q]])

        dep_pairs <-combn(unique_dep, 2, simplify = FALSE)

        test_results <- list()
        adjusted_alpha <- 0.05 / (length(myvar_list) * length(unique_dep))

        for (contingencyvar in myvar_list) {

          myextranewdf <- mynewdf %>%
            select({{q}}, {{contingencyvar}})

          for (pair in dep_pairs) {
            symq <- sym(q)

            subset_df <- myextranewdf %>%
              filter(!!symq %in% pair)

            contingency_table <- table(subset_df[[contingencyvar]], subset_df[[q]])
            print(paste(q, contingencyvar, ":", pair[1], "vs", pair[2]))

            print(contingency_table)

            if (ncol(contingency_table) > 1 && nrow(contingency_table) > 1) {
              test <- fisher.test(contingency_table, simulate.p.value = TRUE)
              print(test)

              if (test$p.value < adjusted_alpha) {
                shortvar <- gsub(paste0(myvar, "_"), "", contingencyvar)
                #shortvar <- str_remove_all(contingencyvar, fixed(q)) #hmmm not working but not world ending
                test_results[[paste(shortvar, ":", pair[1], "vs", pair[2])]] <- test #this OR teststore[[q]] returns totally empty guys but also i still dont care
              }
            }
          }

        }

        if (length(test_results) > 0) {
          test_store[[q]] <- test_results
        }
      }

      #debug
      print(myvar)
      print(q)
    }

    #multi mlm done!!
    else if (grepl("Multi", q_type)) {

      multi_q_list <- colnames(mynewdf) %>%
        .[startsWith(., q)] %>%
        .[. != q]

      print(myvar_list)
      print(multi_q_list)

      mynewdf <- mynewdf %>%
        select(myvar_list, multi_q_list)

      temp_store <- MI.test(data = mynewdf, summary.data = FALSE, I = length(myvar_list), J = length(multi_q_list), type = "bon")

      tested_q <- list( #works
        chi_sq = temp_store$general$X.sq.S,
        #chi_sq_all <- temp_store$general$X.sq.S.ij,
        p_value_bon = temp_store$bon$p.value.bon,
        chi_sq_all_bon = temp_store$bon$X.sq.S.ij.p.bon
        #chi_sq_rs2 <- temp_store$rs2$X.sq.S.rs2, #add back in if rs2 not bon
        #degrees_of_freedom <- temp_store$rs2$df.rs2,
        #p_value_rs2 <- temp_store$rs2$p.value.rs2
      )

      if (length(tested_q) > 0) {
        test_store[[q]] <- tested_q
      }

      print(paste("Updated test_store for", q))
      print(test_store)
    }
  }
  return(test_store)
}




















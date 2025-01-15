# Libliotecha -----------------------------------------------------------

library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggmosaic)
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

library(rlang)

library(MASS) # Ordinal logist reg

# Setup -------------------------------------------------------------------

rm(list = ls())

df_og <- read.csv("cleaned_df.csv", na.strings = "")
df <- df_og %>%
  mutate(across(where(~ all(. %in% c(0, 1, NA))), as.factor))

importance_names <- c("Smooth_importance",
                      "Accurate_importance",
                      "Raws_importance",
                      "Typeset_importance",
                      "SFX_importance",
                      "Redraw_importance",
                      "Font_importance",
                      "Speed_importance")
colnames(df)[(ncol(df)-7):ncol(df)] <- importance_names

big_data_dict = "qs_type_map.xlsx"
question_types <- read_excel(big_data_dict, sheet = "Data Dictionary")

df <- df%>%
  mutate(group_count = fct_relevel(group_count,
                                   "0",
                                   "1",
                                   "2-4",
                                   "5-8",
                                   "9-15",
                                   "15-30",
                                   ">30"),
         years_spent = fct_relevel(years_spent,
                                   "<1 month",
                                   "1-3 months",
                                   "4-6 months",
                                   "7-11 months",
                                   "1-2 years",
                                   "2-4 years",
                                   "4-8 years",
                                   ">8 years"),
         chapters_done = fct_relevel(chapters_done,
                                    "1-5",
                                    "6-10",
                                    "11-20",
                                    "21-50",
                                    "51-100",
                                    "100-200",
                                    "200-500",
                                    "500+"),
         time_hours = fct_relevel(time_hours,
                                  "<1",
                                  "1-2",
                                  "3-5",
                                  "6-10",
                                  "11-20",
                                  "21-39",
                                  "40+"),
         money_spent = fct_relevel(money_spent,
                                   "$0 USD",
                                   "<$10 USD",
                                   "$10-$30 USD",
                                   "$30-$100 USD",
                                   "$100-$200 USD",
                                   "$200-$500 USD",
                                   ">$500 USD"),
         community_change = fct_relevel(community_change, "No, it has become significantly worse.",
                                        "No, it has become slightly worse.",
                                        "It has stayed around the same.",
                                        "Yes, it has improved slightly.",
                                        "Yes, it has improved significantly."),
         quality_change = fct_relevel(quality_change, "No, it has become significantly worse.",
                                        "No, it has become slightly worse.",
                                        "It has stayed around the same.",
                                        "Yes, it has improved slightly.",
                                        "Yes, it has improved significantly."),
         )%>%
  dplyr::select(-c(series_genre_combined_horror,
                   all_software_combined_na))
levels(df$main_software_combined)



# Graffy ------------------------------------------------------------------

custom_palette_1 <- c(
  "#ee356d",
  "#f9a768",
  "#ffde6e",
  "#38b881",
  "#00695a",
  "#00b2b2",
  "#24525b",
  "#73cfe8",
  "#0072b2",
  "#304a81",
  "#656ca3"
)

custom_palette_2 <- c(
  "#ee356d",
  "#f26b66",
  "#f9a768",
  "#ffde6e",
  "#a7ce8d",
  "#38b881",
  "#00695a",
  "#71c8b9",
  "#00b2b2",
  "#24525b",
  "#73cfe8",
  "#0072b2",
  "#304a81",
  "#656ca3"
)


custom_palette_3 <- c(
  "#a7ce8d",
  "#24525b"
)

# START YEAR PREP ---------------------------------------------------------

startyear_df_INC_EX <- df %>%
  filter(!is.na(start_year))%>%
  mutate(start_year = str_replace_all(start_year, "-", "_"))%>%
  mutate(start_year = as.factor(start_year))%>%
  mutate(start_year = fct_collapse( #only 2 pre 2000
    start_year,
    "1970_2010" = c("1970_2000", "2001_2010")
  )) %>%
  mutate(not_ex = "ALL")

startyear_df_NO_EX <- startyear_df_INC_EX %>%
  filter(is.na(ex))%>%
  mutate(not_ex = "CURRENT")

startyear_df <- rbind(startyear_df_INC_EX,
                      startyear_df_NO_EX)

levels(startyear_df$main_role_combined)
table(startyear_df$start_year)

# EXPLORING START YEAR --------------------------------------------------------

startyear_df %>%
  #filter(!is.na(ex))%>%
  count(start_year)

q_single <- "group_count"
# q_single <- ifelse(
#   question_types %>%
#     filter(Shortq == q_single) %>%
#     pull(Type) == "Single-write",
#   paste0(q_single, "_combined"),
#   q_single
# )

a <- startyear_df_INC_EX %>%
  filter(!is.na(.data[[q_single]])) %>%
  mutate(!!sym(q_single) := fct_rev(!!sym(q_single))) %>%
  ggplot() +
  geom_bar(
    aes(x = start_year, fill = !!sym(q_single)),
    position = "fill"
  ) +
  scale_fill_manual(values = custom_palette_1) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Avenir", face = "bold", size = 14),
    plot.subtitle = element_text(family = "Avenir", face = "bold", size = 12),
    axis.title.y = element_blank(),
    axis.title.x = element_text(family = "Avenir", margin = margin(t = 10), size = 14),
    axis.text.x = element_text(
      family = "Avenir",
      size = 10,
      margin = margin(t = -10)
    ),
    axis.text.y = element_text(family = "Avenir", size = 8),
    legend.text = element_text(family = "Avenir", size = 12),
    legend.title = element_text(family = "Avenir", size = 14),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.box = "none",
    legend.background = element_blank(),
    legend.box.background = element_blank()
  )
a
ggsave(paste0("startyear_", "groupcount.png"),
        a,
        bg="transparent")

a_2 <- years_spent_df %>%
  mutate(years_spent = fct_rev(years_spent))%>%
  filter(!is.na(.data[[q_single]])) %>%
  mutate(!!sym(q_single) := fct_rev(!!sym(q_single))) %>%
  ggplot() +
  geom_bar(
    aes(x = years_spent, fill = !!sym(q_single)),
    position = "fill"
  ) +
  scale_fill_manual(values = custom_palette_1) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Avenir", face = "bold", size = 14),
    plot.subtitle = element_text(family = "Avenir", face = "bold", size = 12),
    axis.title.y = element_blank(),
    axis.title.x = element_text(family = "Avenir", margin = margin(t = 10), size = 14),
    axis.text.x = element_text(
      family = "Avenir",
      size = 12,
      margin = margin(t = -10)
    ),
    axis.text.y = element_text(family = "Avenir", size = 8),
    legend.text = element_text(family = "Avenir", size = 12),
    legend.title = element_text(family = "Avenir", size = 14),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.box = "none",
    legend.background = element_blank(),
    legend.box.background = element_blank()
  )
a_2
ggsave(paste0("yearsspent_", "groupcount.png"),
       a_2,
       bg="transparent")


b <- startyear_df_INC_EX%>%
  mutate(active_status = ifelse(is.na(ex),
                               "Current",
                               "Ex"))%>%
  group_by(start_year, active_status) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(start_year) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ggplot(aes(x = start_year, fill = active_status)) +
  geom_bar(stat = "identity", aes(y = count)) +
  geom_text(
    data = . %>% filter(active_status == "Ex"),
    aes(
      y = count,
      label = paste0(round(percent, 0), "%")
    ),
    vjust = -0.5,
    family = "Avenir",
    size = 4.5
  ) +
  scale_fill_manual(values = custom_palette_3) +
  # ggplot+
  # geom_bar(aes(x = start_year,
  #              fill = active_status))+
  # scale_fill_manual(values = custom_palette_3) +
  # geom_text(aes(x = start_year,
  #               y = ..count..,
  #               label = paste0(round(..count../sum(..count..) * 100), "%")),
  #           stat = "count",
  #           vjust = -0.5,
  #           family = "Avenir",
  #           size = 4.5) +
  ggtitle("What year did you start?")+
  labs(subtitle = paste0("(n = ", nrow(startyear_df_INC_EX), ")"))+
  theme_minimal()+
  theme(plot.title = element_text(family = "Avenir", face = "bold", size = 14),
        plot.subtitle = element_text(family = "Avenir", face = "bold", size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_text(family = "Avenir", margin = margin(t = 10)),
        axis.text.x = element_text(family = "Avenir", size = 8,margin = margin(t = -10)),
        axis.text.y = element_text(family = "Avenir", size = 8),
        legend.text = element_text(family = "Avenir", size = 12),
        legend.title = element_text(family = "Avenir", size = 14),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.box = "none",
        legend.background = element_blank(),
        legend.box.background = element_blank())
b
ggsave(paste0("startyear_EXNOTES.png"),
       b,
       bg="transparent")



startyear_df%>%
  filter(!is.na(ex) | not_ex == "CURRENT",
         !is.na(.data[[q_single]]))%>%
  mutate(not_ex = ifelse(not_ex == "ALL", "EX", not_ex))%>%
  count(not_ex, start_year, .data[[q_single]])%>%
  group_by(not_ex,
           start_year) %>%
  mutate(prop = round(n / sum(n) * 100, 0))%>%
  arrange(.data[[q_single]],
          start_year)%>%
  print(n = Inf)

# table(startyear_df$start_year,
#       startyear_df[[q_single]])
#
# round(
#   prop.table(table(startyear_df$start_year,
#                 startyear_df[[q_single]]), 1)* 100,
# 0)

startyear_df %>%
  filter(!is.na(!!sym(q_single)))%>%
  group_by(not_ex, start_year, !!sym(q_single)) %>%
  summarise(count = n()) %>%
  mutate(pct = count / sum(count)) %>%
  ggplot(aes(x = factor(start_year),
             y = pct,
             color = !!sym(q_single),
             group = !!sym(q_single))) +
  geom_point() +
  geom_line() +
  facet_wrap(~not_ex)


###

q_start <- "var"
q_start <- ifelse(
  question_types %>%
    filter(Shortq == q_start) %>%
    pull(Type) == "Multi-write",
  paste0(q_start, "_combined_"),
  q_start
)

startyear_df %>%
  #subset(Question != "en")%>%
  ##### NOT EX
  ##filter(is.na(ex))%>%
  filter(!is.na(ex) | not_ex == "CURRENT")%>%
  mutate(not_ex = ifelse(not_ex == "ALL", "EX", not_ex))%>%
  pivot_longer(cols = starts_with(q_start),
               #cols = ends_with(q_start),
               names_to = "Question",
               values_to = "Response") %>%
  mutate(Question = gsub(q_start, "", Question),
         Response = ifelse(!is.na(Response),
                           ifelse(Response == 1,
                                  "Yes", "No"),
                           NA)) %>%
  group_by(not_ex,
           start_year,
           Question,
           Response) %>%
  summarise(Count = n()) %>%
  mutate(pct = Count / sum(Count) * 100) %>%
  subset(Response == "Yes")%>%
  arrange(Question,
          start_year)%>%
  print(n = Inf) #%>%
# filter(!is.na(Response))%>%
# ggplot(aes(x = Question,
#            y = pct,
#            fill = Response)) +
# geom_bar(stat = "identity")+
# facet_wrap(~start_year, scales = "fixed")+
# theme(axis.text.x = element_text(angle = 90))

startyear_df %>%
  #filter(!is.na(ex))%>%
  pivot_longer(cols = starts_with(q_start),
               #cols = ends_with(q_start),
               names_to = "Question",
               values_to = "Response") %>%
  mutate(Question = gsub(q_start, "", Question),
         Response = ifelse(!is.na(Response),
                           ifelse(Response == 1,
                                  "Yes", "No"),
                           NA)) %>%
  group_by(not_ex, start_year, Question, Response) %>%
  summarise(Count = n()) %>%
  mutate(pct = Count / sum(Count) * 100) %>%
  subset(Response == "Yes")%>%
  #subset(Question != "no")%>%
  #subset(Question != "en")%>%
  #subset(Question != "ps_cc")%>%
  ggplot(aes(x = factor(start_year),
             y = pct,
             color = Question,
             group = Question)) +
  geom_point()+
  geom_line()+
  facet_wrap(~not_ex)


startyear_df %>%
  mutate(types_of = rowSums(dplyr::select(., starts_with(q_start)) == 1, na.rm = TRUE)) %>%
  ggplot(aes(x = types_of))+
  geom_histogram(aes(y = ..density..))+
  facet_wrap(~start_year)

startyear_df %>%
  #filter(tl == "Yes")%>%
  mutate(types_of = rowSums(dplyr::select(., starts_with(q_start)) == 1, na.rm = TRUE)) %>%
  group_by(start_year) %>%
  summarise(avg = mean(types_of),
            med = median(types_of),
            count_rows = n())

###

q_end <- "_difficulty"

startyear_df %>%
  pivot_longer(cols = ends_with(q_end),
               names_to = "Question",
               values_to = "Response") %>%
  mutate(Question = gsub(q_start, "", Question)) %>%
  group_by(not_ex, start_year, Question) %>%
  subset(!is.na(Response))%>%
  summarise(avg = mean(Response, na.rm = TRUE),
            count = n()) %>%
  arrange(Question)%>%
  print(n = Inf) %>%
  ggplot(aes(x = factor(start_year),
             y = avg,
             color = Question,
             group = Question)) +
  geom_point()+
  geom_line()+
  facet_wrap(~not_ex)
# YEARS SPENT PREP --------------------------------------------------------

years_spent_df <- df %>%
  filter(!is.na(years_spent))%>%
  mutate(years_spent = str_replace_all(years_spent, "-", "_"),
         years_spent = str_replace_all(years_spent, "month", "mo"),
         years_spent = str_replace_all(years_spent, "years", "yr"))%>%
  mutate(years_spent = as.factor(years_spent))%>%
  mutate(years_spent = fct_relevel(years_spent,
                    "<1 mo",
                    "1_3 mos",
                    "4_6 mos",
                    "7_11 mos",
                    "1_2 yr",
                    "2_4 yr",
                    "4_8 yr",
                    ">8 yr"))

levels(years_spent_df$series_format_combined)
table(years_spent_df$years_spent)

# EXPLORING YEARS SPENT --------------------------------------------------------

years_spent_df %>%
  count(years_spent)

explore_func <- function(ind_var,
                         q_input,
                         mult_count = 0){

  if (q_input == "difficulty" | q_input == "importance"){
    q_type <- "Other"
    q_adaptive <- q_input
  }

  else {

    q_type <- "Single_yn_split"
    q_adaptive <- q_input

    if (!str_ends(q_input, "yn_split")){

      q_type <- question_types %>%
        filter(Shortq == q_input) %>%
        pull(Type)

      q_adaptive <- ifelse(
        question_types %>%
          filter(Shortq == q_input) %>%
          pull(Type) %>%
          str_ends("-write"),
        paste0(q_input, "_combined"),
        q_input
      )
    }
  }

  q_adaptive <- sym(q_adaptive)
  ind_var <- sym(ind_var)

  ind_var_df_name <- paste0(ind_var, "_df")
  if (exists(ind_var_df_name)) {
    ind_var_df <- get(ind_var_df_name)
  }
  else {
    stop(paste("Data frame", ind_var_df_name, "does not exist."))
  }

  print(paste(q_adaptive, q_type))
  print(typeof(q_adaptive))

  if (str_starts(q_type, "Single") | str_starts(q_type, "Bool")) {

    ind_var_df%>%
      filter(!is.na(!!q_adaptive))%>%
      count(!!ind_var, !!q_adaptive)%>%
      group_by(!!ind_var) %>%
      mutate(pct = round(n / sum(n) * 100, 0))%>%
      arrange(!!q_adaptive,
              !!ind_var)%>%
      print(n = Inf)

    ind_var_df %>%
      filter(!is.na(!!sym(q_adaptive)))%>%
      group_by(!!ind_var, !!sym(q_adaptive)) %>%
      summarise(n = n()) %>%
      mutate(pct = round(n / sum(n) * 100, 0)) %>%
      ggplot(aes(x = factor(!!ind_var),
                 y = pct,
                 color = !!sym(q_adaptive),
                 group = !!sym(q_adaptive))) +
      geom_point() +
      geom_line()

  }

  else if (str_starts(q_type, "Multi")) {

    if(mult_count == 0) {

      ind_var_df %>%
        pivot_longer(cols = starts_with(as.character(q_adaptive)),
                     names_to = "Question",
                     values_to = "Response") %>%
        mutate(Question = gsub(as.character(q_adaptive), "", Question),
               Response = ifelse(!is.na(Response),
                                 ifelse(Response == 1,
                                        "Yes", "No"),
                                 NA)) %>%
        group_by(!!ind_var,
                 Question,
                 Response) %>%
        summarise(Count = n()) %>%
        mutate(pct = Count / sum(Count) * 100) %>%
        subset(Response == "Yes")%>%
        arrange(Question,
                !!ind_var)%>%
        print(n = Inf)

      ind_var_df %>%
        pivot_longer(cols = starts_with(as.character(q_adaptive)),
                     names_to = "Question",
                     values_to = "Response") %>%
        mutate(Question = gsub(as.character(q_adaptive), "", Question),
               Response = ifelse(!is.na(Response),
                                 ifelse(Response == 1,
                                        "Yes", "No"),
                                 NA)) %>%
        group_by(!!ind_var, Question, Response) %>%
        summarise(Count = n()) %>%
        mutate(pct = Count / sum(Count) * 100) %>%
        subset(Response == "Yes")%>%
        ggplot(aes(x = factor(!!ind_var),
                   y = pct,
                   color = Question,
                   group = Question)) +
        geom_point()+
        geom_line()

    }

    else {
      ind_var_df %>%
        mutate(types_of = rowSums(across(starts_with(as_string(q_adaptive))) == 1,
                                  na.rm = TRUE)) %>%
        group_by(!!ind_var) %>%
        summarise(avg = mean(types_of),
                  med = median(types_of),
                  count_rows = n()) %>%
        print(n = Inf)

       ind_var_df %>%
         mutate(types_of = rowSums(across(starts_with(as_string(q_adaptive))) == 1,
                                   na.rm = TRUE)) %>%
         ggplot(aes(x = types_of))+
         geom_histogram(aes(y = ..density..))+
         facet_wrap(as.formula(paste("~", ind_var)))
    }
  }

  else if (str_starts(q_type, "Other")) {

  ind_var_df %>%
    pivot_longer(cols = ends_with(as.character(q_adaptive)),
                 names_to = "Question",
                 values_to = "Response") %>%
    mutate(Question = gsub(as.character(q_adaptive), "", Question)) %>%
    group_by(!!ind_var, Question) %>%
    subset(!is.na(Response))%>%
    summarise(avg = mean(Response, na.rm = TRUE),
              count = n()) %>%
    arrange(Question)%>%
    print(n = Inf) %>%
    ggplot(aes(x = factor(!!ind_var),
               y = avg,
               color = Question,
               group = Question)) +
    geom_point()+
    geom_line()

  }

}


explore_func("years_spent",
             "importance",
             0)




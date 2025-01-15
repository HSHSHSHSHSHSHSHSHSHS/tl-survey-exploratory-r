
# Library  -----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(likert)

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
  "#656ca3",
  "#9281db"
)


custom_palette_3 <- c(
  "#a7ce8d",
  "#24525b",
  "#38b881",
  "#00b2b2"
)

custom_palette_4 <- c(
  "#ffde6e",
  "#a7ce8d",
  "#38b881",
  "#00695a",
  "#24525b"
)

custom_palette_5 <- c(
  "#ee356d",
  "#f9a768",
  "#ffde6e",
  "#38b881",
  "#00695a"
)

custom_palette_6 <- c(
  "#38b881",
  "#00695a",
  "#0072b2"
)

custom_palette_7 <- c(
  "#00b2b2",
  "#0072b2",
  "#304a81"
)


custom_palette_8 <- c(
  "#ee356d",
  "#f9a768",
  "#38b881",
  "#00695a"
)


# Hehe levels -------------------------------------------------------------

question_levels <- startyear_df_INC_EX %>%
  pivot_longer(cols = starts_with(focet),
               names_to = "Question",
               values_to = "Response") %>%
  mutate(Question = gsub(focet, "", Question),
         Response = ifelse(!is.na(Response),
                           ifelse(Response == 1,
                                  "Yes", "No"),
                           NA),
         Question = factor(Question)) %>%
  group_by(start_year,
           Question,
           Response) %>%
  summarise(Count = n()) %>%
  mutate(pct = Count / sum(Count) * 100) %>%
  subset(Response == "Yes") %>%
  filter(!is.na(Response)) %>%
  ungroup() %>%
  mutate(Question = fct_reorder(Question, pct, .desc = TRUE)) %>%
  mutate(Question = fct_relevel(Question, "_other",
                                "_na_engage", after = Inf)) %>%

  pull(Question) %>%
  levels()

# Faceted bar -------------------------------------------------------------

focet <- "another_variable_name"

this_n_startyear_FACET <- startyear_df_INC_EX %>%
  #dplyr::select(-na_engage)%>%
  dplyr::select(starts_with(focet))%>%
  dplyr::filter(if_any(everything(), ~ . == 1)) %>%
  filter(!if_all(everything(), is.na)) %>%
  nrow()

c_2_3 <- startyear_df_INC_EX %>%
  #dplyr::select(-na_engage)%>%
  pivot_longer(cols = starts_with(focet),
               names_to = "Question",
               values_to = "Response") %>%
  mutate(Question = gsub(focet, "", Question),
         Response = ifelse(!is.na(Response),
                           ifelse(Response == 1,
                                  "Yes", "No"),
                           NA),
         Question = factor(Question)) %>%
  group_by(start_year,
           Question,
           Response) %>%
  summarise(Count = n()) %>%
  mutate(pct = Count / sum(Count) * 100) %>%
  subset(Response == "Yes")%>%
  filter(!is.na(Response))%>%
  ungroup()%>%
  # mutate(Question = fct_collapse(Question,
  #                                "_m" = c("_m_forum", "_m_discord"),
  #                                "_misc_discord" = c("_b_discord"),
  #                                "_misc_forum" = c("_u_forum", "_nu_forums", "_e_forum")
  # )) %>%
  mutate(Question = fct_reorder(Question, pct, .desc = TRUE))%>%
  mutate(Question = fct_relevel(Question, "_other",
                              "_na_engage", after = Inf)) %>%
  ggplot(aes(x = start_year,
             y = pct,
             fill = Question)) +
  geom_bar(stat = "identity",
           position = "dodge")+
  facet_wrap(~Question)+
  scale_fill_manual(values = custom_palette_2) +
  ggtitle("Title here?\n(Grouped by start year, in %)")+
  labs(subtitle = paste0("(Multiple options, n = ", this_n_startyear_FACET, ")"))+
  theme(
    plot.title = element_text(family = "Avenir", face = "bold", size = 14),
    plot.subtitle = element_text(family = "Avenir", face = "bold", size = 12),
    axis.title.y = element_text(family = "Avenir"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      family = "Avenir",
      size = 10,
      margin = margin(t = 15),
      angle = 50
    ),
    axis.text.y = element_text(family = "Avenir", size = 8),
    legend.position = "none",
    strip.text = element_text(family = "Avenir", size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(color = "gray90", size = 0.25),
    #panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.box = "none",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    axis.ticks.x = element_blank()
  )

c_2_3
ggsave(paste0("startyear_facet_", focet, ".png"),
       c_2_3,
       width = 7.4,
       height = 7.4,
       bg="transparent")


this_n_yearsspent_FACET <- years_spent_df %>%
  #dplyr::select(-na_engage)%>%
  dplyr::select(starts_with(focet))%>%
  dplyr::filter(if_any(everything(), ~ . == 1)) %>%
  filter(!if_all(everything(), is.na)) %>%
  nrow()

c_2_2 <- years_spent_df %>%
  #dplyr::select(-na_engage)%>%
  pivot_longer(cols = starts_with(focet),
               names_to = "Question",
               values_to = "Response") %>%
  mutate(Question = gsub(focet, "", Question),
         Response = ifelse(!is.na(Response),
                           ifelse(Response == 1,
                                  "Yes", "No"),
                           NA),
         Question = factor(Question)) %>%
  group_by(years_spent,
           Question,
           Response) %>%
  summarise(Count = n()) %>%
  mutate(pct = Count / sum(Count) * 100) %>%
  subset(Response == "Yes")%>%
  filter(!is.na(Response))%>%
  ungroup()%>%
  # mutate(Question = fct_relevel(Question,
  #                               "_tl",
  #                               "_pr",
  #                               "_other"
  # )) %>%
  mutate(Question = factor(Question, levels = question_levels))%>%
  #mutate(Question = fct_reorder(Question, pct, .desc = TRUE))%>%
  # mutate(Question = fct_relevel(Question, "_other", after = Inf)) %>%
  ggplot(aes(x = fct_rev(years_spent),
             y = pct,
             fill = Question)) +
  geom_bar(stat = "identity",
           position = "dodge")+
  facet_wrap(~Question)+
  scale_fill_manual(values = custom_palette_2) +
  ggtitle("Title?\n(Grouped by length spent, in %)")+
  labs(subtitle = paste0("(Multiple options, n = ", this_n_yearsspent_FACET, ")"))+
  theme(
    plot.title = element_text(family = "Avenir", face = "bold", size = 14),
    plot.subtitle = element_text(family = "Avenir", face = "bold", size = 12),
    axis.title.y = element_text(family = "Avenir"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      family = "Avenir",
      size = 10,
      margin = margin(t = 15),
      angle = 50
    ),
    axis.text.y = element_text(family = "Avenir", size = 8),
    legend.position = "none",
    strip.text = element_text(family = "Avenir", size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(color = "gray90", size = 0.25),
    #panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.box = "none",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    axis.ticks.x = element_blank()
  )

c_2_2
ggsave(paste0("/yearsspent_facet_", focet, ".png"),
       c_2_2,
       width = 7.4,
       height = 7.4,
       bg="transparent")


# Beside bar --------------------------------------------------------------

q_start <- "device"
q_start <- ifelse(
  question_types %>%
    filter(Shortq == q_start) %>%
    pull(Type) == "Multi-write",
  paste0(q_start, "_combined_"),
  q_start
)

this_n_startyear <- startyear_df_INC_EX %>%
  dplyr::select(starts_with(q_start))%>%
  filter(!if_all(everything(), is.na)) %>%
  nrow()

c <- startyear_df_INC_EX %>%
  pivot_longer(cols = starts_with(q_start),
               #cols = ends_with(q_start),
               names_to = "Question",
               values_to = "Response") %>%
  mutate(Question = gsub(q_start, "", Question),
         Response = ifelse(!is.na(Response),
                           ifelse(Response == 1,
                                  "Yes", "No"),
                           NA)) %>%
  group_by(start_year,
           Question,
           Response) %>%
  summarise(Count = n()) %>%
  mutate(pct = Count / sum(Count)) %>%
  subset(Response == "Yes")%>%
  arrange(Question,
          start_year)%>%
  filter(!is.na(Response))%>%
  filter(Question != "no")%>%
  ungroup%>%
  mutate(Question = fct_reorder(Question, pct, .desc = TRUE))%>%
  # mutate(Question = fct_relevel(Question,
  #                               "other",
  #                               after = Inf))%>%
  #mutate(Question = fct_relevel(Question, "jp", "en", "cn", "kr", "ind", "vi", "other"))%>%
  #mutate(Question = fct_relevel(Question, "jp", "kr", "cn", "en", "ind", "fr", "spa", "vi", "other"))%>%
  ggplot(aes(x = start_year,
             y = pct,
             fill = Question)) +
  geom_bar(stat = "identity",
           position = "dodge")+
  #facet_wrap(~start_year)+
  scale_fill_manual(values = custom_palette_1) +
  #ggtitle("What languages do you translate from?\n(Grouped by start year, in %)")+
  #labs(subtitle = paste0("(Multiple options, n = ", this_n_startyear, ")"))+
  scale_y_continuous(labels = scales::percent) +
  theme(
    plot.title = element_text(family = "Avenir", face = "bold", size = 14),
    plot.subtitle = element_text(family = "Avenir", face = "bold", size = 12),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      family = "Avenir",
      size = 10,
      margin = margin(t = 15),
      angle = 50
    ),
    axis.text.y = element_text(family = "Avenir", size = 8),
    legend.text = element_text(family = "Avenir", size = 11),
    legend.title = element_text(family = "Avenir", size = 12),
    #legend_title = element_blank(),
    strip.text = element_text(family = "Avenir", size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(color = "gray90", size = 0.25),
    #panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.box = "none",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    axis.ticks.x = element_blank(),
    #legend.position = "none"
  )

c
# ggsave(paste0("startyear_", q_start, ".png"),
#        c,
#        width = 7.4,
#        height = 3.7,
#        bg="transparent")


this_n_yearsspent <- years_spent_df %>%
  dplyr::select(starts_with(q_start))%>%
  filter(!if_all(everything(), is.na)) %>%
  nrow()


c_2 <- years_spent_df %>%
  pivot_longer(cols = starts_with(q_start),
               #cols = ends_with(q_start),
               names_to = "Question",
               values_to = "Response") %>%
  mutate(Question = gsub(q_start, "", Question),
         Response = ifelse(!is.na(Response),
                           ifelse(Response == 1,
                                  "Yes", "No"),
                           NA)) %>%
  group_by(years_spent,
           Question,
           Response) %>%
  summarise(Count = n()) %>%
  mutate(pct = Count / sum(Count) * 100) %>%
  subset(Response == "Yes")%>%
  filter(!is.na(Response))%>%
  ungroup()%>%
  #mutate(Question = fct_reorder(Question, pct, .desc = TRUE))%>%
  # mutate(Question = fct_relevel(Question,
  #                               "_never",
  #                               "_if_proofread",
  #                               "_if_unavailable",
  #                               "_always",
  #                               "other", after = Inf)) %>%
  #mutate(Question = fct_relevel(Question, "jp", "kr", "cn", "en", "ind", "fr", "spa", "vi", "other"))%>%
  ggplot(aes(x = fct_rev(years_spent),
             y = pct,
             fill = Question)) +
  geom_bar(stat = "identity",
           position = "dodge")+
  scale_fill_manual(values = custom_palette_1) +
  labs(subtitle = paste0("(Multiple options, n = ", this_n_yearsspent, ")"))+
  theme(
    plot.title = element_text(family = "Avenir", face = "bold", size = 14),
    plot.subtitle = element_text(family = "Avenir", face = "bold", size = 12),
    axis.title.y = element_text(family = "Avenir"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      family = "Avenir",
      size = 10,
      margin = margin(t = 10),
      angle = 30
    ),
    axis.text.y = element_text(family = "Avenir", size = 8),
    legend.text = element_text(family = "Avenir", size = 11),
    legend.title = element_text(family = "Avenir", size = 12),
    strip.text = element_text(family = "Avenir", size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(color = "gray90", size = 0.25),
    #panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.box = "none",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    axis.ticks.x = element_blank()
  )

c_2
ggsave(paste0("yearsspent", q_start, ".png"),
       c_2,
       width = 7.4,
       height = 3.7,
       bg="transparent")
# Stacked bar -------------------------------------------------------------

q_single <- "var"

a <- startyear_df_INC_EX %>%
  #mutate(!!sym(q_single) := fct_infreq(!!sym(q_single))) %>%
  filter(!is.na(.data[[q_single]])) %>%
  mutate(school = fct_relevel(school,
                                  "No",
                                  "Yes, and I do not use it.",
                                  "Yes, and I no longer use it."))%>%
  ggplot() +
  geom_bar(
    aes(x = start_year, fill = !!sym(q_single)),
    position = "fill"
  ) +
  #ggtitle("What software do you mainly use to 'edit'?\n(Grouped by length spent, in %)")+
  # labs(subtitle = paste0("(n = ",
  #                        nrow(years_spent_df %>% filter(!is.na(.data[[q_single]]))),
  #                        ")"))+
  scale_fill_manual(values = custom_palette_4) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Avenir", face = "bold", size = 14),
    plot.subtitle = element_text(family = "Avenir", face = "bold", size = 12),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    #axis.title.x = element_text(family = "Avenir", margin = margin(t = 10), size = 14),
    axis.text.x = element_text(
      family = "Avenir",
      size = 10,
      margin = margin(t = 0),
      #angle = 40
    ),
    axis.text.y = element_text(family = "Avenir", size = 8),
    legend.text = element_text(family = "Avenir", size = 11),
    legend.title = element_text(family = "Avenir", size = 12),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.position = "none",
    legend.box = "none",
    legend.background = element_blank(),
    legend.box.background = element_blank()
  )
a
ggsave(paste0("/start_year_SMALL", q_single, ".png"),
       a,
       width = 8,
       height = 4,
       bg="transparent")


a_2 <- years_spent_df %>%
  # mutate(!!sym(q_single) := fct_infreq(!!sym(q_single))) %>%
  # mutate(main_platform_combined = fct_relevel(main_platform_combined,
  #                                             "Other",
  #                                             after = Inf))%>%
  #   mutate(re_tl_opinion = fct_relevel(re_tl_opinion,
  #                                      "Should always be avoided.",
  #                                      "Only if no alternative",
  #                                      "Only if few source->target translators",
  #                                      "Are fine."
  #   ))%>%
  # mutate(loc_how_combined = fct_relevel(loc_how_combined,
  #                                     "Other",
  #                                     after = Inf))%>%
  mutate(years_spent = fct_rev(years_spent))%>%
  filter(!is.na(.data[[q_single]])) %>%
  ggplot() +
  geom_bar(
    aes(x = years_spent, fill = !!sym(q_single)),
    position = "fill"
  ) +
  ggtitle("Title\n(Grouped by length spent, in %)")+
  labs(subtitle = paste0("(n = ",
                         nrow(years_spent_df %>% filter(!is.na(.data[[q_single]]))),
                         ")"))+
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
      margin = margin(t = 0),
      #angle = 40
    ),
    axis.text.y = element_text(family = "Avenir", size = 8),
    legend.text = element_text(family = "Avenir", size = 11),
    legend.title = element_text(family = "Avenir", size = 12),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.box = "none",
    legend.background = element_blank(),
    legend.box.background = element_blank()
  )
a_2
ggsave(paste0("yearsspent_", q_single, ".png"),
       a_2,
       width = 7.4,
       height = 3.7,
       bg="transparent")


# Summary view ------------------------------------------------------------
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
       width = 7.4,
       height = 7.4,
       bg="transparent")


# Mosaic Means ------------------------------------------------------------------

q_end <- "_difficulty"

d <- startyear_df %>%
  pivot_longer(cols = ends_with(q_end),
               names_to = "Question",
               values_to = "Response") %>%
  mutate(Question = gsub(q_start, "", Question)) %>%
  group_by(not_ex, start_year, Question) %>%
  subset(!is.na(Response)) %>%
  summarise(avg = mean(Response, na.rm = TRUE),
            count = n()) %>%
  arrange(desc(avg)) %>%
  mutate(Question = factor(Question, levels = unique(Question))) %>%
  print(n = Inf) %>%
  ggplot(aes(x = factor(start_year),
             y = Question,
             fill = avg)) +
  geom_tile() +
  scale_fill_gradient(low = "#a7ce8d", high = "#24525B") +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    text = element_text(family = "Avenir", size = 12),
    axis.text.x = element_text(angle = 20, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.text = element_text(size = 12)
  )
d
ggsave(paste0("years_spent_difficulty.png"),
       d,
       width = 7.4,
       height = 5,
       bg="transparent")



# Mosaic summary ----------------------------------------------------------
m <- startyear_df %>%
  mutate(years_spent = fct_rev(years_spent))%>%
  ggplot()+
  geom_mosaic(aes(x = product(start_year),
                  fill = years_spent)) +
  scale_fill_manual(values = custom_palette_1) +
  ggtitle("Mosaic plot of 'What year did you start?'\nby 'What is the total amount of time you have spent?'")+
  labs(subtitle = paste0("(n = ", nrow(startyear_df_INC_EX), ")"))+
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Avenir", face = "bold", size = 12),
    plot.subtitle = element_text(family = "Avenir", face = "bold", size = 10),
    axis.title.y = element_blank(),
    axis.title.x = element_text(family = "Avenir", margin = margin(t = 10), size = 12),
    axis.text.x = element_text(
      family = "Avenir",
      size = 10,
      margin = margin(t = 10),
      angle = 25
    ),
    axis.text.y = element_blank(),
    legend.text = element_text(family = "Avenir", size = 10),
    legend.title = element_text(family = "Avenir", size = 11),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.box = "none",
    legend.background = element_blank(),
    legend.box.background = element_blank()
  )
m
ggsave(paste0("STARTYEAR_YEARSSPENT.png"),
       m,
       width = 7.4,
       height = 7.4,
       bg="transparent")












# Like me -----------------------------------------------------------------

levels(startyear_df_INC_EX$community_change) <- likert_order
levels(startyear_df_INC_EX$quality_change) <- likert_order
levels(years_spent_df$community_change) <- likert_order
levels(years_spent_df$quality_change) <- likert_order

##

start_year_likert_community <- startyear_df_INC_EX%>%
  mutate(community_change = fct_recode(community_change,
                                        "Significantly worse" = "No, it has become significantly worse.",
                                        "Slightly worse" = "No, it has become slightly worse.",
                                        "Stayed the same" = "It has stayed around the same.",
                                        "Slightly better" = "Yes, it has improved slightly.",
                                        "Significantly better" = "Yes, it has improved significantly."),
         start_year = fct_rev(start_year))%>%
  dplyr::select(start_year,
         community_change)%>%
  group_by(start_year,
           community_change)%>%
  summarise(n = n())%>%
  filter(!is.na(community_change))%>%
  mutate(pct = round(n/sum(n) * 100, 0))%>%
  dplyr::select(-n)%>%
  arrange(community_change)%>%
  pivot_wider(names_from = community_change,
              values_from = pct)%>%
  rename(Item = start_year)%>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))


l <- plot(likert(summary = start_year_likert_community),
     group.order = levels(start_year_likert_community$Item),
     text.family = "Avenir",
     #plot.percent.high=FALSE,
     #plot.percent.low=FALSE,
     plot.percent.neutral=FALSE) +
  scale_fill_manual(values = custom_palette_5)+
  ggtitle("Has the community improved since you began?")+
  labs(subtitle = paste0("(n = ", sum(!is.na(startyear_df_INC_EX$community_change)), ")"))+
  theme_minimal(base_family = "Avenir") +
  theme(
    plot.title = element_text(family = "Avenir", face = "bold", size = 14),
    plot.subtitle = element_text(family = "Avenir", face = "bold", size = 12),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    legend.title = element_blank(),
    axis.text = element_text(family = "Avenir")
  )

l
ggsave(paste0("STARTYEAR_community_likert.png"),
       l,
       width = 7.4,
       height = 4,
       bg="transparent")


# QUICK -------------------------------------------------------------------
library(scales)

g <- startyear_df_NO_EX %>%
  group_by(start_year) %>%
  summarize(
    # pct = 1 - (sum(tl_mtl == "No", na.rm = TRUE) /
    #   sum(!is.na(tl_mtl))),
    pct = (sum(scene_combined_discord == 1, na.rm = TRUE) /
                 sum(!is.na(scene_combined_discord))),
    .groups = "drop"
  ) %>%
  mutate(avg_pct = mean(pct, na.rm = TRUE),
         # gah = ifelse(start_year %in% c("1970_2010", "2011_2015", "2016"), "gen1",
         #              ifelse(start_year %in% c("2017", "2018", "2019", "2020"), "gen2", "gen3")))%>%
         gah = ifelse(start_year %in% c("1970_2010", "2011_2015", "2016", "2017"), "gen1", "gen2"))%>%
         # gah = ifelse(start_year == "1970_2010", "gen1",
  #            ifelse(start_year %in% c("2011_2015", "2016", "2017", "2018"), "gen2", "gen3")))%>%
  # filter(!is.na(main_platform_combined))%>%
   #group_by(start_year)%>%
  # mutate(gah = main_software_combined)%>%
  # filter(!is.na(gah))%>%
  # mutate(gah = fct_recode(gah,
  #                         "Photoshop" = "Photoshop CC",
  #                         "Photoshop" = "Photoshop CS6",
  #                         "Photoshop" = "Photoshop_other"
  # ))%>%
  # group_by(start_year, gah)%>%
  # summarize(n = n())%>%
  # mutate(pct = n/sum(n))%>%
  # mutate(gah = fct_relevel(gah,
  #                          "Photoshop",
  #                          "Gimp",
  #                          "Clip Studio Paint (CSP)",
  #                          "Photopea",
  #                          "ibisPaint"),
  #        gah = fct_rev(gah))%>%
  # ungroup()%>%
  # group_by(gah)%>%
  # mutate(avg_pct_gah = mean(pct))%>%
  # print(n = Inf)
  # #mutate(avg_pct = mean(pct))%>%
  ggplot(aes(x = start_year,
             y = pct,
             fill = "#00695a"
             #fill = gah
             ))+
  geom_bar(stat = "identity",
           #position = "fill"
           position = "dodge",
           alpha = 0.5
  ) +
  #geom_hline(aes(yintercept = avg_pct), color = "#24525b") +
  geom_text(aes(label = sprintf("%.1f%%", pct*100)),
            vjust = -0.5, color = "#24525b", size = 3, family = "Avenir") +
  scale_fill_manual(values = "#656ca3") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    #plot.title = element_text(family = "Avenir", face = "bold", size = 25),
    #plot.subtitle = element_text(family = "Avenir", face = "bold", size = 12),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      family = "Avenir",
      size = 12,
      margin = margin(t = 0),
      #angle = 40
    ),
    axis.text.y = element_text(family = "Avenir", size = 12),
    legend.position="none",
    legend.text = element_text(family = "Avenir", size = 11),
    legend.title = element_text(family = "Avenir", size = 12),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.box = "none",
    legend.background = element_blank(),
    legend.box.background = element_blank()
  )
g
ggsave(paste0("small", "_disc_CURRENTONLY", ".png"),
       g,
       width = 8.4,
       height = 3.7,
       # width = 8.4,
       # height = 7,
       bg="transparent")

# q -----------------------------------------------------------------------

qq <- startyear_df_INC_EX %>%
  dplyr::select(c(starts_with("device_"), "start_year"))%>%
  dplyr::filter(
    !is.na(device_phone) | !is.na(device_tablet) | !is.na(device_desktop_pc) | !is.na(device_laptop)
  )%>%
  mutate(
    device_2 = ifelse(
      ((device_phone == 1 | device_tablet == 1) & (device_laptop == 0 & device_desktop_pc == 0)),
                      "mobile only",
      ifelse(((device_laptop == 1 | device_desktop_pc == 1) & (device_phone == 0 & device_tablet == 0)),
             "computer only", "both"))
  )%>%
  mutate(g = ifelse(start_year %in% c("1970_2010", "2011_2015", "2016", "2017"), "gen1", "gen2")) %>%
  group_by(g, device_2) %>%
  #group_by(start_year, device_2)%>%
  # dplyr::select(c(starts_with("all_platform_combined_"), "start_year"))%>%
  summarize(a = n())%>%
  mutate(pct = a/sum(a))%>%
  mutate(device_2 = fct(device_2))%>%
  ggplot() +
  geom_bar(
    aes(x = start_year,
        fill = device_2),
    position = "fill"
  )+
  scale_fill_manual(values = custom_palette_7) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Avenir", face = "bold", size = 14),
    plot.subtitle = element_text(family = "Avenir", face = "bold", size = 12),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      family = "Avenir",
      size = 10,
      margin = margin(t = 0),
      #angle = 40
    ),
    axis.text.y = element_text(family = "Avenir", size = 8),
    legend.text = element_text(family = "Avenir", size = 11),
    legend.title = element_text(family = "Avenir", size = 12),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.box = "none",
    legend.background = element_blank(),
    legend.box.background = element_blank()
  )
qq
ggsave(paste0("small_device_2" , ".png"),
       qq,
       width = 8.4,
       height = 3.7,
       bg="transparent")



qwq <- startyear_df_INC_EX %>%
  dplyr::select(c(starts_with("all_platform_combined_external"), "start_year"))%>%
  filter(!is.na(all_platform_combined_external))%>%
  group_by(start_year)%>%
  summarize(my_pct = sum(all_platform_combined_external == 1)/sum(!is.na(all_platform_combined_external)))%>%
  mutate(avg_pct = mean(my_pct, na.rm = TRUE))%>%
  ggplot(aes(x = start_year, y = my_pct)) +
  geom_text(aes(label = sprintf("%.1f%%", my_pct*100)),
            vjust = -0.5, color = "#24525b", size = 3, family = "Avenir") +
  geom_bar(stat = "identity", position = "dodge", fill = custom_palette_1[3]) +
  geom_hline(aes(yintercept = avg_pct), color = "#24525b") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Avenir", face = "bold", size = 14),
    plot.subtitle = element_text(family = "Avenir", face = "bold", size = 12),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      family = "Avenir",
      size = 7,
      margin = margin(t = 0),
      #angle = 40
    ),
    axis.text.y = element_text(family = "Avenir", size = 8),
    legend.text = element_text(family = "Avenir", size = 11),
    legend.title = element_text(family = "Avenir", size = 12),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    legend.box = "none",
    legend.background = element_blank(),
    legend.box.background = element_blank()
  )
qwq
ggsave(paste0("external" , ".png"),
       qwq,
       width = 3.7,
       height = 3.7,
       bg="transparent")

# Corcle ------------------------------------------------------------------

library(broom)

df_summary <- startyear_df_INC_EX %>%
  filter(start_year == "2022")%>%
  gather(key = "variable", value = "value", mtl_opinion_always, mtl_opinion_if_unavailable, mtl_opinion_if_proofread, mtl_opinion_never) %>%
  group_by(variable, value) %>%
  summarise(count = n()) %>%
  ungroup()%>%
  filter(!is.na(value))%>%
  group_by(variable) %>%
  mutate(fraction = count / sum(count),
         ymax = cumsum(fraction),
         ymin = c(0, ymax[-length(ymax)]))

baseNum <- 8
df_summary$xmax <- as.numeric(as.factor(df_summary$variable)) + baseNum
df_summary$xmin <- df_summary$xmax - 1
df_summary$fill_color <- ifelse(df_summary$variable == "mtl_opinion_always" & df_summary$value == 1,
                                custom_palette_3[1],
                          ifelse(df_summary$variable == "mtl_opinion_if_unavailable" & df_summary$value == 1,
                                       custom_palette_3[2],
                          ifelse(df_summary$variable == "mtl_opinion_if_proofread" & df_summary$value == 1,
                                custom_palette_3[3],
                          ifelse(df_summary$variable == "mtl_opinion_never" & df_summary$value == 1,
                            custom_palette_3[4],
                            "#fcf5ed"
  ))))

wa <- ggplot(df_summary, aes(fill = fill_color,
                             ymax = ymax, ymin = ymin,
                             xmax = xmax, xmin = xmin)) +
  geom_rect(colour = NA) +
  scale_fill_manual(values = c(custom_palette_1[1],
                               custom_palette_1[2],
                               custom_palette_1[3],
                               custom_palette_1[4],
                               "#fcf5ed")) +
  coord_polar(theta = "y") +
  xlim(c(0, 12)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        legend.position = "none")

wa
ggsave(paste0("mtl_opinion_2022" , ".png"),
       wa,
       width = 3.7,
       height = 3.7,
       bg="transparent")

##

wah <- startyear_df_INC_EX%>%
  filter(start_year == "1970_2010",
         !is.na(mu))%>%
  count(mu)%>%
  mutate(pct = n/sum(n))%>%
  mutate(ymax = cumsum(pct),
         ymin = c(0, head(ymax, -1)))%>%
  ggplot(aes(fill = mu,
             x = 1,
             y = pct,
             ymax = ymax,
             ymin = ymin)) +
  geom_bar(stat = "identity", width = 0.4) +
  scale_fill_manual(values = c(custom_palette_3[3],
                               custom_palette_3[1],
                               "#fcf5ed")) +
  coord_polar(theta = "y") +
  xlim(c(0, 1.5)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

wah
ggsave(paste0("u_use_heard", "1970_2010", ".png"),
       wah,
       width = 3.7,
       height = 3.7,
       bg="transparent")


# mini like ---------------------------------------------------------------

likeeee <- plot(likert(summary = start_year_likert_quality),
               group.order = levels(start_year_likert_quality$Item),
               text.family = "Avenir",
               # plot.percent.high=FALSE,
               # plot.percent.low=FALSE,
               plot.percent.neutral=FALSE,
               include.center=FALSE) +
  scale_fill_manual(values = custom_palette_8)+
  theme_minimal(base_family = "Avenir") +
  theme(
    legend.position = "none",
    plot.title = element_text(family = "Avenir", face = "bold", size = 14),
    plot.subtitle = element_text(family = "Avenir", face = "bold", size = 12),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    legend.title = element_blank(),
    axis.text = element_text(family = "Avenir"),
    axis.title.x = element_blank(),
  )

likeeee

ggsave(paste0("small_qual_like.png"),
       likeeee,
       width = 8,
       height = 4,
       bg="transparent")



likeeee_neut <- start_year_likert_quality %>%
  ggplot()+
  geom_col(aes(x = `Stayed the same`,
               y = fct_rev(Item)),
           fill = custom_palette_5[3])+
  geom_text(aes(x = `Stayed the same`,
                y = fct_rev(Item),
                label = paste0(round(`Stayed the same`, 1), "%")),
            hjust = -0.3,  # Adjust horizontal alignment
            size = 3,
            family = "Avenir")+
  theme_minimal(base_family = "Avenir") +
  theme(
    legend.position = "none",
    plot.title = element_text(family = "Avenir", face = "bold", size = 14),
    plot.subtitle = element_text(family = "Avenir", face = "bold", size = 12),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(family = "Avenir")
  )

likeeee_neut

ggsave(paste0("small_qual_like_neut.png"),
       likeeee_neut,
       width = 5,
       height = 7,
       bg="transparent")

##


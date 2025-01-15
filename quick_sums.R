# Library  -----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(stringr)


# basic -------------------------------------------------------------------

thing <- "variable_name"

years_spent_df%>%
  group_by(years_spent)%>%
  count(!!sym(thing))%>%
  filter(!is.na(!!sym(thing)))%>%
  mutate(prop = n/sum(n)*100)%>%
  arrange(!!sym(thing))%>%
  print(n=Inf)

startyear_df_INC_EX %>%
  # mutate(g = ifelse(start_year == "1970_2010", "gen1",
  #                   ifelse(start_year %in% c("2011_2015", "2016", "2017", "2018"), "gen2", "gen3"))) %>%
  # group_by(g) %>%
  group_by(start_year)%>%
  #summarize(avg = mean(!!sym(thing), na.rm = TRUE))%>%
  count(!!sym(thing)) %>%
  filter(!is.na(!!sym(thing))) %>%
  mutate(prop = n / sum(n) * 100) %>%
  arrange(!!sym(thing)) %>%
  print(n = Inf)


# a -----------------------------------------------------------------------

big_thing <- "another_variable_name"

big_thing <- ifelse(
  question_types %>%
    filter(Shortq == big_thing) %>%
    pull(Type) == "Multi-write",
  paste0(big_thing, "_combined_"),
  big_thing
)

years_spent_df %>%
  pivot_longer(cols = starts_with(big_thing),
               names_to = "Question",
               values_to = "Response") %>%
  mutate(Question = gsub(big_thing, "", Question),
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
  arrange(Question,
          years_spent
          )%>%
  print(n = Inf)

startyear_df_INC_EX %>%
  pivot_longer(cols = starts_with(big_thing),
               names_to = "Question",
               values_to = "Response") %>%
  mutate(Question = gsub(big_thing, "", Question),
         Response = ifelse(!is.na(Response),
                           ifelse(Response == 1,
                                  "Yes", "No"),
                           NA)) %>%
  group_by(start_year,
           Question,
           Response) %>%
  summarise(Count = n()) %>%
  mutate(pct = Count / sum(Count) * 100) %>%
  subset(Response == "Yes")%>%
  arrange(Question,
          start_year
          )%>%
  print(n = Inf)

# b -----------------------------------------------------------------------


split_df <- split(startyear_df_INC_EX, startyear_df_INC_EX$start_year)

filtered_counts <- lapply(split_df, function(df) {
  nrow(df %>%
         #filter(device_tablet == 1 | device_phone == 1)%>%
         filter(main_software_combined != "Photoshop CC" & main_software_combined != "Photoshop CS6")
       )/
         # filter(device_desktop_pc == 0,
         #        device_laptop == 0))/
    nrow(df)
})

bah_counts <- lapply(split_df, function(df) {
  nrow(df %>%
         filter(device_tablet == 1 | device_phone == 1)%>%
         filter(main_software_combined != "Photoshop CC" & main_software_combined != "Photoshop CS6")
  )/
    # filter(device_desktop_pc == 0,
    #        device_laptop == 0))/
    nrow(df)
})

mapply(`/`, bah_counts, filtered_counts)

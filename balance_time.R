#' ---
#' title: "Sarah's time"
#' date: "`r Sys.Date()`"
#' output: pdf_document
#' ---

#+ echo=FALSE
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

# Process clockify report
library(tidyverse)
library(clockify)

# Date to project from. End of Clockify import (exclusive), start of outlook import
proj_date <- "2025-04-01"

set_api_key(keyring::key_get("Clockify_API"))

dat_in <- reports_detailed("2025-04-01", proj_date)

time_allot <- tribble(
  ~project_name, ~prop_time_ideal,
  "Ilona",    0.4,
  "Josie",    0.4,
  "LERS",     0.1,
  "ccviR",    0.1,
  "to add",   0,
)

# get previous years hours
dat_old <- list.files(pattern = "hours") %>% map(\(x) read_csv(x, show_col_types = FALSE)) %>% list_rbind()

dat <- dat_in %>%
  select(project_name, task_name, start, end, duration) %>%
  mutate(duration = duration/60/60) %>%
  mutate(across(c(start, end), lubridate::as_datetime))%>%
  bind_rows(dat_old)

# On April first store old hours in csv
# write_csv(dat, "hours2024-2025.csv")

current_status <- dat %>%
  mutate(project_name = ifelse(project_name == "WIST", "LERS", project_name)) %>%
  group_by(project_name) %>%
  filter(project_name != "Administrative") %>%
  summarise(project_time = sum(duration)) %>%
  left_join(time_allot, by = join_by(project_name)) %>%
  mutate(prop_time = project_time/sum(project_time),
         time_ideal = sum(project_time)*prop_time_ideal,
         dif_time = round(time_ideal - project_time, 2),
         dif_time_days = dif_time/8 %>% round(2))


admin_time <- dat %>% filter(project_name == "Administrative") %>%
  group_by(project_name) %>%
  summarise(project_time = sum(duration))
# import outlook calendar

# In Outlook go to calendar>File>Export>csv
o_cal_in <- read_csv("outlook_cal.csv", show_col_types = FALSE)

o_cal1 <- o_cal_in %>%
  filter(str_detect(Subject, "Sarah w")) %>%
  mutate(project_name = str_remove(Subject, "Sarah w "),
         start = lubridate::mdy(`Start Date`),
         end = lubridate::mdy(`End Date`),
         n_days = interval(start, end)/days(1) - 1,
         .keep = "none") %>%
  # need to separate longer appointments into individual days or else there is overlap
  rowwise() %>%
  mutate(day = list(start + lubridate::days(0:n_days))) %>%
  unnest(day) %>%
  mutate(duration = 8) %>%
  filter(day >= lubridate::ymd(proj_date))

o_cal <- o_cal1 %>%
  group_by(project_name) %>%
  summarise(project_time = sum(duration))

# Compare to expected time
projected_status <- dat %>%
  filter(project_name != "Administrative") %>%
  mutate(project_name = ifelse(project_name == "WIST", "LERS", project_name)) %>%
  # add_row(project_name = "to add", duration = 8*30) %>%
  group_by(project_name) %>%
  summarise(project_time = sum(duration)) %>%
  bind_rows(o_cal) %>%
  group_by(project_name) %>%
  summarise(project_time = sum(project_time)) %>%
  left_join(time_allot, by = join_by(project_name)) %>%
  mutate(prop_time = project_time/sum(project_time),
         time_ideal = sum(project_time)*prop_time_ideal,
         dif_time = round(time_ideal - project_time, 2),
         dif_time_days = dif_time/8 %>% round(2))


#' Negative values indicate that the project is over its allocated time and
#' positive values show the amount of time owing to the project. Administrative
#' time includes LERS team meetings, mandatory training, vacation and sick leave.
#' It is not included in the time allocation calculation so it is shared
#' equally among all projects.
#'

#' ## `r paste0("Current status as of ", max(as.character(lubridate::as_date(dat$end))), ":\n")`
current_status %>%
  bind_rows(admin_time) %>% knitr::kable(digits = 4)

#' ## `r paste0("Projected status as of ", max(as.character(o_cal1$day)), ":\n")`
projected_status %>% knitr::kable(digits = 4)


# rmarkdown::render("balance_time.R", output_file = paste0("Sarah_time_report_", Sys.Date()))

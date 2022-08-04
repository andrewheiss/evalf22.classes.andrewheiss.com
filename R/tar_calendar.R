suppressPackageStartupMessages(library(lubridate))
library(glue)
library(calendar)

# Once again, {targets} needs the path of the saved file so we return it here
save_ical <- function(df, path) {
  calendar::ic_write(df, path)
  return(path)
}

# Read the schedule CSV file and create/format columns for displaying on the
# schedule page. Returns a data frame with all rows nested by group to make it
# easier to display the schedule by group
build_schedule_for_page <- function(schedule_file) {
  schedule <- read_csv(schedule_file, show_col_types = FALSE) %>%
    mutate(group = fct_inorder(group)) %>%
    mutate(subgroup = fct_inorder(subgroup)) %>%
    mutate(var_note = ifelse(!is.na(note),
                             glue('<br><span class="content-note">{note}</span>'),
                             glue(""))) %>%
    mutate(var_title = ifelse(!is.na(content),
                              glue('<span class="content-title">{title}</span>'),
                              glue('{title}'))) %>%
    mutate(var_deadline = ifelse(!is.na(deadline),
                                 glue('&emsp;&emsp;<small>(submit by {deadline})</small>'),
                                 glue(""))) %>%
    mutate(var_content = ifelse(!is.na(content),
                                glue('<a href="{content}.qmd"><i class="fa-solid fa-book-open-reader fa-lg"></i></a>'),
                                glue('<font color="#e9ecef"><i class="fa-solid fa-book-open-reader fa-lg"></i></font>'))) %>%
    mutate(var_example = ifelse(!is.na(example),
                                glue('<a href="{example}.qmd"><i class="fa-solid fa-laptop-code fa-lg"></i></a>'),
                                glue('<font color="#e9ecef"><i class="fa-solid fa-laptop-code fa-lg"></i></font>'))) %>%
    mutate(var_assignment = ifelse(!is.na(assignment),
                                   glue('<a href="{assignment}.qmd"><i class="fa-solid fa-pen-ruler fa-lg"></i></a>'),
                                   glue('<font color="#e9ecef"><i class="fa-solid fa-pen-ruler fa-lg"></i></font>'))) %>%
    mutate(col_date = ifelse(is.na(date_end),
                             glue('<strong>{format(date, "%B %e")}</strong>'),
                             glue('<strong>{format(date, "%B %e")}</strong>–<strong>{format(date_end, "%B %e")}</strong>'))) %>%
    mutate(col_title = glue('{var_title}{var_deadline}{var_note}')) %>%
    mutate(col_content = var_content,
           col_example = var_example,
           col_assignment = var_assignment)

  schedule_nested <- schedule %>%
    select(group, subgroup,
           ` ` = col_date, Title = col_title, Content = col_content,
           Example = col_example, Assignment = col_assignment) %>%
    group_by(group) %>%
    nest() %>%
    mutate(subgroup_count = map(data, ~count(.x, subgroup)),
           subgroup_index = map(subgroup_count, ~{
             .x %>% pull(n) %>% set_names(.x$subgroup)
           }))

  return(schedule_nested)
}

# Read the schedule CSV file and create a dataset formatted as iCal data that
# calendar::ic_write() can use
build_ical <- function(schedule_file, base_url, page_suffix, class_number) {
  dtstamp <- ic_char_datetime(now("UTC"), zulu = TRUE)

  schedule <- read_csv(schedule_file, show_col_types = FALSE) %>%
    mutate(session = if_else(is.na(content), glue(""), glue("({subgroup}) "))) %>%
    mutate(summary = glue("{class_number}: {session}{title}")) %>%
    mutate(date_start_dt = date,
           date_end_dt = if_else(is.na(date_end), date_start_dt, date_end)) %>%
    mutate(date_start_cal = map(date_start_dt, ~as.POSIXct(., format = "%B %d, %Y")),
           date_end_cal = map(date_end_dt, ~as.POSIXct(., format = "%B %d, %Y"))) %>%
    mutate(url = coalesce(content, example, assignment),
           url = if_else(is.na(url), glue(""), glue("{base_url}{url}{page_suffix}")))

  schedule_ics <- schedule %>%
    mutate(id = row_number()) %>%
    group_by(id) %>%
    nest() %>%
    mutate(ical = map(data,
                      ~ic_event(start = .$date_start_cal[[1]],
                                end = .$date_end_cal[[1]] + 24*60*60,
                                summary = .$summary[[1]],
                                more_properties = TRUE,
                                event_properties = c("DESCRIPTION" = .$url[[1]],
                                                     "DTSTAMP" = dtstamp)))) %>%
    ungroup() %>%
    select(-id, -data) %>%
    unnest(ical) %>%
    ical() %>%
    rename(`DTSTART;VALUE=DATE` = DTSTART,
           `DTEND;VALUE=DATE` = DTEND)

  return(schedule_ics)
}

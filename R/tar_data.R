suppressPackageStartupMessages(library(scales))
library(ggplot2)

# Helpful functions ----

# When using a file-based target, {targets} requires that the function that
# saves the file returns a path to the file. write_csv() invisibly returns the
# data frame being written, so we need a wrapper function to save the file and
# return the path.
save_csv <- function(df, path) {
  readr::write_csv(df, path)
  return(path)
}

# fs::file_copy() returns a path to the copied file, which is nice for
# {targets}. This is a wrapper function to make it so we only need to specify
# the destination folder; the filename of the copied file will remain the same
copy_file <- function(original_file, new_folder) {
  fs::file_copy(path = original_file,
                new_path = fs::path(new_folder, basename(original_file)),
                overwrite = TRUE)
}


# Pipeline just for generating, saving, and copying data ----
save_data <- list(
  ## Generate or save data ----
  #
  ### Save any data files from packages ----
  # mpg for problem set 1
  tar_target(data_mpg,
             save_csv(ggplot2::mpg,
                      here_rel("files", "data", "package_data", "cars.csv")),
             format = "file"),

  # palmerpenguins for problem set 2
  tar_target(data_penguins,
             save_csv(tidyr::drop_na(palmerpenguins::penguins, body_mass_g),
                      here_rel("files", "data", "package_data", "penguins.csv")),
             format = "file"),

  # gapminder for class 2
  tar_target(data_gapminder,
             save_csv(gapminder::gapminder,
                      here_rel("files", "data", "package_data", "gapminder.csv")),
             format = "file"),

  # injury for diff-in-diff example
  tar_target(data_injury,
             save_csv(wooldridge::injury,
                      here_rel("files", "data", "package_data", "injury.csv")),
             format = "file"),

  # card for IV example
  tar_target(data_card,
             save_csv(wooldridge::card,
                      here_rel("files", "data", "package_data", "card.csv")),
             format = "file"),

  # wage2 for IV example
  tar_target(data_wage2,
             save_csv(wooldridge::wage2,
                      here_rel("files", "data", "package_data", "wage2.csv")),
             format = "file"),

  # wage for problem set 7
  tar_target(data_wage,
             save_csv(
               {wooldridge::wage1 %>%
                   mutate(wage = round(wage, 1)) %>%
                   select(wage, education = educ, n_kids = numdep)},
               here_rel("files", "data", "package_data", "wages.csv")),
             format = "file"),

  ### Generate and save fake data ----
  # Mosquito net data
  tar_target(gen_nets, create_nets()),
  tar_target(data_nets,
             save_csv(gen_nets,
                      here_rel("files", "data", "generated_data", "mosquito_nets.csv")),
             format = "file"),

  # Village RCT data
  tar_target(gen_village, create_village()),
  tar_target(data_village_rct,
             save_csv(gen_village$village_randomized,
                      here_rel("files", "data", "generated_data", "village_randomized.csv")),
             format = "file"),
  tar_target(data_village_obs,
             save_csv(gen_village$village_self_selected,
                      here_rel("files", "data", "generated_data", "village_observational.csv")),
             format = "file"),

  # Fake rain barrel RCT and observational data for problem set 3
  # DAG plots
  tar_target(gen_barrel_dags, create_barrel_dags()),
  tar_target(data_plot_barrel_dag_rct,
             ggsave(here_rel("files", "data", "generated_data", "barrel-dag-rct.png"),
                    plot = gen_barrel_dags$plot_rct_dag,
                    width = 5, height = 2.5, units = "in", bg = "white",
                    dev = grDevices::png, type = "cairo-png", dpi = 300),
             format = "file"),
  tar_target(data_plot_barrel_dag_obs,
             ggsave(here_rel("files", "data", "generated_data", "barrel-dag-observational.png"),
                    plot = gen_barrel_dags$plot_obs_dag,
                    width = 5, height = 2.5, units = "in", bg = "white",
                    dev = grDevices::png, type = "cairo-png", dpi = 300),
             format = "file"),

  # Actual data
  tar_target(gen_barrels, create_barrels()),
  tar_target(data_barrels_rct,
             save_csv(gen_barrels$rain_rct,
                      here_rel("files", "data", "generated_data", "barrels_rct.csv")),
             format = "file"),
  tar_target(data_barrels_obs,
             save_csv(gen_barrels$rain,
                      here_rel("files", "data", "generated_data", "barrels_observational.csv")),
             format = "file"),

  # Fake attendance program for regression discontinuity
  tar_target(gen_attendance, create_attendance()),
  tar_target(data_attendance,
             save_csv(gen_attendance,
                      here_rel("files", "data", "generated_data", "attendance_program.csv")),
             format = "file"),

  # Fake tutoring for regression discontinuity
  tar_target(gen_data_tutoring, create_tutoring()),
  tar_target(gen_data_tutoring_sharp, create_tutoring_sharp(gen_data_tutoring)),
  tar_target(gen_data_tutoring_fuzzy, create_tutoring_fuzzy(gen_data_tutoring)),
  tar_target(data_tutoring_sharp,
             save_csv(gen_data_tutoring_sharp,
                      here_rel("files", "data", "generated_data", "tutoring_program.csv")),
             format = "file"),
  tar_target(data_tutoring_fuzzy,
             save_csv(gen_data_tutoring_fuzzy,
                      here_rel("files", "data", "generated_data", "tutoring_program_fuzzy.csv")),
             format = "file"),

  # Fake father's education for instrumental variables
  tar_target(gen_data_father_educ, create_father_educ()),
  tar_target(data_father_educ,
             save_csv(gen_data_father_educ,
                      here_rel("files", "data", "generated_data", "father_education.csv")),
             format = "file"),

  # Fake bed net compliance for CACE and ITT
  tar_target(gen_data_bed_nets, create_bed_nets()),
  tar_target(gen_data_bed_nets_real, create_bed_nets_real(gen_data_bed_nets)),
  tar_target(data_bed_nets_time_machine,
             save_csv(gen_data_bed_nets,
                      here_rel("files", "data", "generated_data", "bed_nets_time_machine.csv")),
             format = "file"),
  tar_target(data_bed_nets_real,
             save_csv(gen_data_bed_nets_real,
                      here_rel("files", "data", "generated_data", "bed_nets_observed.csv")),
             format = "file"),

  ### Copy files to project folders ----
  tar_target(copy_penguins,
             copy_file(data_penguins,
                       new_folder = here_rel("projects", "problem-set-2", "data"))),
  tar_target(copy_food_health_politics,
             copy_file(here_rel("files", "data", "external_data", "food_health_politics.csv"),
                       new_folder = here_rel("projects", "problem-set-2", "data"))),

  tar_target(copy_plot_barrel_dag_rct,
             copy_file(data_plot_barrel_dag_rct,
                       new_folder = here_rel("projects", "problem-set-3"))),
  tar_target(copy_plot_barrel_dag_obs,
             copy_file(data_plot_barrel_dag_obs,
                       new_folder = here_rel("projects", "problem-set-3"))),
  tar_target(copy_barrels_rct,
             copy_file(data_barrels_rct,
                       new_folder = here_rel("projects", "problem-set-3", "data"))),
  tar_target(copy_barrels_obs,
             copy_file(data_barrels_obs,
                       new_folder = here_rel("projects", "problem-set-3", "data"))),

  tar_target(copy_eitc,
             copy_file(here_rel("files", "data", "external_data", "eitc.dta"),
                       new_folder = here_rel("projects", "problem-set-4", "data"))),

  tar_target(copy_monthly_panel,
             copy_file(here_rel("files", "data", "external_data", "MonthlyPanel.dta"),
                       new_folder = here_rel("projects", "problem-set-5", "data"))),

  tar_target(copy_attendance,
             copy_file(data_attendance,
                       new_folder = here_rel("projects", "problem-set-6", "data"))),

  tar_target(copy_wage,
             copy_file(data_wage,
                       new_folder = here_rel("projects", "problem-set-7", "data"))),
  tar_target(copy_public_housing,
             copy_file(here_rel("files", "data", "external_data", "public_housing.csv"),
                       new_folder = here_rel("projects", "problem-set-7", "data"))),

  tar_target(copy_evaluation,
             copy_file(here_rel("files", "data", "external_data", "evaluation.dta"),
                       new_folder = here_rel("projects", "problem-set-8", "data")))
)


# Data creation functions ----

create_village <- function() {
  set.seed(1234)

  village <- tibble(id = 1:1000) %>%
    mutate(sex = rbinom(n(), 1, 0.6),
           age = rnorm(n(), mean = 35, sd = 10),
           pre_income = rnorm(n(), mean = 800, sd = 100))

  village_self_selected <- village %>%
    mutate(prob_program = (15 * sex) + (1.5 * age) + (0.5 * pre_income / 100),
           prob_program = rescale(prob_program, to = c(0.5, 0.95))) %>%
    mutate(program = rbinom(n(), 1, prob_program)) %>%
    mutate(post_income = 800 + (20 * sex) + (10 * age) + (0.5 * pre_income / 100) +
             (100 * program) + rnorm(n(), 15, 5)) %>%
    select(-prob_program) %>%
    mutate(across(c(age, pre_income, post_income), ~round(., 0)))

  village_randomized <- village %>%
    mutate(program = rbinom(n(), 1, 0.5)) %>%
    mutate(post_income = 800 + (20 * sex) + (10 * age) + (0.5 * pre_income / 100) +
             (100 * program) + rnorm(n(), 15, 5)) %>%
    mutate(sex_num = sex, program_num = program) %>%
    mutate(across(c(age, pre_income, post_income), ~round(., 0))) %>%
    mutate(sex = factor(sex_num, labels = c("Female", "Male")),
           program = factor(program_num, labels = c("No program", "Program")))

  return(lst(village_self_selected, village_randomized))
}


create_nets <- function() {
  num <- 1752

  # Create confounder variables that are related to each other
  mu <- c(income = 900, temperature = 75, health = 50)
  stddev <- c(income = 200, temperature = 10, health = 20)
  lower <- c(income = 100, temperature = 60, health = 5)
  upper <- c(income = 2000, temperature = 90, health = 100)

  # https://stackoverflow.com/a/46563034/120898
  correlations_confounders <- tribble(
    ~var1, ~var2, ~correlation,
    "income", "temperature", 0.2,
    "income", "health", 0.8,
    # "temperature", "health", 0.6,
    "temperature", "health", 0.2,
  ) %>%
    mutate_at(vars(starts_with("var")),
              ~factor(., levels = c("income", "temperature", "health"))) %>%
    xtabs(correlation ~ var1 + var2, ., drop.unused.levels = FALSE) %>%
    '+'(., t(.)) %>%
    `diag<-`(1) %>%
    as.data.frame.matrix() %>% as.matrix()

  # Convert correlation matrix to covariance matrix using fancy math
  cov_matrix_confounders <- stddev %*% t(stddev) * correlations_confounders

  # Force the covariance matrix to be positive definite and symmetric
  # https://stats.stackexchange.com/q/153166/3025
  sigma <- as.matrix(Matrix::nearPD(cov_matrix_confounders)$mat)

  set.seed(123)
  confounders <- tmvtnorm::rtmvnorm(num, mean = mu, sigma = sigma,
                                    lower = lower, upper = upper) %>%
    magrittr::set_colnames(names(mu)) %>% as_tibble() %>%
    mutate(health = round(health, 0),
           temperature = round(temperature, 1))

  set.seed(123)
  mosquito_nets <- tibble(id = 1:num) %>%
    bind_cols(confounders) %>%
    mutate(household = rpois(n(), 2) + 1) %>%
    mutate(enrolled = household > 4 & income < 700) %>%
    mutate(resistance = rescale(rnorm(n(), 0, 1), to = c(5, 95))) %>%
    # Simulate data from a logit model: https://stats.stackexchange.com/a/46525/3025
    # But then do all sorts of weird distortion to change the likelihood of using a net
    mutate(net_effect = (1.85 * income / 10) + (-1.7 * temperature) + (1.8 * health / 10) +
             (150 * enrolled) + (2.9 * household),
           net_diff = net_effect - mean(net_effect),
           net_effect = ifelse(net_diff < 0, net_effect - (net_diff / 2), net_effect),
           net_effect_rescaled = rescale(net_effect, to = c(-2.2, 2.2)),
           inv_logit = 1 / (1 + exp(-net_effect_rescaled)),
           net_num = rbinom(n(), 1, inv_logit),
           net = net_num == 1) %>%
    mutate(malaria_risk_effect = (-5 * income / 10) + (3.9 * temperature) +
             (1.4 * resistance) + (9 * health / 10) + (-80 * net_num),
           malaria_risk_diff = malaria_risk_effect - mean(malaria_risk_effect),
           malaria_risk_effect = ifelse(malaria_risk_diff < 0,
                                        malaria_risk_effect - (malaria_risk_diff / 2),
                                        malaria_risk_effect),
           malaria_risk_effect_rescaled = rescale(malaria_risk_effect, to = c(-2.2, 2.2)),
           malaria_risk = 1 / (1 + exp(-malaria_risk_effect_rescaled)),
           malaria_risk = round(malaria_risk * 100, 0)) %>%
    mutate_at(vars(income, resistance), ~round(., 0)) %>%
    mutate(temperature = (temperature - 32) * 5/9,
           temperature = round(temperature, 1)) %>%
    mutate(malaria_risk = malaria_risk)

  mosquito_nets_final <- mosquito_nets %>%
    select(id, net, net_num, malaria_risk, income, health, household,
           eligible = enrolled, temperature, resistance)

  return(mosquito_nets_final)
}


# Rain barrel dags and data for problem set 3
create_barrel_dags <- function() {
  suppressPackageStartupMessages(library(ggdag))

  node_details <- tribble(
    ~name, ~label, ~x, ~y,
    "barrel", "Rain barrel", 1, 3,
    "water_bill", "Water bill", 3, 3,
    "yard_size", "Yard size", 2, 5,
    "garden", "Home garden", 2, 4,
    "attitude_env", "Environmental attitudes", 2, 2,
    "temperature", "Average temperature", 2, 1
  )

  node_labels <- node_details$label
  names(node_labels) <- node_details$name

  rain_dag <- dagify(water_bill ~ barrel + yard_size + attitude_env + temperature + garden,
                     barrel ~ yard_size + attitude_env + temperature + garden,
                     garden ~ yard_size + attitude_env,
                     exposure = "barrel",
                     outcome = "water_bill",
                     coords = node_details,
                     labels = node_labels)

  # Turn DAG into a tidy data frame for plotting
  rain_dag_tidy <- rain_dag %>%
    tidy_dagitty() %>%
    node_status()   # Add column for exposure/outcome/latent

  status_colors <- c(exposure = "#0074D9", outcome = "#FF851B", latent = "grey50")

  plot_obs_dag <- ggplot(rain_dag_tidy, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges(start_cap = ggraph::circle(1, "lines"),
                   end_cap = ggraph::circle(1, "lines"),
                   edge_width = 0.5,
                   arrow_directed = grid::arrow(length = grid::unit(0.25, "lines"), type = "closed")) +
    geom_dag_point(aes(color = status), size = 7) +
    geom_dag_label_repel(aes(label = label, fill = status), seed = 1234,
                         color = "white", fontface = "bold", size = 3) +
    scale_color_manual(values = status_colors, na.value = "grey20") +
    scale_fill_manual(values = status_colors, na.value = "grey20") +
    guides(color = "none", fill = "none") +
    theme_dag()

  rain_dag_rct <- dagify(water_bill ~ barrel + yard_size + attitude_env + temperature + garden,
                         garden ~ yard_size + attitude_env,
                         exposure = "barrel",
                         outcome = "water_bill",
                         coords = node_details,
                         labels = node_labels)

  rain_dag_tidy_rct <- rain_dag_rct %>%
    tidy_dagitty() %>%
    node_status()   # Add column for exposure/outcome/latent

  plot_rct_dag <- ggplot(rain_dag_tidy_rct, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges(start_cap = ggraph::circle(1, "lines"),
                   end_cap = ggraph::circle(1, "lines"),
                   edge_width = 0.5,
                   arrow_directed = grid::arrow(length = grid::unit(0.25, "lines"), type = "closed")) +
    geom_dag_point(aes(color = status), size = 7) +
    geom_dag_label_repel(aes(label = label, fill = status), seed = 1234,
                         color = "white", fontface = "bold", size = 3) +
    scale_color_manual(values = status_colors, na.value = "gr ey20") +
    scale_fill_manual(values = status_colors, na.value = "grey20") +
    guides(color = "none", fill = "none") +
    theme_dag()

  return(lst(plot_obs_dag, plot_rct_dag))
}


create_barrels <- function() {
  num <- 1241
  num_rct <- 493

  # Build correlations between nodes ----------------------------------------

  # Create confounder variables that are related to each other
  # Average yard size: https://www.homeadvisor.com/r/average-yard-size-by-state/
  mu <- c(yard_size = 20000, home_garden = 35, attitude_env = 70)
  stddev <- c(yard_size = 10000, home_garden = 20, attitude_env = 40)
  lower <- c(yard_size = 500, home_garden = 0, attitude_env = 0)
  upper <- c(yard_size = 40000, home_garden = 100, attitude_env = 150)

  # https://stackoverflow.com/a/46563034/120898
  correlations_confounders <- tribble(
    ~var1, ~var2, ~correlation,
    "yard_size", "home_garden", 0.7,
    "yard_size", "attitude_env", 0.1,
    "home_garden", "attitude_env", 0.9,
  ) %>%
    mutate_at(
      vars(starts_with("var")),
      ~ factor(., levels = c("yard_size", "home_garden", "attitude_env"))
    ) %>%
    xtabs(correlation ~ var1 + var2, ., drop.unused.levels = FALSE) %>%
    `+`(., t(.)) %>%
    `diag<-`(1) %>%
    as.data.frame.matrix() %>%
    as.matrix()

  # Convert correlation matrix to covariance matrix using fancy math
  cov_matrix_confounders <- stddev %*% t(stddev) * correlations_confounders

  # Force the covariance matrix to be positive definite and symmetric
  # https://stats.stackexchange.com/q/153166/3025
  sigma <- as.matrix(Matrix::nearPD(cov_matrix_confounders)$mat)


  # Make RCT data -----------------------------------------------------------

  set.seed(12345)
  confounders_rct <- tmvtnorm::rtmvnorm(num_rct, mean = mu, sigma = sigma,
                                        lower = lower, upper = upper) %>%
    magrittr::set_colnames(names(mu)) %>%
    as_tibble() %>%
    mutate(home_garden = home_garden / 100) %>%
    mutate(home_garden_binary = home_garden > 0.5) %>%
    mutate(attitude_env = rescale(attitude_env, to = c(1, 10)),
           attitude_env = round(attitude_env, 0),
           yard_size = round(abs(yard_size), 0)) %>%
    mutate(temperature = round(rnorm(n(), mean = 70, sd = 5), 1))

  set.seed(12345)
  rain_rct <- tibble(id = 1:493) %>%
    bind_cols(confounders_rct) %>%
    mutate(barrel_num = rbinom(n(), 1, 0.5),
           barrel = factor(barrel_num, labels = c("No barrel", "Barrel"))) %>%
    mutate(bill_noise = rnorm(n(), 0, 15)) %>%
    mutate(water_bill = 30 + (-40 * barrel_num) + (-5 * attitude_env) + (2.5 * temperature) +
             (20 * home_garden) + (2.1 * yard_size / 1000) + bill_noise) %>%
    mutate(water_bill = round(water_bill, 2)) %>%
    mutate(home_garden = factor(home_garden_binary, labels = c("No home garden", "Home garden")),
           home_garden_num = as.numeric(home_garden_binary)) %>%
    select(id, water_bill, barrel, barrel_num, yard_size,
           home_garden, home_garden_num, attitude_env, temperature)


  # Make observational data -------------------------------------------------

  set.seed(12345)
  confounders <- tmvtnorm::rtmvnorm(num, mean = mu, sigma = sigma,
                                    lower = lower, upper = upper) %>%
    magrittr::set_colnames(names(mu)) %>%
    as_tibble() %>%
    mutate(home_garden = home_garden / 100) %>%
    mutate(home_garden_binary = home_garden > 0.5) %>%
    mutate(attitude_env = rescale(attitude_env, to = c(1, 10)),
           attitude_env = round(attitude_env, 0),
           yard_size = round(abs(yard_size), 0)) %>%
    mutate(temperature = round(rnorm(n(), mean = 70, sd = 5), 1))

  set.seed(1234)
  rain <- tibble(id = 1:num) %>%
    bind_cols(confounders) %>%
    # Simulate data from a logit model
    # https://stats.stackexchange.com/a/46525/3025
    # But then do all sorts of weird distortion to make it less likely to have a barrel
    mutate(barrel_effect = (0.4 * attitude_env) + (4 * home_garden) +
             (0.05 * yard_size / 1000) + (0.7 * temperature),
           barrel_diff = barrel_effect - mean(barrel_effect),
           barrel_effect = ifelse(barrel_diff < 0, barrel_effect - (barrel_diff / 2), barrel_effect),
           barrel_effect_rescaled = scales::rescale(barrel_effect, to = c(-2.2, 2.2)),
           inv_logit = 1 / (1 + exp(-barrel_effect_rescaled)),
           barrel_num = rbinom(n(), 1, inv_logit)) %>%
    mutate(barrel = factor(barrel_num, labels = c("No barrel", "Barrel"))) %>%
    mutate(bill_noise = rnorm(num, 0, 15)) %>%
    mutate(water_bill = 30 + (-40 * barrel_num) + (-5 * attitude_env) + (2.5 * temperature) +
             (20 * home_garden) + (2.1 * yard_size / 1000) + bill_noise) %>%
    mutate(water_bill = round(water_bill, 2)) %>%
    mutate(home_garden = factor(home_garden_binary, labels = c("No home garden", "Home garden")),
           home_garden_num = as.numeric(home_garden_binary)) %>%
    select(id, water_bill, barrel, barrel_num, yard_size,
           home_garden, home_garden_num, attitude_env, temperature)

  return(lst(rain_rct, rain))
}


# Fake tutoring program for regression discontinuity
create_tutoring <- function() {
  set.seed(1234)

  num_students <- 1000

  tutoring <- tibble(
    id = 1:num_students,
    entrance_exam = rbeta(num_students, shape1 = 7, shape2 = 2),
    exit_exam_base = rbeta(num_students, shape1 = 5, shape2 = 3)
  ) %>%
    mutate(entrance_exam = round(entrance_exam * 100, 1)) %>%
    mutate(tutoring_sharp = entrance_exam <= 70) %>%
    mutate(tutoring_fuzzy = case_when(
      entrance_exam >= 50 & entrance_exam <= 70 ~ sample(c(TRUE, FALSE), n(), replace = TRUE, prob = c(0.8, 0.2)),
      entrance_exam > 70 & entrance_exam <= 90 ~ sample(c(TRUE, FALSE), n(), replace = TRUE, prob = c(0.2, 0.8)),
      entrance_exam < 50 ~ TRUE,
      entrance_exam > 90 ~ FALSE
    )) %>%
    mutate(tutoring_sharp_text = factor(tutoring_sharp, levels = c(FALSE, TRUE),
                                        labels = c("No tutor", "Tutor")),
           tutoring_fuzzy_text = factor(tutoring_fuzzy, levels = c(FALSE, TRUE),
                                        labels = c("No tutor", "Tutor"))) %>%
    mutate(exit_exam_sharp = exit_exam_base * 40 + 10 * tutoring_sharp + entrance_exam / 2) %>%
    mutate(exit_exam_fuzzy = exit_exam_base * 40 + 10 * tutoring_fuzzy + entrance_exam / 2) %>%
    mutate(across(starts_with("exit_exam"), ~round(., 1)))

  return(tutoring)
}

create_tutoring_sharp <- function(tutoring) {
  tutoring_sharp <- tutoring %>%
    select(id, entrance_exam, tutoring = tutoring_sharp,
           tutoring_text = tutoring_sharp_text, exit_exam = exit_exam_sharp)

  return(tutoring_sharp)
}

create_tutoring_fuzzy <- function(tutoring) {
  tutoring_fuzzy <- tutoring %>%
    select(id, entrance_exam, tutoring = tutoring_fuzzy,
           tutoring_text = tutoring_fuzzy_text, exit_exam = exit_exam_fuzzy)

  return(tutoring_fuzzy)
}


# Fake attendance program for regression discontinuity
create_attendance <- function() {
  set.seed(1234)

  num <- 1200

  program <- tibble(
    id = 1:num,
    attendance = rbeta(num, shape1 = 7, shape2 = 2)
  ) %>%
    mutate(attendance = rescale(attendance, to = c(20, 100))) %>%
    mutate(treatment = attendance < 80) %>%
    mutate(grade = (200 * treatment) + (20 * attendance) + rnorm(n(), 600, 100)) %>%
    mutate(grade = rescale(grade, to = c(0, 100))) %>%
    mutate(grade = ifelse(grade < 80, grade * (attendance / rnorm(n(), 80, 3)), grade)) %>%
    mutate(across(c(attendance, grade), ~ round(., 2)))

  return(program)
}


# Fake wage education father's education for instrumental variables
create_father_educ <- function() {
  set.seed(123456)

  nrows <- 1000

  father_education <- tibble(
    ability = rnorm(nrows, 35000, 10000),  # Ability
    fathereduc = rnorm(nrows, 15000, 20000),  # Father's education (IV)
    e_y = 0.43 * rnorm(nrows, 50000, 10000)  # Error for outcome
  ) %>%
    mutate(educ = 3.7 + (0.52 * fathereduc) + (0.40 * ability),  # Education (policy variable)
           wage = 5 + (0.23 * educ) + (0.5 * ability) - e_y) %>%  # Wage (outcome variable)
    mutate(wage = rescale(wage, to = c(7.75, 300)),  # Rescale from minimum wage to director wage (hourly)
           educ = rescale(educ, to = c(10, 23)),  # Rescale as years of school. Min 10 to max 23 (PhD)
           fathereduc = rescale(fathereduc, to = c(10, 23)),  # Rescale father's education
           ability = rescale(ability, to = c(0, 600))) %>%  # Rescale as hypothetical test scores
    select(wage, educ, ability, fathereduc) %>%
    mutate(across(everything(), ~round(., 2)))

  return(father_education)
}


# Fake bed net compliance for CACE and ITT
create_bed_nets <- function() {
  set.seed(1234)

  N <- 2000
  df <- tibble(
    status = sample(c("Always taker", "Never taker", "Complier"), N,
                    replace = TRUE, prob = c(0.2, 0.4, 0.4)),
    treatment = sample(c("Treatment", "Control"), N, replace = TRUE, prob = c(0.5, 0.5))
  ) %>%
    mutate(bed_net_0 = (status == "Always taker") * 1,
           bed_net_1 = (status != "Never taker") * 1) %>%
    mutate(health_0 = case_when(
      status == "Always taker" ~ rnorm(N, 1, 0.5),
      status == "Never taker"  ~ rnorm(N, 0, 0.6),
      status == "Complier"     ~ rnorm(N, 0.1, 0.4),
    )) %>%
    mutate(health_1 = case_when(
      status == "Always taker" ~ rnorm(N, 1, 0.5),
      status == "Never taker"  ~ rnorm(N, 0, 0.6),
      status == "Complier"     ~ rnorm(N, 0.9, 0.7),
    )) %>%
    mutate(bed_net = case_when(
      treatment == "Treatment" ~ bed_net_1,
      treatment == "Control"   ~ bed_net_0
    )) %>%
    mutate(health = case_when(
      bed_net == 0 ~ health_0,
      bed_net == 1 ~ health_1
    )) %>%
    mutate(bed_net = factor(bed_net, labels = c("No bed net", "Bed net")))

  min_health <- min(df$health_0, df$health_1)
  max_health <- max(df$health_0, df$health_1)

  df <- df %>%
    mutate(across(starts_with("health"),
                  ~rescale(., to = c(0, 100),
                           from = c(min_health, max_health)))) %>%
    mutate(across(starts_with("health"),
                  ~round(., 1)))

  return(df)
}

create_bed_nets_real <- function(df) {
  df_real <- df %>%
    select(treatment, bed_net, health)

  return(df_real)
}

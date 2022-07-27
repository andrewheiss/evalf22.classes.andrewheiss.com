library(targets)
library(tarchetypes)
suppressPackageStartupMessages(library(dplyr))

options(tidyverse.quiet = TRUE,
        dplyr.summarise.inform = FALSE)

tar_option_set(
  packages = c("tibble"),
  format = "rds",
  workspace_on_error = TRUE
)

# here::here() returns an absolute path, which then gets stored in tar_meta and
# becomes computer-specific (i.e. /Users/andrew/Research/blah/thing.Rmd).
# There's no way to get a relative path directly out of here::here(), but
# fs::path_rel() works fine with it (see
# https://github.com/r-lib/here/issues/36#issuecomment-530894167)
here_rel <- function(...) {fs::path_rel(here::here(...))}

# Load functions for the pipeline
source("R/tar_save-files.R")

# THE PIPELINE ----
list(
  ## Knit xaringan slides ----
  tar_files(xaringan_files, list.files(here_rel("slides"),
                                       pattern = "\\.Rmd",
                                       full.names = TRUE)),
  tar_target(xaringan_slides,
             render_xaringan(xaringan_files),
             pattern = map(xaringan_files),
             format = "file"),


  ## Create project folders and zip files ----
  ### Save any data files from packages ----
  tar_target(data_penguins,
             save_csv(tidyr::drop_na(palmerpenguins::penguins, body_mass_g),
                      here_rel("projects", "problem-set-2", "data", "penguins.csv"))),
  ### Link all these data files into one dependency
  tar_target(build_data, {
    data_penguins
  }),

  ### Dynamic branching! ----
  #
  # Get a list of all folders in the project folder, create dynamic branches,
  # then create a target for each that runs the custom zippy() function, which
  # uses system2() to zip the folder and returns a path to keep targets happy
  # with `format = "file"`
  #
  # The main index.qmd page loads project_zips as a target to link it as a dependency
  tar_target(project_paths, list.dirs(here_rel("projects"), full.names = FALSE, recursive = FALSE)),
  tar_target(project_files, project_paths, pattern = map(project_paths)),
  tar_target(project_zips, {
    build_data
    zippy(project_files, "projects")
  },
  pattern = map(project_files),
  format = "file"),

  ## Build site ----
  tar_quarto(site, path = ".")

  ## Upload site ----
)

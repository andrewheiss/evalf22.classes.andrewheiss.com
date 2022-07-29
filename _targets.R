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
source("R/tar_slides.R")
source("R/tar_projects.R")
source("R/tar_data.R")

# THE MAIN PIPELINE ----
list(
  ## Run all the data building and copying targets ----
  save_data,

  ### Link all these data building and copying targets into individual dependencies ----
  tar_combine(copy_data, tar_select_targets(save_data, starts_with("copy_"))),
  tar_combine(build_data, tar_select_targets(save_data, starts_with("data_"))),


  ## xaringan stuff ----
  #
  ### Knit xaringan slides ----
  #
  # Use dynamic branching to get a list of all .Rmd files in slides/ and knit them
  #
  # The main index.qmd page loads xaringan_slides as a target to link it as a dependency
  tar_files(xaringan_files, list.files(here_rel("slides"),
                                       pattern = "\\.Rmd",
                                       full.names = TRUE)),
  tar_target(xaringan_slides,
             render_xaringan(xaringan_files),
             pattern = map(xaringan_files),
             format = "file"),

  ### Convert xaringan HTML slides to PDF ----
  #
  # Use dynamic branching to get a list of all knitted slide .html files and
  # convert them to PDF with pagedown
  #
  # The main index.qmd page loads xaringan_pdfs as a target to link it as a dependency
  tar_files(xaringan_html_files, {
    xaringan_slides
    list.files(here_rel("slides"),
               pattern = "\\.html",
               full.names = TRUE)
  }),
  tar_target(xaringan_pdfs,
             xaringan_to_pdf(xaringan_html_files),
             pattern = map(xaringan_html_files),
             format = "file"),


  ## Project folders ----

  ### Zip up each project folder ----
  #
  # Get a list of all folders in the project folder, create dynamic branches,
  # then create a target for each that runs the custom zippy() function, which
  # uses system2() to zip the folder and returns a path to keep targets happy
  # with `format = "file"`
  #
  # The main index.qmd page loads project_zips as a target to link it as a dependency
  #
  # Use tar_force() and always run this because {targets} seems to overly cache
  # the results of list.dirs()
  tar_force(project_paths,
            list.dirs(here_rel("projects"),
                      full.names = FALSE, recursive = FALSE),
            force = TRUE),
  tar_target(project_files, project_paths, pattern = map(project_paths)),
  tar_target(project_zips, {
    copy_data
    build_data
    zippy(project_files, "projects")
  },
  pattern = map(project_files),
  format = "file"),


  ## Class schedule file ----
  tar_target(schedule_file, here_rel("data", "schedule.csv"), format = "file"),


  ## Build site ----
  tar_quarto(site, path = "."),


  ## Upload site ----
  tar_target(deploy_script, here_rel("deploy.sh"), format = "file"),
  tar_target(deploy_site, {
    # Force a dependency
    site
    # Run the deploy script
    if (Sys.getenv("UPLOAD_WEBSITES") == "TRUE") processx::run(paste0("./", deploy_script))
  })
)

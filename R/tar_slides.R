# We need to return the path to the rendered HTML file. In this case,
# rmarkdown::render() *does* return a path, but it returns an absolute path,
# which makes the targets pipline less portable. So we return our own path to
# the HTML file instead.
render_xaringan <- function(slide_path) {
  # crayon does weird things to R Markdown and xaringan output, so we need to
  # disable it here. This is the same thing that tarchetypes::tar_render() does
  # behind the scenes too.
  withr::local_options(list(crayon.enabled = NULL))
  rmarkdown::render(slide_path, quiet = TRUE)
  return(paste0(tools::file_path_sans_ext(slide_path), ".html"))
}


# Use pagedown to convert xaringan HTML slides to PDF. Return a relative path to
# the PDF to keep targets happy.
#
# Slides for sessions 10 and 14 are huge, so use chromote to convert them instead
xaringan_to_pdf <- function(slide_path) {
  path_sans_ext <- tools::file_path_sans_ext(slide_path)

  if (path_sans_ext %in% c("slides/10-slides",
                           "slides/14-slides")) {
    complex <- TRUE
  } else {
    complex <- FALSE
  }

  renderthis::to_pdf(slide_path,
                     to = paste0(path_sans_ext, ".pdf"),
                     complex_slides = complex)

  return(paste0(tools::file_path_sans_ext(slide_path), ".pdf"))
}

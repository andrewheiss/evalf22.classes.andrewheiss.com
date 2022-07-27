# When using a file-based target, {targets} requires that the function that
# saves the file returns a path to the file. write_csv() invisibly returns the
# data frame being written, so we need a wrapper function to save the file and
# return the path.
save_csv <- function(df, path) {
  readr::write_csv(df, path)
  return(path)
}


# Once again, we need to return the path to the rendered HTML file. In this
# case, rmarkdown::render() *does* return a path, but it returns an absolute
# path, which makes the targets pipline less portable. So we return our own path
# to the HTML file instead.
render_xaringan <- function(slide_path) {
  rmarkdown::render(slide_path, "xaringan::moon_reader", quiet = TRUE)
  return(paste0(tools::file_path_sans_ext(slide_path), ".html"))
}


# Use pagedown to convert xaringan HTML slides to PDF. Return a relative path to
# the PDF to keep targets happy.
xaringan_to_pdf <- function(slide_path) {
  path_sans_ext <- tools::file_path_sans_ext(slide_path)

  pagedown::chrome_print(slide_path,
                         output = paste0(path_sans_ext, ".pdf"),
                         options = list(printBackground = TRUE),
                         wait = 10, timeout = 600)

  return(paste0(tools::file_path_sans_ext(slide_path), ".pdf"))
}


# Zip a folder inside a parent folder
#
# The cd is necessary because zip assumes that it's working in the current
# folder. That is, if you do `zip -r projects/whatever.zip projects/whatever/`,
# you'll end up with a folder named "projects" in the resulting .zip file, which
# is annoying. Moving into "projects" first *and then* zipping from there is the
# only way to fix this.
#
# Also annoying is how system2() works. The first argument (command) must be a
# regular bash command (here `cd`). There's no intuitive way to concatenate
# commands like `cd blah; zip bloop`, so we feed the second command (here `zip`)
# as an argument to `cd`, which feels hacky, but it works :shrug:
zippy <- function(folder_to_zip, parent) {
  system2("cd", c(parent, "; zip", "-FSrX", paste0(folder_to_zip, ".zip"),
                  folder_to_zip, '-x "*.DS_Store"'))
  return(file.path(parent, paste0(folder_to_zip, ".zip")))
}

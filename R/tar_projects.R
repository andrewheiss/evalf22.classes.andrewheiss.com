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

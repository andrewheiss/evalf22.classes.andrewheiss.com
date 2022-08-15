slide_buttons <- function(slide_id) {
  glue::glue('<p class="buttons"><a class="btn btn-danger" target="_blank" href="{slide_id}.html"><i class="fa-solid fa-arrow-up-right-from-square"></i> View all slides in new window</a> <a class="btn btn-danger" target="_blank" href="{slide_id}.pdf" role="button"><i class="fa-solid fa-file-pdf"></i> Download PDF of all slides</a></p>')
}

slide_tabs <- function(slide_df, slide_url) {
  slugify <- function(x) {
    x <- stringr::str_replace_all(x, "[^[:alnum:] ]", "")
    x <- stringr::str_replace_all(x, " ", "-")
    x <- stringr::str_to_lower(x)
    return(x)
  }

  nav_li <- function(title, selected = FALSE) {
    select_flag <- ifelse(selected, "true", "false")
    active_flag <- ifelse(selected, " active", "")
    out <- glue::glue('<li class="nav-item">\n',
                      '<a class="nav-link{active_flag}" id="{slugify(title)}-tab" data-toggle="tab" ',
                      'href="#{slugify(title)}" role="tab" ',
                      'aria-controls="{slugify(title)}" aria-selected="{select_flag}">',
                      '{title}</a>\n',
                      '</li>')
    return(out)
  }

  tab_pane <- function(title, slide, active, url) {
    select_flag <- ifelse(active, " show active", "")
    out <- glue::glue('<div class="tab-pane fade{select_flag}" id="{slugify(title)}" ',
                      'role="tabpanel" aria-labelledby="{slugify(title)}-tab">\n',
                      '<div class="ratio ratio-16x9">\n',
                      '<iframe src="{url}#{slide}"></iframe>\n',
                      '</div>\n</div>')
    return(out)
  }

  sections <- dplyr::mutate(
    slide_df,
    li = purrr::pmap_chr(list(title, active), nav_li),
    pane = purrr::pmap_chr(list(title, slide, active, slide_url), tab_pane)
  )

  tabset <- paste('<ul class="nav nav-tabs" id="slide-tabs" role="tablist">',
                  paste(sections$li, collapse = "\n"),
                  '</ul>',
                  '<div class="tab-content" id="slide-tabs">',
                  paste(sections$pane, collapse = "\n"),
                  '</div>', sep = "\n")

  cat(tabset)
}


<!-- README.md is generated from README.Rmd. Please edit that file -->

# Program Evaluation for Public Service <a href='https://evalf22.classes.andrewheiss.com/'><img src='files/icon-512.png' align="right" height="139" /></a>

[PMAP 8521 • Fall 2022](https://evalf22.classes.andrewheiss.com/)  
[Andrew Heiss](https://www.andrewheiss.com/) • Andrew Young School of
Policy Studies • Georgia State University

------------------------------------------------------------------------

**[Quarto](https://quarto.org/) +
[{targets}](https://docs.ropensci.org/targets/) +
[{renv}](https://rstudio.github.io/renv/) +
[{xaringan}](https://github.com/yihui/xaringan) = magic! 🪄**

------------------------------------------------------------------------

## How to build the site

1.  Install
    [RStudio](https://www.rstudio.com/products/rstudio/download/#download)
    version 2022.07.1 or later since it has a
    [Quarto](https://quarto.org/) installation embedded in it.
    Otherwise, download and install [Quarto](https://quarto.org/)
    separately.
2.  Open `evalf22.Rproj` to open an [RStudio
    Project](https://r4ds.had.co.nz/workflow-projects.html).
3.  If it’s not installed already, R *should* try to install the [{renv}
    package](https://rstudio.github.io/renv/) when you open the RStudio
    Project for the first time. If you don’t see a message about package
    installation, install it yourself by running
    `install.packages("renv")` in the R console.
4.  Run `renv::restore()` in the R console to install all the required
    packages for this project.
5.  Run `targets::tar_make()` in the R console to build everything.
6.  🎉 All done! 🎉 The complete website will be in a folder named
    `_site/`.

## {targets} pipeline

I use the [{targets} package](https://docs.ropensci.org/targets/) to
build this site and all its supporting files. The complete pipeline is
defined in [`_targets.R`](_targets.R) and can be run in the R console
with:

``` r
targets::tar_make()
```

The pipeline does several major tasks:

-   **Create supporting data files**: The problem sets and examples I
    use throughout the course use many different datasets that come
    prepackaged in R packages, I downloaded from sources online, or that
    I generated myself. To make sure I and my students are using the
    latest, most correct datasets, the functions in
    [`R/tar_data.R`](R/tar_data.R) save and/or generate these datasets
    prior to building the website.

-   **Compress project folders**: To make it easier to distribute
    problem sets and in-class activities to students, I compress all the
    folders in the [`/projects/`](/projects/) folder so that students
    can download and unzip a self-contained RStudio Project as a `.zip`
    file. These targets are [dynamically
    generated](https://books.ropensci.org/targets/dynamic.html) so that
    any new folder that is added to `/projects/` will automatically be
    zipped up when running the pipeline.

-   **Render xaringan slides to HTML and PDF**: Quarto supports
    HTML-based slideshows through
    [reveal.js](https://quarto.org/docs/presentations/revealjs/).
    However, I created all my slides using
    [{xaringan}](https://github.com/yihui/xaringan), which is based on
    [remark.js](https://remarkjs.com/) and doesn’t work with Quarto.
    Since (1) I recorded all the class videos using my {xaringan} slides
    with a fancy template I made, and (2) I don’t want to recreate my
    fancy template in reveal.js yet, I want to keep using {xaringan}.

    The pipeline [dynamically generates
    targets](https://books.ropensci.org/targets/dynamic.html) for all
    the `.Rmd` files in [`/slides/`](/slides/) and renders them using R
    Markdown rather than Quarto.

    The pipeline then uses
    [{renderthis}](https://jhelvy.github.io/renderthis/) to convert each
    set of HTML slides into PDFs.

-   **Build Quarto website**: This project is a [Quarto
    website](https://quarto.org/docs/websites/), which compiles and
    stitches together all the `.qmd` files in this project based on the
    settings in [`_quarto.yml`](_quarto.yml). See the [Quarto website
    documentation](https://quarto.org/docs/websites/) for more details.

-   **Upload resulting `_site/` folder to my remote server**: Quarto
    places the compiled website in a folder named `/_site/`. The
    pipeline uses `rsync` to upload this folder to my personal remote
    server. This target will only run if the `UPLOAD_WEBSITES`
    environment variable is set to `TRUE`, and it will only work if you
    have an SSH key set up on my personal server, which only I do.

The complete pipeline looks like this:

<small>(This uses [`mermaid.js`
syntax](https://mermaid-js.github.io/mermaid/) and should display as a
graph on GitHub. You can also view it by pasting the code into
<https://mermaid.live>.)</small>

``` mermaid
graph LR
  subgraph Graph
    x20b85e3488818f5e(["gen_data_tutoring"]):::skipped --> x5c240766086c102f(["gen_data_tutoring_fuzzy"]):::queued
    x1917c787a0a4a0fd["project_zips"]:::queued --> x7aa56383a054e8ba(["site"]):::queued
    x4d31f5a49d5ae49f(["schedule_ical_file"]):::queued --> x7aa56383a054e8ba(["site"]):::queued
    x063edd335cc1b36f(["schedule_page_data"]):::queued --> x7aa56383a054e8ba(["site"]):::queued
    xccbb2c85646c611a["xaringan_pdfs"]:::queued --> x7aa56383a054e8ba(["site"]):::queued
    x60c212b45249134a["xaringan_slides"]:::queued --> x7aa56383a054e8ba(["site"]):::queued
    x07bd1301298fd82f(["gen_barrel_dags"]):::skipped --> x676cecdcd5eb7813(["data_plot_barrel_dag_obs"]):::queued
    x1b5d71f80f0ded23(["gen_data_father_educ"]):::skipped --> x068350206b5f4fee(["data_father_educ"]):::queued
    xf38d3f5e6365ad72(["workflow_graph"]):::started --> x6e52cb0f1668cc22(["readme"]):::queued
    x9a78ab75449e880d(["data_wage"]):::queued --> x0897796b858a5b3d(["copy_wage"]):::queued
    xa48826fcb4dc2e34(["project_paths_change"]):::built --> xb4844be3dca7f02b(["project_paths"]):::queued
    xcd68d1a7c07ebab6(["data_barrels_obs"]):::queued --> x3b13eed8c2f4209e(["copy_barrels_obs"]):::queued
    xb4844be3dca7f02b(["project_paths"]):::queued --> x7a8b235bff1bfb75["project_files"]:::queued
    xb91d56300ed67e72(["gen_attendance"]):::queued --> x9061f97ff2027ff8(["data_attendance"]):::queued
    x7ece18ea4dfd37ad(["data_barrels_rct"]):::queued --> x04215792a9a4d36b(["copy_barrels_rct"]):::queued
    xf4774655f169db90["xaringan_files"]:::queued --> x60c212b45249134a["xaringan_slides"]:::queued
    x07bd1301298fd82f(["gen_barrel_dags"]):::skipped --> xc72ce427df7cb6d6(["data_plot_barrel_dag_rct"]):::queued
    xdf832f8e1f99baf2(["schedule_file"]):::queued --> x063edd335cc1b36f(["schedule_page_data"]):::queued
    x9061f97ff2027ff8(["data_attendance"]):::queued --> x7b056887098d4c56(["copy_attendance"]):::queued
    xc5cdd24fb6bd9f0e(["gen_village"]):::queued --> x7ba0dec890393ab6(["data_village_obs"]):::queued
    xa3d8306cecf136f4(["data_penguins"]):::queued --> x2a5bb41380dcc5b0(["copy_penguins"]):::queued
    x4a210bdf90796bca(["xaringan_files_files"]):::queued --> xf4774655f169db90["xaringan_files"]:::queued
    x5c240766086c102f(["gen_data_tutoring_fuzzy"]):::queued --> x313ad24da404b651(["data_tutoring_fuzzy"]):::queued
    x1a70645cdb0e8eb9(["gen_barrels"]):::queued --> x7ece18ea4dfd37ad(["data_barrels_rct"]):::queued
    x1a70645cdb0e8eb9(["gen_barrels"]):::queued --> xcd68d1a7c07ebab6(["data_barrels_obs"]):::queued
    x0751853b619def05["xaringan_html_files"]:::queued --> xccbb2c85646c611a["xaringan_pdfs"]:::queued
    x6deca4ab95db78c5(["gen_data_bed_nets"]):::queued --> x9d65856d614f77f4(["gen_data_bed_nets_real"]):::queued
    xccb29afdb6aede8f(["gen_nets"]):::queued --> xbe28472fe2bce29e(["data_nets"]):::queued
    x7b056887098d4c56(["copy_attendance"]):::queued --> x41092a7251862a9e(["copy_data"]):::queued
    x3b13eed8c2f4209e(["copy_barrels_obs"]):::queued --> x41092a7251862a9e(["copy_data"]):::queued
    x04215792a9a4d36b(["copy_barrels_rct"]):::queued --> x41092a7251862a9e(["copy_data"]):::queued
    xbbb6d7ed9a6f640a(["copy_eitc"]):::skipped --> x41092a7251862a9e(["copy_data"]):::queued
    xf17ad1e9c3822d18(["copy_evaluation"]):::queued --> x41092a7251862a9e(["copy_data"]):::queued
    xf812cd9b8b5444a5(["copy_food_health_politics"]):::queued --> x41092a7251862a9e(["copy_data"]):::queued
    xd2260b533f1829cb(["copy_monthly_panel"]):::queued --> x41092a7251862a9e(["copy_data"]):::queued
    x2a5bb41380dcc5b0(["copy_penguins"]):::queued --> x41092a7251862a9e(["copy_data"]):::queued
    xa7f6f0c1b16f542a(["copy_plot_barrel_dag_obs"]):::queued --> x41092a7251862a9e(["copy_data"]):::queued
    x6271c0b6a170e94e(["copy_plot_barrel_dag_rct"]):::queued --> x41092a7251862a9e(["copy_data"]):::queued
    x9c50e551b1b09085(["copy_public_housing"]):::queued --> x41092a7251862a9e(["copy_data"]):::queued
    x0897796b858a5b3d(["copy_wage"]):::queued --> x41092a7251862a9e(["copy_data"]):::queued
    x60c212b45249134a["xaringan_slides"]:::queued --> x7a0d40becb063bda(["xaringan_html_files_files"]):::queued
    xc72ce427df7cb6d6(["data_plot_barrel_dag_rct"]):::queued --> x6271c0b6a170e94e(["copy_plot_barrel_dag_rct"]):::queued
    xdf832f8e1f99baf2(["schedule_file"]):::queued --> x35552a73efe9c59f(["schedule_ical_data"]):::queued
    x676cecdcd5eb7813(["data_plot_barrel_dag_obs"]):::queued --> xa7f6f0c1b16f542a(["copy_plot_barrel_dag_obs"]):::queued
    x35552a73efe9c59f(["schedule_ical_data"]):::queued --> x4d31f5a49d5ae49f(["schedule_ical_file"]):::queued
    x9d65856d614f77f4(["gen_data_bed_nets_real"]):::queued --> xcd2bd51d3f2880dc(["data_bed_nets_real"]):::queued
    x9061f97ff2027ff8(["data_attendance"]):::queued --> xb453b5ae08dcaee7(["build_data"]):::queued
    xcd68d1a7c07ebab6(["data_barrels_obs"]):::queued --> xb453b5ae08dcaee7(["build_data"]):::queued
    x7ece18ea4dfd37ad(["data_barrels_rct"]):::queued --> xb453b5ae08dcaee7(["build_data"]):::queued
    xcd2bd51d3f2880dc(["data_bed_nets_real"]):::queued --> xb453b5ae08dcaee7(["build_data"]):::queued
    x10e4e9d82e7b691d(["data_bed_nets_time_machine"]):::queued --> xb453b5ae08dcaee7(["build_data"]):::queued
    xdba7a42d19fbbe49(["data_card"]):::queued --> xb453b5ae08dcaee7(["build_data"]):::queued
    x068350206b5f4fee(["data_father_educ"]):::queued --> xb453b5ae08dcaee7(["build_data"]):::queued
    x81182810f96b04c1(["data_injury"]):::queued --> xb453b5ae08dcaee7(["build_data"]):::queued
    x182180f03bcfc8dc(["data_mpg"]):::queued --> xb453b5ae08dcaee7(["build_data"]):::queued
    xbe28472fe2bce29e(["data_nets"]):::queued --> xb453b5ae08dcaee7(["build_data"]):::queued
    xa3d8306cecf136f4(["data_penguins"]):::queued --> xb453b5ae08dcaee7(["build_data"]):::queued
    x676cecdcd5eb7813(["data_plot_barrel_dag_obs"]):::queued --> xb453b5ae08dcaee7(["build_data"]):::queued
    xc72ce427df7cb6d6(["data_plot_barrel_dag_rct"]):::queued --> xb453b5ae08dcaee7(["build_data"]):::queued
    x313ad24da404b651(["data_tutoring_fuzzy"]):::queued --> xb453b5ae08dcaee7(["build_data"]):::queued
    x6182dfd3a1ca6e02(["data_tutoring_sharp"]):::queued --> xb453b5ae08dcaee7(["build_data"]):::queued
    x7ba0dec890393ab6(["data_village_obs"]):::queued --> xb453b5ae08dcaee7(["build_data"]):::queued
    x4df77a4d5c017917(["data_village_rct"]):::queued --> xb453b5ae08dcaee7(["build_data"]):::queued
    x9a78ab75449e880d(["data_wage"]):::queued --> xb453b5ae08dcaee7(["build_data"]):::queued
    xb9fb625c05443344(["data_wage2"]):::skipped --> xb453b5ae08dcaee7(["build_data"]):::queued
    xc5cdd24fb6bd9f0e(["gen_village"]):::queued --> x4df77a4d5c017917(["data_village_rct"]):::queued
    x9c20b8c56debbe9a(["deploy_script"]):::skipped --> x78f3e0b711425f1c(["deploy_site"]):::queued
    x7aa56383a054e8ba(["site"]):::queued --> x78f3e0b711425f1c(["deploy_site"]):::queued
    xb453b5ae08dcaee7(["build_data"]):::queued --> x1917c787a0a4a0fd["project_zips"]:::queued
    x41092a7251862a9e(["copy_data"]):::queued --> x1917c787a0a4a0fd["project_zips"]:::queued
    x7a8b235bff1bfb75["project_files"]:::queued --> x1917c787a0a4a0fd["project_zips"]:::queued
    x6deca4ab95db78c5(["gen_data_bed_nets"]):::queued --> x10e4e9d82e7b691d(["data_bed_nets_time_machine"]):::queued
    x5cef82ddbf74dbd2(["gen_data_tutoring_sharp"]):::queued --> x6182dfd3a1ca6e02(["data_tutoring_sharp"]):::queued
    x20b85e3488818f5e(["gen_data_tutoring"]):::skipped --> x5cef82ddbf74dbd2(["gen_data_tutoring_sharp"]):::queued
    x7a0d40becb063bda(["xaringan_html_files_files"]):::queued --> x0751853b619def05["xaringan_html_files"]:::queued
  end
```

## Licenses

**Text and figures:** All prose and images are licensed under Creative
Commons ([CC-BY-NC
4.0](https://creativecommons.org/licenses/by-nc/4.0/))

**Code:** All code is licensed under the [MIT License](LICENSE.md).

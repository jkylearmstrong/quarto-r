# Internal helper to prepare a directory containing a titlepage QMD and optional image
# Not exported. Returns a list with elements `dir` and `input_qmd` and `copied_image` (if provided)
prepare_titlepage_dir <- function(dir, title = NULL, author = NULL, date = NULL, image = NULL, theme = NULL, quiet = FALSE, install_template = TRUE, no_prompt = FALSE) {
  # ensure dependencies
  if (!dir.exists(dir)) {
    fs::dir_create(dir)
  }

  dest_image <- NULL
  if (!is.null(image)) {
    if (!file.exists(image)) {
      cli::cli_abort(c(
        "Image file not found: {.path {image}}",
        "i" = "Provide a valid path to an image file to include on the title page."
      ))
    }
    dest_image <- file.path(dir, basename(image))
    file.copy(image, dest_image, overwrite = TRUE)
  }

  # Optionally install template into dir. This uses the quarto CLI and will error if not available.
  if (isTRUE(install_template)) {
    quarto_use_template("nmfs-opensci/quarto_titlepages", dir = dir, no_prompt = no_prompt, quiet = quiet)
  }

  qmds_all <- list.files(dir, pattern = "\\.qmd$", full.names = TRUE, recursive = TRUE)
  qmds_top <- list.files(dir, pattern = "\\.qmd$", full.names = TRUE, recursive = FALSE)

  # Prefer example files at top-level provided by the template, then any top-level qmd,
  # then a documented titlepage article, otherwise fall back to the first qmd found.
  chosen_qmd <- NULL
  preferred_names <- c("example_1.qmd", "example_2.qmd", "example_3.qmd")
  for (nm in preferred_names) {
    cand <- file.path(dir, nm)
    if (file.exists(cand)) {
      chosen_qmd <- cand
      break
    }
  }
  if (is.null(chosen_qmd) && length(qmds_top) > 0) {
    chosen_qmd <- qmds_top[[1]]
  }
  if (is.null(chosen_qmd) && length(qmds_all) > 0) {
    # try to find documentation titlepage article qmds
    docs <- grep("documentation.*titlepages.*article\\.qmd$", qmds_all, value = TRUE)
    if (length(docs) > 0) chosen_qmd <- docs[[1]]
  }
  if (is.null(chosen_qmd) && length(qmds_all) > 0) {
    chosen_qmd <- qmds_all[[1]]
  }

  if (is.null(chosen_qmd)) {
    qmd <- file.path(dir, "titlepage.qmd")
    meta <- list()
    if (!is.null(title)) meta$title <- title
    if (!is.null(author)) meta$author <- author
    if (!is.null(date)) meta$date <- as.character(date)
    if (!is.null(image)) meta$image <- basename(image)
    if (!is.null(theme)) meta$theme <- theme

    meta$`quarto-titlepage` <- list(template = "nmfs-opensci/quarto_titlepages")

    header <- as_yaml_block(meta)
    body <- "\n\n"
    writeLines(c(header, body), qmd, useBytes = TRUE)
    chosen_qmd <- qmd
  }

  list(dir = dir, input_qmd = chosen_qmd, copied_image = dest_image)
}

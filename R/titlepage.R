#' Create a PDF title page using the nmfs-opensci/quarto_titlepages template
#'
#' Convenience wrapper that prepares a minimal Quarto document using the
#' nmfs-opensci/quarto_titlepages extension and renders a PDF title/cover
#' page via the Quarto CLI. The function will install the template into a
#' temporary directory by default, or into the current project if the user
#' approves installation.
#'
#' Behavior summary:
#' - If the template is already present under the current working directory,
#'   it will be used without modification.
#' - If not present and `dir` is left as the default temporary directory,
#'   the function will prompt (interactive sessions) to install the template
#'   into the current project. Use `no_prompt = TRUE` to skip prompts and
#'   auto-install.
#' - If `dir` is a persistent directory provided by the caller, the template
#'   will be installed there (or re-used if already present).
#'
#' Requirements:
#' - Quarto command-line tool must be installed and discoverable by this R
#'   process (see [find_quarto()]). Rendering to PDF may also require a LaTeX
#'   installation depending on the template/output configuration.
#'
#' @param output Character path for the output PDF. Defaults to "titlepage.pdf".
#' @param title Character title to place on the title page. If `NULL`, the
#'   template's defaults are used.
#' @param author Character author(s) for the title page.
#' @param date Character or Date for the date to display on the title page.
#' @param theme Optional character theme/style to pass to the template.
#' @param image Optional path to an image file to include on the title/cover.
#'   If provided the file is copied into the template directory and referenced
#'   by basename in the generated YAML metadata.
#' @param dir Directory where the template and temporary files will be
#'   installed/created. Defaults to a temporary directory created by
#'   `tempfile()`; provide a persistent path to inspect or reuse the files.
#' @param quiet Logical; if `TRUE` suppresses Quarto CLI output and informational
#'   messages where possible.
#' @param no_prompt Logical; if `TRUE` do not ask the user for interactive
#'   approval to install the template into the current project. This is the
#'   standard "non-interactive" override used by many R helper functions.
#'   Defaults to `FALSE`; in non-interactive sessions it is automatically
#'   treated as `TRUE` to avoid blocking prompts (useful in CI).
#' @param ... Additional arguments forwarded to [quarto_render()] (advanced).
#'
#' @return Invisibly returns the normalized path to the generated PDF on
#'   success. Throws an error (via cli) on failure.
#'
#' @seealso
#' - Template documentation: <https://nmfs-opensci.github.io/quarto_titlepages/>
#' - [quarto_use_template()], [quarto_render()], [find_quarto()]
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' quarto::titlepage_pdf(output = "my-titlepage.pdf", title = "Report", author = "Me")
#'
#' # Skip interactive prompts (CI or scripted use)
#' quarto::titlepage_pdf("my-titlepage.pdf", title = "Report", no_prompt = TRUE)
#'
#' # Include an image from disk
#' quarto::titlepage_pdf("cover.pdf", title = "Report", image = "logo.png")
#' }
#' @export
titlepage_pdf <- function(
  output = "titlepage.pdf",
  title = NULL,
  author = NULL,
  date = NULL,
  theme = NULL,
  image = NULL,
  dir = tempfile("quarto-titlepage-"),
  quiet = FALSE,
  no_prompt = FALSE,
  ...
) {
  # In non-interactive sessions, default to no_prompt = TRUE unless the user
  # explicitly set no_prompt = FALSE. This makes scripting/CI behave as
  # expected without prompts.
  if (!rlang::is_interactive() && identical(no_prompt, FALSE)) {
    no_prompt <- TRUE
  }
  # ensure quarto available
  quarto_bin <- find_quarto()

  # If the template is already installed in the current project, offer to use it
  installed_in_cwd <- FALSE
  try({
    installed_in_cwd <- any(grepl("quarto_titlepages", list.dirs(getwd(), recursive = TRUE), fixed = TRUE))
  }, silent = TRUE)

  # Detect whether user passed a non-temporary dir (heuristic: not under tempdir())
  dir_norm <- tryCatch(normalizePath(dir, winslash = "/", mustWork = FALSE), error = function(e) dir)
  user_provided_dir <- !startsWith(dir_norm, normalizePath(tempdir(), winslash = "/", mustWork = FALSE))

  install_template_into_cwd <- FALSE
  if (!installed_in_cwd && !user_provided_dir && rlang::is_interactive()) {
    # Ask user if they want to install the template into the current directory
    approved <- check_extension_approval(
      no_prompt = no_prompt,
      what = "Quarto template 'nmfs-opensci/quarto_titlepages'",
      see_more_at = "https://nmfs-opensci.github.io/quarto_titlepages/"
    )
    if (isTRUE(approved)) {
      install_template_into_cwd <- TRUE
      # install into current directory
      quarto_use_template("nmfs-opensci/quarto_titlepages", dir = getwd(), no_prompt = no_prompt, quiet = quiet)
      dir <- getwd()
      install_template_flag <- FALSE
    } else {
      install_template_flag <- TRUE
    }
  } else {
    # default behavior: install into provided dir
    install_template_flag <- TRUE
  }

  # Prepare the dir and qmd using the helper (it will install template if requested)
  prep <- prepare_titlepage_dir(dir = dir, title = title, author = author, date = date, image = image, theme = theme, quiet = quiet, install_template = install_template_flag, no_prompt = no_prompt)
  input_qmd <- prep$input_qmd

  # prepare metadata override to set output file name and pass title/author/date
  metadata <- list()
  if (!is.null(title)) metadata$title <- title
  if (!is.null(author)) metadata$author <- author
  if (!is.null(date)) metadata$date <- as.character(date)
  if (!is.null(theme)) metadata$theme <- theme
  if (!is.null(image)) metadata[["cover-image"]] <- basename(image)
  # set output-file so quarto_render writes the expected filename
  metadata[["output-file"]] <- normalizePath(output, winslash = "/", mustWork = FALSE)

  # render
  quarto_render(input = input_qmd, output_format = "pdf",
                output_file = basename(output), metadata = metadata,
                quiet = quiet, ...)

  out_path <- normalizePath(file.path(getwd(), basename(output)), mustWork = FALSE)

  if (!file.exists(out_path)) {
    cli::cli_abort(c(
      "Failed to generate title page PDF.",
      "i" = "Look in {.path {prep$dir}} for the files generated by the template and try rendering manually."
    ))
  }

  invisible(out_path)
}

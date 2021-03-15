#' @export
runInterface <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package \"shiny\" needed for this function to work. Please install it.", call. = FALSE)
  }
  appDir <- system.file("shiny-examples", "qmethod-gui", package = "qmethod")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `qmethod`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}

#' @export
runInterface <- function() {
  appDir <- system.file("shiny-examples", "qmethod-gui", package = "qmethod")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `qmethod`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}

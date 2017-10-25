# retrieving the OS:
getos <- function() {
  paste0(Sys.info()["sysname"])
}


# ------------------------------------------------------------------------------


#' Define the GAMA path
#'
#' Define the path of GAMA Java plugins.
#'
#' @param path Absolute path to the GAMA folder.
#' @keywords internal
defpath <- function(path) {
# if we use OSX, the plugin is located in the Contents/eclipse sub directory of
# gama otherwise it is at its root:
  subpath <- ifelse(getos() == "Darwin", "/Contents/eclipse", "")
  gamapath <- paste0(path, subpath, "/plugins")
  plugins <- grep("org.eclipse.equinox.launcher_.*", dir(gamapath), value = TRUE)
  options(gamar.plugins = paste(paste0(gamapath, "/", plugins), collapse = ":"))
  options(gamar.startjar = paste0(gamapath, "/", plugins))
  options(gamar.Xmx = "2048m")
  options(gamar.Xms = "512m")
}


# ------------------------------------------------------------------------------


.onAttach <- function(...) {
  os <- getos()
  path <- "/Applications/Gama.app"
  if(getos() == "Darwin" & file.exists(path)) {
    defpath(path)
    packageStartupMessage(paste("Using the following GAMA executable:", path))
  } else {
    packageStartupMessage("WARNINGS: (1) make sure that GAMA (v >= 1.7.0) is installed on your machine.")
    packageStartupMessage("              if not, check http://gama-platform.org")
    packageStartupMessage("          (2) define the path of your GAMA executable by typing:")
    packageStartupMessage("              defpath('the_path_to_your_GAMA_executable')")
  }
}


# ------------------------------------------------------------------------------


.onDetach <- function(...) {
  options(gamar.plugins = NULL)
  options(gamar.startjar = NULL)
  options(gamar.Xmx = NULL)
  options(gamar.Xms = NULL)
}

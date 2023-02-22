detach_packages <- function(packages) {
  invisible(lapply(packages, function(pkg) {
    # Try to unload the package with unloadNamespace()
    # If it throws an error, catch it and print a warning message
    result <- tryCatch(
      expr = {
        unloadNamespace(pkg)
      },
      error = function(e) {
        NULL
      }
    )
    # Return the result of the tryCatch(), which will be NULL if there was an error,
    # or the message if the package was successfully unloaded
    result
  }))
}
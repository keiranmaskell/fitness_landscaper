
#init

#the list of R packages needed for this project
list_of_packages <- c("xlsx")

#check to see if packages are installed, if not, install them
lapply(list_of_packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
})

#check to see if packages successfully loaded (and installed)
#and return the names of any packages that failed to load
loaded_packages <- lapply(list_of_packages, require,
      character.only = TRUE, quietly = TRUE)

if (all(unlist(loaded_packages))) {
  message("Packages successfully loaded")
} else {
  message(sprintf("The following failed to load: %s",list_of_packages[loaded_packages==FALSE]))
  sorry_dave()
  stop("One or more packages failed to load")
}


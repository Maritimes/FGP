.onAttach <- function(libname, pkgname) {
  localVer = utils::packageDescription('FGP')$Version
  packageStartupMessage(paste0("\nVersion: ", localVer))
}

.onLoad <- function(libname, pkgname){
  options(stringsAsFactors = FALSE)
  Mar.utils::updateCheck(gitPkg = 'Maritimes/FGP')
}

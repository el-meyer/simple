
.onAttach <- function(libname, pkgname) {
  
  v <- getNamespaceVersion("simple")
  rule <- paste0(rep("-", getOption("width")), collapse = "")
  
  packageStartupMessage(rule)
  packageStartupMessage(paste0("simple ", v, " loaded."))
  packageStartupMessage("Visit for more information: ")
  packageStartupMessage("https://github.com/el-meyer/simple/")
  packageStartupMessage(rule)
  packageStartupMessage("For additional designers, install the simpleDesigners package:")
  packageStartupMessage("https://github.com/el-meyer/simpleDesigners/")
  packageStartupMessage(rule)
  
}


.onDetach <- function(libpath) {
  
  rule <- paste0(rep("-", getOption("width")), collapse = "")
  packageStartupMessage(rule)
  packageStartupMessage("Thank you for using the simple package!")
  packageStartupMessage("Don't forget to report bugs and request features under:")
  packageStartupMessage("https://github.com/el-meyer/simple/issues")
  packageStartupMessage(rule)
}

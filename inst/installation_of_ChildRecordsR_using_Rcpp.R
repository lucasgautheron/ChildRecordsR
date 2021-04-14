

PKG_DIR = "/mnt/94707AA4707A8CAC/CNRS/"

devtools::document(pkg = file.path(PKG_DIR, 'ChildRecordsR'))
remove_so_files = sapply(list.files(file.path(PKG_DIR, 'ChildRecordsR', 'src'), pattern = ".o$|.so$", full.names = T), function(x) file.remove(x))       # this command will remove all '.so' and '.o' files from the 'src' directory

# Open the NAMESPACE file and remove the first line which is:   S3method(aggregate,rating)   Then save and close the file

setwd(file.path(PKG_DIR, 'ChildRecordsR'))
tools::package_native_routine_registration_skeleton('.', con = file.path(PKG_DIR, 'ChildRecordsR', 'src', 'init.c'), character_only = FALSE)
Rcpp::compileAttributes(verbose = T)
setwd(PKG_DIR)
system("R CMD build ChildRecordsR")
system("R CMD check --as-cran ChildRecordsR_0.1.0.tar.gz")     # This command should give only Warnings and Notes but not Errors (if you have setup latex and pandoc to create the pdf versions of the documentation / vignettes)
system("R CMD INSTALL ChildRecordsR_0.1.0.tar.gz")



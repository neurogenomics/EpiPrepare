find_bin_package <- function(package,
                             verbose=TRUE){
    bin_dirs <- strsplit(Sys.getenv("PATH"),":")[[1]]
    pkg_paths <- file.path(bin_dirs,package)
    pkg_paths <- pkg_paths[file.exists(pkg_paths)]
    messager(length(pkg_paths),"valid path(s) found for package:",package,
             v=verbose)
    return(pkg_paths)
}
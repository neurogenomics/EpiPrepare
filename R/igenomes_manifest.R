#' iGenomes manifest
#' 
#' List all files in the iGenomes manifest file.
#' @source \href{https://github.com/ewels/AWS-iGenomes/issues/13}{GitHub Issue}
igenomes_manifest <- function(){
    message("Retrieving manifest file from iGenomes.")
    manifest <- data.table::fread(file.path(
        "https://raw.githubusercontent.com/ewels",
        "AWS-iGenomes/master/ngi-igenomes_file_manifest.txt"
    ), header = FALSE, col.names = "url") 
    split <- stringr::str_split(manifest$url,pattern = "/", simplify = TRUE)
    manifest$species <- split[,5] 
    manifest$source <- split[,6] 
    manifest$build <- split[,7]  
    manifest$type <- split[,8]  
    manifest$tool <- split[,9] 
    manifest$name <- basename(manifest$url) 
    manifest$extension <- tools::file_ext(manifest$name)
    return(data.frame(manifest))
}
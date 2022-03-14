


rename_files <- function(files,
                         sample_dict,
                         max_char=NULL){
    split_names <- stringr::str_split(names(files),"[.]", simplify = TRUE)
    names(files) <- paste(
        sample_dict[split_names[,1]],
        split_names[,1],
        split_names[,2],
        sep = "."
    )
    if(!is.null(max_char)){
        names(files) <- stringr::str_trunc(names(files), width = max_char) 
    }
    names(files) <- make.unique(names(files))
    message("New names:\n",paste("-",names(files),collapse = "\n"))
    return(files)
}


gather_files <- function(path,
                         type = "peaks.consensus.filtered",
                         as_granges = TRUE,
                         mc.cores = 1){
    type <- tolower(type[1])
    type_key <- c(
        "peaks.stringent"= "*.peaks.bed.stringent.bed$",
        "peaks.consensus"="*.consensus.peaks.bed$",
        "peaks.consensus.filtered"="*.consensus.peaks.filtered.awk.bed$",
        "picard"= "*.target.markdup.MarkDuplicates.metrics.txt$"
    )
    pattern <- type_key[type]
    if(is.na(pattern)){
        stop("type must be one of:\n",
             paste("-",names(type_key), collapse = "\n"))
    }
    message("Searching for ",type," files...")
    paths <- list.files(path = path,
                        pattern = unname(pattern), 
                        recursive = TRUE, 
                        full.names = TRUE) 
    #### Report files found ####
    if(length(paths)==0) stop(length(paths)," matching files identified.")
    message(length(paths)," matching files identified.")
    #### Construct names ####
    message("Constructing file names.")
    if(startsWith(type,"peaks")){
        # paths <- grep("03_peak_calling", paths, value = TRUE)
        names <- paste(basename(dirname(dirname(dirname(paths)))),
                       stringr::str_split(basename(paths),"[.]",
                                          simplify = TRUE)[,1], 
                       sep=".")
    } else if(type=="picard"){
        names <- paste(basename(dirname(dirname(dirname(dirname(dirname(dirname(paths))))))),
                       stringr::str_split(basename(paths),"[.]",
                                          simplify = TRUE)[,1], 
                       sep=".")
    } 
    #### Import files ####
    message("Importing files.")
    files <- parallel::mclapply(paths, function(x){
        message_parallel(x,"\n")
        if(startsWith(type,"peaks")){
            dat <- ChIPseeker::readPeakFile(x, as = "GRanges")
        } else if(type=="picard"){ 
            dat <- data.table::fread(x, skip = "LIBRARY",
                                     fill = TRUE,
                                     nrows = 1)
        }
        return(dat)
    }, mc.cores = mc.cores) %>% `names<-`(names)
    return(files)
}

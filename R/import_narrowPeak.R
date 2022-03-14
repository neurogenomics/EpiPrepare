

#' Import narrow peaks
#' 
#'   @source \href{https://support.bioconductor.org/p/80535/#80537}{
#' Solution from Bioconductor forum}
import_narrowPeak <- function(con){
    extraCols <- c(signalValue = "numeric", 
                   pValue = "numeric",
                   qValue = "numeric",
                   peak = "integer")
    V2 <- NULL;
    #### Parse column metadat ####
    as_file <- file.path(
        "https://www.encodeproject.org/documents",
        "948203bb-8eb2-42a2-8b12-1c10f356c998/@@download/attachment",
        "narrowPeak.as"
    )
    f <- readLines(as_file)
    f <- f[!f %in% c("(",")")][-seq_len(2)]
    type_dict <- c("string"="character", 
                   "char[1]"="character",
                   "uint"="integer",
                   "int"="integer",
                   "float"="numeric")
    meta <- data.table::fread(text = f, 
                              header = FALSE) %>%
        tidyr::separate(col = "V1", 
                        into = c("type","name"), 
                        sep = " +") %>%
        dplyr::rename(info=V2) %>% 
        dplyr::mutate(typer=type_dict[type])
    extraCols <- stats::setNames(meta$typer, meta$name)
    reserved <- c("chrom","name","score","strand","chromStart","chromEnd")
    extraCols <- extraCols[!names(extraCols) %in% reserved]
    #### Import peak data ####
    peaks <- rtracklayer::import(con = con, 
                                 extraCols = extraCols)
    message(formatC(length(peaks), big.mark = ",")," peaks imported.")
    return(peaks)
}

#' iGenomes search
#' 
#' Search iGenomes
#' @export
igenomes_search <- function(source="Ensembl",
                            species="Homo_sapiens", 
                            build = c("hg19","grch37"),
                            type=NULL,
                            tool="bowtie2",
                            name=NULL,
                            extension="fa",
                            verbose = TRUE){
    manifest <- igenomes_manifest()
    for(x in c("source","species","build","type","tool","name","extension")){ 
        opts <- eval(parse(text=x))
        query <- paste(opts, collapse = "|")
        if(!is.null(opts)) { 
            messager("Filtering by:",x)
            message(query)
            manifest <- manifest[
                grepl(pattern = query,
                      x = manifest[[x]],
                      ignore.case = TRUE
                      ), 
            ]
            messager(" -",nrow(manifest),"files remaining.",
                     v=verbose)
        }
    }
    return(manifest)
}
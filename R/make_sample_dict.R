make_sample_dict <- function(batch_list, 
                             metadata_path){
    
    batches <- data.frame(batch=batch_list) %>%
        tidyr::separate(col = "batch",
                        into = c("V1","phase","day","month","year"),
                        remove = FALSE, sep = "_") %>%
        dplyr::mutate(date = strptime(paste(day,month,year,sep="/"),
                                      format="%d/%b/%Y", tz = "UTC")) 
    sample_meta <- readxl::read_excel(metadata_path,
                                      skip = 6) %>%
        dplyr::mutate(name = gsub(" +","",paste(`histone mark`,
                                                `antibody company`,
                                                # `antibody catalog`,
                                                # `antibody concentration`,
                                                # `cell line`,
                                                sep="_"))
        )  %>%
        data.frame() %>%  
        dplyr::left_join(batches, by="date")
    sample_dict <- stats::setNames(sample_meta$name, 
                                   sample_meta$batch)
    return(sample_dict)
}

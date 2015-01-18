pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## directory is character vector indicating data location
        ## pollutant is either sulfate or nitrate
        ## id is an integer vector
        if (file.exists(directory)) {
                file_list <- list.files(directory, full.names=TRUE)
        }
        else {
                sprintf('The directory [%s] does not exist.', directory)
        }
                
        all_data_tmp <- lapply(file_list, read.csv)
        all_data <- do.call(rbind, all_data_tmp)        
        
        selected_ids <- all_data[which(all_data[, "ID"] %in% id),]
        round(mean(selected_ids[[pollutant]], na.rm = TRUE), digits = 3)
}
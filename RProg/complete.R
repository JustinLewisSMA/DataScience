complete <- function(directory, id = 1:332) {
        ## 'id' is an integer vector indicating the monitor ID numbers to be 
        ## used
        file_list <- list.files(directory, full.names=TRUE)
 
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the number of 
        ## complete cases
        
        all_data_tmp <- lapply(file_list, read.csv)
        all_data <- do.call(rbind, all_data_tmp)
        

        new_matrix <- matrix(0, 0, 2)

        for (i in id) {
                selected_ids <- all_data[which(all_data[, "ID"] == i),]
                com_rows <- nrow(na.omit(selected_ids))
                new_matrix <- rbind(new_matrix, c(i, com_rows))                
        }
        
        df = data.frame(new_matrix)
        names(df)[1] <- paste("id")
        names(df)[2] <- paste("nobs")
        df
}
#' An Image Scrambling Function
#'
#' This function copies the information from tif image files, renames the images with a scramble key, then saves two csv files, one with the scramble key and one with just the scramble numbeers.
#' @param path What is the path to the folder containing the images?
#' @param chunks How many blocks of information should the image names be broken into as separated by "_"? Defaults to 1.
#' @keywords image scrambler
#' image_scrambler()










image_scrambler <- function(path, chunks = 1){
  file_paths <- list.files(path = path)
  file_number <- length(file_paths)
  file_info <- regmatches(file_paths, regexpr(pattern = "([A-z0-9_\\. ]+)(?=\\.tif$)", file_paths, perl = TRUE)) #this is for grabbing all the info from the file name which will have some sort of identifying information of the image in regards to other images in the folder. It removes the path and the extension from the file name.
  scramble_num <- sample(1:file_number, file_number, replace = FALSE) #this creates a set of random numbers from 1 to the number of files so the highest number used for scrambling is the number of files. These will be what the function uses to replace the file names

  if(chunks > 1){
    info_split <- strsplit(file_info, split = "_")
    empty_matrix <- c()
    empty_matrix <- matrix(nrow = file_number, ncol = chunks)
    name_vec <- c(rep(0, times = chunks))
    for(i in 1:chunks){
      name_vec[i] <- paste0("Column_", i)
    } #For producing generic column names (Column_1 .... Column_i) based on the number of chunks
    colnames(empty_matrix) <- name_vec
    for(i in 1:chunks){
      empty_matrix[,i] <- c(sapply(info_split, "[",i))
    }
    file_df <- cbind(file_info, empty_matrix, scramble_num, file_paths) ### Combine everything into a matrix
  } else if(chunks == 1){
    file_df <- as.data.frame(cbind(file_info, scramble_num, file_paths))
  } else {
    print("Error: Chunks parameter can not be negative")
  } #This block is for building columns by breaking up the info it pulled from the file number to a number of "chunks" as specified by the chunk arguement



  file.rename(from = file.path(path, file_df[,"file_paths"]) , to = file.path(path, file_df[,"scramble_num"])) ###Rename the folder

  key_file <- subset(file_df, select = -c(file_paths))
  scrambled_file <- subset(file_df, select = c(scramble_num))

  setwd(path)
  write.csv(key_file, file = "key_file.csv")
  write.csv(scrambled_file, file = "scrambled_file.csv")
}

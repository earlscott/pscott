#' An ITCN Results parser
#'
#' This function used the results txt file produced by the ImageJ plugin ITCN and parses it to extract the cell counts and area measurements.
#' @param path What is the path to the folder containing the results file?
#' @export
#' @keywords ImageJ FIJI parse ITCN Results
#' parse()

parse <- function(path, scramble = TRUE){
  results <- readChar(path, nchars = file.info(path)$size)
  img_names <- regmatches(results, gregexpr(pattern = "(?<=Image:\\s)(\\d+)(?=\\.tif)", results, perl = TRUE))
  cell_count <- regmatches(results, gregexpr(pattern = "(?<=\nNumber\\sof\\sCells:\\s)(\\d+)(?=\\sin\\s)", results, perl = TRUE))
  img_area <- regmatches(results, gregexpr(pattern = "(?<=\\sin\\s)((\\d+)(\\.*)(\\d*))(?=\\ssquare)", results, perl = TRUE))
  itcn <- as.data.frame(c(img_names, cell_count, img_area))
  colnames(itcn) <- c("Image_Name", "Cell_Count", "Image_Area")
  itcn$Order <- c(1:nrow(itcn))
  write.csv(itcn, path = file.path(path, "ITCN_Results.csv"), row.names = FALSE)
}


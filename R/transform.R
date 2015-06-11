# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Constructor function for \code{extracted} class
#'
#' @param data A \code{data.frame} extracted from a database
#' @return A \code{data.frame} with primary class \code{extracted}
extracted <- function(data) {
  if(class(data) != "data.frame") stop("must be a data.frame")
  class(data) <- c("extracted", "data.frame")
  data
}

#' Transform data for database migration
#'
#' Transform data of class \code{extracted} for migrating to a database
#' @param extracted_data A tidy data set with class \code{extracted}
#' @param trans_functions A list of functions that will be applied to the \code{extracted_data}
#' object. The one parameter of the function must be the \code{extracted_data} object. The
#' function must return a tidy data set that will be passed sequentially from one
#' transformation function to the next.
#' @param new_columns Character vector of the column to be returned as a transformed data output
#' @return A \code{data.frame}.
#' @examples
#' df <- data.frame(
#' a = rep(1:4, each = 4),
#' b = rep(c(0L, 1L, 2L, 3L), 4),
#' c = rep(letters[1:8], 2),
#' d = rep(1:2, each = 8),
#' e = as.character(seq(as.Date("2015/1/1"), as.Date("2015/1/16"), "days")),
#' stringsAsFactors = FALSE
#' )
#'
#' makeVisit <- function(data){
#'   old.visit <- c("a", "b", "c", "d", "e", "f", "g")
#'   new.visit <- paste0("V0", 1:7)
#'   data$Visit <- apply(data, 1, function(row){
#'      if(row["c"] %in% old.visit){
#'        new.visit[grep(row["c"], old.visit, fixed = TRUE)]
#'      }else{
#'        row["c"]
#'      }
#'    })
#'    data
#'  }
#'
#' makeSubject <- function(data){
#'   data$Subject <- paste0("PPMI-", data$d)
#'   data
#' }
#'
#' makeCase <- function(data){
#'   data$Case <- paste(data$Subject, data$Visit, sep = "-")
#'    data
#' }
#'
#' makeTest <- function(data){
#'    data <- gather(data, Test, Value, a:b)
#'    i <- sapply(data, is.factor)
#'    data[i] <- lapply(data[i], as.character)
#'    data
#'  }
#'
#' output <- transform(extracted_data = extracted(df),
#'                     trans_functions = list(makeVisit, makeSubject, makeCase, makeTest),
#'                     new_columns = c("Subject", "Visit", "Case", "Test", "Value"))
transform.extracted <- function(extracted_data, trans_functions, new_columns){
  if(class(extracted_data)[1] != "extracted") stop("must be an object of class extracted")
  class(extracted_data) <- "data.frame"
  for(i in 1:length(trans_functions)){
    extracted_data <- trans_functions[[i]](extracted_data)
  }
  extracted_data[, new_columns]
}




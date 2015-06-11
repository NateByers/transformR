library(dplyr)
library(tidyr)
library(stringr)

context("check transform.extracted()")

df <- data.frame(
  a = rep(1:4, each = 4),
  b = rep(c(0L, 1L, 2L, 3L), 4),
  c = rep(letters[1:8], 2),
  d = rep(1:2, each = 8),
  e = as.character(seq(as.Date("2015/1/1"), as.Date("2015/1/16"), "days")),
  stringsAsFactors = FALSE
)

makeVisit <- function(data){
  old.visit <- c("a", "b", "c", "d", "e", "f", "g")
  new.visit <- paste0("V0", 1:7)
  data$Visit <- apply(data, 1, function(row){
    if(row["c"] %in% old.visit){
      new.visit[grep(row["c"], old.visit, fixed = TRUE)]
    }else{
      row["c"]
    }
  })
  data
}

makeSubject <- function(data){
  data$Subject <- paste0("PPMI-", data$d)
  data
}

makeCase <- function(data){
  data$Case <- paste(data$Subject, data$Visit, sep = "-")
  data
}

makeTest <- function(data){
  data <- gather(data, Test, Value, a:b)
  i <- sapply(data, is.factor)
  data[i] <- lapply(data[i], as.character)
  data
}

output <- transform(extracted_data = extracted(df), trans_functions = list(makeVisit, makeSubject, makeCase, makeTest),
                    new_columns = c("Subject", "Visit", "Case", "Test", "Value"))


test_that("column names are correct", {
  expect_identical(names(output), c("Subject", "Visit", "Case", "Test", "Value"))
})

test_that("values are changed", {
  expect_equal(unique(output$Visit)[str_order(unique(output$Visit))],
               c(paste0("V0", 1:7), "h")[str_order(c(paste0("V0", 1:7), "h"))])
  expect_equal(unique(output$Subject)[str_order(unique(output$Subject))],
               unique(paste0("PPMI-", df$d))[str_order(unique(paste0("PPMI-", df$d)))])
  expect_equal(unique(output$Case)[str_order(unique(output$Case))],
               paste("PPMI", df$d, rep(c(paste0("V0", 1:7), "h")), sep = "-")[str_order(paste("PPMI", df$d, rep(c(paste0("V0", 1:7), "h")), sep = "-"))])
  expect_equal(unique(output$Test), c("a", "b"))
})

test_that("records are correct", {
  expect_equal(filter(output, Subject == "PPMI-1", Visit == "V01", Test == "a")[, "Value"],
               filter(df, d == 1, c == "a")[, "a"])
  expect_equal(filter(output, Subject == "PPMI-1", Visit == "V01", Test == "b")[, "Value"],
               filter(df, d == 1, c == "a")[, "b"])
  expect_equal(filter(output, Subject == "PPMI-2", Visit == "V01", Test == "a")[, "Value"],
               filter(df, d == 2, c == "a")[, "a"])
  expect_equal(filter(output, Subject == "PPMI-2", Visit == "V01", Test == "b")[, "Value"],
               filter(df, d == 2, c == "a")[, "b"])
  expect_equal(filter(output, Subject == "PPMI-1", Visit == "V03", Test == "a")[, "Value"],
               filter(df, d == 1, c == "c")[, "a"])
  expect_equal(filter(output, Subject == "PPMI-1", Visit == "V03", Test == "b")[, "Value"],
               filter(df, d == 1, c == "c")[, "b"])
  expect_equal(filter(output, Subject == "PPMI-2", Visit == "V03", Test == "a")[, "Value"],
               filter(df, d == 2, c == "c")[, "a"])
  expect_equal(filter(output, Subject == "PPMI-2", Visit == "V03", Test == "b")[, "Value"],
               filter(df, d == 2, c == "c")[, "b"])
})

test_that("reshaping works", {
  expect_equal(dim(output)[1], length(unique(df$c))*length(unique(df$d))*length(c("a", "b")))
})

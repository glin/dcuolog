library(testthat)

context("utils")

test_that("read_lines handles UTF-8", {
  lines <- c("\u00b0", "\u00f3", "\u00e6", "\u00f8", "\u00e5")
  tmp <- tempfile()
  write.table(lines, file=tmp, quote=FALSE, row.names=FALSE, col.names=FALSE, fileEncoding="UTF-8")
  expect_equal(lines, read_lines(tmp))
  file.remove(tmp)
})

test_that("read_lines handles Latin-1", {
  lines <- c("\u00b0", "\u00f3", "\u00e6", "\u00f8", "\u00e5")
  tmp <- tempfile()
  write.table(lines, file=tmp, quote=FALSE, row.names=FALSE, col.names=FALSE, fileEncoding="Latin1")
  expect_equal(lines, read_lines(tmp))
  file.remove(tmp)
})
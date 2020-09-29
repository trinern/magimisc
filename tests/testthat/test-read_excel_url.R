test_that("read_excel_url works as expected",{

  excel_test <- read_excel_url(path = "https://file-examples.com/wp-content/uploads/2017/02/file_example_XLSX_100.xlsx")

  expect_equal(nrow(excel_test), 100)
  expect_equal(ncol(excel_test), 8)
  expect_true(all(complete.cases(excel_test)))


})

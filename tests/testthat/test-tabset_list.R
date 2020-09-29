context("test tabset_list")

list = list(a = 1,
            b = 2)

unnamed_list = list(1,2)

test_that("output is correct", {

  expect_output(tabset_list(list, 2), '##   a \\n \\n\\[1\\] 1\\n\\n \\n##   b \\n \\n\\[1\\] 2\\n\\n ')
})

test_that("prints warning message", {
  expect_warning(tabset_list(list, 2), "results must be set to 'asis' in chunk options")
})

test_that("stops if list is not named", {
  expect_error(tabset_list(unnamed_list, 2))
})


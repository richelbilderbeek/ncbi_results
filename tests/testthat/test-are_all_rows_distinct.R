test_that("use", {

  t <- tibble::tibble(
    a = 1,
    b = 2
  )

  expect_true(are_all_rows_distinct(t))

  t <- tibble::tibble(
    a = 1,
    b = 2
  )
  t <- dplyr::bind_rows(t, t)
  expect_false(are_all_rows_distinct(t))
})

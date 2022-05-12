test_that("multiplication works", {
  expected = tibble(mic_column = c(4, ">8", "≤2", "=<2"),
      left_bound = c(2, 8, 0, 0),
      right_bound = c(4, Inf, 2, 2))

  actual = import_mics(mic_column = c(4, ">8", "≤2", "=<2"))
  expect_equal(actual, expected)
})

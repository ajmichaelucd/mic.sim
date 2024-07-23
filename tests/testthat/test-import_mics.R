test_that("multiplication works", {
  expected = tibble(
      left_bound = c(1, 3, -Inf, -Inf),
      right_bound = c(2, Inf, 1, 1),
      mic_column = c(4, ">8", "≤2", "=<2"))
  attr(expected, "source") = "imported"
  attr(expected, "lr_col") = FALSE
  attr(expected, "mic_class") = "imported_mic_column"
  attr(expected, "metadata") = FALSE
  attr(expected, "scale") = "log"

  actual = import_mics(mic_column = c(4, ">8", "≤2", "=<2"))
  expect_equal(actual, expected)
})

actual %>% attributes()

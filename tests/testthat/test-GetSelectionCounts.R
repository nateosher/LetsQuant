test_that("`GetSelectionCounts` works", {
  # Check first five selection counts
  expect_equal(selection_count_tib$selection_counts[1], 30)
  expect_equal(selection_count_tib$selection_counts[2], 9)
  expect_equal(selection_count_tib$selection_counts[3], 23)
  expect_equal(selection_count_tib$selection_counts[4], 1)
  expect_equal(selection_count_tib$selection_counts[5], 0)

  # Check sum (haha)
  expect_equal(sum(selection_count_tib$selection_counts), 1819)

  # Check distributions
  expect_equal(selection_count_tib$distribution[1], "intercept")
  expect_equal(selection_count_tib$distribution[2], "normal")
  expect_equal(selection_count_tib$distribution[3], "beta")
})

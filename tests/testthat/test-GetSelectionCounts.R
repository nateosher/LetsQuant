test_that("`GetSelectionCounts` works", {
  # Check first five selection counts
  selection_count_tib = test_quantlet_basis$selection_counts
  expect_equal(selection_count_tib$selection_counts[1], 30)
  expect_equal(selection_count_tib$selection_counts[2], 9)
  expect_equal(selection_count_tib$selection_counts[3], 23)
  expect_equal(selection_count_tib$selection_counts[4], 2)
  expect_equal(selection_count_tib$selection_counts[5], 1)

  # Check sum (haha)
  expect_equal(sum(selection_count_tib$selection_counts), 2339)

  # Check distributions
  expect_equal(selection_count_tib$distribution[1], "intercept")
  expect_equal(selection_count_tib$distribution[2], "normal")
  expect_equal(selection_count_tib$distribution[3], "beta")
})

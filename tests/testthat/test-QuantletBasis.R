test_that("`new_QuantletBasis` catches malformed inputs", {
  expect_error(new_QuantletBasis(NULL), "raw data must have at least one entry")
  expect_error(new_QuantletBasis(1:5), "raw data must be passed as list")

  # Not all numeric
  expect_error(new_QuantletBasis(list(
        1:10 %>% as.numeric(),
        10:15 %>% as.numeric(),
        c("a", "b", "c")
    )), "all `raw_data` list entries must be numeric")

  # Not all have ids
  expect_error(new_QuantletBasis(list(
    list(data = 1:10 %>% as.numeric(), id = "A"),
    list(data = 10:15 %>% as.numeric())
  )), "all `raw_data` list entries must be numeric")

  # Not all have data
  expect_error(new_QuantletBasis(list(
    list(data = 1:10 %>% as.numeric(), id = "A"),
    list(id = "B")
  )),
  "all `raw_data` list entries must be numeric")

  # Not all sorted
  expect_error(new_QuantletBasis(list(
    1:10 %>% as.numeric(),
    seq(10, 1) %>% as.numeric()
  )), "all numeric vector entries must be sorted low -> high")

  # a and b
  expect_error(new_QuantletBasis(list(
    1:10 %>% as.numeric(),
    10:15 %>% as.numeric()
  ), a = c(1,2,-3), b = c(1,2, 3)),
  "all entries of `a` and `b` must be strictly greater than 0")

  expect_error(new_QuantletBasis(list(
    1:10 %>% as.numeric(),
    10:15 %>% as.numeric()
  ), a = c("a", "b", "c"), b = c(1,2, 3)),
  "`a` and `b` parameters must both be numeric vectors")


  # Metadata
  expect_error(
    new_QuantletBasis(
      list(
        1:10 %>% as.numeric(),
        10:15 %>% as.numeric()
      ),
      id_list = c("a", "b"),
      metadata = "abc"
    ),
    "metadata should be passed as a data.frame or tibble"
  )

  expect_error(
    new_QuantletBasis(
      list(
        1:10 %>% as.numeric(),
        10:15 %>% as.numeric()
      ),
      metadata = tibble(v = 1:2)
    ),
    "in order to use"
  )

  expect_error(
    new_QuantletBasis(
      list(
        1:10 %>% as.numeric(),
        10:15 %>% as.numeric()
      ),
      id_list = c("a", "b"),
      metadata = tibble(id = c("c", "d"), v = 1:2)
    ),
    "no common ids"
  )

})


test_that("`new_QuantletBasis` produces `QuantletBasis` object", {
  expect_no_error({
    qb_1 = new_QuantletBasis(ex_data)
  })

  expect_equal(class(qb_1), "QuantletBasis")

  expect_no_error({
    qb_2 = new_QuantletBasis(ex_data, id_list = test_metadata$id)
  })

  expect_false(is.null(qb_2$ids))

  expect_no_error({
    qb_3 = new_QuantletBasis(ex_data,
                             id_list = test_metadata$id,
                             metadata = test_metadata)
  })

  expect_false(is.null(qb_3$ids))
  expect_false(is.null(qb_3$metadata))
})

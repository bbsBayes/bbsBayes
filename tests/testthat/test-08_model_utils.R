test_that("get_convergence()", {

  expect_warning(conv <- get_convergence(pacific_wren_model,
                                         variables = "strata_raw")) %>%
    suppressWarnings()

  expect_s3_class(conv, "data.frame")
  expect_named(conv,
               c("variable_type", "variable", "rhat", "ess_bulk", "ess_tail"))
  expect_equal(unique(conv$variable_type), "strata_raw")

  orig <- pacific_wren_model$model_fit$summary("strata_raw") %>%
    suppressWarnings()
  expect_equal(orig[, c("rhat", "ess_bulk", "ess_tail")],
               conv[, c("rhat", "ess_bulk", "ess_tail")],
               ignore_attr = TRUE)
})


test_that("get_model_vars()", {
  expect_silent(vars <- get_model_vars(pacific_wren_model))
  expect_true(all(c("strata_raw", "beta", "yeareffect", "n", "YearEffect") %in%
                    vars))
  expect_false("n_slope" %in% vars)

  expect_silent(vars <- get_model_vars(slope_test_model))
  expect_true(all(c("strata_raw", "beta", "yeareffect", "n", "n_slope") %in%
                    vars))
  expect_false("YearEffect" %in% vars)
})

test_that("get_summary()", {
  expect_warning(sum <- get_summary(pacific_wren_model,
                                     variables = "strata_raw")) %>%
    suppressWarnings()

  expect_s3_class(sum, "data.frame")

  orig <- suppressWarnings(pacific_wren_model$model_fit$summary("strata_raw"))
  expect_equal(sum, orig)
})

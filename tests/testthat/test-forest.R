context("Forest plots")

# Load a fitted brms model (from Dropbox--this can clearly be improved)
load(url("https://www.dropbox.com/s/woobuzqkjnf79yx/fit_rem.rda?raw=1"))

test_that("forest() returns a ggplot", {
  expect_true(is(forest(fit_rem), "ggplot"))
})

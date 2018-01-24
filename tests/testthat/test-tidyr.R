context("Testing tidyr-ing of labelled data")

library(dplyr)
library(tidyr)

# Input data
d <- tibble(
  id = 1:5,
  int = as.integer(1:5),
  num = int * 1.3,
  ch = letters[id],
  fnum = factor(num),
  fch = factor(ch)
)
l <- labelled(1:5, c(one=1, four=4, five=5))
var_label(l) <- 'This is a labelled variable'
na_values(l) <- 1
na_range(l) <- 4:5
d$lab <- l



# Subsetting --------------------------------------------------------------

funs <- c("var_label", "val_labels", "na_range", "na_values")

for(f in funs) {
  test_that(
    paste("function", f, "works when subsetting with ["),
    expect_identical(do.call(f, list(x=l)), do.call(f, list(x=l[1]))
    )
  )
}










# Gathering ---------------------------------------------------------------


test_that("gather() gives warning by default", {
  expect_warning(
    gather(d, variable, value, -id)
  )
})

test_that("gathering list columns works", {
  expect_silent(
    long <-
      d %>%
      mutate_at(
        vars(-id),
        as.list
      ) %>%
      gather(variable, value, -id)
  )
})

test_that("Gathered list column can be unnested", {
  z <- long %>%
    spread(variable, value)
})




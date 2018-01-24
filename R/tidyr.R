#' tidyr-related functions
#' @name tidyr

#' @rdname tidyr
#' @details
#' `as.list.labelled`
#' @export
as.list.labelled <- function(x, ...) {
  res <- vector("list", length(x))
  for (i in seq_along(x)) res[[i]] <- x[i]
  res
}

#' @rdname tidyr
#' @details
#' `[` method copies labels, na_values, and na_range
#' @export
"[.labelled_spss" <- function(x, ...) {
  res <- labelled_spss(
    NextMethod(),
    labels = attr(x, "labels"),
    na_values = na_values(x),
    na_range = na_range(x)
  )
  var_label(res) <- var_label(x)
  res
}

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


#' @rdname tidyr
#' @details
#' Unlist and copy attributes from first element
#' @export
smart_unlist <- function(x) {
  res <- unlist(x)
  all_attrs <- lapply(x, attributes)
  stopifnot(all_identical(all_attrs))
  mostattributes(res) <- attributes(x[[1]])
  res
}

is_lablist <- function(x) {
  inherits(x[[1]], 'labelled')
}


unnest2 <- function(data, ...) {
  mutate_if(data, is_lablist, smart_unlist) %>%
    unnest(...)
}






# Utils -------------------------------------------------------------------

all_identical <- function(x, verbose=getOption("verbose", FALSE)) {
  ch <- vapply(x, function(v) identical(v, x[[1]]), logical(1))
  if(!verbose) {
    return(all(ch))
  } else {
    message("elements different from 1st: ", paste(which(!ch), collapse=", "))
    all(ch)
  }
}

# Open this script before executing it. Argument completion, hover and
# signature help select a method from the inferred input class.
present <- function(x, ...) UseMethod("present")

#' Present selected columns.
#' @param x A data frame.
#' @param columns Columns to include.
#' @param digits Decimal places.
#' @param format Output format.
present.data.frame <- function(x, columns = names(x), digits = 3,
    format = c("text", "html"), ...) {
    list(data = x[columns], digits = digits, format = match.arg(format))
}

#' Present a numeric vector.
#' @param x Numeric values.
#' @param digits Decimal places.
#' @param scientific Use scientific notation.
present.numeric <- function(x, digits = 2, scientific = FALSE, ...) {
    format(x, digits = digits, scientific = scientific)
}

present.default <- function(x, ...) x

frame <- data.frame(value = c(1.2, 2.3, 3.4))
values <- c(1.2, 2.3, 3.4)
present(frame, format = "text") # columns, digits, format
present(values, scientific = TRUE) # digits, scientific

alias <- present
alias(frame, columns = "value")
frame |> present(format = "html")

# Dataset metadata and reviewed constructor summaries also work statically.
summary(iris, digits = 3)
fit <- stats::lm(mpg ~ wt, data = mtcars)
stats::predict(fit, interval = "confidence")

# An unknown input retains the generic's present(x, ...) information.
unknown_input <- function(x) present(x)

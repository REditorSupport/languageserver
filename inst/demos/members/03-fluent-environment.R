# No extra packages. Methods return self or a different statically known shape.
make_pipeline <- function(source) {
    self <- new.env(parent = emptyenv())
    self$source <- source
    self$filter <- function(predicate) self
    self$limit <- function(n = 10L) self
    self$collect <- function(format = "list") {
        list(data = source, format = format, metadata = list(cached = FALSE))
    }
    self
}

pipeline <- make_pipeline(iris)

# After the last $: collect, filter, limit, source.
# Signature help inside limit(): limit(n = 10L).
limited <- pipeline$filter("Sepal.Length > 5")$limit(n = 5L)

# Signature help inside collect(): collect(format = "list").
output <- limited$collect(format = "list")

# After output$: data, format, metadata. After metadata$: cached.
output$metadata$cached

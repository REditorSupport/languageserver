# R6 is a languageserver dependency. Public declarations supply the shape;
# the provider never calls initialize or the active getter to discover members.
Dataset <- R6::R6Class("Dataset", public = list(
    source = NULL,
    initialize = function(source) self$source <- source,
    select = function(columns) self,
    preview = function(n = 6L) list(data = self$source, rows = n)
))

LoggedDataset <- R6::R6Class("LoggedDataset",
    inherit = Dataset,
    public = list(log = function(message, level = "info") self),
    active = list(expensive = function(value) {
        stop("This getter must not run during language features")
    })
)

dataset <- LoggedDataset$new(iris)

# After the last $: inherited preview/select/source plus log/expensive.
# Signature help inside preview(): preview(n = 6L).
preview <- dataset$select(c("Species"))$log("selected")$preview(n = 3L)

# After preview$: data, rows. Hover rows shows its statically inferred value.
preview$rows

# Hover log/preview shows their respective signatures.
# expensive is offered as a declared member, but its result stays unknown.

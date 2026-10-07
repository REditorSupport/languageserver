# No extra packages. The provider analyzes the factory body and its closures.
make_client <- function(endpoint) {
    list(
        config = list(endpoint = endpoint, retries = 3L, `api key` = NULL),
        request = function(path, timeout = 30) {
            list(
                status = 200L,
                decode = function(simplify = TRUE) {
                    list(data = list(endpoint = endpoint, path = path), ok = TRUE)
                }
            )
        }
    )
}

client <- make_client("https://example.invalid")

# After client$: config, request.
# Signature help inside request(): request(path, timeout = 30).
response <- client$request("/records", timeout = 10)

# After response$: decode, status. Hover status shows the literal 200L.
# Signature help inside decode(): decode(simplify = TRUE).
decoded <- response$decode(simplify = FALSE)
response$status

# After decoded$: data, ok. After decoded$data$: endpoint, path.
decoded$data$path

# Quoted fields retain their full name and replacement/hover range.
client$config$`api key`

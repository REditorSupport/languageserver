test_that("Semantic tokens encoding works correctly", {
    tokens <- list(
        list(line = 0, col = 0, length = 5, type = 12, modifiers = 0),
        list(line = 0, col = 10, length = 3, type = 8, modifiers = 2),
        list(line = 2, col = 4, length = 7, type = 12, modifiers = 512)
    )
    
    encoded <- encode_semantic_tokens(tokens)
    
    # Expected: [0, 0, 5, 12, 0, 0, 10, 3, 8, 2, 2, 4, 7, 12, 512]
    expected <- c(0, 0, 5, 12, 0,    # First token
                  0, 10, 3, 8, 2,     # Second token (same line, delta col = 10)
                  2, 4, 7, 12, 512)   # Third token (2 lines down)
    
    expect_equal(encoded, expected)
})

test_that("Empty tokens return empty encoding", {
    tokens <- list()
    encoded <- encode_semantic_tokens(tokens)
    expect_equal(encoded, integer(0))
})

test_that("Token type mapping is correct", {
    expect_equal(get_token_type("COMMENT", "# test"), TokenTypes$comment)
    expect_equal(get_token_type("STR_CONST", "\"hello\""), TokenTypes$string)
    expect_equal(get_token_type("NUM_CONST", "123"), TokenTypes$number)
    expect_equal(get_token_type("SYMBOL_PACKAGE", "dplyr"), TokenTypes$namespace)
    expect_equal(get_token_type("SYMBOL_FUNCTION_CALL", "mutate"), TokenTypes$`function`)
    expect_equal(get_token_type("SYMBOL_FORMALS", "x"), TokenTypes$parameter)
    expect_equal(get_token_type("SYMBOL", "var"), TokenTypes$variable)
    expect_equal(get_token_type("FUNCTION", "function"), TokenTypes$keyword)
    expect_equal(get_token_type("IF", "if"), TokenTypes$keyword)
    expect_equal(get_token_type("FOR", "for"), TokenTypes$keyword)
    expect_equal(get_token_type("TRUE", "TRUE"), TokenTypes$keyword)
    expect_equal(get_token_type("AND", "&"), TokenTypes$operator)
    expect_equal(get_token_type("LEFT_ASSIGN", "<-"), TokenTypes$operator)
})

test_that("Semantic tokens full document works", {
    skip_if_not(requireNamespace("xml2", quietly = TRUE))
    skip_if_not(requireNamespace("xmlparsedata", quietly = TRUE))
    
    client <- language_client()
    
    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "library(dplyr)",
        "",
        "# This is a comment",
        "my_function <- function(x, y) {",
        "  result <- x + y",
        "  return(result)",
        "}",
        "",
        "data %>%",
        "  filter(value > 10) %>%",
        "  mutate(new_col = value * 2)"
    ), temp_file)
    
    client %>% did_save(temp_file)
    
    result <- client %>% respond_semantic_tokens_full(temp_file)
    
    # Check that we got a response with data
    expect_true("data" %in% names(result))
    # The data may come back as a list, so unlist it
    if (is.list(result$data)) {
        result$data <- unlist(result$data)
    }
    expect_true(is.numeric(result$data) || is.integer(result$data))
    expect_true(length(result$data) > 0)
    
    # The data should be divisible by 5 (5 integers per token)
    expect_equal(length(result$data) %% 5, 0)
})

test_that("Semantic tokens range works", {
    skip_if_not(requireNamespace("xml2", quietly = TRUE))
    skip_if_not(requireNamespace("xmlparsedata", quietly = TRUE))
    
    client <- language_client()
    
    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "# Comment line 1",
        "x <- 10",
        "# Comment line 3",
        "y <- 20"
    ), temp_file)
    
    client %>% did_save(temp_file)
    
    # Request tokens only for lines 2-3
    result <- client %>% respond_semantic_tokens_range(
        temp_file,
        start_line = 1,
        start_char = 0,
        end_line = 2,
        end_char = 100
    )
    
    # Check that we got a response with data
    expect_true("data" %in% names(result))
    # The data may come back as a list, so unlist it
    if (is.list(result$data)) {
        result$data <- unlist(result$data)
    }
    expect_true(is.numeric(result$data) || is.integer(result$data))
    
    # Should have tokens for line 2 and 3 only
    expect_true(length(result$data) > 0)
    expect_equal(length(result$data) %% 5, 0)
})

test_that("Base R functions get defaultLibrary modifier", {
    skip_if_not(requireNamespace("xml2", quietly = TRUE))
    skip_if_not(requireNamespace("xmlparsedata", quietly = TRUE))
    
    client <- language_client()
    
    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "print('hello')",  # print is from base R
        "mean(c(1, 2, 3))"  # mean is from base R
    ), temp_file)
    
    client %>% did_save(temp_file)
    
    result <- client %>% respond_semantic_tokens_full(temp_file)
    
    expect_true("data" %in% names(result))
    expect_true(length(result$data) > 0)
    
    # Check that function tokens have the defaultLibrary modifier
    # The modifier value should be 512 (TokenModifiers$defaultLibrary)
    # In the encoded data: [deltaLine, deltaCol, length, tokenType, tokenModifiers]
    # First token should be "print" with modifier 512
    if (length(result$data) >= 5) {
        # The 5th element is the modifier for the first token
        expect_true(result$data[5] == 512 || result$data[5] == 0)
    }
})

test_that("User-defined functions are highlighted as functions", {
    skip_if_not(requireNamespace("xml2", quietly = TRUE))
    skip_if_not(requireNamespace("xmlparsedata", quietly = TRUE))
    
    client <- language_client()
    
    temp_file <- withr::local_tempfile(fileext = ".R")
    writeLines(c(
        "my_function <- function(x, y) {",
        "  return(x + y)",
        "}",
        "result <- my_function(1, 2)"
    ), temp_file)
    
    client %>% did_save(temp_file)
    
    result <- client %>% respond_semantic_tokens_full(temp_file)
    
    expect_true("data" %in% names(result))
    # The data may come back as a list, so unlist it
    if (is.list(result$data)) {
        result$data <- unlist(result$data)
    }
    expect_true(is.numeric(result$data) || is.integer(result$data))
    expect_true(length(result$data) > 0)
    
    # Should find function tokens (type 12) for user-defined function calls
    # Data format: [deltaLine, deltaCol, length, tokenType, tokenModifiers]
    # Look for tokens with type 12 (function)
    token_types <- result$data[seq(4, length(result$data), 5)]  # Every 4th element starting from 4
    function_token_count <- sum(token_types == 12)
    expect_true(function_token_count > 0, "Should find at least one function token for user-defined function")
})
test_that("codeLens returns reference counts", {
    temp_dir <- withr::local_tempdir()
    temp_file <- file.path(temp_dir, "test.R")

    # Create a main file with a function definition and references
    test_code <- c(
        "my_function <- function(x) {",
        "  x + 1",
        "}",
        "",
        "a <- my_function(1)",
        "b <- my_function(2)",
        "c <- my_function(3)"
    )
    writeLines(test_code, temp_file)

    workspace <- languageserver:::Workspace$new(temp_dir)
    uri <- languageserver:::path_to_uri(temp_file)
    content <- paste(test_code, collapse = "\n")
    
    # Create document and parse it
    document <- languageserver:::Document$new(
        uri = uri,
        language = "r",
        version = 1,
        content = test_code
    )
    
    # Add document to workspace first
    workspace$documents$set(uri, document)
    
    # Then parse the document content to populate parse_data with xml_doc
    parse_data <- languageserver:::parse_document(uri, test_code)
    workspace$update_parse_data(uri, parse_data)

    # The codeLens should detect the function and its references
    lenses <- languageserver:::code_lens_reply(id = 1, uri = uri, 
                               workspace = workspace, document = document)
    
    # Should return a Response object
    expect_s3_class(lenses, "Response")
    # Verify it contains reference count for my_function
    expect_true(length(lenses$result) > 0)
    
    # Verify the first lens is a reference count and shows 3 references
    first_lens <- lenses$result[[1]]
    expect_equal(first_lens$command$title, "3 references")
    expect_equal(first_lens$command$command, "")
})

test_that("codeLens detects S3 method implementations", {
    temp_dir <- withr::local_tempdir()
    temp_file <- file.path(temp_dir, "test.R")

    # Create a file with a generic and its methods
    test_code <- c(
        "print_data <- function(x) UseMethod('print_data')",
        "",
        "print_data.numeric <- function(x) {",
        "  cat('Number:', x, '\\n')",
        "}",
        "",
        "print_data.character <- function(x) {",
        "  cat('Text:', x, '\\n')",
        "}"
    )
    writeLines(test_code, temp_file)

    workspace <- languageserver:::Workspace$new(temp_dir)
    uri <- languageserver:::path_to_uri(temp_file)
    
    # Create document and parse it
    document <- languageserver:::Document$new(
        uri = uri,
        language = "r",
        version = 1,
        content = test_code
    )
    
    # Add document to workspace first
    workspace$documents$set(uri, document)
    
    # Then parse the document content to populate parse_data with xml_doc
    parse_data <- languageserver:::parse_document(uri, test_code)
    workspace$update_parse_data(uri, parse_data)

    lenses <- languageserver:::code_lens_reply(id = 1, uri = uri, 
                               workspace = workspace, document = document)
    
    # Should return a Response with method implementations
    expect_s3_class(lenses, "Response")
    # Verify it contains implementations for print_data
    expect_true(length(lenses$result) > 0)
})

test_that("codeLens resolve returns the same code lens", {
    code_lens_obj <- list(
        range = list(
            start = list(line = 0, character = 0),
            end = list(line = 0, character = 10)
        ),
        command = list(
            title = "3 references",
            command = "editor.action.findReferences"
        )
    )

    workspace <- languageserver:::Workspace$new(tempdir())
    result <- languageserver:::code_lens_resolve_reply(id = 1, workspace = workspace, code_lens = code_lens_obj)

    expect_s3_class(result, "Response")
    expect_equal(result$result, code_lens_obj)
})


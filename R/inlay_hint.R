InlayHintKind <- list(
    Type = 1,
    Parameter = 2
)

#' Get inlay hints for parameter names in function calls
#'
#' @param xdoc XML document from parse data
#' @param workspace Workspace object
#' @param document Document object
#' @param range Optional range to restrict hints (NULL for entire document)
#' @return List of parameter name inlay hints
#' @noRd
get_parameter_inlay_hints <- function(xdoc, workspace, document, range = NULL) {
    if (is.null(xdoc)) {
        return(list())
    }

    # Find all function calls
    function_calls <- xml_find_all(xdoc, "//SYMBOL_FUNCTION_CALL/parent::expr")
    
    if (length(function_calls) == 0) {
        return(list())
    }

    hints <- list()
    
    for (call_node in function_calls) {
        # Get function name
        func_symbol <- xml_find_first(call_node, "SYMBOL_FUNCTION_CALL")
        if (length(func_symbol) == 0) next
        
        func_name <- xml_text(func_symbol)
        
        # Get the function call expression which contains the arguments
        # The structure is: expr[SYMBOL_FUNCTION_CALL, OP-LEFT-PAREN, arguments..., OP-RIGHT-PAREN]
        left_paren <- xml_find_first(call_node, "OP-LEFT-PAREN")
        if (length(left_paren) == 0) next
        
        # Find all expr nodes that are direct children (these are the arguments)
        # But exclude the first expr which might be the function itself with package qualifier
        all_expr_children <- xml_find_all(call_node, "expr")
        
        # Skip the first expr if it's the one containing the function symbol
        arg_exprs <- list()
        for (expr_child in all_expr_children) {
            if (length(xml_find_first(expr_child, "SYMBOL_FUNCTION_CALL")) > 0) {
                # This is the function expr, skip it
                next
            }
            # Check if this expr is between the left and right parens
            expr_start <- as.integer(xml_attr(expr_child, "start"))
            paren_start <- as.integer(xml_attr(left_paren, "start"))
            if (expr_start > paren_start) {
                arg_exprs <- c(arg_exprs, list(expr_child))
            }
        }
        
        if (length(arg_exprs) == 0) next
        
        # Try to get the function signature
        package <- workspace$guess_namespace(func_name, isf = TRUE)
        formals_list <- NULL
        
        if (!is.null(package)) {
            formals_list <- workspace$get_formals(func_name, package, exported_only = TRUE)
        }
        
        if (is.null(formals_list)) next
        
        param_names <- names(formals_list)
        if (is.null(param_names) || length(param_names) == 0) next
        
        # Process each argument expression
        arg_index <- 1
        for (arg_expr in arg_exprs) {
            # Check if this is a named argument (has EQ_ASSIGN or EQ_SUB)
            has_equal <- length(xml_find_first(arg_expr, "EQ_ASSIGN | EQ_SUB")) > 0
            
            if (has_equal) {
                # This is a named argument, skip it
                next
            }
            
            # This is an unnamed positional argument
            if (arg_index > length(param_names)) {
                # Beyond the parameter list, might be in ... 
                break
            }
            
            param_name <- param_names[arg_index]
            
            # Skip if this is ... (ellipsis)
            if (param_name == "...") {
                break
            }
            
            # Skip if parameter name is empty
            if (is.null(param_name) || nchar(param_name) == 0 || param_name == "") {
                arg_index <- arg_index + 1
                next
            }
            
            # Get position for the hint (start of the argument expression)
            arg_line <- as.integer(xml_attr(arg_expr, "line1"))
            arg_col <- as.integer(xml_attr(arg_expr, "col1"))
            
            # Check if this position is within the requested range
            if (!is.null(range)) {
                hint_pos <- document$to_lsp_position(row = arg_line - 1, col = arg_col - 1)
                if (!position_in_range(hint_pos, range)) {
                    arg_index <- arg_index + 1
                    next
                }
            }
            
            # Create the inlay hint
            hint_position <- document$to_lsp_position(row = arg_line - 1, col = arg_col - 1)
            
            hint <- list(
                position = hint_position,
                label = paste0(param_name, ":"),
                kind = InlayHintKind$Parameter,
                paddingRight = TRUE
            )
            
            hints <- c(hints, list(hint))
            arg_index <- arg_index + 1
        }
    }
    
    hints
}

#' Get inlay hints for pipe chain types
#'
#' @param xdoc XML document from parse data
#' @param workspace Workspace object
#' @param document Document object
#' @param range Optional range to restrict hints
#' @return List of type inlay hints for pipe chains
#' @noRd
get_pipe_type_hints <- function(xdoc, workspace, document, range = NULL) {
    if (is.null(xdoc)) {
        return(list())
    }

    # Find all pipe operators (both %>% and |>)
    # In the AST, these appear as SPECIAL or PIPE operators
    pipe_exprs <- xml_find_all(xdoc, "//expr[SPECIAL[contains(text(), '%>%')] | PIPE]")
    
    if (length(pipe_exprs) == 0) {
        return(list())
    }

    hints <- list()
    
    # For now, we'll skip pipe type hints as they require runtime evaluation
    # This would need significant work to evaluate partial pipe chains
    # and determine their types without executing the code
    
    hints
}

#' Get inlay hints for function return types
#'
#' @param xdoc XML document from parse data
#' @param workspace Workspace object
#' @param document Document object
#' @param range Optional range to restrict hints
#' @return List of type inlay hints for assignments
#' @noRd
get_return_type_hints <- function(xdoc, workspace, document, range = NULL) {
    if (is.null(xdoc)) {
        return(list())
    }

    hints <- list()
    
    # Find assignment expressions with function calls on the right side
    # Look for patterns like: var <- function_call()
    assignments <- xml_find_all(xdoc, 
        "//expr[LEFT_ASSIGN | RIGHT_ASSIGN | EQ_ASSIGN]")
    
    if (length(assignments) == 0) {
        return(list())
    }

    for (assign_expr in assignments) {
        # Determine assignment direction
        has_left_assign <- length(xml_find_first(assign_expr, "LEFT_ASSIGN")) > 0
        has_right_assign <- length(xml_find_first(assign_expr, "RIGHT_ASSIGN")) > 0
        has_eq_assign <- length(xml_find_first(assign_expr, "EQ_ASSIGN")) > 0
        
        if (!has_left_assign && !has_right_assign && !has_eq_assign) next
        
        # Get the value expression (right side for <- and =, left side for ->)
        child_exprs <- xml_find_all(assign_expr, "expr")
        if (length(child_exprs) < 2) next
        
        if (has_left_assign || has_eq_assign) {
            val_expr <- child_exprs[[2]]
        } else {
            # RIGHT_ASSIGN
            val_expr <- child_exprs[[1]]
        }
        
        # Check if the value is a function call
        func_call <- xml_find_first(val_expr, "SYMBOL_FUNCTION_CALL")
        if (length(func_call) == 0) next
        
        func_name <- xml_text(func_call)
        
        # Try to determine the return type for common functions
        type_hint <- get_function_return_type(func_name, workspace)
        if (is.null(type_hint)) next
        
        # Get position for the hint (end of the line)
        assign_line <- as.integer(xml_attr(assign_expr, "line2"))
        assign_col <- as.integer(xml_attr(assign_expr, "col2"))
        
        # Check if this position is within the requested range
        if (!is.null(range)) {
            hint_pos <- document$to_lsp_position(row = assign_line - 1, col = assign_col)
            if (!position_in_range(hint_pos, range)) {
                next
            }
        }
        
        # Create the hint at the end of the assignment line
        hint_position <- document$to_lsp_position(row = assign_line - 1, col = assign_col)
        
        hint <- list(
            position = hint_position,
            label = paste0(" → ", type_hint),
            kind = InlayHintKind$Type,
            paddingLeft = TRUE
        )
        
        hints <- c(hints, list(hint))
    }
    
    hints
}

#' Get the return type for common R functions
#'
#' @param func_name Function name
#' @param workspace Workspace object
#' @return Character string describing the return type, or NULL
#' @noRd
get_function_return_type <- function(func_name, workspace) {
    # Common function return types
    type_map <- list(
        # Data reading functions
        "read.csv" = "data.frame",
        "read.table" = "data.frame",
        "read.delim" = "data.frame",
        "readRDS" = "object",
        "readLines" = "character",
        
        # Statistical models
        "lm" = "lm",
        "glm" = "glm",
        "aov" = "aov",
        
        # Data manipulation
        "data.frame" = "data.frame",
        "matrix" = "matrix",
        "array" = "array",
        "list" = "list",
        "c" = "vector",
        "seq" = "numeric",
        
        # String functions
        "paste" = "character",
        "paste0" = "character",
        "sprintf" = "character",
        "substr" = "character",
        "grep" = "integer",
        "grepl" = "logical",
        "gsub" = "character",
        "sub" = "character",
        
        # Logical operations
        "all" = "logical",
        "any" = "logical",
        "is.na" = "logical",
        "is.null" = "logical",
        
        # Math functions
        "mean" = "numeric",
        "median" = "numeric",
        "sum" = "numeric",
        "prod" = "numeric",
        "min" = "numeric",
        "max" = "numeric",
        "range" = "numeric",
        "length" = "integer",
        "nrow" = "integer",
        "ncol" = "integer",
        "dim" = "integer"
    )
    
    if (func_name %in% names(type_map)) {
        return(type_map[[func_name]])
    }
    
    NULL
}

#' Helper to check if a position is within a range
#'
#' @param position LSP position
#' @param range LSP range
#' @return Logical indicating if position is in range
#' @noRd
position_in_range <- function(position, range) {
    if (is.null(range) || is.null(position)) {
        return(TRUE)
    }
    
    start_line <- range$start$line
    start_char <- range$start$character
    end_line <- range$end$line
    end_char <- range$end$character
    
    pos_line <- position$line
    pos_char <- position$character
    
    # Check if position is after start
    after_start <- (pos_line > start_line) || 
                   (pos_line == start_line && pos_char >= start_char)
    
    # Check if position is before end
    before_end <- (pos_line < end_line) ||
                  (pos_line == end_line && pos_char <= end_char)
    
    after_start && before_end
}

#' Main response function for textDocument/inlayHint
#'
#' @param id Request ID
#' @param uri Document URI
#' @param workspace Workspace object
#' @param document Document object
#' @param range Optional range to restrict hints
#' @return Response with inlay hints
#' @noRd
inlay_hint_reply <- function(id, uri, workspace, document, range = NULL) {
    parse_data <- workspace$get_parse_data(uri)
    if (is.null(parse_data) ||
        (!is.null(parse_data$version) && parse_data$version != document$version)) {
        return(Response$new(id))
    }

    xdoc <- parse_data$xml_doc
    if (is.null(xdoc)) {
        return(Response$new(id))
    }

    # Collect all types of hints
    param_hints <- get_parameter_inlay_hints(xdoc, workspace, document, range)
    pipe_hints <- get_pipe_type_hints(xdoc, workspace, document, range)
    type_hints <- get_return_type_hints(xdoc, workspace, document, range)
    
    all_hints <- c(param_hints, pipe_hints, type_hints)
    
    # Return response
    if (length(all_hints) == 0) {
        Response$new(id, result = list())
    } else {
        Response$new(id, result = all_hints)
    }
}

# LSP 3.18 editor demos for vscode-R 3.2.2 or newer.
# Install the PR build in the R library used by your language server:
# remotes::install_github("REditorSupport/languageserver#817", upgrade = "never")
# Then run "Developer: Reload Window" in the VS Code Command Palette.
# Open this file before running anything. The examples use source analysis.
# The intentionally invalid argument examples are inside if (FALSE), so this
# whole script can be sourced safely.

# 1. Signature help: move the caret immediately after an equals sign or onto
# the argument value, then run "Trigger Parameter Hints" (Cmd+Shift+Space on
# macOS). Keep the caret inside the parentheses.
add_pair <- function(first, second) {
    first + second
}

capture_extra <- function(first, ...) {
    list(first = first, ...)
}

if (FALSE) {
    add_pair(first = 1, second = 2) # first/second highlights its own parameter
    add_pair(unknown = 1, second = 2) # unknown: signature visible, no highlight
    add_pair(1, 2, 3) # third argument: signature visible, no highlight
    add_pair(sec = 2, first = 1) # sec: second is highlighted (partial matching)
    add_pair("fi\u0072st" = 1, second = 2) # escaped tag: first is highlighted
    capture_extra(first = 1, extra = 2) # extra: ... is highlighted
}

# 2. Extract Variable: select only "price * quantity" below, right-click and
# choose "Refactor...", then "Extract expression to variable".
# The generated extracted_value name is selected. Type "line_total": both
# its declaration and its use should change together. Press Tab to finish.
demo_variable <- function(price, quantity) {
    subtotal <- price * quantity
    subtotal * 1.1
}

# Run this before and after extraction: the result should stay 22.
demo_variable(10, 2)

# 3. Extract Function: select the two complete assignment lines beginning
# "shifted <-" and "scaled <-" below. Choose "Extract selection to function".
# Type "adjust_values" over the selected extracted_function name: both the
# nested function declaration and its call should change together.
demo_function <- function(values) {
    shifted <- values + 1
    scaled <- shifted * 2
    sum(scaled)
}

# Run this before and after extraction: the result should stay 18.
demo_function(1:3)

# 4. Multiline literal preservation: select only "argument$value" below and
# extract it to a variable. This uses ordinary edits, so there are no linked
# name tab stops. The four spaces before b are part of the string and must
# stay intact, including when the editor is configured to indent with tabs.
demo_literal <- function(argument) {
    paste0("a
    b", argument$value + 1)
}

# The result should remain "a\n    b3" before and after extraction.
demo_literal(list(value = 2))

# 5. Virtual package definitions: put the caret on print, then use F12 or
# right-click -> "Go to Definition". A generated print.R body should open in
# a read-only tab with an r-lsp-... URI, instead of an editable temporary file.
# Repeat with mean to open another generated package definition.
base::print
base::mean

# 6. Refactoring documentation: select "price * quantity" in demo_variable
# (undo the earlier extraction first if needed) and open "Refactor...".
# Click "Learn about R refactorings" in the action menu to open the package
# README. The new Refactoring section is also in this branch's README.md.

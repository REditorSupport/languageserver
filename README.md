# languageserver: An implementation of the Language Server Protocol for R

[![R-CMD-check](https://github.com/REditorSupport/languageserver/actions/workflows/rcmdcheck.yml/badge.svg)](https://github.com/REditorSupport/languageserver/actions/workflows/rcmdcheck.yml)
[![codecov](https://codecov.io/gh/REditorSupport/languageserver/graph/badge.svg)](https://app.codecov.io/gh/REditorSupport/languageserver)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/languageserver)](https://cran.r-project.org/package=languageserver)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/languageserver)](https://cran.r-project.org/package=languageserver)
[![r-universe](https://reditorsupport.r-universe.dev/badges/languageserver)](https://reditorsupport.r-universe.dev/#package:languageserver)

`languageserver` brings completion, hover help, navigation, refactoring, diagnostics, and formatting to R editors through the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/).

[Documentation](https://reditorsupport.github.io/languageserver/) · [Five-minute walkthrough](https://reditorsupport.github.io/languageserver/guide/quick-start) · [Guide sources](website/docs/guide)

## Installation

Install the CRAN release in the R library your editor uses.

```r
install.packages("languageserver")
```

For the default diagnostics and formatting tools, also install `lintr` and `styler`. Linux users building from source may need system libraries. Development builds are available on r-universe. [Linux build prerequisites](https://reditorsupport.github.io/languageserver/guide/editors#linux-source-installation) · [Development installation](https://reditorsupport.github.io/languageserver/guide/development)

## Language Clients

Open an R file in VS Code, Neovim, Emacs, or another LSP client. The setup guide shows how to connect the editor and check the connection by jumping to a local function. [Set up your editor](https://reditorsupport.github.io/languageserver/guide/editors)

## Services Implemented

- Complete package functions and members, read hover help, and see argument suggestions. [Write and understand R](https://reditorsupport.github.io/languageserver/guide/writing-r)
- Follow a call into another file, find references, and rename a parameter. [Navigate and refactor a project](https://reditorsupport.github.io/languageserver/guide/navigation)
- Edit and format executable R chunks in a mixed-language document. [R Markdown and Quarto](https://reditorsupport.github.io/languageserver/guide/literate)
- Browse every editor provider, its availability in CRAN or development builds, and an example of using it. [Feature index](https://reditorsupport.github.io/languageserver/reference/capabilities) · [Release history](https://reditorsupport.github.io/languageserver/releases/)

### S3 method information

Signature Help can show method-specific arguments when the server can infer the input class. [Try an S3 generic](https://reditorsupport.github.io/languageserver/guide/writing-r#find-the-arguments-for-an-s3-method)

### Refactoring

Select an expression to extract a variable, select statements to extract a function, or inline a local value when its evaluation is preserved. [Try an extraction](https://reditorsupport.github.io/languageserver/guide/navigation#extract-an-expression)

### Go to Definition for S4 methods

Development builds can open a source generic and its registered methods. [Explore S4 declarations](https://reditorsupport.github.io/languageserver/guide/development#go-to-definition-for-s4-methods)

### Go to Definition for project helpers

Development builds can find helpers loaded by a test runner from a project-specific file mapping. [Configure project helpers](https://reditorsupport.github.io/languageserver/guide/development#find-a-helper-loaded-by-your-test-runner)

### Generated package definitions

Inspect the generated body of an installed function when its original source file is unavailable. [Open a package definition](https://reditorsupport.github.io/languageserver/guide/navigation#generated-package-definitions)

## Settings

Configure the server through the editor or the R profile that its R process reads. The reference lists defaults, accepted values, and configuration examples. [Server settings](https://reditorsupport.github.io/languageserver/reference/settings) · [Server launch options](https://reditorsupport.github.io/languageserver/reference/run)

## FAQ

For missing completions, definitions, or formatting commands, work through the checks for your symptom. [Troubleshooting](https://reditorsupport.github.io/languageserver/guide/troubleshooting)

### Linters

Configure lintr in the project and inspect diagnostics in the editor. [Read a lint finding](https://reditorsupport.github.io/languageserver/guide/diagnostics-formatting#read-a-lint-finding)

### Selecting diagnostics and formatting backends

Choose lintr or Jarl for diagnostics, and styler or Air for formatting. [Choose a backend](https://reditorsupport.github.io/languageserver/guide/diagnostics-formatting#choose-a-backend)

### Custom R adapters

Supply a custom diagnostics or formatting function through an R option. [Custom adapters](https://reditorsupport.github.io/languageserver/reference/adapters#adapter-contract)

### Customizing server capabilities

Override the capabilities the server advertises at startup. [Capability overrides](https://reditorsupport.github.io/languageserver/reference/capabilities#override-an-advertised-capability)

### Customizing formatting style

Give styler a formatting style function in the server's R profile. [Custom styles](https://reditorsupport.github.io/languageserver/reference/adapters#change-how-styler-formats-a-selection)

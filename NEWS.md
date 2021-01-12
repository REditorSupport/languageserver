# languageserver 0.3.10

- Recongize promise and active binding symbols (#362)

# languageserver 0.3.9

- skip tests on solaris 

# languageserver 0.3.8

- When closing a file, "Problems" should be removed (#348)
- Implement renameProvider (#337)
- Hover on symbol in a function with functional argument causes parse error (#345)
- Hover on non-function symbol in other document should show definition and - documentation (#343)
- Check if symbol on rhs of assignment in definition (#341)
- Implement referencesProvider (#336)
- Add comment of notice above temp code of definition (#353)
- Implement renameProvider (#339)
- Fix tests (#351)
- Update coc help in README (#350)
- Remove diagnostics of closed files in non-package workspace (#349)
- Fix hover func arg (#346)
- Fix hover on non-function symbol (#344)
- Fix checking symbol on rhs of assignment expression (#342)
- Implement referencesProvider (#338)
- Do not load sysdata.rda (#347)
- release v0.3.7 (#335)

# languageserver 0.3.7

- Local function support (#330)
- Update xpaths to adapt to token change in parse data in R 4.0 (#328)
- Exclude existing completion items in token_completion (#326)
- Add token completion (#324)
- Only provide imported completions without package (#323)
- Improve folding (#317)
- More robust rmd chunk pattern (#318)
- Limit string length to provide color and link (#314)
- Minor improvements (#312)
- Improve xpath to work with cursor on token ending (#311)
- Support folding ranges for comments (#309)
- fix covr test (#310)
- Not trigger on-type-formatting on comment line (#308)
- Convert roxygen comments to documentation (#305)
- Support all symbols in definitions (#295)
- Use isf=FALSE (#303)
- Support startup library specified in profile (#302)
- Fix rmd chunk pattern (#297)
- Try parsing string in link and color (#300)
- Implement foldingRangeProvider (#294)
- Check uri in diagnostics_callback (#292)
- Fix for #283 - "unexpected '/'" on save (#291)
- use mac.binary instead of mac.binary.el-capitan (#293)
- Change dev version download script in the README (#287)
- Use sortText in completions (#286)
- Rmd chunk symbol (#280)
- make sure the path is UTF-8 (#278)
- make sure process is alive (#275)
- Fix handling raw string in link and color (#274)
- Indent all elements except braces in apply_initial_indention (#271)
- Use old behavior if formatting scope < indention (#269)
- Fix range formatting handling initial indentation (#268)
- Use more conservative pool_size (#267)
- add session pool for task manager (#265)

# languageserver 0.3.6

- Show error message when diagnostics failed
- fix enclosed_by_quotes
- fix a bug in returning NA locations
- requires collections 0.3.0
- Run tasks with delay to reduce CPU usage on input 
- Refine args (Merge 9fd102b)
- respect NAMESPACE file


# languageserver 0.3.5

- Remove dependency on readr
- Use stringi to replace stringr
- Respect snippetSupport
- Respect linter_file in diagnostics


# languageserver 0.3.4

- on-type-formatting
- documentLinkProvider
- textDocument/documentColor
- use writeLines to write raw data in Windows
- Support section/subsection in document symbols
- make sure the output are UTF8
- set O_BINARY for stdin/stdout
- allows user to override ServerCapabilities
- Incremental resolving packages
- Disable lintr for temp files
- and a lot of minor fixes and improvements


# languageserver 0.3.3

- xml based parsing which provides more information on hovering, definitions and completions
- a bug fix in reading stdin
- unicode support improvement
- some internal refactoring

# languageserver 0.3.2

- read other header fields
- require newer version of collections
- allow colon to trigger completion
- specify kind for non exported package objects
- Only provide package exported objects at :: (#84)
- run tests on github actions
- implementation of scope completion using xpath
- Allow user customizable formatting style
- use readr's read_lines and write_lines
- use a better function name
- Provide completion for language constants
- bump lintr to 2.0.0


# languageserver 0.3.1

- Recursive parsing (#56)
- improve way to check if process becomes orphan
- unicode support


# languageserver 0.3.0

- added symbol definitions
- added document and workspace symbols
 
# languageserver 0.2.6

- fix a bug in completion items
- lower bound lintr to 1.0.3
- fix a bug in desc_get_deps
- better support vscode settings

# languageserver 0.2.5

- deprecate languageserver.default_linters

# languageserver 0.2.4

- require latest styler release
- handle windows crlf
- disable homedir config until lintr is released
- concat multiple lines in signature
- Allow package name to contain dots (e.g. data.table)
- completing variables defined in document 
- support completions and function signatures from documents
- improve worksync consistency
- sync all R files of a package
- load packages in Depends field


# languageserver 0.2.3

- ensure unicode documentation
- use * as itemBullet for hover help
- check if rootUri is NULL

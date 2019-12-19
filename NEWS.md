languageserver 0.3.3

   - xml based parsing which provides more information on hovering, definitions and completions
   - a bug fix in reading stdin
   - unicode support improvement
   - some internal refactoring

languageserver 0.3.2

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

  Contributors:
   - Kun Ren
   - Randy Lai


languageserver 0.3.1

   - Recursive parsing (#56)
   - improve way to check if process becomes orphan
   - unicode support

  Contributors:
   - Kun Ren
   - Randy Lai


languageserver 0.3.0

  - added symbol definitions
  - added document and workspace symbols

  Contributors:
   - Andrew Craig
   
languageserver 0.2.6
  
  - fix a bug in completion items
  - lower bound lintr to 1.0.3
  - fix a bug in desc_get_deps
  - better support vscode settings

languageserver 0.2.5

   - deprecate languageserver.default_linters

languageserver 0.2.4

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

  Contributors:
   - Randy Lai
   - Lorenz Walthert
   - Kun Ren

languageserver 0.2.3

  various improvements

  bug fixes:
   - ensure unicode documentation
   - use * as itemBullet for hover help
   - check if rootUri is NULL

  Contributors:
   - Randy Lai
   - Kun Ren
   - Ista Zahn

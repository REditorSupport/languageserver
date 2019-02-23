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

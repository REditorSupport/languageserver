# Launch the server

An editor extension normally starts the R process and connects it to the current workspace. If you are setting up a generic LSP client, configure it to start this command and communicate over standard input and output.

```sh
R --no-echo -e "languageserver::run()"
```

If the client has separate command and argument fields, use `R` as the executable and `["--no-echo", "-e", "languageserver::run()"]` as the arguments. Choose the R executable that has access to the installed package. Once connected, open an R file and try the [quick start](/guide/quick-start).

## R function

```r
languageserver::run(debug = FALSE, host = "localhost", port = NULL)
```

| Argument | Purpose |
| --- | --- |
| `debug` | `TRUE` sends diagnostic logging to standard error. A character path enables logging to that file. With the default `FALSE`, the server uses its configured logging settings. |
| `host` | Host to connect to when using TCP. |
| `port` | With `NULL`, the server reads LSP messages from standard input and writes replies to standard output. Supplying a port connects to a TCP listener at `host` on that port. |

For example, a client that opens a local listening socket on port 8888 can start R with this command.

```sh
R --no-echo -e "languageserver::run(port = 8888)"
```

To read the help matching your installation, enter `?languageserver::run` in R. [Package manual on CRAN](https://cran.r-project.org/web/packages/languageserver/languageserver.pdf) · [Release notes](https://github.com/REditorSupport/languageserver/releases)

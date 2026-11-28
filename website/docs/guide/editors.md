# Set up your editor

Install R, then run `install.packages("languageserver")` in the R installation your editor will start. In a remote workspace, use R on the remote host. You can check which copy is available from that R session with `find.package("languageserver")`. [Download R](https://cran.r-project.org/) · [Linux source installation](#linux-source-installation)

## VS Code

1. In **Extensions**, install **R** by **REditorSupport**. [R extension](https://marketplace.visualstudio.com/items?itemName=REditorSupport.r)
2. Use **File → Open Folder** to open your project, then open an `.R` file. The language mode shown in the status bar should be **R**. The extension starts the server when it opens the file.
3. If VS Code picks up another R installation, set `r.executablePath` to the full path to your R executable in VS Code settings. Check that `r.lsp.enabled` is enabled and run **Developer: Reload Window**.
4. Try the connection check below. To see the server's startup messages, open **View → Output** and select **R Language Server**.

The extension also provides the R console and plot viewer. Its own guide describes the VS Code commands and settings for the language service. [R language service in VS Code](https://reditorsupport.github.io/vscode-R/features/language-service)

## Neovim 0.11 or newer

Add this to your `init.lua` to enable Neovim’s built-in LSP client. The command starts R over standard input and output. Replace `"R"` with an absolute path if Neovim should use a different R installation.

```lua
vim.lsp.config("r_language_server", {
  cmd = { "R", "--no-echo", "-e", "languageserver::run()" },
  filetypes = { "r", "rmd", "quarto" },
  root_dir = function(bufnr, on_dir)
    on_dir(vim.fs.root(bufnr, { { "DESCRIPTION", ".git" } }) or vim.fn.getcwd())
  end,
})
vim.lsp.enable("r_language_server")
```

Start Neovim in your project directory and open `center.R`. The configuration uses the nearest ancestor containing a `DESCRIPTION` file or a `.git` entry as the workspace root. In a plain scripts folder it uses Neovim’s working directory. Run `:checkhealth vim.lsp` to see the active client and its root. [Neovim LSP documentation](https://neovim.io/doc/user/lsp.html)

## Check the connection

Enter these lines in `center.R` and place the cursor on `center` in the second line.

```r
center <- function(values) values - mean(values)
center(c(2, 4, 6))
```

In VS Code, right-click and choose **Go to Definition**. In Neovim, run `:lua vim.lsp.buf.definition()`. Both should take you to line one. Use these commands to follow the rest of the quick start.

| Action | VS Code command | Neovim 0.11 |
| --- | --- | --- |
| Complete a name | Trigger Suggest | Insert mode, `Ctrl-X` then `Ctrl-O` |
| See the parameters | Trigger Parameter Hints | Insert mode, `Ctrl-S`, or `:lua vim.lsp.buf.signature_help()` |
| Rename a symbol | Rename Symbol | Normal mode, `grn` |
| Find references | Go to References | Normal mode, `grr` |

[Get started in five minutes](./quick-start) · [Troubleshoot a missing result](./troubleshooting)

## Other clients

In Emacs, open the file in ESS R mode and run `M-x eglot`, or configure lsp-mode’s R client. Neovim users with an existing coc.nvim setup can install coc-r-lsp with `:CocInstall coc-r-lsp`. [ESS](https://ess.r-project.org/) · [Eglot](https://www.gnu.org/software/emacs/manual/html_mono/eglot.html) · [lsp-mode for R](https://emacs-lsp.github.io/lsp-mode/page/lsp-r/) · [coc-r-lsp](https://github.com/neoclide/coc-r-lsp)

Other clients include JupyterLab with jupyterlab-lsp, Sublime Text with R-IDE, BBEdit 14 or newer, Nova with R-Nova, and Atom with atom-ide-r. [JupyterLab](https://github.com/jupyter-lsp/jupyterlab-lsp) · [Sublime Text](https://github.com/REditorSupport/sublime-ide-r) · [BBEdit](https://www.barebones.com/support/bbedit/lsp-notes.html) · [Nova](https://github.com/jonclayden/R-Nova) · [Atom](https://github.com/REditorSupport/atom-ide-r)

For an editor that accepts an LSP launch command, select the R file type and use the following command with the stdio transport.

```sh
R --no-echo -e "languageserver::run()"
```

Open an R file in that editor and repeat the connection check. [Server launch options](../reference/run)

## Linux source installation

If R builds the package and its dependencies from source, first install the compiler and system headers for your distribution. Run the matching command with administrator privileges, then run `install.packages("languageserver")` in R.

::: code-group

```sh [Debian / Ubuntu]
apt install --no-install-recommends build-essential libcurl4-openssl-dev libssl-dev libxml2-dev libuv1-dev r-base
```

```sh [Fedora]
dnf install --setopt=install_weak_deps=False @development-tools libcurl-devel openssl-devel libxml2-devel libuv-devel R
```

```sh [Alpine]
apk add --no-cache curl-dev g++ gcc libxml2-dev linux-headers make R R-dev
```

:::

# Edit R Markdown and Quarto

Open a folder in your editor and create `report.Rmd` or `report.qmd`. The editor needs to send the file or its R cells to the R language server. Try this short report.

````markdown
# Scores

The scores come from three rounds.

```{r}
scores<-c(3,7,8)
mean(scores)
```
````

## Complete a name in a chunk

On the `mean(scores)` line, replace `scores` with `sco` and request completion just after the `o`. Choose `scores`. With the cursor on that argument, invoke **Go to Definition** to jump to the assignment above. You can do both before running the chunk.

Now place the cursor on `scores` in the prose sentence. The R server treats this occurrence as Markdown text. The same boundary applies to document YAML, Quarto `#|` option lines, and cells written in another language. Use an executable R fence (three backticks followed by `{r}`) when you want language features in a code example.

## Format a chunk

With the default styler backend installed, select just the `scores<-c(3,7,8)` line and choose **Format Selection**. It becomes `scores <- c(3, 7, 8)`. Keep both ends of the selection inside the same R chunk, leaving the opening and closing fences outside it. Use separate selections for chunks separated by prose. [Diagnostics and formatting](./diagnostics-formatting)

When the client sends the whole document, its outline can show headings and named chunks alongside the R functions they contain. Name the sample chunk by changing its opening fence to three backticks followed by `{r scores-example}`, then look for `scores-example` in the outline. A client that sends individual R cells may provide its own document outline.

If the same example works in an `.R` file but the chunk has no R completions, check the file's language mode and your editor's R Markdown or Quarto integration. [Editor setup](./editors) · [VS Code R Markdown guide](https://reditorsupport.github.io/vscode-R/guide/r-markdown)

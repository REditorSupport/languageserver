# Explore data frames and queries

## Complete the columns of a dataset

Enter this example in an R file. The built-in `iris` dataset works with the CRAN release.

```r
flowers <- datasets::iris
flowers$Sepal.Length
```

On the second line, remove `Sepal.Length` and request completion just after `$`. Look for `Sepal.Length`, `Petal.Width`, and `Species`. The server can follow an assignment back to a package dataset and read its stored column names before you run the script. If you reassign `flowers` to a different known data frame, later completions use that value. A column computed inside an arbitrary function may only become known at runtime.

## Follow a Polars query

With the **polars** package installed in the server’s R library, enter this script. Request member completion at each `$` while building the expressions.

```r
library(polars)

flowers <- as_polars_df(iris)
query <- flowers$lazy()
result <- query$collect()
```

- After `flowers$`, look for data-frame methods such as `filter`, `select`, and `lazy`.
- After `query$`, the receiver is a lazy frame and offers `collect`. After `result$`, the receiver is a data frame again.
- After `pl$`, look for Polars constructors and functions such as `DataFrame` and `col`. Inside a recognized method call, request Signature Help to see its arguments.

These features are in CRAN 0.3.22. They use the installed package’s metadata, so the suggestions follow that version’s API. On the first request the server may still be reading that metadata. Give it a moment and request completion again. The editor can follow the query without collecting its data. [Member completion and uncertain receivers](./writing-r#complete-members-from-your-script)

## Find the source of a column

**Development.** A bare name inside a data.table query can belong to the table even when the script defines a variable with the same name. Try this with **Go to Definition**.

```r
value <- 99L
first <- data.table::data.table(value = 1:3)
second <- data.table::data.table(value = 4:6)
first[, value]
second[, value]
```

Select `value` inside each pair of brackets. The first opens the named column in `first` and the second opens the one in `second`. Each jump selects that constructor’s `value` argument. A later operation such as `data.table::setnames()` can make the original target uncertain, so the editor may have no definition to open.

The same lookup works for a known column inside `with()`, `subset()`, and supported dplyr verbs. In `dplyr::filter(scores, .data$score > .env$score)`, the first name refers to the declared column and the second to the enclosing R binding. Find References or Rename on the enclosing variable follows `.env$score`. The column keeps its name. [Try the data-mask rename example](./development#rename-a-variable-used-beside-a-data-frame-column)

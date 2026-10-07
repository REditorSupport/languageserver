# Open this file with this branch's languageserver. Execution is optional.
# Completion: put the cursor after a $, deleting the following member name.
# Signature help: put the cursor inside a method's parentheses.
# Hover: point at a method name or named argument.

library(polars)

csv_file <- tempfile(fileext = ".csv")
write.csv(iris, csv_file, row.names = FALSE)

q <- pl$scan_csv(csv_file, infer_schema_files = 10)

# After the last $: agg, head, map_groups, ... (the grouped receiver).
# Hover group_by or .maintain_order for method/argument documentation.
grouped <- q$group_by("Species", .maintain_order = TRUE)
grouped$agg(pl$all()$sum())

# After the last $: collect, filter, group_by, ... (a lazy frame again).
result <- grouped$agg(pl$all()$sum())
result$collect()

# Nested namespace: complete/hover to_uppercase; signature: to_uppercase().
expression <- pl$col("Species")$str$to_uppercase()

# The Series receiver retains its return shape while delegating sum's docs.
series <- pl$Series("values", 1:3)
series$sum()

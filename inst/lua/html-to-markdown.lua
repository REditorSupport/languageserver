--[[
Converts the Arguments from a table to sections

Conversion happens if the table cannot be represented by the GFM-syntax.
Cells in the 1st column become headers and those in the 2nd column becomes
content of the corresponding headers.
--]]
function Table(el)
  local tb = pandoc.utils.to_simple_table(el)

  -- Do nothing if the table has headers or is not composed of 2 columns
  if #tb.headers > 0 or #tb.rows[1] ~= 2 then return end

  -- Do nothing if the 1st column has cells not comprised of single Plain block
  -- or if the 2nd column can be represented by the GFM-syntax.
  local complex_table = false
  for _, row in pairs(tb.rows) do
    if (#row[1] ~= 1) or (row[1][1].t ~= "Plain") then return end
    complex_table = complex_table or (#row[2] > 1 or row[2][1].t ~= "Plain")
  end
  if not complex_table then return end

  -- Conversion
  local header
  local blocks = {}
  for _, row in pairs(tb.rows) do
    header = row[1][1].content
    table.insert(header, pandoc.Str(':'))
    table.insert(blocks, pandoc.Header(4, header))
    for _, b in pairs(row[2]) do
      table.insert(blocks, b.t == "Plain" and pandoc.Para(b.content) or b)
    end
  end
  return blocks
end


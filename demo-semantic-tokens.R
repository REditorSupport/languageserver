# Demo: Semantic Tokens Support in R Language Server
# This demonstrates that the language server now supports semantic tokens,
# which will enable proper syntax highlighting for function calls from packages

# Example R code that will benefit from semantic tokens:
library(dplyr)
library(ggplot2)

# Function calls from packages will now be highlighted differently
data <- mtcars %>%
  filter(mpg > 20) %>%          # filter is from dplyr
  mutate(efficiency = mpg / wt) %>%  # mutate is from dplyr  
  arrange(desc(efficiency))      # arrange is from dplyr

# Base R functions will have the defaultLibrary modifier
result <- mean(data$mpg)        # mean is from base R
output <- print(result)          # print is from base R

# User-defined functions
my_function <- function(x, y) {  # function keyword is highlighted
  if (x > y) {                   # if keyword is highlighted
    return(x)                    # return keyword is highlighted
  } else {                       # else keyword is highlighted
    return(y)
  }
}

# User-defined function calls are now properly highlighted
result <- my_function(10, 5)     # my_function is now highlighted as a function!

# The semantic tokens feature provides:
# 1. Different token types for functions, variables, parameters, keywords, etc.
# 2. Modifiers like 'defaultLibrary' for base R functionsz
# 3. Proper classification of function calls from loaded packages

print("Semantic tokens are now enabled in the R language server!")
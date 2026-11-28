# Explore R classes and methods

With CRAN 0.3.22, you can inspect the members an object exposes and the classes it inherits from before running your script.

## Inspect class members

```r
Item <- R6::R6Class("Item", public = list(
  label = function(prefix = "") paste0(prefix, "item")
))
Book <- R6::R6Class("Book", inherit = Item)
book <- Book$new()
book$label(prefix = "new ")
```

Request completion after `book$` to see the inherited `label` method. Hover over `label` or request **Signature Help** just inside `book$label(` to see its `prefix` argument. In classes with a public `initialize` method, its parameters also appear at `$new(`. When writing R6 methods, you can use completion on `self$`, `private$`, and `super$` where those bindings are available.

The server can follow supported factories and member chains into installed packages using their metadata. For instance, with the R Polars package installed, completing methods on a lazy query and then on its `collect()` result offers methods for the respective object. A receiver built by running arbitrary R code can leave the list of members incomplete. [Member completion in scripts](./writing-r#complete-members-from-your-script)

## Browse a type hierarchy

In the R6 example, put the cursor on `Book` at the start of its declaration and open your editor’s **Type Hierarchy**. Choose **Supertypes** to see `Item`. Start on the `Item` declaration and choose **Subtypes** to see `Book`. Expand a result to follow another inheritance level where one exists.

The same commands work on formal R class declarations. In the example below, put the cursor inside the quoted `"Sample"` in its declaration. Its supertype is `"Observation"` and its subtype is `"Trial"`.

```r
methods::setClass("Observation", slots = c(value = "numeric"))
methods::setClass("Sample", contains = "Observation")
methods::setClass("Trial", contains = "Sample")
```

For reference classes, use the class name in `setRefClass("Sample", contains = "Observation")` with matching reference-class declarations. For an S3 method such as `print.widget`, the server can identify the class name `widget`. S3 dispatch can depend on a class vector chosen at runtime, so the hierarchy returns no parent or child links for that method. [S3 dispatch and signatures](./writing-r#find-the-arguments-for-an-s3-method)

## Work with S4 slots and S7 properties

With the S4 declarations above, add `sample <- methods::new("Sample", value = 2)`. Request completion after `sample@` for the inherited slot `value`. Hover on `value` in `sample@value`. The server can follow the supplied constructor argument and shows `2` in this example.

With S7 installed, try a class that declares a property.

```r
Pet <- S7::new_class("Pet", properties = list(name = S7::class_character))
pet <- Pet(name = "Rex")
pet@name
```

Request argument completion inside `Pet(` to see `name`, then request member completion after `pet@` for the same property. Constructor signatures depend on the installed S7 version. The server can read declared and inherited properties without executing their getters, validators, or default expressions.

### Discover S4 constructor arguments

**Development.** Enter the following example in a new R file. In the first call to `Swatch()`, request argument completion immediately after the opening parenthesis to see the declared slot `shade`. In `methods::slot(paint, "shade")`, completion inside the quoted name also offers `shade`.

```r
Swatch <- methods::setClass("Swatch", slots = c(shade = "character"))
paint <- Swatch(shade = "blue")
methods::slot(paint, "shade")

methods::setMethod("initialize", "Swatch", function(.Object, color = "white") {
  .Object@shade <- color
  .Object
})
Swatch("red")
```

Request **Signature Help** inside the last call. The source-defined initializer gives it the signature `Swatch(color = "white")`. In `methods::new("Swatch", …)`, the same initializer supplies `color` for argument completion. A subclass can inherit an initializer from its nearest known parent.

### Follow an S7 property through a change

**Development.** Continue the `Pet` example above with S7 1.0. Hover over `name` in each of the last two lines. The server shows `"Ada"` on `pet` and `"Rex"` on the earlier copy.

```r
original <- pet
pet@name <- "Ada"
pet@name
original@name
```

You can also request completion inside the quoted key in `S7::prop(pet, "name")`. When a property is renamed with `deprecated_property()`, the server can follow the old name to the new property's value. Inside a class validator such as `function(self) self@name`, completing `self@` uses the properties of that class.

An S4 subclass can expose both kinds of fields. Register the S7 parent, declare an S4 subclass, and request completion after `tagged@` on the last line. Look for the inherited `name` property and the S4 slot `tag`.

```r
S7::S4_register(Pet)
methods::setClass("TaggedPet", contains = S7::S4_contains(Pet), slots = c(tag = "integer"))
tagged <- methods::new("TaggedPet", name = "Ada", tag = 1L)
tagged@tag
```

## Follow a specific member or method

**Development.** Add the following to an R file and run **Find References** on `run` in `first$run(1)`.

```r
first <- list(run = function(input) input)
second <- list(run = function(input) input + 1)
use_first <- function(x) first[["run"]](input = x)
first$run(1)
second$run(2)
```

The declaration in `first`, the literal `[["run"]]` access, and the `first$run(1)` call belong to the same member. Renaming that member to `execute` updates both spellings, keeping `second$run` independent. Selecting `input` in the first method and renaming it updates its declaration, body, and the named argument inside `use_first`. **Go to Definition** on that named argument opens the matching parameter.

Call hierarchy uses the same member identity. On the first method, inspect **Incoming Calls** to find `use_first`. With CodeLens enabled, its lens counts the two call sites. The server can also follow inherited R6 methods to the class that declares them, and distinguishes overrides in subclasses. Dynamic keys or a receiver with competing declarations can leave a target unresolved.

The development build also follows R6 `$clone()` results and source-defined reference-class methods. For example, a method declared with `methods = list(read = function(scale = 1) value * scale)` in `setRefClass()` supplies the `scale` parameter for completion, hover, and Signature Help on an instance’s `$read(` call.

## Open the type or implementation

**Development.** To inspect which class created `book` in the R6 example, place the cursor on `book` in `book$label(...)` and run **Go to Type Definition**. It opens the `Book` declaration. The same operation supports a uniquely identified source class for reference-class, S4, and S7 objects.

For an S3 generic such as `present(frame, ...)`, **Go to Definition** opens the generic. **Go to Implementation** opens the selected method when the server can infer the dispatch class. With S4, **Go to Definition** on a generic can list the source generic and its visible method registrations even if the class of the argument is unknown. Find References can include its registrations and calls. [Try S3 implementation lookup](./development#open-an-s3-implementation-or-a-class-declaration) · [Try S4 method navigation](./development#go-to-definition-for-s4-methods)

With S7 1.0 installed, open this example. It declares a subclass and a method with a known dispatch class.

```r
library(S7)
Pet := new_class(properties = list(name = class_character))
Dog := new_class(Pet)
speak := new_generic("x")
method(speak, Dog) <- function(x) x@name
dog <- Dog(name = "Rex")
speak(dog)
```

On `Dog` in its declaration, **Type Hierarchy → Supertypes** shows `Pet`. **Subtypes** on `Pet` leads back to `Dog`. On `speak` in the final call, **Go to Implementation** opens the `method(speak, Dog)` registration. Inside that method, **Go to Definition** on `name` in `x@name` opens the inherited property on `Pet`. S7 class, property, generic, and registration declarations can also appear in the outline and workspace symbol search. [Install a development build](./development)

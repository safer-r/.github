

# The safer project <a href=""><img src="./profile/safer_whole.png" align="right" height="155" /></a>

## Why the safer project?

[R](https://www.r-project.org) is a permissive programming language: it will 'try to work' in many situations and returns something, when other programming languages would have returned an error. This advantage partly explains its success, as it is commonly used by non programers. But it comes with several problems which could soften reproducibility or consistency aspects:
- Non intuitive behaviors (example of the [`sample()`](http://127.0.0.1:25073/library/base/html/sample.html) function when the input is a single integer).
- Lack of control of the arguments of functions (example with the [`range()`](https://bugs.r-project.org/show_bug.cgi?id=17654) function, or the presence of the `...` argument in functions).
- Lack of explicit error messages.
- Weak control of objects with identical names in the R scope.

The safer project gathers R functions of class S3 with a similar encoding that better controls their expected behavior.

## Features of the safer functions

Functions of class S3 from the safer project present the same encoding structure before the 'main' code section, which tackle the aspects described above, including:
- Reproducibility
    - Package systematically indicated for any used function (R Scope seeking non authorized). Example `base::paste()` instead of `paste()`.
    - Argument `scope_check` added, that checks 1) the presence in local R library folders of all the functions and corresponding packages used in the code and 2) that all these functions and classical R operators (`<-`, `(`, etc.) are not overwritten by other packages, always preceding the base R items in the R scope.
    - Seeding of the random number generator by protecting potential seeding in the global environment.
- Intuitiveness
    - Argument `...` not authorized.
- Explicit messages
    - Name of the function and corresponding package returning error and warning messages.
    - All warning messages added in the error message string.
    - explicit error messages following argument checking if: 
        - No values for arguments with no default values.
        - Unexpected class, type, mode, length, restricted values panel, kind of numeric values in addition to the distinction between 'integer' and 'double' (proportion only? Inf values authorized? negative values authorized?).
        - Unauthorized `NA` (among other values or as unique value).
        - Unauthorized `NULL` value.
        - Unexpected structure of complex objects, like data frames and lists.

## safer Packages

- [saferDev](https://github.com/safer-r/saferDev): R function and pipeline development.
- [saferMatrix](https://github.com/safer-r/saferMatrix): matrix handling.
- [saferGraph](https://github.com/safer-r/saferGraph): classical graphic handling.
- [saferTool](https://github.com/safer-r/saferTool): basic tools.
- [saferTool2](https://github.com/safer-r/saferTool2): sophisticated tools.

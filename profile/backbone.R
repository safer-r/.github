BACKBONE <- function(data, lib_path = NULL, seed_value_arg, safer_check = TRUE){ 

    #### package name
    package_name <- "saferDev" # write NULL if the function developed is not in a package
    #### end package name

    #### function name
    tempo_settings <- base::as.list(x = base::match.call(definition = base::sys.function(which = base::sys.parent(n = 0)), call = base::sys.call(which = base::sys.parent(n = 0)), expand.dots = FALSE, envir = base::parent.frame(n = 2L))) # warning: I have written n = 0 to avoid error when a safer function is inside another functions
    function_name <- base::paste0(tempo_settings[[1]], "()", collapse = NULL, recycle0 = FALSE) 
    # function name with "()" paste, which split into a vector of three: c("::()", "package ()", "function ()") if "package::function()" is used.
    if(function_name[1] == "::()" | function_name[1] == ":::()"){
        function_name <- function_name[3]
    }
    #### end function name

    #### arguments settings
    arg_user_setting <- tempo_settings[-1] # list of the argument settings (excluding default values not provided by the user)
    arg_names <- base::names(x = base::formals(fun = base::sys.function(which = base::sys.parent(n = 2)), envir = base::parent.frame(n = 1))) # names of all the arguments
    #### end arguments settings

    #### critical operator checking
    if( ! (base::all(safer_check %in% base::c(TRUE, FALSE), na.rm = FALSE) & base::length(safer_check) == 1 & base::all(base::is.logical(safer_check), na.rm = TRUE))){
        tempo.cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\nsafer_check ARGUMENT MUST BE EITHER TRUE OR FALSE. HER IT IS:\n", base::paste0(safer_check, collapse = "\n", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    if(safer_check == TRUE){
        saferDev:::.base_op_check(
            external_function_name = function_name, 
            external_package_name = package_name
        )
    }
    #### end critical operator checking

    #### package checking

    ######## check of lib_path
    if( ! base::is.null(x = lib_path)){
        if( ! base::all(base::typeof(x = lib_path) == "character", na.rm = FALSE)){ # no na.rm = TRUE with typeof
            tempo.cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\nDIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n", base::paste(lib_path, sep = " ", collapse = "\n", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }else if( ! base::all(base::dir.exists(paths = lib_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib_path == NA
            tempo.cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\nDIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXISTS:\n", base::paste(lib_path, sep = " ", collapse = "\n", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
           base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }else{
            base:::.libPaths(new = base::sub(x = lib_path, pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base:::.libPaths(new = ) add path to default path. BEWARE: base:::.libPaths() does not support / at the end of a submitted path. Thus check and replace last / or \\ in path
            lib_path <- base:::.libPaths(new = , include.site = TRUE)
        }
    }else{
        lib_path <- base:::.libPaths(new = , include.site = TRUE) # base:::.libPaths(new = lib_path) # or base:::.libPaths(new = base::c(base:::.libPaths(), lib_path))
    }
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        saferDev:::.pack_and_function_check(
            fun = base::c(
                "saferDev::arg_check", # write each function preceeded by their package name
                "lubridate::seconds_to_period"
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            external_function_name = function_name,
            external_package_name = package_name
        )
    }
    ######## end check of the required functions from the required packages

    #### end package checking

    #### argument primary checking

    ######## arg with no default values
    mandat.args <- base::c(
        "data"
    )
    tempo <- base::eval(expr = base::parse(text = base::paste0("base::c(base::missing(", base::paste0(mandat.args, collapse = "),base::missing(", recycle0 = FALSE), "))", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(base::any(tempo, na.rm = FALSE)){
        tempo.cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\nFOLLOWING ARGUMENT", base::ifelse(test = base::sum(tempo, na.rm = TRUE) > 1, yes = "S HAVE", no = " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args[tempo], collapse = "\n", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end arg with no default values

    ######## argument checking with arg_check()
    argum.check <- NULL
    text.check <- NULL
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum.check <- base::c(argum.check, tempo$problem) , text.check <- base::c(text.check, tempo$text) , checked.arg.names <- base::c(checked.arg.names, tempo$object.name))
    tempo <- saferDev::arg_check(data = data, class = NULL, typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double.as.integer.allowed = FALSE, options = NULL, all.options.in.data = FALSE, na.contain = TRUE, neg.values = TRUE, inf.values = TRUE, print = FALSE, data.name = NULL, fun.name = function_name, safer_check = FALSE) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # copy - paste this line as much as necessary
    if( ! base::is.null(x = path_out)){ # for all arguments that can be NULL, write like this:
        tempo <- saferDev::arg_check(data = path_out, class = "vector", typeof = NULL, mode = "character", length = NULL, prop = FALSE, double.as.integer.allowed = FALSE, options = NULL, all.options.in.data = FALSE, na.contain = TRUE, neg.values = TRUE, inf.values = TRUE, print = FALSE, data.name = NULL, fun.name = function_name, safer_check = FALSE) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    # lib_path already checked above
    tempo <- saferDev::arg_check(data = safer_check, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double.as.integer.allowed = FALSE, options = NULL, all.options.in.data = FALSE, na.contain = FALSE, neg.values = TRUE, inf.values = TRUE, print = FALSE, data.name = NULL, fun.name = function_name, safer_check = FALSE) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # rechecked even if already used above 
    if( ! base::is.null(x = argum.check)){
        if(base::any(argum.check, na.rm = TRUE)){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], sep = " ", collapse = "\n", recycle0 = FALSE), "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # check with r_debugging_tools
    # source("https://gitlab.pasteur.fr/gmillot/debugging_tools_for_r_dev/-/raw/v1.8/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    ######## end argument checking with arg_check()


    ######## management of NA arguments
    if(base::length(x = arg_user_setting) != 0){
        tempo.log <- base::suppressWarnings(expr = base::sapply(X = base::lapply(X = arg_user_setting, FUN = function(x){base::is.na(x = x)}), FUN = function(x){base::any(x = x, na.rm = FALSE)}, simplify = TRUE, USE.NAMES = TRUE), classes = "warning") & base::lapply(X = arg_user_setting, FUN = function(x){base::length(x = x)}) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log, na.rm = FALSE)){ # normally no NA because base::is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\n", base::ifelse(test = base::sum(tempo.log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(arg_names[tempo.log], collapse = "\n", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }
    }
    ######## end management of NA arguments

    ######## management of NULL arguments
    tempo.arg <-base::c(
        "x", 
        "export", 
        "path_out", 
        "df_name", 
        "overwrite", 
        # "lib_path", # because can be NULL
        "safer_check"
    )
    tempo.log <- base::sapply( X = base::lapply(X = tempo.arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())
    if(base::any(tempo.log, na.rm = FALSE)){ # normally no NA with base::is.null()
        tempo.cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\n", base::ifelse(test = base::sum(tempo.log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS\n", no = "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n", recycle0 = FALSE),"\nCANNOT BE NULL", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end management of NULL arguments

    ######## management of "" in arguments of mode character
    tempo.arg <-base::c(
        "lib_path"
    )
    tempo.log <- ! base::sapply(X = base::lapply(X = tempo.arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())  # for character argument that can also be NULL, if NULL -> considered as character
    if(base::any(tempo.log, na.rm = TRUE)){
        tempo.cat <- base::paste0("INTERNAL ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\n", base::ifelse(test = base::sum(tempo.log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS ARE", no = "THIS ARGUMENT IS"), " NOT MODE \"character\":\n", base::paste0(tempo.arg[tempo.log], collapse = "\n", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }else{
        tempo.log <- base::sapply(X = base::lapply(X = tempo.arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::any(x == "", na.rm = FALSE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())
        if(base::any(tempo.log, na.rm = TRUE)){
            tempo.cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\n", base::ifelse(test = base::sum(tempo.log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS\n", no = "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n", recycle0 = FALSE),"\nCANNOT CONTAIN \"\"", collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        }
    }
    ######## end management of "" in arguments of mode character

    #### end argument primary checking

    #### second round of checking and data preparation

    ######## reserved words (to avoid bugs)
    reserved_words <- base::c("fake_x", "fake_y", "fake_categ", "color")
    ######## end reserved words (to avoid bugs)

    ######## new environment
    env.name <- base::paste0("env", base::as.numeric(x = base::Sys.time()), collapse = NULL, recycle0 = FALSE)
    if(base::exists(x = env.name, where = -1, envir = if(base::missing(x = frame)){base::as.environment(x = where)}else{base::sys.frame(which = frame)}, frame = , mode = "any", inherits = TRUE)){ # verify if still ok when info() is inside a function
        tempo.cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\nENVIRONMENT env.name ALREADY EXISTS. PLEASE RERUN ONCE", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }else{
        base::assign(x = env.name, value = base::new.env(hash = TRUE, parent = base::parent.frame(n = 1), size = 29L), pos = -1, envir = base::as.environment(x = pos), inherits = FALSE, immediate = TRUE)
    }
    ######## end new environment

    ######## code that protects set.seed() in the global environment
    if (base::exists(x = ".Random.seed", where = -1, envir = .GlobalEnv, frame = , mode = "any", inherits = TRUE)) {
        # if .Random.seed does not exists, it means that no random operation has been performed yet in any R environment
        tempo.random.seed <- .Random.seed
        base::on.exit(expr = base::assign(x = ".Random.seed", value = tempo.random.seed, pos = -1, envir = .GlobalEnv, inherits = FALSE, immediate = TRUE), add = FALSE, after = TRUE)
    } else{
        base::on.exit(expr = base::set.seed(seed = NULL, kind = NULL, normal.kind = NULL, sample.kind = NULL), add = FALSE, after = TRUE) # inactivate seeding -> return to complete randomness
    }
    base::set.seed(seed = seed_value_arg, kind = NULL, normal.kind = NULL, sample.kind = NULL) # seed_value argument of the function
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ini.warning.length <- base::options()$warning.length # required to have the max characters of output messages
    base::options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    ######## end warning initiation

    ######## other checkings
    if(base::length(x = data) == 0){
        tempo.cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\ndata ARGUMENT CANNOT BE LENGTH 0", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if( ! base::is.null(x = lib_path)){
        if( ! base::all(base::dir.exists(paths = lib_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib_path == NA
            tempo.cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\nDIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXISTS:\n", base::paste(lib_path, sep = " ", collapse = "\n", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code

    ######## warning
    if(loop > 100){
        warn.count <- warn.count + 1
        tempo.warn <- base::paste0("(", warn.count,") THE FIRST FOR & WHILE LOOP STEPS HAVE BEEN TOO FAR AND SUBSEQUENT LOOP STEPS WILL NOT RUN", collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo.warn, no = base::paste0(warn, "\n\n", tempo.warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
    }
    ######## end warning


    ######## reserved word checking
    if(base::any(base::names(x = data1[[i1]]) %in% reserved_words, na.rm = FALSE)){
        tempo.cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\nCOLUMN NAMES OF ", base::ifelse(test = base::length(x = data1) == 1, yes = "data1", no = base::paste0("data1 NUMBER ", i1, collapse = NULL, recycle0 = FALSE)), " ARGUMENT CANNOT BE ONE OF THESE WORDS\n", base::paste(reserved_words, sep = " ", collapse = " ", recycle0 = FALSE), "\nTHESE ARE RESERVED FOR THE ", function_name, " FUNCTION", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end reserved word checking

    #### end main code

    #### output
    base::return(output)
    #### end output

    #### warning output
    if( ! base::is.null(x = warn)){
        base::on.exit(expr = base::warning(base::paste0("FROM ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\n\n", warn, collapse = NULL, recycle0 = FALSE), call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL), add = FALSE, after = TRUE)
      }
      base::on.exit(expr = base::options(warning.length = ini.warning.length), add = TRUE, after = TRUE)
    #### end warning output
}
a <- function(){ 
    #### package name
    package.name <- "saferDev"
    #### end package name

    #### function name
    function.name <- base::paste0(base::as.list(base::match.call(expand.dots = FALSE))[[1]], "()", collapse = NULL, recycle0 = FALSE) # function name with "()" paste, which split into a vector of three: c("::()", "package ()", "function ()") if "package::function()" is used.
    if(function.name[1] == "::()" | function.name[1] == ":::()"){
        function.name <- function.name[3]
    }
    arg.names <- base::names(base::formals(fun = base::sys.function(base::sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- base::as.list(base::match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    #### end function name

    #### critical operator checking
    if(safer_check == TRUE){
        saferDev:::.base_op_check(
            external.function.name = function.name, 
            external.package.name = package.name
        )
    }
    #### end critical operator checking

    #### package checking
    ######## check of lib.path
    if( ! base::is.null(lib.path)){
        if( ! base::all(base::typeof(lib.path) == "character")){ # no na.rm = TRUE with typeof
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nDIRECTORY PATH INDICATED IN THE lib.path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n", base::paste(lib.path, collapse = "\n", recycle0 = FALSE))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }else if( ! base::all(base::dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nDIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", base::paste(lib.path, collapse = "\n", recycle0 = FALSE))
           base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }else{
            base:::.libPaths(new = base::sub(x = lib.path, pattern = "/$|\\\\$", replacement = "")) # base:::.libPaths(new = ) add path to default path. BEWARE: base:::.libPaths() does not support / at the end of a submitted path. Thus check and replace last / or \\ in path
            lib.path <- base:::.libPaths()
        }
    }else{
        lib.path <- base:::.libPaths() # base:::.libPaths(new = lib.path) # or base:::.libPaths(new = base::c(base:::.libPaths(), lib.path))
    }
    ######## end check of lib.path
    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        saferDev:::.pack_and_function_check(
            fun = base::c(
                "saferDev::arg_check",
                "parallel::stopCluster",
                "lubridate::seconds_to_period"
            ),
            lib.path = lib.path, # write NULL if your function does not have any lib.path argument
            external.function.name = function.name,
            external.package.name = package.name
        )
    }
    ######## end check of the required functions from the required packages
    #### end package checking

    #### argument primary checking
    ######## arg with no default values
    mandat.args <- base::c(
        "data", 
        "window.size", 
        "step", 
        "FUN"
    )
    tempo <- base::eval(base::parse(text = base::paste0("base::c(base::missing(", base::paste0(mandat.args, collapse = "),base::missing(", recycle0 = FALSE), "))", collapse = NULL, recycle0 = FALSE)))
    if(base::any(tempo)){ # normally no NA for base::missing() output
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nFOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args[tempo], collapse = "\n", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    ######## end arg with no default values
    ######## argument checking with arg_check()
    argum.check <- NULL
    text.check <- NULL
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum.check <- base::c(argum.check, tempo$problem) , text.check <- base::c(text.check, tempo$text) , checked.arg.names <- base::c(checked.arg.names, tempo$object.name))
    tempo <- saferDev::arg_check(data = data, mode = "numeric", na.contain = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = safer_check, class = "vector", typeof = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee) # even if already used above
    if( ! base::is.null(argum.check)){
        if(base::any(argum.check, na.rm = TRUE)){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], collapse = "\n", recycle0 = FALSE), "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE)
        }
    }
    # check with r_debugging_tools
    # source("https://gitlab.pasteur.fr/gmillot/debugging_tools_for_r_dev/-/raw/v1.8/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    ######## end argument checking with arg_check()
    ######## management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) %in% base::c("list", "NULL"), na.rm = TRUE) & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, envir = base::sys.nframe(), inherits = FALSE), FUN = base::is.na), FUN = base::any)) & base::lapply(base::lapply(tempo.arg, FUN = base::get, envir = base::sys.nframe(), inherits = FALSE), FUN = base::length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log)){ # normally no NA because base::is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    ######## end management of NA arguments
    ######## management of NULL arguments
    tempo.arg <- base::c(
        "data", 
        "safer_check"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, envir = base::sys.nframe(), inherits = FALSE), FUN = base::is.null)
    if(base::any(tempo.log)){ # normally no NA with base::is.null()
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n", recycle0 = FALSE),"\nCANNOT BE NULL", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    ######## end management of NULL arguments
    ######## management of "" in arguments of mode character
    tempo.arg <-base::c(
        "data", 
        "text"
    )
    tempo.log <- ! base::sapply(base::lapply(tempo.arg, FUN = get, envir = base::sys.nframe(), inherits = FALSE), FUN = function(x){
        if(base::is.null(x)){
            base::return(TRUE) # for character argument that can also be NULL, if NULL -> considered as character
        }else{
            base::all(base::mode(x) == "character", na.rm = TRUE)
        }
    })
    if(base::any(tempo.log, na.rm = TRUE)){
        tempo.cat <- base::paste0("INTERNAL ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS ARE", "THIS ARGUMENT IS"), " NOT MODE \"character\":\n", base::paste0(tempo.arg[tempo.log], collapse = "\n", recycle0 = FALSE))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }else{
        tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = get, env = base::sys.nframe(), inherit = FALSE), FUN = function(x){base::any(x == "")})
        if(base::any(tempo.log, na.rm = TRUE)){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n", recycle0 = FALSE),"\nCANNOT CONTAIN \"\"")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    ######## end management of "" in arguments of mode character
    #### end argument primary checking

    #### second round of checking and data preparation
    ######## reserved words (to avoid bugs)
    reserved.words <- base::c("fake_x", "fake_y", "fake_categ", "color")
    ######## end reserved words (to avoid bugs)
    ######## new environment
    env.name <- base::paste0("env", base::as.numeric(base::Sys.time()), collapse = NULL, recycle0 = FALSE)
    if(base::exists(env.name, where = -1)){ # verify if still ok when info() is inside a function
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nENVIRONMENT env.name ALREADY EXISTS. PLEASE RERUN ONCE", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }else{
        base::assign(env.name, base::new.env())
    }
    ######## end new environment
    ######## code that protects set.seed() in the global environment
    if (base::exists(".Random.seed", envir = .GlobalEnv)) {
        # if .Random.seed does not exists, it means that no random operation has been performed yet in any R environment
        tempo.random.seed <- .Random.seed
        base::on.exit(base::assign(".Random.seed", tempo.random.seed, env = .GlobalEnv))
    } else{
        base::on.exit(base::set.seed(NULL)) # inactivate seeding -> return to complete randomness
    }
    base::set.seed(seed_value_arg) # seed_value argument of the function
    ######## end code that protects set.seed() in the global environment
    ######## warning initiation
    ini.warning.length <- base::options()$warning.length # required to have the max characters of output messages
    base::options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    ######## end warning initiation
    ######## other checkings
    if(base::length(data) == 0){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\ndata ARGUMENT CANNOT BE LENGTH 0", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE)
    }
    if( ! base::is.null(lib.path)){
        if( ! base::all(base::dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nDIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", base::paste(lib.path, collapse = "\n", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE)
        }
    }
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    ######## warning
    warn.count <- warn.count + 1
    tempo.warn <- base::paste0("(", warn.count,") THE FIRST FOR & WHILE LOOP STEPS HAVE BEEN TOO FAR AND SUBSEQUENT LOOP STEPS WILL NOT RUN", collapse = NULL, recycle0 = FALSE)
    warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)), collapse = NULL, recycle0 = FALSE)
    ######## end warning
    ######## reserved word checking
    if(base::any(base::names(data1[[i1]]) %in% reserved.words)){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nCOLUMN NAMES OF ", base::ifelse(base::length(data1) == 1, "data1", base::paste0("data1 NUMBER ", i1)), " ARGUMENT CANNOT BE ONE OF THESE WORDS\n", base::paste(reserved.words, collapse = " ", recycle0 = FALSE), "\nTHESE ARE RESERVED FOR THE ", function.name, " FUNCTION", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE)
    }
    ######## end reserved word checking
    #### end main code

    #### output
    #### end output

    #### warning output
    if(warn.print == TRUE & ! base::is.null(warn)){
        base::on.exit(base::warning(base::paste0("FROM ", function.name, " OF THE ", package.name, " PACKAGE\n\n", warn, collapse = NULL, recycle0 = FALSE), call. = FALSE))
      }
      base::on.exit(expr = base::options(warning.length = ini.warning.length), add = TRUE)
    #### end warning output
}
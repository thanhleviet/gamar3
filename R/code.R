# This function copies the files of given example ("what") to a given directory
# ("to").
# Used by "examples". "what" and "to" are character strings. "what" is the
# name of the model as defined in "examples_files$model" and "to" is the path of
# the directory where to put the files.
.examples <- function(what, to = NA) {
# this function inserts "_\\d" before the extension of a file to avoid
# overwritting. "i" is the digit to use for the insertion and "x" is the
# character string in which to do the insertion:
  insert <- function(i, x) sub("^(.*)(\\.gaml)$", paste0("\\1_", i,"\\2"), x)
# the path to the examples:
  path <- system.file("examples", package = "gamar")
# selecting the information of the example we are interested in:
  what <- subset(get("examples_files"), model == what)
  copy <- what$copy  # what to copy
  to0 <- to          # where to copy
  if (is.na(to)) {  # in case no directory is specified, we set to working dir.
    print_to <- "working directory"  # will be updated if the file/dir exists
    to <- getwd()
  } else {  # in case a directory is specified we create it if it does not exist
    print_to <- paste0("'./", to, copy, "'")
    if (!dir.exists(to)) dir.create(to, recursive = TRUE)
  }
  to_try <- paste0(to, copy)
  if (grepl("\\.gaml$", copy)) {  # in case we copy a file ---------------------
    if (file.exists(to_try)) {  # in case the file already exists
      i <- 1; while(file.exists(insert(i, to_try))) i <- i + 1  # look for "i"
      to <- insert(i, to_try)  # update "to"
      if (is.na(to0)) print_to <- paste0("'.", insert(i, copy), "'")  #| update...
      else print_to <- paste0("'./", to0, insert(i, copy), "'")       #| ... "print_to"
    }
    file.copy(paste0(path, copy), to)
    obj <- "File"
  } else {  # in case we copy a directory instead of a file --------------------
    if (dir.exists(to_try)) {  # in case the directory already exists
      i <- 1; while(dir.exists(paste0(to_try, "_", i))) i <- i + 1  # look for "i"
      tmp <- tempdir()                                      #| because we want
      file.copy(paste0(path, copy), tmp, recursive = TRUE)  #| the name of the
      name1 <- paste0(tmp, copy)                            #| copied directory
      name2 <- paste0(name1, "_", i)                        #| to be different
      file.rename(name1, name2)                             #| from the original
      file.copy(name2, to, recursive = TRUE)                #| name, we need to
      unlink(name2, recursive = TRUE)                       #| go through tmpdir
      if (is.na(to0)) print_to <- paste0("'.", paste0(copy, "_", i), "'")  #| update
      else print_to <- paste0("'./", to, copy, "_", i, "'")                #| "print_to"
    } else file.copy(paste0(path, copy), to, recursive = TRUE)
    obj <- "Directory"
  }
  message(paste0(obj, " '", sub("/" , "", copy), "' copied to ", print_to, "."))
}


# ------------------------------------------------------------------------------


#' @export
# views built-in examples:
# if "what" and "to" are missing, it prints the list of built-in examples.
# "what" is the name of an example.
# "to" is the path to where we want to save the example.
examples <- function(what, to) {
# (1) first case: no argument values provided to the function, just show the
# list of built-in examples:
  if (missing(what)) {
    examples_files <- get("examples_files")
    nc <- nchar(examples_files[, 1])  # the length of the white space before ":"
    whitesp <- sapply(max(nc) - nc,   # the vector of white spaces.
                      function(x)
                        paste0(paste(rep(" ", x), collapse = ""), " : "))
    cat(paste(apply(cbind(examples_files[, 1], whitesp, examples_files[, 2]), 1,
                    paste, collapse = ""), collapse = "\n"))  # the print out.
# (2) second case: moving model files from examples directory:
  } else {  # this is basically a vectorized call of .examples
    if (missing(to)) to <- rep(NA, length(what))
    else if (length(to) < 2) to <- rep(to, length(what))
    for(i in seq_along(to)) .examples(what[i], to[i])
  }
}


# ------------------------------------------------------------------------------


# creates the workgamar directory if it does not exist:
createworkingdirectory <- function(dir) {
  if(missing(dir)) dir <- getwd()
  outdirectory <- paste0(dir, "/workgamar")
  if(!file.exists(outdirectory)) dir.create(outdirectory)
  outdirectory
}


# ------------------------------------------------------------------------------


# looks for an available file name:
createmodelparameterfilename <- function(experimentname, dir) {
  outdirectory <- createworkingdirectory(dir)  # makes sure that workgamar exists
  i <- 0
  repeat {
    i <- i + 1
    outfile <- paste0(outdirectory, "/", experimentname, "_", i, ".xml")
    if(!file.exists(outfile)) break
  }
  outfile
}


# ------------------------------------------------------------------------------


#' @export
experiment <- function(experimentname, modelfile) {
  on.exit(message(paste0("Experiment '", experimentname, "' from '", modelfile,
                         "' saved in '", outfile, "'.")))
  message(paste0("Loading experiment '", experimentname,
                 "' from file '", basename(modelfile), "'..."))
  outfile <- createmodelparameterfilename(experimentname, getwd())  # creates workgamar
  trycommand <- system(paste0("java -jar ", getOption("gamar.startjar"), " -Xms",
                              getOption("gamar.Xms"), " -Xmx",
                              getOption("gamar.Xmx"),
                              " -Djava.awt.headless=true org.eclipse.core.launcher.Main ",
                              "-application msi.gama.headless.id4 -xml ",
                              experimentname, " ", modelfile, " ", outfile, ">/dev/null"),
                       ignore.stdout = TRUE, ignore.stderr = TRUE)
# removing the "workspace" directory that is created by the command above.
  unlink("workspace", TRUE, TRUE)
  if (trycommand > 0) return(-1)
  out <- XML::xmlToList(XML::xmlParse(outfile))
  structure(out$Simulation, class = "experiment")
#  assign(experimentname, structure(out$Simulation, class = "experiment"),
#         envir = parent.frame())
}




# ------------------------------------------------------------------------------


#' @export
is.experiment <- function(object) {
  "experiment" %in% class(object)
}


# ------------------------------------------------------------------------------


# This function retrieves the locations of all the parameters of the model in
# the 3 levels of the hierarchical list.
# Used by "map_par". "exp" should be a list.
map_mod_par <- function(exp) {
  get_names <- function(x) sapply(x, "[", "name")
  get_level3 <- function(level1, slot)
    sapply(exp[[level1]], function(x) which(names(x) == slot))
  par_names <- lapply(exp[c("Parameters", "Outputs")], get_names)
  level1 <- rep(names(par_names), sapply(par_names, length))
  level2 <- unlist(lapply(par_names, seq_along))
  level3 <- unlist(mapply(get_level3,
                          c("Parameters", "Outputs"),
                          c("value", "framerate")))
  data.frame(param  = unlist(par_names),level1, level2, level3,
             stringsAsFactors = FALSE)
}


# ------------------------------------------------------------------------------


# This function retrieves the locations of all the parameters of the experiment
# in the 3 levels of the hierarchical list.
# Used by "map_par". "exp" can be of class list or experiment.
map_exp_par <- function(exp) {
  level1 <- ".attrs"
  param <- names(exp[[level1]])
  data.frame(param, level1, level2 = 1, level3 = seq_along(param),
             stringsAsFactors = FALSE)
}


# ------------------------------------------------------------------------------


# This function uses "map_mod_par" and "map_exp_par" to retrieve the locations
# of all the model and experiment parameters in the 3 levels of the hierarchical
# list.
# Used by "as.list.experiment", "names.experiment" and "`[<-.experiment`".
map_par <- function(exp) {
  exp <- unclass(exp)  # required by "map_mod_par"
  out <- do.call(rbind, lapply(list(map_mod_par, map_exp_par),
                               function(f) f(exp)))
  rownames(out) <- NULL
  out <- data.frame(section = out$level1, out, stringsAsFactors = FALSE)
  level1 <- names(exp)
  lut <- setNames(seq_along(level1), level1)
  out$level1 <- lut[out$level1]
  out
}


# ------------------------------------------------------------------------------


# This function reformats the output of "map_par" in a data frame.
# Used by "as.list.experiment" and "names.experiment".
reformat <- function(x)
  setNames(as.data.frame(apply(x[, paste0("level", 1:3)], 1, "c")), x$param)


# ------------------------------------------------------------------------------


#' @export
as.list.experiment <- function(exp) {
  map <- map_par(exp)
  slots <- unique(map$section)
  exp$.attrs <- list(exp$.attrs)  # to comply with the map_par output structure
  map <- split(map, map$section)[slots]
  out <- lapply(map, reformat)
  lapply(out, function(x) sapply(x, function(y) exp[[y]]))
}


# ------------------------------------------------------------------------------


as_numeric <- function(x) setNames(as.numeric(x), names(x))


# ------------------------------------------------------------------------------


#' @export
print.experiment <- function(exp) {
  exp2 <- as.list(exp)
  cat("An experiment of the model defined in:\n")
  cat(paste0(exp2$.attrs["sourcePath"], "\n"))
  cat("\nParameters values:\n")
  print(as_numeric(exp2$Parameters))
  cat(paste("\nNumber of steps in the simulation:", exp2$.attrs["finalStep"]))
  cat("\n\nPeriods (in steps) at which variables are monitored:\n")
  print(as_numeric(exp2$Outputs))
  cat(paste("\nSeed value:", exp2$.attrs["seed"]))
  invisible(exp)
}


# ------------------------------------------------------------------------------


flatten <- function(exp) unlist(unname(as.list(exp)))


# ------------------------------------------------------------------------------


#' @export
names.experiment <- function(exp) map_par(exp)$param


# ------------------------------------------------------------------------------


#' @export
`[.experiment` <- function(exp, values) {
  exp <- flatten(exp)
  exp_names <- names(exp)
  if (all(values %in% exp_names)) {
    exp <- exp[values]
    if (!(any(c("experiment", "file") %in% exp_names))) exp <- as.numeric(exp)
  } else exp <- unclass(exp)[value]
  suppressWarnings(num <- as_numeric(exp))
  if (any(is.na(num))) return(exp) else return(num)
}


# ------------------------------------------------------------------------------


#' @export
name <- function(exp) {
  unname(exp["experiment"])
}


# ------------------------------------------------------------------------------


#' @export
`name<-` <- function(x, value) {
  x["experiment"] <- value
  x
}


# ------------------------------------------------------------------------------

#' @export
fname <- function(x) UseMethod("fname", x)

#' @export
fname.experiment <- function(exp) {
  unname(exp["sourcePath"])
}


# ------------------------------------------------------------------------------


#' @export
`fname<-` <- function(x, ...) UseMethod("fname<-", x)

#' @export
`fname<-.experiment` <- function(x, value) {
  if (substring(value, 1, 1) != "/") value <- paste0(getwd(), "/", value)
  x["sourcePath"] <- value
  x
}


# ------------------------------------------------------------------------------


#' @export
`[<-.experiment` <- function(x, slot, value) {
  map <- map_par(x)
  x$.attrs <- list(x$.attrs)  # to comply with the map_par output structure
  map <- as.list(reformat(map)[slot])
  for(i in seq_along(value)) x[[map[[i]]]] <- value[i]
  x$.attrs <- unlist(x$.attrs, recursive = FALSE)  # back to initial structure
  x
}


# ------------------------------------------------------------------------------


#' @export
model <- function(experiment) utils::file.edit(fname(experiment))


# ------------------------------------------------------------------------------


#' @export
print.plan <- function(plan) {
  plan2 <- lapply(unclass(plan), flatten)
  the_file <- plan2[[1]]["sourcePath"]
  sel <- which(!(names(plan2[[1]]) %in% c("experiment", "sourcePath")))
  plan2 <- lapply(plan2, function(x) as_numeric(x[sel]))
  print(do.call(rbind, plan2))
  cat(paste("\nModel's file:", the_file))
  invisible(plan)
}


# ------------------------------------------------------------------------------


rename <- function(args) {
#  get_names <- function(exp) as.list(exp)$.attrs["experiment"]
#  thenames <- sapply(args, get_names)
#  nametable <- table(thenames)
#  names_to_change <- nametable[which(nametable > 1)]
#  for (i in names(names_to_change))
#    thenames[thenames == i] <- paste0(i, "_", seq_len(names_to_change[i]))
#  names(args) <- thenames
#  for(i in seq_along(args)) name(args[[i]]) <- thenames[i]
  structure(args, class = c("plan", "experiment"))
}


# ------------------------------------------------------------------------------


#' @export
`c.experiment` <- function(...) {
  args <- list(...)
  sel <- sapply(args, function(x) "plan" %in% class(x))
  if (any(sel)) {
    for(i in which(sel)) args[i] <- unclass(args[i])
    if (any(!sel)) for(i in which(!sel)) args[i] <- list(args[i])
    args <- unlist(args, recursive = FALSE)
  }
  if (length(unique(sapply(args, "[", "sourcePath"))) > 1)
    stop("The experiments do not all call the same model.")
  for(i in seq_along(args)) args[[i]]$.attrs["id"] <- i
  rename(args)
}


# ------------------------------------------------------------------------------


#' @export
rep.experiment <- function(exp, times) {
  plan <- rename(rep(list(exp), times))
  for(i in seq_along(plan)) plan[[i]]$.attrs["id"] <- i
  plan
}


# ------------------------------------------------------------------------------


#' @export
plan <- function(df, exp) {
  nb <- nrow(df)
  out <- rep(exp, nb)
  param <- names(df)
  expes <- rownames(df)
  for(i in seq_len(nb)) {
    out[[i]][param] <- unname(df[i, ])
    out[[i]]["experiment"] <- expes[i]
  }
  setNames(out, expes)
}


# ------------------------------------------------------------------------------


#' @export
is.plan <- function(object) {
  "plan" %in% class(object)
}


# ------------------------------------------------------------------------------


#' @export
names.plan <- function(object) {
  names(unclass(object))
}


# ------------------------------------------------------------------------------


#' @export
`names<-.plan` <- function(x, value) {
  x <- unclass(x)
  names(x) <- value
  for(i in seq_along(value)) x[[i]]["experiment"] <- value[i]
  structure(x, class = c("plan", "experiment"))
}


# ------------------------------------------------------------------------------


#' @export
fname.plan <- function(object) {
  fname(object[1])
}


# ------------------------------------------------------------------------------


#' @export
#`fname<-.plan` <- function(x, value) {
`f<-` <- function(x, value) {
  for(i in seq_along(x)) fname(x[i]) <- value
#  if (substring(value, 1, 1) != "/") value <- paste0(getwd(), "/", value)
#  x["sourcePath"] <- value
  x
}


# ------------------------------------------------------------------------------


#' @export
`[.plan` <- function(x, value) {
  out <- unclass(x)[value]
  if(length(out) > 1) return(structure(out, class = c("plan", "experiment")))
  else {
    out <- unlist(out, recursive = FALSE)
    tmp <- sub("^.*\\.", "", names(out))
    names(out) <- sub("^attrs$", ".attrs", tmp)
    return(structure(out, class = "experiment"))
  }
}


# ------------------------------------------------------------------------------


f <- function(x, slot, value) {
  x[slot][names(value)] <- value
  x
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

getdefaultexperimentplanname <- function(experimentplan) {
  deparse(substitute(experimentplan))
}

# ------------------------------------------------------------------------------

createoutputdirectoryname <- function(experimentplan) {
  outdirectory <- createworkingdirectory()
  defaultname <- getdefaultexperimentplanname(experimentplan)
  i <- 0
  repeat {
    i <- i + 1
    outfile <- paste0(outdirectory, "/out_", defaultname, "_", i)
    if(!file.exists(outfile)) break
  }
  outfile
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

buildxmlfromparameter <- function(name, type, value) {
  paste0("<Parameter name=\"", name, "\" type=\"", type, "\" value=\"", value, "\"/>")
}

# ------------------------------------------------------------------------------

buildxmlfromoutput <- function(name, id, framerate) {
  paste0("<Output name=\"", name, "\" id=\"", id, "\" framerate=\"", framerate, "\"/>")
}

# ------------------------------------------------------------------------------

buildxmlfromsimulation <- function(sim) {
  siminput <- sim$Parameters
  simoutput <- sim$Outputs
  experimentname <- sim$.attrs["experiment"]
  finalstep <- sim$.attrs["finalStep"]
  id <- sim$.attrs["id"]
  seed <- sim$.attrs["seed"]
  sourcepath <- sim$.attrs["sourcePath"]
  result <- paste0("<Simulation id=\"", id, "\" sourcePath=\"", sourcepath,
                   "\" experiment=\"", experimentname, "\" finalStep=\"",
                   finalstep, "\" seed=\"", seed, "\">")
  i <- 1
  result <- paste0(result, "<Parameters>")
  while(i < length(siminput) + 1) {
    name <- siminput[i]$Parameter["name"]
    type <- siminput[i]$Parameter["type"]
    value <- siminput[i]$Parameter["value"]
    result <- paste0(result, buildxmlfromparameter(name, type, value))
    i <- i + 1
  }
  result <- paste0(result, "</Parameters><Outputs>")
  i <- 1
  while(i < length(simoutput) + 1) {
    name <- simoutput[i]$Output["name"]
    id <- simoutput[i]$Output["id"]
    framerate <- simoutput[i]$Output["framerate"]
    result <- paste0(result, buildxmlfromoutput(name, id, framerate))
    i <- i + 1
  }
  paste0(result, "</Outputs></Simulation>")
}

# ------------------------------------------------------------------------------

buildxmlfromexperimentplan0 <- function(experimentplan) {
  out <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><Experiment_plan>"
  i <- 1
  while(i < length(experimentplan) + 1) {
    out <- paste0(out, buildxmlfromsimulation(experimentplan[i]$Simulation))
    i <- i + 1
  }
  paste0(out, "</Experiment_plan>")
}

buildxmlfromexperimentplan <- function(experimentplan) {
  out <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><Experiment_plan>"
  i <- 1
  while(i < length(experimentplan) + 1) {
    out <- paste0(out, buildxmlfromsimulation(experimentplan[[i]]))
    i <- i + 1
  }
  paste0(out, "</Experiment_plan>")
}


# ------------------------------------------------------------------------------

###
writemodelparameterfile <- function(experimentplan, outfile) {
  if(missing(outfile))
    outfile <- createmodelparameterfilename(getdefaultexperimentplanname(experimentplan))
  xml <- buildxmlfromexperimentplan(experimentplan)
  write(xml, outfile, sep = "")
  outfile
}

# ------------------------------------------------------------------------------

#' @export
startexperimentplan <- function(experimentplan, hpc = 1, outputdirectory) {
#  cat(paste0("Running experiment plan '", experimentplan, "'..."))
  parameterxmlfile <- writemodelparameterfile(experimentplan) # creates "workgamar"
  if(missing(outputdirectory))
    outputdirectory <- createoutputdirectoryname(experimentplan)
  trycommand <- system(paste0("java -jar ", getOption("gamar.startjar"), " -Xms",
                              getOption("gamar.Xms"), " -Xmx", getOption("gamar.Xmx"),
                              " -Djava.awt.headless=true org.eclipse.core.launcher.Main ",
                              "-application msi.gama.headless.id4 -hpc ", hpc, " ",
                              parameterxmlfile, " ", outputdirectory, " >/dev/null"),
                       ignore.stdout = FALSE, ignore.stderr = TRUE)

  if(trycommand > 0) return(-1)
  return(dir(path = outputdirectory, pattern = "*.xml",  full.names = TRUE))
}


# ------------------------------------------------------------------------------

#' @export
run <- function(x, ...) UseMethod("run", x)




# ------------------------------------------------------------------------------

#' @importFrom XML xmlParse xmlToList
#' @importFrom plyr ldply
#' @importFrom dplyr select everything mutate_all %>%
#' @export
readxmlfile <- function(xmlfile) {
  # In the "ouput" folder of the local directory they are:
  # A XML file is created by gama headless called "simulation-outputs" suffixed by an Id
  # A list of Snapshots are in the snaphsot
  # parse the xml http://www.informit.com/articles/article.aspx?p=2215520
  # http://stackoverflow.com/questions/31913567/r-xml-to-dataframe-questions Super !
  xmlresult <- xmlParse(xmlfile)
  # transform into a list
  listresult <- xmlToList(xmlresult)
  df <- ldply(listresult, data.frame)
  df <- df[-nrow(df), ]
  nbcol <- ncol(df)
  names(df) <- paste(replicate(nbcol, "Col"), seq(1, nbcol), sep = "")
  # remove last column
  df <- df[-nbcol]
  # Build the vector of names of the columns that are the values of the odd columns
  # e.g.  Step 993 Susceptible   7 Infected 0 <NA>
  #
  coltodelete <- c(seq(3, nbcol - 1, by = 2), 1)
  coltoreplace <- seq(2, nbcol - 1, by = 2)
  a <- df[1, coltodelete]
  names(df)[coltoreplace] <- sapply(a, as.character)
  #Remove all columns not useful
  df <- df[-coltodelete]
  #The last column contains the step and goes first
  df %>%
    select(Step, everything()) %>%
    mutate_all(as.character) %>%
    mutate_all(as.integer)
#  df <- select(df, Step, everything())
#  return(df)
}


# ------------------------------------------------------------------------------

#' @export
run.experiment <- function(x, hpc = 1, output) {
  files <- startexperimentplan(list(x), hpc, output)
  lapply(files, readxmlfile)
}

# ------------------------------------------------------------------------------

#' @export
run.plan <- function(x, hpc = 1, output) {
  files <- startexperimentplan(x, hpc, output)
  lapply(files, readxmlfile)
}


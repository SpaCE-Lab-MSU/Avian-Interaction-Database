# config.R  read in configuration specifically for file paths, used by all scripts
# TITLE:          Avian Interaction Database Workflow Configuration
# AUTHORS:        Phoebe Zarnetske, Pat Bills, Kelly Kapsar
# COLLABORATORS:  Vincent Miele, Stephane Dray
# DATA INPUT:     file filepaths.R and/or config.yml in main folder of this repository
# DATA OUTPUT:    sets variables used to construct file paths
# PROJ7ECT:        Avian Interaction Database & Avian Meta-Network
# DATE:           July 2025
# NOTES:

# The goal of this function is to set configuration of the file paths in one
# script instead of at the beginning of every script or notebook
# to allow collaboration.
# see the readme.md for details about how to setup the file paths.

#' # DO NOT EDIT THESE VARIABLES
#' # global variables that are set and overridden by function below
#' # these are here for compability with scripts that don't use the configuration system.
#' DATA_FOLDER <- ""
#'
#' #' wrapper for config file with flex file
#' #'
#' #' config for us is mostly setting the folders to use to find files
#' #' this fn just wraps config.get in try catch and allows explicitly specifying
#' #' a config file in case you want to use different folders
#' #' alternatively, can create different sections in the default config file
#' #' for each folder location.
#' #' See the fiel config.yml.example for an example file.
#' #' if not config file is found, will use some default values
#' get_file_config <- function(config_file = NULL, config_section = 'default'){
#'
#'   try(
#'     if(! is.null(config_file)){
#'       configuration <- config::get(file = config_file, config=config_section)
#'     } else { # no config file was sent, use the defaults
#'       configuration <- config::get(config=config_section)
#'     }
#'   )
#'
#'   # make them all uppercase, which allows for any case in config file
#'   names(configuration) <- toupper(names(configuration))
#'
#'   if( 'DATA_FOLDER' %in% names(configuration)) {
#'     DATA_FOLDER <- configuration$DATA_FOLDER
#'     if(!dir.exists(DATA_FOLDER)){
#'       warning("the DATA_FOLDER in the config file can't be found")
#'     }
#'   } else {
#'     warning("the configuration file config.yml must have an entry DATA_FOLDER See README.md for how to set-up")
#'   }
#'
#'   # these hard-coded paths are set by convention for the repository.
#'   # If the repository structure changes,this code must be updated!
#'   # it's better to set the paths when loading the file list
#'   configuration$L0 <- file.path(DATA_FOLDER, 'L0')
#'   configuration$L1 <- file.path(DATA_FOLDER, 'L1')
#'   return(configuration)
#'
#' } # eof get_file_config()
#'
#'
#'
######################
##### ALTERNATE VERSION USING SIMPLE R VARIABLES INSTEAD OF YAML ETC

#' get and check file paths
#'
#' this reads the files paths set in the config file file_paths.R and checks that
#' the config file exists, that the vars are set, and that the paths set
#' exists on this computer.  The script stops execution if not
#' put this at the top of very script
#'
#' @param config_file character path to a config file, if you want to use
#'        a different file than the default (for testing for example)
#'
#' @return list with named files paths for use in all scripts
get_file_paths <- function(config_file = NULL){


  default_config_file <- here::here('R/filepaths.R')

  if(is.null(config_file)){ config_file <- default_config_file }

  if(!file.exists(config_file)) {
    configFileMissingMessage <- paste("could not find file paths file ",
                                      config_file,
                                      "which must be set to find files")
    simpleError(configFileMissingMessage)
    stop()
  }

  source(config_file)

  if(!exists("DATA_FOLDER")) {
    message = paste("the config file ",
                    config_file,
                    "must set the variable DATA_FOLDER See README.md for how to set-up")
    warning(message)
    stop()
  } else if(!dir.exists(DATA_FOLDER)){
      message = paste("the DATA_FOLDER '", DATA_FOLDER, " in the config file ", config_file, "can't be found")
      warning(message)
      stop()
  }

  # create a list of these to be able to return a single var, to allow for
  # more folders, and to share among programs
  configuration <- list()
  configuration$DATA_FOLDER <- DATA_FOLDER

  # these hard-coded paths are set by convention for the repository.
  # If the repository structure changes,this code must be updated!

  configuration$L0 <- file.path(DATA_FOLDER, 'L0')
  configuration$L1 <- file.path(DATA_FOLDER, 'L1')

  # other optional configuration
  default_checklist_folder <- file.path(configuration$L0, "species_checklists")
  if(exists("CHECKLIST_FOLDER")){
    configuration$CHECKLIST_FOLDER <- CHECKLIST_FOLDER
  } else {
    configuration$CHECKLIST_FOLDER <- CHECKLIST_FOLDER

  }



  return(configuration)

}



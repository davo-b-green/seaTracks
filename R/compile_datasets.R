# Title: Code for extracting location and dive data from associated ACCESS databases, and compiling
# into master location and dive datasets
# Compiled by David Green
# Date: 17-05-2022
# Last edited: 03-02-2023
# Querying ACCESS databases, and compiling individual-level data into consolidated data.frames of (1) location, (2) summarised dive profiles and (3) summarised ctd casts

# ----------------------------------------------------------------------------------------------------------------------------------- #
# library(curl)
# library(RODBC)
# library(tidyverse)
# library(fs)
# library(rSRDL)
# library(future.apply)
# library(data.table)
# source("./called_functions/called_functions.R")
# ----------------------------------------------------------------------------------------------------------------------------------- #

#' Binding individual location (diag) datasets
#'
#' @param acc_path directory where the SMRU Microsoft Access files can be found for querying. Default is "./access_files"
#' @param dir directory where combined data.frames can be written. Default is "./compiled_raw_datasets"
#' @param comp_file prefix for writing the combined locations data.frame to a CSV. Default is "loc_all_raw_pre-qc"
#' @param parallel logical - should the data be processed in parallel?
#'
#' @return CSV of the combined location dataset for all available campaigns
#' @export
#'
#' @examples
#'\dontrun{
#' compile_diag()
#'}
compile_diag <- function(acc_path  = "./access_files",
                         dir = "./compiled_raw_datasets",
                         comp_file = "loc_all_raw_pre-qc",
                         parallel = FALSE){
  # require(curl)
  # require(RODBC)
  # require(tidyverse)
  # require(fs)
  # require(rSRDL)
  # require(future.apply)
  # require(data.table)
  dir_create(path = dir) # Create directory for the compiled loc, dive, ctd and haulout datasets

  comp_file <- paste0("/",comp_file,".txt")

  # ----------------------------------------------------------------------------------------------------------------------------------- #
  # Combining datasets #

  ## Get list of all ACCESS files
  fl <- dir_ls(path = acc_path,
               glob = "*.mdb")

  ## Check for already processed campaigns
  if(file_exists(paste0(dir,comp_file))){
    camp_comp <- fread(paste0(dir,comp_file)) %>%
      pull(.data$campaign) %>%
      unique()
    p <- str_split(fl[1], "/")[[1]]
    p <- paste0(p[1:length(p)-1], collapse = "/")
    camp_comp <- paste0(p,"/",camp_comp,".mdb")
  }else{
    camp_comp <- NULL
  }

  # Get only campaigns that haven't yet been compiled
  fl <- subset(fl,
               !(fl %in% camp_comp))

  # ----------------------------------------------------------------------------------------------------------------------------------- #
  ## Combine location data files

  if(length(fl) == 0){par.ls <- NULL; cat("No new location data available")}else{
    # par.ls <- split(fl, cut(seq_along(fl), floor(length(fl)/5), labels = FALSE))
    par.ls <- fl
  }

  dataLoc <- combDiag(fl, parallel = parallel)

  dataLoc <- dataLoc %>%
    mutate(
      across( # Coercing everything to character prior to binding
        .cols = everything(),
        .fns = as.character
      ))

  # Read in previously processed dataset if it exists
  if(file_exists(paste0(dir,comp_file))){
    olderLoc <- read_delim(paste0(dir,comp_file)) %>%
      mutate(
        across( # Coercing everything to character prior to binding
          .cols = everything(),
          .fns = as.character
        ))
  }else{
    olderLoc <- data.frame()
  }

  # dataLoc <- bind_df_diffClass(list(olderLoc, dataLoc)) # append newly compiled data onto older dataset
  dataLoc <- bind_rows(list(olderLoc, dataLoc)) # append newly compiled data onto older dataset

  # Save dataset
  fwrite(x = dataLoc,
         file = paste0(dir,comp_file),
         sep = "\t"
  )
}

# ----------------------------------------------------------------------------------------------------------------------------------- #
## Create dive dataset

#' Binding individual dive datasets
#'
#' @param acc_path directory where the SMRU Microsoft Access files can be found for querying. Default is "./access_files"
#' @param dir directory where combined data.frames can be written. Default is "./compiled_raw_datasets"
#' @param comp_file prefix for writing the combined dive data.frame to a CSV. Default is "dive_all_raw_pre-qc"
#' @param parallel logical - should the data be processed in parallel?
#'
#' @return CSV of the combined dive dataset for all available campaigns
#' @export
#'
#' @examples
#'\dontrun{
#' compile_dive()
#'}
compile_dive <- function(acc_path  = "./access_files",
                         dir = "./compiled_raw_datasets",
                         comp_file = "dive_all_raw_pre-qc",
                         parallel = FALSE){
  # require(curl)
  # require(RODBC)
  # require(tidyverse)
  # require(fs)
  # require(rSRDL)
  # require(future.apply)
  # require(data.table)
  dir_create(path = dir) # Create directory for the compiled loc, dive, ctd and haulout datasets

  comp_file <- paste0("/",comp_file,".txt")
  ## Get list of all ACCESS files
  fl <- dir_ls(path = acc_path,
               glob = "*.mdb")

  ## Check for already processed campaigns
  if(file_exists(paste0(dir,comp_file))){
    camp_comp <- fread(paste0(dir,comp_file)) %>%
      pull(.data$campaign) %>%
      unique()
    p <- str_split(fl[1], "/")[[1]]
    p <- paste0(p[1:length(p)-1], collapse = "/")
    camp_comp <- paste0(p,"/",camp_comp,".mdb")
  }else{
    camp_comp <- NULL
  }

  # Get only campaigns that haven't yet been compiled
  fl <- subset(fl,
               !(fl %in% camp_comp))

  if(length(fl) == 0){par.ls <- NULL; cat("No new deployments available")}else{
    # par.ls <- split(fl, cut(seq_along(fl), floor(length(fl)/5), labels = FALSE))
    par.ls <- fl
  }


  dataDive <- combDive(par.ls, parallel = parallel)

  dataDive <- data.table(dataDive)
  # Read in previously processed dataset if it exists
  if(file_exists(paste0(dir,comp_file))){
    olderDive <- fread(paste0(dir,comp_file)) #%>%
    # mutate(
    #   across( # Coercing everything to character prior to binding
    #     .cols = everything(),
    #     .fns = as.character
    # ))
  }else{
    olderDive <- data.frame()
  }

  dataDive <- bind_rows(olderDive, dataDive) # append newly compiled data onto older dataset
  # dataDive <- bind_df_diffClass(list(olderDive, dataDive))

  # Save dataset
  fwrite(x = dataDive,
         file = paste0(dir,comp_file),
         sep = "\t"
  )
}
# ----------------------------------------------------------------------------------------------------------------------------------- #

## Create ctd dataset

#' Binding individual ctd datasets
#'
#' @param acc_path directory where the SMRU Microsoft Access files can be found for querying. Default is "./access_files"
#' @param dir directory where combined data.frames can be written. Default is "./compiled_raw_datasets"
#' @param comp_file prefix for writing the combined ctd data.frame to a CSV. Default is "ctd_all_raw_pre-qc"
#' @param parallel logical - should the data be processed in parallel?
#'
#' @return CSV of the combined ctd dataset for all available campaigns
#' @export
#'
#' @examples
#'\dontrun{
#' compile_ctd()
#'}
compile_ctd <- function(acc_path  = "./access_files",
                        dir = "./compiled_raw_datasets",
                        comp_file = "ctd_all_raw_pre-qc",
                        parallel = FALSE){
  # require(curl)
  # require(RODBC)
  # require(tidyverse)
  # require(fs)
  # require(rSRDL)
  # require(future.apply)
  # require(data.table)

  dir_create(path = dir) # Create directory for the compiled loc, dive, ctd and haulout datasets

  comp_file <- paste0("/",comp_file,".txt")
  ## Get list of all ACCESS files
  fl <- dir_ls(path = acc_path,
               glob = "*.mdb")

  ## Check for already processed campaigns
  if(file_exists(paste0(dir,comp_file))){
    camp_comp <- fread(paste0(dir,comp_file)) %>%
      pull(.data$campaign) %>%
      unique()
    p <- str_split(fl[1], "/")[[1]]
    p <- paste0(p[1:length(p)-1], collapse = "/")
    camp_comp <- paste0(p,"/",camp_comp,".mdb")
  }else{
    camp_comp <- NULL
  }

  # Get only campaigns that haven't yet been compiled
  fl <- subset(fl,
               !(fl %in% camp_comp))

  # if(length(fl) == 0){par.ls <- NULL; cat("No new deployments available")}else{
  #   par.ls <- split(fl, cut(seq_along(fl), floor(length(fl)/5), labels = FALSE))
  # }
  par.ls = fl

  dataCTD <- combCTD(par.ls, parallel = parallel)

  # Read in previously processed dataset if it exists
  if(file_exists(paste0(dir,comp_file))){
    olderCTD <- read_delim(paste0(dir,comp_file)) %>%
      mutate(
        across( # Coercing everything to character prior to binding
          .cols = everything(),
          .fns = as.character
        ))
  }else{
    olderCTD <- data.frame()
  }

  # dataCTD <- bind_df_diffClass(list(olderCTD, dataCTD)) # append newly compiled data onto older dataset
  dataCTD <- bind_rows(list(olderCTD, dataCTD)) # append newly compiled data onto older dataset

  # Save dataset
  fwrite(x = dataCTD,
         file = paste0(dir,comp_file),
         sep = "\t"
  )
}
# ----------------------------------------------------------------------------------------------------------------------------------- #

#' Binding individual haulout datasets
#'
#' @param acc_path directory where the SMRU Microsoft Access files can be found for querying. Default is "./access_files"
#' @param dir directory where combined data.frames can be written. Default is "./compiled_raw_datasets"
#' @param comp_file prefix for writing the combined haulout data.frame to a CSV. Default is "haul_all_raw_pre-qc"
#' @param parallel logical - should the data be processed in parallel?
#'
#' @return CSV of the combined haulout dataset for all available campaigns
#' @export
#'
#' @examples
#'\dontrun{
#' compile_haul()
#'}
## Create haulout dataset

compile_haul <- function(acc_path  = "./access_files",
                         dir = "./compiled_raw_datasets",
                         comp_file = "haulout_all_raw_pre-qc",
                         parallel = FALSE){
  # require(curl)
  # require(RODBC)
  # require(tidyverse)
  # require(fs)
  # require(rSRDL)
  # require(future.apply)
  # require(data.table)
  dir_create(path = dir) # Create directory for the compiled loc, dive, ctd and haulout datasets

  comp_file <- paste0("/",comp_file,".txt")
  ## Get list of all ACCESS files
  fl <- dir_ls(path = acc_path,
               glob = "*.mdb")
  ## Check for already processed campaigns
  if(file_exists(paste0(dir, comp_file))){
    camp_comp <- fread(paste0(dir, comp_file)) %>%
      pull(.data$campaign) %>%
      unique()
    p <- str_split(fl[1], "/")[[1]]
    p <- paste0(p[1:length(p)-1], collapse = "/")
    camp_comp <- paste0(p,"/",camp_comp,".mdb")
  }else{
    camp_comp <- NULL
  }

  # Get only campaigns that haven't yet been compiled
  fl <- subset(fl,
               !(fl %in% camp_comp))

  # if(length(fl) == 0){par.ls <- NULL; cat("No new deployments available")}else{
  #   par.ls <- split(fl, cut(seq_along(fl), floor(length(fl)/5), labels = FALSE))
  # }

  par.ls <- fl

  dataHaul <- combHaul(par.ls, parallel = parallel)

  # Read in previously processed dataset if it exists
  if(file_exists(paste0(dir, comp_file))){
    olderHaul <- read_delim(paste0(dir, comp_file)) %>%
      mutate(
        across( # Coercing everything to character prior to binding
          .cols = everything(),
          .fns = as.character
        ))
  }else{
    olderHaul <- data.frame()
  }

  # dataHaul <- bind_df_diffClass(list(olderHaul, dataHaul)) # append newly compiled data onto older dataset
  dataHaul <- bind_rows(list(olderHaul, dataHaul)) # append newly compiled data onto older dataset

  # Save dataset
  fwrite(x = dataHaul,
         file = paste0(dir, comp_file),
         sep = "\t"
  )
}
# ----------------------------------------------------------------------------------------------------------------------------------- #

## Create summary dataset

#' Binding individual summary datasets
#'
#' @param acc_path directory where the SMRU Microsoft Access files can be found for querying. Default is "./access_files"
#' @param dir directory where combined data.frames can be written. Default is "./compiled_raw_datasets"
#' @param comp_file prefix for writing the combined summary data.frame to a CSV. Default is "sum_all_raw_pre-qc"
#' @param parallel logical - should the data be processed in parallel?
#'
#' @return CSV of the combined summary dataset for all available campaigns
#' @export
#'
#' @examples
#'\dontrun{
#' compile_sum()
#'}
compile_sum <- function(acc_path  = "./access_files",
                        dir = "./compiled_raw_datasets",
                        comp_file = "summary_all_raw_pre-qc",
                        parallel = FALSE){
  # require(curl)
  # require(RODBC)
  # require(tidyverse)
  # require(fs)
  # require(rSRDL)
  # require(future.apply)
  # require(data.table)
  dir_create(path = dir) # Create directory for the compiled loc, dive, ctd and haulout datasets

  comp_file <- paste0("/",comp_file,".txt")
  ## Get list of all ACCESS files
  fl <- dir_ls(path = acc_path,
               glob = "*.mdb")

  ## Check for already processed campaigns
  if(file_exists(paste0(dir, comp_file))){
    camp_comp <- fread(paste0(dir, comp_file)) %>%
      pull(.data$campaign) %>%
      unique()
    p <- str_split(fl[1], "/")[[1]]
    p <- paste0(p[1:length(p)-1], collapse = "/")
    camp_comp <- paste0(p,"/",camp_comp,".mdb")
  }else{
    camp_comp <- NULL
  }

  # Get only campaigns that haven't yet been compiled
  fl <- subset(fl,
               !(fl %in% camp_comp))

  # if(length(fl) == 0){par.ls <- NULL; cat("No new deployments available")}else{
  #   par.ls <- split(fl, cut(seq_along(fl), floor(length(fl)/5), labels = FALSE))
  # }
  par.ls <- fl

  dataSum <- combSum(par.ls, parallel = parallel)
  # Read in previously processed dataset if it exists
  if(file_exists(paste0(dir, comp_file))){
    olderSum <- read_delim(paste0(dir, comp_file)) %>%
      mutate(
        across( # Coercing everything to character prior to binding
          .cols = everything(),
          .fns = as.character
        ))
  }else{
    olderSum <- data.frame()
  }

  # dataSum <- bind_df_diffClass(list(olderSum, dataSum)) # append newly compiled data onto older dataset
  dataSum <- bind_rows(list(olderSum, dataSum)) # append newly compiled data onto older dataset

  # Save dataset
  fwrite(x = dataSum,
         file = paste0(dir, comp_file),
         sep = "\t"
  )
}
# ----------------------------------------------------------------------------------------------------------------------------------- #

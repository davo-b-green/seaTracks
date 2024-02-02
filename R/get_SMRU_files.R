# Download tracking datasets from SMRU portal

# Author: David Green
# Date: 08/12/2023

library(tidyverse)
library(httr)
library(fs)
library(data.table)

#` Download SRDL files from the SMRU Data Portal
#`
#` @param dir directory to which data files are downloaded (default is "./downloaded_campaigns/")
#` @param campaigns either (1) a 2-column data.frame containing a column named "campaigns" with rows containing the SMRU campaign number (e.g. ct119) and a second column called "provider" with rows containing the provider (e.g. imos), else, (2) a character vector with the SMRU campaign number(s) to be downloaded. Note: in this case "provider cannot be left NULL
#` @param providers if "campaigns" input is a character vector, providers should be a character vector containing the data provider (e.g. imos)
#` @param replace logical - should files with the same name be overwritten?
#` @param compiled_fpath path to previously compiled data. If data.sets have been previously downloaded and compiled using this package, specifying the path of the compiled csv here can be used to skip downloads of these data. Default is "./compiled_raw_datasets/loc_all_raw_pre-qc.txt"
#` @param unpack logical - should the compressed file downloads be unzipped?
#` @param unpack_path directory to which unzipped files are written. Default is directory is "./access_files/"

#' Download SRDL files from the SMRU Data Portal
#'
#' @param dir directory to which data files are downloaded (default is "./downloaded_campaigns/")
#' @param campaigns either (1) a 2-column data.frame containing a column named "campaigns" with rows containing the SMRU campaign number (e.g. ct119) and a second column called "provider" with rows containing the provider (e.g. imos), else, (2) a character vector with the SMRU campaign number(s) to be downloaded. Note: in this case "provider cannot be left NULL
#' @param providers if "campaigns" input is a character vector, providers should be a character vector containing the data provider (e.g. imos). Note must be the same length as vector for "campaigns"
#' @param replace logical - should files with the same name be overwritten?
#' @param compiled_fpath path to previously compiled data. If data.sets have been previously downloaded and compiled using this package, specifying the path of the compiled csv here can be used to skip downloads of these data. Default is "./compiled_raw_datasets/loc_all_raw_pre-qc.txt"
#' @param unpack logical - should the compressed file downloads be unzipped?
#' @param unpack_path directory to which unzipped files are written. Default is directory is "./access_files/"
#'
#' @return collection of zipped or unzipped Microsoft Access files in the specified directory, and containing the requested SRDL deployment data
#' @export
#'
#' @examples
#'\dontrun{
#' campaigns <- c("ct164", "ct79", "ct91")
#' providers <- c("imos", "imos", "imos")
#'
#' get_SMRU_files(campaigns = campaigns, providers = providers)
#'}
get_SMRU_files <- function(dir = NULL,
                           campaigns,
                           providers = NULL,
                           replace = FALSE,
                           compiled_fpath = NULL,
                           unpack = TRUE,
                           unpack_path = NULL){

  if(is.null(dir)){
    dir = "./downloaded_campaigns"
  }

  # Create directory for downloaded files
  dir_create(path = dir)

  if(is.data.frame(campaigns)){
    camps = campaigns
  }else{
    if(!is.null(providers)){
      camps = data.frame(campaign = campaigns,
                         provider = providers)
    }else{stop("cannot download campaign/s without provider details")}
  }

  prov <- unique(camps$provider)

  provider_login <- lapply(prov, function(ii){
    return(
      data.frame(provider = ii,
                 user = assign(paste0("un_",ii), readline(paste0("Input ", ii, " username - leave blank if unknown - :"))),
                 pwd = assign(paste0("pw_",ii), readline(paste0("Input ", ii, " password - leave blank if unknown - :"))))
    )
  }) %>% bind_rows

  suppressMessages(camps <- camps %>%
                     left_join(provider_login)
  )


  ## Check for already processed campaigns
  compiled_fpath <- ifelse(is.null(compiled_fpath),
                           "./compiled_raw_datasets/loc_all_raw_pre-qc.txt",
                           compiled_fpath)

  if(replace){
    if(file_exists(compiled_fpath)){
      camp_comp <- fread(compiled_fpath) %>%
        pull(campaign) %>%
        unique()
    }else{
      camp_comp <- NULL
    }
  }else{
    camp_comp <- NULL
  }


  # Get only undownloaded campaigns
  `%nin%` <- Negate(`%in%`)
  camps <- filter(camps,
                  campaign %nin% camp_comp)

  # Based on campaigns information and associated login details, now download the compressed access files

  lapply(1:nrow(camps), function(ii){
    x = camps$campaign[ii]
    prov = camps$provider[ii]
    un = camps$user[ii]
    pw = camps$pwd[ii]
    cat(paste("Downloading campaign: ", x, "\n"))
    tryCatch( # Catch any errors coming from file already having been downloaded, or errors with username/password
      GET(paste0("http://www.smru.st-andrews.ac.uk/protected/",x,"/db/",x,".zip"),
          authenticate(un, pw),
          write_disk(paste0(dir,"/",x,".zip"), overwrite = TRUE),
          timeout(180)),
      error = function(e){
        message(paste("File exists or incorrect username/password - campaign:", x))
      })
  })

  if(unpack){


    ## Create directory for access files
    if(is.null(unpack_path)){
      unpack_path = "./access_files/"
    }

    dir_create(unpack_path)

    # ----------------------------------------------------------------------------------------------------------------------------------- #

    # Unzipping files #

    ## Get list of all *.zip files
    flz <- dir_ls(path = dir,
                  glob = "*.zip")

    ## Unzip files to new directory
    lapply(flz, function(x){
      unzip(zipfile = x,
            exdir = unpack_path,
            overwrite = FALSE)
    })

  }
}







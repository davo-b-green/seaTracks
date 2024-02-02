# library(tidyverse)
# library(fs)
# library(data.table)
# process_tracks <- function(in_loc = "./compiled_raw_datasets/loc_all_raw_pre-qc.txt",
#                            out_dir = "./processed_datasets/",
#                            out_loc = "loc_ssm_6h",
#                            process_what = "all",
#                            in_dive = NULL,
#                            out_dive = NULL,
#                            in_ctd = NULL,
#                            out_ctd = NULL,
#                            haulout_filter = TRUE,
#                            haulout_dat = NULL,
#                            append = TRUE,
#                            note_discards = TRUE,
#                            aniMotum_formula = NULL,
#                            add_mpm = TRUE,
#                            tstep = 6,
#                            vmax = 4,
#                            max_tgap = 4,
#                            min_day_pre = 5,
#                            min_day_post = 5,
#                            threshold_dist = 5,
#                            parallel = TRUE
# ){}

prep_diag <- function(in_loc,
                      out_dir,
                      out_loc,
                      dep_date,
                      append,
                      min.d,
                      ref_sub
){

  # Read in raw location dataset
  diag <- fread(in_loc)
  diag <- diag %>%
    filter(ref %in% ref_sub)
  diag <- diag %>%
    rename(id = ref)

  ## Check for and remove already processed individuals
  if(append){
    if(file_exists(paste0(out_dir,"/",out_loc,".csv"))){
      # Remove already processed tracks
      camp_comp <- fread(paste0(out_dir,"/",out_loc,".csv")) %>%
        pull(id) %>%
        unique()
      diag <- diag %>%
        filter(!(id %in% camp_comp))

      # remove tracks that previously failed in processing
      if(file_exists(paste0(out_dir,"/loc-tracks-removed-during-processing.csv"))){
        rem_ids <- fread(paste0(out_dir,"/loc-tracks-removed-during-processing.csv")) %>%
          pull(id) %>%
          unique()

        diag <- diag %>%
          filter(!(.data$id %in% rem_ids))
      }
    }
  }

  rr1 <-  diag %>%
    dplyr::select(.data$id, .data$d_date, .data$lq, .data$lon, .data$lat, .data$campaign) %>%
    dplyr::rename(lc=.data$lq, date = .data$d_date) %>%
    mutate(lc = case_when(lc == -9 ~ "Z", ## convert lc to aniMotum format
                          lc == -2 ~ "B",
                          lc == -1 ~ "A",
                          TRUE ~ as.character(lc) # Remaining values stay the same
    )
    )

  suppressMessages(
    if(!is.null(dep_date)){
      dep_date <- dep_date %>%
        select(.data$id, .data$dep_date)

      rr1 <- left_join(rr1, dep_date) %>%
        group_by(.data$id) %>%
        mutate(dep_date = ifelse(is.na(.data$dep_date),
                                 min(.data$date, na.rm=T),
                                 .data$dep_date)) %>%
        filter(.data$date >= .data$dep_date) %>%
        select(-.data$dep_date)
      remDat1 <- diag %>%
        select(.data$id) %>%
        distinct() %>%
        filter(!(.data$id %in% unique(rr1$id))) %>%
        mutate(reason = "Removed because there are no data after the deployment date (i.e. all transmitted data is from prior to deployment)")
    }
  )
  ############################################################

  if(!is.null(min.d)){
    ## only keep seals with "min.d" days of data
    dur <- rr1 %>%
      group_by(.data$id) %>%
      summarise(first=min(.data$date, na.rm = TRUE),
                last=max(.data$date, na.rm = TRUE)
      ) %>%
      mutate(dur = difftime(.data$last, .data$first, units = "days")
      ) %>%
      filter(.data$dur > min.d) %>%
      pull(.data$id)
  }else{dur = NULL}

  ## output dataframe of removed individuals because they had fewer than 5 days of data
  remDat <- rr1 %>%
    select(.data$id) %>%
    distinct() %>%
    filter(!(.data$id %in% dur)) %>%
    mutate(reason = paste0("< ", min.d, " days data following deployment"))

  remDat <- bind_rows(remDat1, remDat) %>% distinct()

  d1 <- rr1 %>%
    filter(.data$id %in% dur) %>%
    arrange(.data$campaign, .data$id, .data$date) %>%
    group_by(.data$id) %>%
    distinct(.data$date, .keep_all = TRUE) %>%
    dplyr::select(1:5)

  return(list(d1, remDat))
}

prep_dive <- function(in_dive,
                      out_dir,
                      out_dive,
                      dep_date,
                      append,
                      ref_sub
){

  dive <- fread(in_dive)
  dive <- dive %>%
    filter(.data$ref %in% ref_sub)
  dive <- dive %>%
    rename(id = .data$ref,
           date = .data$de_date
    )
  tdive <- dive %>%
    select(.data$id, .data$date) %>%
    na.omit()


  ## Check for and remove already processed individuals
  if(append){
    if(file_exists(paste0(out_dir,"/",out_dive,".csv"))){
      camp_comp <- fread(paste0(out_dir,"/",out_dive,".csv")) %>%
        pull(.data$id) %>%
        unique()

      dive <- dive %>%
        filter(!(.data$id %in% camp_comp))
      # diag <- diag %>%
      #   filter(!(id %in% camp_comp))
    }
  }

  suppressMessages(
    if(!is.null(dep_date)){
      dep_date <- dep_date %>%
        select(.data$id, .data$dep_date)

      tdive <- left_join(tdive, dep_date) %>%
        group_by(.data$id) %>%
        mutate(dep_date = ifelse(is.na(.data$dep_date),
                                 min(.data$date, na.rm=T),
                                 .data$dep_date)) %>%
        filter(.data$date >= .data$dep_date) %>%
        select(-.data$dep_date)

      dive <- semi_join(dive,
                        tdive)

    }
  )

  dive <- dive %>%
    arrange(.data$id, .data$date) %>%
    group_by(.data$id) %>%
    distinct(.data$date, .keep_all = TRUE)

  # d1 <- data.frame(diag) %>%
  #   filter(id %in% unique(tdive$id))

  return(list(dive))
}

prep_ctd <- function(in_ctd,
                     out_dir,
                     out_ctd,
                     dep_date,
                     append,
                     ref_sub
){
  ctd <- fread(in_ctd)

  ctd <- ctd %>%
    filter(.data$ref %in% ref_sub)

  ctd <- ctd %>%
    rename(id = .data$ref,
           date = .data$end_date
    )

  ## Check for and remove already processed individuals
  if(append){
    if(file_exists(paste0(out_dir,"/",out_ctd,".csv"))){
      camp_comp <- fread(paste0(out_dir,"/",out_ctd,".csv")) %>%
        pull(.data$id) %>%
        unique()
      ctd <- ctd %>%
        filter(!(.data$id %in% camp_comp))
    }
  }

  suppressMessages(
    if(!is.null(dep_date)){
      dep_date <- dep_date %>%
        select(.data$id, .data$dep_date)

      ctd <- left_join(ctd, dep_date) %>%
        group_by(.data$id) %>%
        mutate(dep_date = ifelse(is.na(.data$dep_date),
                                 min(.data$date, na.rm=T),
                                 .data$dep_date)) %>%
        filter(.data$date >= .data$dep_date) %>%
        select(-.data$dep_date)

    }
  )
  return(list(ctd))
}

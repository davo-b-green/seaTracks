##run aniMotum on the ARGOS data

# Last update: 30 Jan 2023

# library(tidyverse)
# # library(MASS)
# library(aniMotum)
# library(bsam)
# library(geosphere)
# library(viridis)
# library(mapdata)
# library(maps)
# library(maptools)
# library(rnaturalearth)
# library(ggspatial)
# library(sf)
# library(fs)
# library(trip)
# # library(here)
# library(data.table)
# library(lubridate)
# library(future.apply)

# source("./called_functions/called_functions.R")
# source("./called_functions/preparing_datasets_for_ssm.R")
## Function call

#' Running compiled datasets through aniMotum's ssm
#'
#' @param in_loc file path for the compiled location dataset.
#' @param out_dir destination directory for outputs.
#' @param out_loc prefix for the output location file name.
#' @param in_dive file path for the compiled dive dataset.
#' @param out_dive prefix for the output dive file name.
#' @param in_ctd file path for the compiled ctd dataset.
#' @param out_ctd prefix for the output ctd file name.
#' @param dep_date Must be a 2-column data.frame with animal id and deployment date
#' @param append logical - would you like the dataset to be appended to a previously processed dataset, or processed from scratch?
#' @param note_discards logical - would you like a running tally of ids that have been discarded over the course of processing?
#' @param add_mpm logical - should a move persistence model be fitted in addition to the ssm?
#' @param mpm_model specify the move persistence model to use. Can be either c("mpm" or "jmpm"). See \link[aniMotum]{fit_mpm} for details
#' @param min_d minimum number of days of tracking data for a deployment to be processed
#' @param tstep specify the duration of the regular timestep to be fitted during the ssm
#' @param tstep_units specifiy the timestep units; it can be one of following c("hour", "hours", "min", "mins", "sec", "secs")
#' @param vmax the maximum speed in m/s that an individual can travel. This is used for the ssm speed filter.
#' @param max_tgap what is the maximum gap (in days) between location times before it should be flagged?
#' @param parallel logical - would you like processing to be run in parallel?
#' @param chunk_prop relative size of data chunks for splitting the dataset during processing. Value must be (0,1]
#' @param return_output logical - would you like the output to be returned? If FALSE, output will be written to disk only.
#'
#' @return a collection of csv and Rdata files with the ssm estimated locations (and move persistence values) for tracks at a regularised time interval, as well as for every dive and ctd profile. Also, a file noting, with reasons, all tracks that were removed during processing. If return_output = TRUE, a list of data.frames containing processed location, dive and ctd datasets will be returned
#' @export
#'
#' @examples
#' \dontrun{
#' process_tracks(tstep = 4, parallel = FALSE)
#'
#' # If output should be returned to global environment
#'
#' track_dat <- process_tracks(tstep = 4, parallel = FALSE, return_output = TRUE)
#'
#'}
process_tracks <- function(in_loc = "./compiled_raw_datasets/loc_all_raw_pre-qc.txt",
                           out_dir = "./processed_datasets/",
                           out_loc = "loc_ssm_6h",
                           in_dive = "./compiled_raw_datasets/dive_all_raw_pre-qc.txt",
                           out_dive = "loc_ssm_dive",
                           in_ctd = "./compiled_raw_datasets/ctd_all_raw_pre-qc.txt",
                           out_ctd = "loc_ssm_ctd",
                           dep_date = NULL, # Metadata dataframe with animal id and deployment date
                           append = TRUE, # would you like the dataset to be appended to a previously processed dataset, or processed from scratch?
                           note_discards = TRUE, # would you like a running tally of ids that have been discarded over the course of processing?
                           add_mpm = TRUE, # should a move persistence model be fitted in addition to the ssm?
                           mpm_model = "mpm", # either c("mpm" or "jmpm") see aniMotum::fit_mpm for details
                           min_d = 5, # minimum number of days for a deployment to be processed
                           tstep = 6, # regular timestep for ssm to be fitted to (units = hours)
                           tstep_units = "hours", # can be one of following c("hour", "hours", "min", "mins", "sec", "secs")
                           vmax = 4, # m/s
                           max_tgap = 4, # flags a timestep gap >= "max_tgap" in data (units  = days)
                           parallel = FALSE, # should this run in parallel?
                           chunk_prop = 0.1, # chunk size relative to the full datablock
                           return_output = FALSE
){

  dir_create(path = out_dir)

  if(!is.null(dep_date)){
    dep_date <- fread(dep_date)
  }

  # Get list of animal ids and split into proportional chunks based on "chunk_prop"
  ref_ls <- fread(in_loc, select = "ref") %>%
    pull(ref) %>%
    unique() %>%
    na.omit()

  chunk_size <- floor(length(ref_ls)*chunk_prop)

  ref_ls <- split(ref_ls, ceiling(seq_along(ref_ls)/chunk_size))

  # Load and preprocess location data
  for(ii in seq_along(ref_ls)){
    n_chunk <- length(ref_ls)
    ref_sub <- ref_ls[[ii]]

    print(paste0("preparing data from chunk: [", ii,"] of [", n_chunk, "]"))

    diag <- prep_diag(in_loc, out_dir, out_loc, dep_date, append, min_d, ref_sub)

    d1 <- diag[[1]]

    if(nrow(d1) == 0){
      like_reason <- ifelse(is.na(diag[[2]]$reason[1]), "already processed", diag[[2]]$reason[1])
      print(paste0("skipping data chunk [",ii,"] as no location data available. Likely reason: ", like_reason))
      next
    }

    remDat <- diag[[2]]

    # ## Find all time gaps of 4 or more days

    if(!is.null(max_tgap)){
      # tgaps <- d1 %>%
      #   group_by(id) %>%
      #   mutate(tgap = difftime(time1 = lead(date),
      #                          time2 = date,
      #                          units = "days"
      #                         ),
      #          n.date = lead(date)) %>%
      #   filter(tgap > 4) %>%
      #   dplyr::select(c(id, date, n.date))
      tgaps <- d1 %>%
        group_by(id) %>%
        mutate(tgap = interval(date, lead(date)),
               tgap_dur = tgap/ddays(1)) %>%
        filter(tgap_dur > max_tgap) %>%
        select(id, tgap)
    }

    # Load dive data if provided
    if(!is.null(in_dive)){
      dive <- prep_dive(in_dive, out_dir, out_dive, dep_date, append, ref_sub)
      dive <- dive[[1]]
    }

    # Load ctd data if provided
    if(!is.null(in_ctd)){
      ctd <- prep_ctd(in_ctd, out_dir, out_ctd, dep_date, append, ref_sub)
      ctd <- ctd[[1]]
    }


    by_step <- paste0(tstep," ",tstep_units)

    if(!is.null(tstep) & nrow(d1) > 0){
      diag_tstep <- d1 %>%
        group_by(id) %>%
        summarise(date1 = ceiling_date(min(date, na.rm = T),
                                       unit = tstep_units),
                  date2 = floor_date(max(date, na.rm = T),
                                     unit = tstep_units),
                  date = paste(format(seq(date1, date2, by=by_step), '%Y-%m-%d %H:%M:%S'), collapse = ",")
        ) %>%
        select(id, date) %>%
        separate_rows(date, sep = ",") %>%
        mutate(date = as.POSIXct(date, tz = "UTC")) %>%
        drop_na()


    }else{
      diag_tstep <- d1 %>%
        select(id, date) %>%
        drop_na()}

    pred_times <- diag_tstep

    if(!is.null(in_dive)){
      pred_times <- pred_times %>%
        bind_rows(dive %>% select(id, date))
    }

    if(!is.null(in_ctd)){
      pred_times <- pred_times %>%
        bind_rows(ctd %>% select(id, date))
    }

    pred_times <- pred_times %>%
      group_by(id) %>%
      mutate(date = as.POSIXct(date, tz = "UTC")) %>%
      arrange(date, .by_group = TRUE) %>%
      distinct %>%
      drop_na()

    ## Step 2. Fit the SSM (and mpm if selected)

    # bring in spatial dataset for buffer
    sf_use_s2(FALSE)
    ne_buffer <- st_read("./land_buffer/land_buffer.shp") # read shapefile with high res polgons of world + 5km buffer around them

    d3 <- data.frame(d1)

    par.ls <- unique(d3$id)

    print(paste0("Starting to run SSMs for data chunk: [", ii,"] of [", n_chunk, "] | This will take a while if you have many tracks. Sit tight..."))

    if(parallel){
      plan("future::multisession")
      ts <- Sys.time()

      dat_size <- floor(length(par.ls)*chunk_prop)
      fit_all <- future_lapply(split(par.ls, ceiling(seq_along(par.ls)/dat_size)), function(z) {
        # fit_all <- future_lapply(par.ls, function(z) {
        ## time step of 24 = 1 per day
        print(paste0("processing track IDs: ", paste0(z, collapse = ", ")))
        fits <- aniMotum::fit_ssm(x = subset(d3, id %in% z),
                                  vmax=vmax,
                                  model="crw",
                                  control = ssm_control(optim=c("nlminb")),
                                  time.step = subset(pred_times, id %in% z)
        )

        fit_dat <- grab(fits, "predicted", as_sf=FALSE)

        # flag initial trip segment prior to leaving
        fit_dat <- lapply(unique(fit_dat$id), function(z) {
          sfp_sub = subset(fit_dat, id %in% z)
          sfp_sub = st_as_sf(sfp_sub,
                             coords = c("lon", "lat"),
                             crs = 4326)
          sfp_sub = st_join(
            sfp_sub,
            ne_buffer,
            join = st_within
          ) %>%
            data.frame() %>%
            mutate(home = case_when(
              type %in% "land" ~ 1,
              TRUE ~ 0
            ))
          sfp_sub = sf_to_df(sfp_sub) %>%
            dplyr::select(-c(geometry, type))
          sfp_sub$home <- as.integer(sfp_sub$home)
          return(sfp_sub)
        })

        fit_dat <- bind_rows(fit_dat)

        fit_dat <- fit_dat %>%
          group_by(id) %>%
          mutate(
            away = frollapply(home,
                              n = floor(24/tstep),
                              align = "left",

                              FUN = function(x){
                                all(x < 1)
                              }),
            notback = frollapply(rev(home),
                                 n = floor(24/tstep),
                                 align = "left",

                                 FUN = function(x){
                                   all(x < 1)
                                 }),
            notback = rev(notback),
            away = case_when(is.na(away) ~ notback,
                             TRUE ~ away),
            notback = case_when(is.na(notback) ~ away,
                                TRUE ~ notback),
            away = cumsum(away),
            notback = rev(cumsum(rev(notback)))
          ) %>%
          # filter(away > 0 & notback > 0) %>%
          # dplyr::select(id, date, lon, lat)
          dplyr::select(-c(north, home, notback, away))


        if(add_mpm){ # only adds move persistence model for regular timestep and not for individual dives and ctd profiles
          sub_mpm <- fit_dat %>%
            right_join(diag_tstep) %>%
            drop_na(date)
          # fits <- fit_mpm(fits, what = "predicted", model = mpm_model)
          fits <- fit_mpm(sub_mpm, what = "predicted", model = mpm_model)
          mpm_dat <- grab(fits, "fitted", as_sf=FALSE)
          if(nrow(mpm_dat) > 0){
            suppressMessages(
              fit_dat <- left_join(fit_dat, mpm_dat)
            )
          }
        }
        return(fit_dat)
      })
      difftime(Sys.time(),ts)
      plan(sequential)
    }else{
      fits <- aniMotum::fit_ssm(d3, vmax=vmax, model="crw",
                                control = ssm_control(optim=c("nlminb")),
                                time.step = pred_times)

      fit_dat <- grab(fits, "predicted", as_sf=FALSE)

      # flag initial trip segment prior to leaving
      fit_dat <- lapply(unique(fit_dat$id), function(z) {
        sfp_sub = subset(fit_dat, id %in% z)
        sfp_sub = st_as_sf(sfp_sub,
                           coords = c("lon", "lat"),
                           crs = 4326)
        sfp_sub = st_join(
          sfp_sub,
          ne_buffer,
          join = st_within
        ) %>%
          data.frame() %>%
          mutate(home = case_when(
            type %in% "land" ~ 1,
            TRUE ~ 0
          ))
        sfp_sub = sf_to_df(sfp_sub) %>%
          dplyr::select(-c(geometry, type))
        sfp_sub$home <- as.integer(sfp_sub$home)
        return(sfp_sub)
      })

      fit_dat <- bind_rows(fit_dat)

      fit_dat <- fit_dat %>%
        group_by(id) %>%
        mutate(
          away = frollapply(home,
                            n = floor(24/tstep),
                            align = "left",

                            FUN = function(x){
                              all(x < 1)
                            }),
          notback = frollapply(rev(home),
                               n = floor(24/tstep),
                               align = "left",

                               FUN = function(x){
                                 all(x < 1)
                               }),
          notback = rev(notback),
          away = case_when(is.na(away) ~ notback,
                           TRUE ~ away),
          notback = case_when(is.na(notback) ~ away,
                              TRUE ~ notback),
          away = cumsum(away),
          notback = rev(cumsum(rev(notback)))
        ) %>%
        # filter(away > 0 & notback > 0) %>%
        # dplyr::select(id, date, lon, lat)
        dplyr::select(-c(north, home, notback, away))

      if(add_mpm){
        sub_mpm <- fit_dat %>%
          right_join(diag_tstep) %>%
          drop_na(date)
        # fits <- fit_mpm(fits, what = "predicted", model = mpm_model)
        fits <- fit_mpm(sub_mpm, what = "predicted", model = mpm_model)
        mpm_dat <- grab(fits, "fitted", as_sf=FALSE)
        suppressMessages(
          fit_dat <- left_join(fit_dat, mpm_dat)
        )
      }

      fit_all <- fit_dat
    }

    if(parallel){
      fit_all <- bind_df_diffClass(fit_all)
    }

    if(note_discards){
      remDat2 <- d1 %>%
        select(id) %>%
        distinct() %>%
        filter(!(id %in% unique(fit_all$id))) %>%
        mutate(reason = "track not successfully estimated during ssm or mpm fitting (aniMotum)")

      remDat <- bind_df_diffClass(list(remDat, remDat2)) %>% distinct()
    }
    # Flag any tracks with gaps larger than the inputted tgap
    suppressMessages(
      if(nrow(tgaps) > 0){
        fit_all_sub <- fit_all %>%
          filter(id %in% unique(tgaps$id)) %>%
          group_split(id) %>%
          lapply(., function(x){
            sub.gap = filter(tgaps, id %in% x$id[1])
            x = x %>%
              rowwise() %>%
              mutate(time_gap_ok = ifelse(any(date %within% sub.gap$tgap), "no", "yes"))
          }) %>%
          bind_df_diffClass

        fit_all <- fit_all %>%
          left_join(fit_all_sub) %>%
          mutate(time_gap_ok = ifelse(is.na(time_gap_ok), "yes", time_gap_ok)) %>%
          group_by(id) %>%
          arrange(date, .by_group = TRUE)
      }else{
        fit_all <- fit_all %>%
          mutate(time_gap_ok = "yes") %>%
          group_by(id) %>%
          arrange(date, .by_group = TRUE)
      }
    )
    # Separate back into diag, dive and ctd components
    print(paste0("Saving data outputs from data chunk [", ii, "] ..."))
    # Diag
    assign("fit_all_diag", suppressMessages(semi_join(fit_all, diag_tstep)))
    assign("fit_new_diag", fit_all_diag)

    # Dive
    if(!is.null(in_dive)){
      assign("fit_all_dive", suppressMessages(right_join(fit_all, select(dive, -c(lon, lat)) #%>% select(id, date)
      ) %>%
        select(-c(logit_g, logit_g.se, g))
      ))
      assign("fit_new_dive", fit_all_dive)
    }

    # CTD
    if(!is.null(in_ctd)){
      assign("fit_all_ctd", suppressMessages(right_join(fit_all, select(ctd, -c(lon, lat)) #%>% select(id, date)
      ) %>%
        select(-c(logit_g, logit_g.se, g))
      ))
      assign("fit_new_ctd", fit_all_ctd)
    }



    ##save the chunk outputs (and removed tracks tally if this was selected) as temporary csv files
    if(!is.null(out_loc)){
      # save(fit_all_diag,
      #      file = paste0(out_dir, "/", out_loc, "_temp_",ii,".Rdata"))
      fwrite(fit_all_diag,
             file = paste0(out_dir, "/", out_loc, "_temp_",ii,".csv"))
    }

    if(!is.null(out_dive)){
      # save(fit_all_dive,
      #      file = paste0(out_dir, "/", out_dive, "_temp_",ii,".Rdata"))
      fwrite(fit_all_dive,
             file = paste0(out_dir, "/", out_dive, "_temp_",ii,".csv"))
    }

    if(!is.null(out_ctd)){
      # save(fit_all_ctd,
      #      file = paste0(out_dir, "/", out_ctd, "_temp_",ii,".Rdata"))
      fwrite(fit_all_ctd,
             file = paste0(out_dir, "/", out_ctd, "_temp_",ii,".csv"))
    }
    if(note_discards){
      fwrite(remDat,
             file = paste0(out_dir, "/tracks-removed-during-processing_temp_",ii,".csv"))
    }
  }



  ## Create a list of all temporary file prefixes so they can be read in and joined
  if(!is.null(out_loc)){
    diag_fn_ls <- sapply(1:length(ref_ls), function(ii){
      return(paste0(out_dir, "/", out_loc, "_temp_",ii,".csv"))
    })
    diag_fn_ls <- diag_fn_ls[file_exists(diag_fn_ls)]
  }
  if(!is.null(out_dive)){
    dive_fn_ls <- sapply(1:length(ref_ls), function(ii){
      return(paste0(out_dir, "/", out_dive, "_temp_",ii,".csv"))
    })
    dive_fn_ls <- dive_fn_ls[file_exists(dive_fn_ls)]
  }

  if(!is.null(out_ctd)){
    ctd_fn_ls <- sapply(1:length(ref_ls), function(ii){
      return(paste0(out_dir, "/", out_ctd, "_temp_",ii,".csv"))
    })
    ctd_fn_ls <- ctd_fn_ls[file_exists(ctd_fn_ls)]
  }
  if(note_discards){
    remDat_fn_ls <- sapply(1:length(ref_ls), function(ii){
      return(paste0(out_dir, "/tracks-removed-during-processing_temp_",ii,".csv"))
    })
    remDat_fn_ls <- remDat_fn_ls[file_exists(remDat_fn_ls)]
  }


  ## Read in and bind all temporary csv files
  if(!is.null(diag_fn_ls)){
    fit_all_diag <- lapply(diag_fn_ls, fread)
    row_check <- sapply(fit_all_diag, nrow)
    index_emptyDF <- which(row_check == 0)
    repDF <- fit_all_diag[which(row_check != 0)[1]][[1]][FALSE, ]
    for(x in index_emptyDF){
      fit_all_diag[[x]] <- repDF
    }
    fit_all_diag <- bind_df_diffClass(fit_all_diag)
  }

  if(nrow(fit_all_diag)==0){
    cat("\n # ~~~~~~~~~~~~~~ no new data to add ~~~~~~~~~~~~~~ # \n \n")
  }else{
    print("data chunk outputs now combined")



    if(!is.null(dive_fn_ls)){
      fit_all_dive <- lapply(dive_fn_ls, fread)
      row_check <- sapply(fit_all_dive, nrow)
      index_emptyDF <- which(row_check == 0)
      repDF <- fit_all_dive[which(row_check != 0)[1]][[1]][FALSE, ]
      for(x in index_emptyDF){
        fit_all_dive[[x]] <- repDF
      }
      fit_all_dive <- bind_df_diffClass(fit_all_dive)
    }

    if(!is.null(ctd_fn_ls)){
      fit_all_ctd <- lapply(ctd_fn_ls, fread)
      row_check <- sapply(fit_all_ctd, nrow)
      index_emptyDF <- which(row_check == 0)
      repDF <- fit_all_ctd[which(row_check != 0)[1]][[1]][FALSE, ]
      for(x in index_emptyDF){
        fit_all_ctd[[x]] <- repDF
      }
      fit_all_ctd <- bind_df_diffClass(fit_all_ctd)
    }

    if(!is.null(remDat_fn_ls)){
      remDat <- lapply(remDat_fn_ls, function(x){do.call(fread, args = list(input = x,colClasses = rep("character", 2)))})
      remDat <- bind_df_diffClass(remDat)
    }

    if(append){
      print("combining new output with previously processed data")
    }
    ## Append fits to existing processed dataset (if it exists)
    if(!is.null(out_loc) & nrow(fit_all_diag) > 0){
      if(file_exists(paste0(out_dir, "/", out_loc, ".Rdata")) & append){
        fit_new_diag <- fit_all_diag
        fit_all_old <- load(file= paste0(out_dir, "/", out_loc, ".Rdata"))
        assign("fit_all_old",
               get(fit_all_old)
        )

        med_tstep_old <- fit_all_old %>%
          group_by(id) %>%
          select(date) %>%
          mutate(tdif = date - lag(date)) %>%
          summarise(tdif = median(tdif, na.rm = TRUE)) %>%
          pull(tdif) %>%
          mean(na.rm = TRUE)

        med_tstep_new <- diag_tstep %>%
          group_by(id) %>%
          select(date) %>%
          mutate(tdif = date - lag(date)) %>%
          summarise(tdif = median(tdif, na.rm= TRUE)) %>%
          pull(tdif) %>%
          mean(na.rm = TRUE)

        message(paste0("imported dataset has mean median timestep of: ", med_tstep_old, " ", units(med_tstep_old), " | current dataset has mean median timestep of: ", med_tstep_new, " ", units(med_tstep_new)))

        assign("fit_all_diag",
               bind_df_diffClass(list(fit_all_old,
                                      fit_new_diag)
               ) %>%
                 distinct())
      }
    }

    if(!is.null(out_dive) & nrow(fit_all_dive) > 0){
      if(file_exists(paste0(out_dir, "/", out_dive, ".Rdata")) & append){
        fit_new_dive <- fit_all_dive
        fit_dive_old <- load(file= paste0(out_dir, "/", out_loc, ".Rdata"))
        assign("fit_dive_old",
               get(fit_dive_old))

        fit_all_dive <- bind_df_diffClass(list(fit_dive_old,
                                               fit_new_dive)) %>%
          distinct()
      }
    }

    if(!is.null(out_ctd) & nrow(fit_all_ctd) > 0){
      if(file_exists(paste0(out_dir, "/", out_ctd, ".Rdata")) & append){
        fit_new_ctd <- fit_all_ctd
        fit_ctd_old <- load(file= paste0(out_dir, "/", out_ctd, ".Rdata"))
        assign("fit_ctd_old",
               get(fit_ctd_old))
        fit_all_ctd <- bind_df_diffClass(list(fit_ctd_old,
                                              fit_new_ctd)) %>%
          distinct()
      }
    }

    if(note_discards){
      if(file_exists(paste0(out_dir, "/tracks-removed-during-processing.csv")) & append){
        remDat_old <- fread(paste0(out_dir, "/tracks-removed-during-processing.csv"),
                            colClasses = rep("character", 2))

        remDat <- bind_df_diffClass(list(remDat_old,
                                         remDat)) %>%
          distinct()
      }
    }

    ## Now delete all temporary files
    if(!is.null(diag_fn_ls)){
      file_delete(diag_fn_ls)
    }

    if(!is.null(dive_fn_ls)){
      file_delete(dive_fn_ls)
    }

    if(!is.null(ctd_fn_ls)){
      file_delete(ctd_fn_ls)
    }

    if(!is.null(remDat_fn_ls)){
      file_delete(remDat_fn_ls)
    }

    print("saving all outputs...")

    ## Save the full processed dataframes as .Rdata and .csv
    if(!is.null(out_loc) & nrow(fit_all_diag) > 0){
      # print("yes - I am running this step (combining diag)")
      save(fit_all_diag,
           file = paste0(out_dir, "/", out_loc, ".Rdata"))
      fwrite(fit_all_diag,
             file = paste0(out_dir, "/", out_loc, ".csv"))
    }

    if(!is.null(out_dive) & nrow(fit_all_dive) > 0){
      # print("yes - I am running this step (combining dive)")
      save(fit_all_dive,
           file = paste0(out_dir, "/", out_dive, ".Rdata"))
      fwrite(fit_all_dive,
             file = paste0(out_dir, "/", out_dive, ".csv"))
    }

    if(!is.null(out_ctd) & nrow(fit_all_ctd) > 0){
      # print("yes - I am running this step (combining ctd)")
      save(fit_all_ctd,
           file = paste0(out_dir, "/", out_ctd, ".Rdata"))
      fwrite(fit_all_ctd,
             file = paste0(out_dir, "/", out_ctd, ".csv"))
    }

    if(note_discards){
      fwrite(remDat,
             file = paste0(out_dir, "/tracks-removed-during-processing.csv"))
    }

    # If outputs to be assigned to object
    if(return_output){
      ssm_out <- list(fit_all_diag = fit_all_diag,
                      fit_all_dive = ifelse(!is.null(in_dive), fit_all_dive, NULL),
                      fit_all_ctd = ifelse(!is.null(in_ctd), fit_all_ctd, NULL))

      return(ssm_out)
    }
  }
}

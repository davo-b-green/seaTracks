### Calculate foraging metrics for dive data

## Author: David Green
## Date: 28-06-2022
#
# library(tidyverse)
# library(data.table)
# library(nlme)
# library(purrr)
# library(future.apply)

# Define function
#' Computing elephant seal dive effort metrics
#'
#' @param out_dir destination directory for outputs.
#' @param in_dive file path for the location processed dive dataset.
#' @param out_dive prefix for the output dive metrics file name.
#' @param dep_min threshold for minimum dive depth (metres)
#' @param dep_max threshold for maximum dive depth (metres)
#' @param dur_min threshold for minimum dive duration (seconds)
#' @param dur_max threshold for maximum dive duration (seconds)
#' @param surf_max threshold for maximum surface interval (seconds)
#' @param append logical. Would you like this dive metrics dataset to be appended to a previously processed one?
#' @param chunk_prop relative size of data chunks for splitting the dataset during processing. Value must be (0,1]
#'
#' @return a csv file with dive effort metrics computed for each dive
#' @export
#'
#' @examples
#' \dontrun{
#'
#'compute_dive_metrics()
#'
#'}
#'
compute_dive_metrics <- function(out_dir = "./processed_datasets/",
                                 in_dive = "./processed_datasets/loc_ssm_dive.csv",
                                 out_dive = "dive_metrics",
                                 dep_min = 15,
                                 dep_max = 1200,
                                 dur_min = 300,
                                 dur_max = 2800,
                                 surf_max = 300,
                                 append = TRUE, # would you like the dataset to be appended to a previously processed dataset, or processed from scratch?
                                 chunk_prop = 0.1 # chunk size relative to the full datablock
                                 ){
    dir_create(path = out_dir)

    ## Bring in dive dataset
    dive <- fread(in_dive)

    ## Creating general dive metrics
    dive.met <- dive %>%
      select(id, date, depth_str, propn_str, dive_dur, max_dep, surf_dur, d_mask) %>%
      filter(d_mask == 0) %>% # Filter using Biuw qc - removing dives with vertical rates > 5m/s
      filter(max_dep < dep_max) %>% ## Remove dives following Cox et al. as laid out in Allegue et al. 2023
      filter(max_dep > dep_min) %>%
      filter(dive_dur < dur_max) %>%
      filter(dive_dur > dur_min) %>%
      filter(surf_dur < surf_max)

    rm(dive)

    ## Already processed
    if(append & file_exists(paste0("/",out_dir,"/",out_dive,".csv"))){

      dive.comp <- fread(paste0("/",out_dir,"/",out_dive,".csv")) %>%
        select(id, date)

      dive.met <- dive.met[!dive.comp, on = .(id, date)] #anti join - remove rows that have already been processed
      rm(dive.comp)
      }

    ##
    gc()

    dat_rows <- 1:nrow(dive.met)
    chunk_size <- floor(length(dat_rows)*chunk_prop)
    dat_rows <- split(dat_rows, ceiling(seq_along(dat_rows)/chunk_size))


    for(jj in 1:length(dat_rows)){
      sub.l <- dat_rows[[jj]]
      dive.met2 <- list()
      t1 = Sys.time()
      # print(jj)
      cat("processing ", length(sub.l), " rows in chunk <", jj, "> of <", length(dat_rows),"> chunks")
      for(ii in 1:length(sub.l)){
        dive_row <- dive.met[sub.l[ii],] #%>%

        depth_str = as.numeric(unlist(strsplit(dive_row$depth_str, ",")))
        propn_str = as.numeric(unlist(strsplit(dive_row$propn_str, ",")))
        time_str = dive_row$dive_dur*(propn_str/100)
        dep_diff_str = c(0, depth_str,0)
        time_diff_str = c(0, time_str,dive_row$dive_dur)
        desc_rate = abs(dep_diff_str[2] - dep_diff_str[1])/abs(time_diff_str[2] - time_diff_str[1])
        asc_rate = abs(dep_diff_str[6] - dep_diff_str[5])/abs(time_diff_str[6] - time_diff_str[5])
        speed_str = na.omit(abs(dep_diff_str-lead(dep_diff_str))/abs(time_diff_str - lead(time_diff_str)))
        hunt = sum(na.omit(abs(time_diff_str - lead(time_diff_str)))[(speed_str)<0.4]) ## calculate hunting time
        bot_time = time_diff_str[5] - time_diff_str[2] ## Calculate bottom time
        max_v_speed = max(speed_str)

        dive_row <- data.frame(id = dive_row$id,
                               date = dive_row$date,
                               depth_str = I(list(depth_str)),
                               propn_str = I(list(propn_str)),
                               dive_dur = dive_row$dive_dur,
                               max_dep = dive_row$max_dep,
                               surf_dur = dive_row$surf_dur,
                               time_str = I(list(time_str)),
                               dep_diff_str = I(list(dep_diff_str)),
                               time_diff_str = I(list(time_diff_str)),
                               desc_rate,
                               asc_rate,
                               speed_str = I(list(speed_str)),
                               hunt,
                               bot_time,
                               max_v_speed,
                               time_ordered = all(time_diff_str == sort(time_diff_str))
        )
        dive.met2[[ii]] <- dive_row
      }
      dive.met2 <- rbindlist(dive.met2)
      fwrite(dive.met2,
             file = paste0("processed_datasets/dive_metrics-halfway-",jj,".csv"))
      difftime(Sys.time(), t1)
      gc()
    }

    ### Merge all small outputed datasets into single dataset

    met.list <- fs::dir_ls("processed_datasets/", glob = "*dive_metrics-halfway*.csv")
    dive_bind <- lapply(met.list, fread)
    dive_bind <- rbindlist(dive_bind)
    dive_bind <- unique(dive_bind)

    dive.met <- dive_bind
    rm(dive_bind)

    dive.met <- dive.met %>% # remove dives where time string isn't in order
      rowwise() %>%
      filter(time_ordered) %>%
      filter(max_v_speed < 4) %>% ## Vertical speed filter (4m.s-1) following Cox et al. as laid out in Allegue et al. 2023
      filter(max(table(time_str)) == 1) %>% # Remove any dives with duplicate time records
      filter(!any(is.na(depth_str))) %>% # Remove any dives with missing depth values
       select(-c(depth_str, time_str, propn_str, time_ordered)) # remove now unnecessary columns

    ## Calculating behavioural residuals

    ## Bring in dive dataset again
    dive <- fread(in_dive)

    ## Calculating dive residual
    # ensure data order is id,time,dur,depth
    dr.dat <- dive.met %>%
      select(id, date, dive_dur, max_dep)

    dr.fits <- dive.residual(dr.dat)

    dr.fit.dat <- dr.fits[[1]] %>%
      select(id, time, res.pears) %>%
      rename(date = time,
             dr = res.pears)

    ## Calculating surface residual
    # input divedat: incl columns named dive_dur, surf_dur, id

    min.SI <- 30 # minimum surface interval
    adl <- divedat %>%
      filter(dive_dur > 0) %>%
      filter(surf_dur > min.SI) %>%
      group_by(id, dive_dur) %>%
      summarise(surf_dur = min(surf_dur, na.rm = TRUE)) %>%
      ungroup() %>%
      rename(x = dive_dur,
             y = surf_dur)

    adl.fit <- lme(log(y) ~ x, # fit model
                   data=adl,
                   random=~1+x|id,
                   na.action=na.exclude)

    divedat <- divedat %>%
      mutate(min.sfc.dur = exp(predict(adl.fit,
                                       newdata = data.frame(x = .$dive_dur,
                                                            y = .$surf_dur,
                                                            id = .$id)))) %>%
      mutate(min.sfc.dur = ifelse(dive_dur == 0 | surf_dur <= min.SI,
                                  NA,
                                  min.sfc.dur)
      ) %>%
      mutate(sfc.res = surf_dur - min.sfc.dur,
             rel.sfc.res = sfc.res/min.sfc.dur,
             rel.sfc.res = rel.sfc.res + 1,
             log.sfc.res = log(rel.sfc.res)
      )

    # may adjust few outlier values
    qq <- quantile(divedat$rel.sfc.res,
                   c(0.0005,0.995),
                   na.rm=TRUE)

    divedat$log.sfc.res[divedat$rel.sfc.res < qq[1]] <- log(qq[1])
    divedat$log.sfc.res[divedat$rel.sfc.res > qq[2]] <- log(qq[2])

    divedat <- divedat %>%
      select(log.sfc.res, surf_dur) %>%
      rename(sr = log.sfc.res)

    # combining dive and surface resid data
    fit.dat <- bind_cols(dr.fit.dat, divedat)
    ## Bringing together general metrics and behavioural residuals

    dive.met <- left_join(dive.met, fit.dat) %>%
      distinct

    # Reattach location information
    ## Bring in dive dataset
    dive <- fread(in_dive, select = c("id", "date", "lon", "lat"))
    dive.met <- left_join(dive.met, dive) # Join all together again

    # If appending to previously processed dataset
    if(append & file_exists(paste0("/",out_dir,"/",out_dive,".csv"))){

      dive.comp <- fread(paste0("/",out_dir,"/",out_dive,".csv"))

      dive.met <- rbindlist(dive.comp, dive.met) %>%
        distinct
    }

    fwrite(dive.met, file = paste0("/",out_dir,"/",out_dive,".csv"))
    file_delete(met.list)

}

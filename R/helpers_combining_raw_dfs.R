# library(curl)
# library(RODBC)
# library(tidyverse)
# library(fs)
# library(rSRDL)
# # library(here) # sets root directory
# library(future.apply)
# library(data.table)

### Functions for combining diag files

openDiag <- function(x){ # Function opens and formats a given campaign's location dataset
  # library(curl)
  # library(RODBC)
  # library(tidyverse)
  # library(fs)
  # library(rSRDL)
  # # library(here) # sets root directory
  # library(future.apply)
  # library(data.table)
  fl_split = str_split(x, pattern = "/")[[1]]
  theDB = fl_split[length(fl_split)] %>%
    str_remove(pattern = ".mdb")
  thePath = paste0(fl_split[1:(length(fl_split)-1)],
                   collapse = "/")
  cat("Processing locations: ", theDB, "\n")
  con = odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",x))
  data = sqlFetch(con,
                  "diag",
                  as.is = TRUE
  )
  data = select(data, -which(as.character(sapply(data, class)) %in% "ODBC_binary")) %>%  # remove unrecognized ODBC_binary variables
    mutate(
      across( # Correcting decimal degrees that have comma separator
        where(~ is.character(.x) &&
                isTRUE(
                  max(str_count(.x, ","), na.rm = TRUE) == 1 &&
                    any(str_count(.x, ",")) == 1
                )
        ),
        ~ as.numeric(str_replace(.x, ",", "."))
      ),
      across( # Coercing everything to character prior to binding
        .cols = everything(),
        .fns = as.character
      )
    ) %>%
    rename_with(.cols = everything(), .fn = tolower) # Change all colnames to lowercase

  odbcClose(con) # close ACCESS db connection
  return(data)

}

combDiag <- function(x, parallel){ # Function feeds in a list of campaign access files, opens them with openDiag, and outputs a compiled dataset
  if(parallel){
    plan("future::multisession")
    datDiag = future_lapply(x, function(z){
      openDiag(z)
    }, future.seed = TRUE) %>%
      bind_rows() %>%
      rowwise() %>%
      mutate(campaign = str_split(ref, pattern = "[\\_-]+")[[1]][1],
             d_date = as.POSIXct(d_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    plan(sequential)
    return(datDiag)
  }else{
    datDiag = lapply(x, function(z){
      openDiag(z)
    }) %>%
      bind_rows() %>%
      rowwise() %>%
      mutate(campaign = str_split(ref, pattern = "[\\_-]+")[[1]][1],
             d_date = as.POSIXct(d_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    return(datDiag)
  }
}

### Functions for combining dive files

openDive <- function(x){
  fl_split = str_split(x, pattern = "/")[[1]]
  theDB = fl_split[length(fl_split)] %>%
    str_remove(pattern = ".mdb")
  thePath = paste0(fl_split[1:(length(fl_split)-1)],
                   collapse = "/")
  cat("Processing dives: ", theDB, "\n")

  data = tryCatch( # Catch any errors coming from there being no dive data
    get.SRDLdb(theDB = theDB,
               theTable = "dive",
               theFields = "All",
               theRef = "All",
               thePath = thePath
    ),
    error = function(e){
      message(paste("Unable to find dive data for", str_split(x, "/")[[1]][length(str_split(x, "/")[[1]])]))
    })

  if(is.null(data)){
    # odbcClose(con) # close ACCESS db connection
    return(data.frame(REF = str_split(x, "/")[[1]][length(str_split(x, "/")[[1]])]))
  }else{
    data = data %>%
      qcDive(flag.only = TRUE,
             max.v.speed = 5) %>%
      mutate(
        across( # Coercing everything to character prior to binding
          .cols = everything(),
          .fns = as.character
        )
      ) %>%
      rename_with(~ str_replace_all(.x, pattern = "\\.", replacement = "_")) %>%
      rename_with(.cols = everything(), .fn = tolower) # Change all colnames to lowercase

    return(data)
  }
}

combDive <- function(x, parallel){ # Function feeds in a list of campaign access files, opens them with openDive, and outputs a compiled dataset
  if(parallel){
    plan("future::multisession")
    datDive = future_lapply(x, function(z){
      openDive(z)
    }, future.seed = TRUE) %>%
      bind_rows() %>%
      rowwise() %>%
      mutate(campaign = str_split(ref, pattern = "[\\_-]+")[[1]][1],
             de_date = as.POSIXct(de_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    plan(sequential)
    return(datDive)
  }else{
    datDive = lapply(x, function(z){
      openDive(z)
    }) %>%
      bind_rows() %>%
      rowwise() %>%
      mutate(campaign = str_split(ref, pattern = "[\\_-]+")[[1]][1],
             de_date = as.POSIXct(de_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    return(datDive)
  }
}

### Functions for combining CTD files

openCTD <- function(x){
  fl_split = str_split(x, pattern = "/")[[1]]
  theDB = fl_split[length(fl_split)] %>%
    str_remove(pattern = ".mdb")
  thePath = paste0(fl_split[1:(length(fl_split)-1)],
                   collapse = "/")
  cat("Processing ctd profiles: ", theDB, "\n")

  data = tryCatch( # Catch any errors coming from there being no dive data
    get.SRDLdb(theDB = theDB,
               theTable = "ctd",
               theFields = "All",
               theRef = "All",
               thePath = thePath
    ),
    error = function(e){
      message(paste("Unable to find ctd data for", str_split(x, "/")[[1]][length(str_split(x, "/")[[1]])]))
    })

  if(is.null(data)){
    # odbcClose(con) # close ACCESS db connection
    return(data.frame(ref = str_split(x, "/")[[1]][length(str_split(x, "/")[[1]])])) # testing change REF to ref
  }else{
    data = data %>%
      mutate(
        across( # Coercing everything to character prior to binding
          .cols = everything(),
          .fns = as.character
        )
      ) %>%
      rename_with(~ str_replace_all(.x, pattern = "\\.", replacement = "_")) %>%
      rename_with(.cols = everything(), .fn = tolower) # Change all colnames to lowercase

    return(data)

  }
}

combCTD <- function(x, parallel){ # Function feeds in a list of campaign access files, opens them with openCTD, and outputs a compiled dataset
  if(parallel){
    plan("future::multisession")
    datCTD = future_lapply(x, function(z){
      openCTD(z)
    }, future.seed = TRUE) %>%
      bind_rows() %>%
      rowwise() %>%
      mutate(campaign = str_split(ref, pattern = "[\\_-]+")[[1]][1],
             end_date = as.POSIXct(end_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    plan(sequential)
    return(datCTD)
  }else{
    datCTD = lapply(x, function(z){
      openCTD(z)
    }) %>%
      bind_rows() %>%
      rowwise() %>%
      mutate(campaign = str_split(ref, pattern = "[\\_-]+")[[1]][1],
             end_date = as.POSIXct(end_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    return(datCTD)
  }
}

### Functions for combining Haulout files

openHaul <- function(x){
  fl_split = str_split(x, pattern = "/")[[1]]
  theDB = fl_split[length(fl_split)] %>%
    str_remove(pattern = ".mdb")
  thePath = paste0(fl_split[1:(length(fl_split)-1)],
                   collapse = "/")
  cat("Processing haulouts: ", theDB, "\n")

  data = tryCatch( # Catch any errors coming from there being no dive data
    get.SRDLdb(theDB = theDB,
               theTable = "haulout",
               theFields = "All",
               theRef = "All",
               thePath = thePath
    ),
    error = function(e){
      message(paste("Unable to find haulout data for", str_split(x, "/")[[1]][length(str_split(x, "/")[[1]])]))
    })

  if(is.null(data)){
    # odbcClose(con) # close ACCESS db connection
    return(data.frame(REF = str_split(x, "/")[[1]][length(str_split(x, "/")[[1]])]))
  }else{
    data = data %>%
      mutate(
        across( # Coercing everything to character prior to binding
          .cols = everything(),
          .fns = as.character
        )
      ) %>%
      rename_with(~ str_replace_all(.x, pattern = "\\.", replacement = "_")) %>%
      rename_with(.cols = everything(), .fn = tolower) # Change all colnames to lowercase

    return(data)

  }
}

combHaul <- function(x, parallel){ # Function feeds in a list of campaign access files, opens them with openHaul, and outputs a compiled dataset
  if(parallel){
    plan("future::multisession")
    datHaul = future_lapply(x, function(z){
      openHaul(z)
    }, future.seed = TRUE) %>%
      bind_rows() %>%
      rowwise() %>%
      mutate(campaign = str_split(ref, pattern = "[\\_-]+")[[1]][1],
             s_date = as.POSIXct(s_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
             e_date = as.POSIXct(e_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    plan(sequential)
    return(datHaul)
  }else{
    datHaul = lapply(x, function(z){
      openHaul(z)
    }) %>%
      bind_rows() %>%
      rowwise() %>%
      mutate(campaign = str_split(ref, pattern = "[\\_-]+")[[1]][1],
             s_date = as.POSIXct(s_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
             e_date = as.POSIXct(e_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    return(datHaul)
  }
}

### Functions for compiling Summary files

openSum <- function(x){
  fl_split = str_split(x, pattern = "/")[[1]]
  theDB = fl_split[length(fl_split)] %>%
    str_remove(pattern = ".mdb")
  thePath = paste0(fl_split[1:(length(fl_split)-1)],
                   collapse = "/")
  cat("Processing summary data: ", theDB, "\n")

  data = tryCatch( # Catch any errors coming from there being no dive data
    get.SRDLdb(theDB = theDB,
               theTable = "summary",
               theFields = "All",
               theRef = "All",
               thePath = thePath
    ),
    error = function(e){
      message(paste("Unable to find summary data for", str_split(x, "/")[[1]][length(str_split(x, "/")[[1]])]))
    })

  if(is.null(data)){
    # odbcClose(con) # close ACCESS db connection
    return(data.frame(REF = str_split(x, "/")[[1]][length(str_split(x, "/")[[1]])]))
  }else{
    data = data %>%
      mutate(
        across( # Coercing everything to character prior to binding
          .cols = everything(),
          .fns = as.character
        )
      ) %>%
      rename_with(~ str_replace_all(.x, pattern = "\\.", replacement = "_")) %>%
      rename_with(.cols = everything(), .fn = tolower) # Change all colnames to lowercase

    return(data)
  }
}

combSum <- function(x, parallel){ # Function feeds in a list of campaign access files, opens them with openSum, and outputs a compiled dataset
  if(parallel){
    plan("future::multisession")
    datSum = future_lapply(x, function(z){
      openSum(z)
    }, future.seed = TRUE) %>%
      bind_rows() %>%
      rowwise() %>%
      mutate(campaign = str_split(ref, pattern = "[\\_-]+")[[1]][1],
             s_date = as.POSIXct(s_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
             e_date = as.POSIXct(e_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    plan(sequential)
    return(datSum)
  }else{
    datSum = lapply(x, function(z){
      openSum(z)
    }) %>%
      bind_rows() %>%
      rowwise() %>%
      mutate(campaign = str_split(ref, pattern = "[\\_-]+")[[1]][1],
             s_date = as.POSIXct(s_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
             e_date = as.POSIXct(e_date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    return(datSum)
  }
}

####

# Function to bind data.frames when matching columns are of different classes
# bind_df_diffClass <- function(df1, df2){
#   col_classesdf1 <- lapply(df1, class)
#   col_classesdf2 <- lapply(df2, class)
#   # overlap <- names(df1)[names(df1) %in% names(df2)]
#   # col_classesdf1 <- col_classesdf1[overlap]
#   # col_classesdf2 <- col_classesdf2[overlap]
#   # classMismatch <- col_classesdf1[!which(col_classesdf1 %in% col_classesdf2)]
#   # matchNames <- names(col_classesdf1)[names(col_classesdf1) %in% names(col_classesdf2)]
#   matchNames <- intersect(names(col_classesdf1), names(col_classesdf2))
#   classMismatch <- unlist(lapply(matchNames, function(x){
#     df1_class <- col_classesdf1[[which(names(col_classesdf1) == x)]]
#     df2_class <- col_classesdf2[[which(names(col_classesdf2) == x)]]
#     return(any(df1_class != df2_class))
#   }))
#   classMismatch <- matchNames[classMismatch] %>% na.omit
#
#   # lapply(classMismatch, function(x) {
#   for(x in classMismatch){
#     # df2_col <- df2[[x]]
#     # setClass(df2_col, class(df1[[x]]))
#     # print(x)
#     # eval(parse(text = paste(fname, "(x)")))
#     # df2[[x]] <- eval(parse(text = paste("as.", as.character(class(df1[[x]])),"(df1[[x]])", sep = "")))
#     eval(parse(text = paste("df2$",x," <- as.", as.character(class(df1[[x]]))[1],"(df2[[x]])", sep = "")))
#   }#)
#
#   # df_merged <- rbindlist(list(df1, df2), fill = TRUE)
#   df_merged <- rbindlist(list(df1, df2), fill = T)
# }

# Function to bind data.frames when matching columns are of different classes
bind_df_diffClass <- function(dt_list){
  # Step 1: Determine the most common class for each column
  get_common_class <- function(dts, col) {
    classes <- unlist(lapply(dts, function(x) class(x[[col]])))
    most_common <- names(sort(table(classes), decreasing = TRUE))[1]
    return(most_common)
  }

  # Assuming data tables don't have the same number of columns
  # column_names <- unique(c(names(dt_list[[1]]), names(dt_list[[2]])))

  column_names <- unique(unlist(sapply(dt_list, names)))

  all_col_df <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  names(all_col_df) <- column_names

  dt_list <- lapply(dt_list, function(x){
    all_col_subdf = rbindlist(list(all_col_df,
                                   x),
                              fill = TRUE)
    return(all_col_subdf)
  })

  # dt_list[[1]] <- rbindlist(list(all_col_df ,dt_list[[1]]),
  #                           fill = TRUE)
  #
  # dt_list[[2]] <- rbindlist(list(all_col_df ,dt_list[[2]]),
  #                           fill = TRUE)

  common_classes <- sapply(column_names, function(col) get_common_class(dt_list, col))

  # Step 2: Convert columns to the most common class
  convert_columns <- function(dt, common_classes) {
    for (col in names(common_classes)) {
      class_type <- common_classes[col]
      dt[[col]] <- switch(class_type,
                          "numeric" = as.numeric(dt[[col]]),
                          "character" = as.character(dt[[col]]),
                          "integer" = as.integer(dt[[col]]),
                          "factor" = as.factor(dt[[col]]),
                          "logical" = as.logical(dt[[col]]),
                          "Date" = as.Date(dt[[col]], tz = "UTC"),
                          "POSIXct" = as.POSIXct(dt[[col]], tz = "UTC"),
                          dt[[col]])  # default to not converting if class type not handled
    }
    return(dt)
  }

  dt_list <- lapply(dt_list, convert_columns, common_classes)

  # Step 3: Bind the data tables together
  combined_dt <- rbindlist(dt_list, fill = TRUE)
  return(combined_dt)
}

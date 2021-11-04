
#' Update files stored on Google drive
#'
#' Files stored on Google drive will be updated or created if they have changed
#' since the last time the function was run. Directories on google drive will
#' not be created.
#'
#' This is helpful for when you are trying to keep some files tracked on GitHub
#' and others on Google Drive.
#'
#' Before using the function all folders need to exist on Google drive and
#' locally and the tracker files needs to be created with a dummy row filled in.
#' Files will be created on Drive if they don't exist and updated if they do.
#'
#' @param dirs character vector of directories to sync all files in
#' @param fls character vector of individual files to sync
#' @param tracker path to file that tracks when files were modified
#' @param gd_dir google drive directory path
#' @param gd_dir_local The absolute path to the local file that will mirror
#'   gd_dir
#'
#' @return nothing
#' @export
#'
#' @examples
#' # whole folders to sync
#' dirs <- c("data/", "../Manuscript/", "reports/", "figs/")
#'
#' # individual files to sync
#' fls <- c("../README.docx")
#'
#' tracker <- "data/interim/track_file_modtime.csv"
#'
#' gd_dir <- "SAR_climate_change/3_SAR_CCVI_CAN_USA"
#'
#' gd_dir_local <- "C:/Users/endicotts/Documents/Ilona/SAR_CCVI_CAN_USA"
#'
#' # This should only need to be done once
#' #googledrive::drive_auth()
#'
#' #look for files on google drive
#' #googledrive::drive_find(q = c("starred = true"))
#'
#' update_gd(c("data/", "../Manuscript/"), c("../README.docx"),
#'           "data/interim/track_file_modtime.csv",
#'           "SAR_climate_change/3_SAR_CCVI_CAN_USA",
#'           "C:/Users/endicotts/Documents/Ilona/SAR_CCVI_CAN_USA")
#'
update_gd <- function(dirs, fls, tracker, gd_dir, gd_dir_local){
  # get files in dirs and combine
  fls_all <- c(fs::dir_ls(dirs, type = "file"), fls)

  # figure out which files have changed
  fls_tracker <- read.csv(tracker) %>%
    dplyr::transmute(filename, mtime = as.POSIXct(mtime))

  fls_info <- file.info(fls_all) %>% tibble::rownames_to_column("filename") %>%
    dplyr::select(filename, mtime)

  fls_info <- dplyr::left_join(fls_info, fls_tracker, by = "filename") %>%
    dplyr::mutate(mtime.y = ifelse(is.na(mtime.y), mtime.x - lubridate::minutes(10), mtime.y)) %>%
    dplyr::filter(mtime.x - mtime.y > 2)

  fls <- fls_info$filename

  if(length(fls) == 0){
    return(message("All files are up-to-date"))
  }

  fls_out <- R.utils::getAbsolutePath(fls) %>%
    stringr::str_remove(paste0(fs::path_dir(gd_dir_local), "/")) %>%
    stringr::str_replace(fs::path_file(gd_dir_local), gd_dir)

  purrr::walk2(fls, fls_out, ~googledrive::drive_put(media = .x, path = .y))

  # track which files have changed
  fls_mod <- file.info(fls_all) %>% tibble::rownames_to_column("filename")
  write.csv(fls_mod, tracker, row.names = FALSE)
  message("File tracker updated")

}


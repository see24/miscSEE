% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_gd.R
\name{update_gd}
\alias{update_gd}
\title{Update files stored on Google drive}
\usage{
update_gd(dirs, fls, tracker, gd_dir, gd_dir_local)
}
\arguments{
\item{dirs}{character vector of directories to sync all files in}

\item{fls}{character vector of individual files to sync}

\item{tracker}{path to file that tracks when files were modified}

\item{gd_dir}{google drive directory path}

\item{gd_dir_local}{The absolute path to the local file that will mirror
gd_dir}
}
\value{
nothing
}
\description{
Files stored on Google drive will be updated or created if they have changed
since the last time the function was run. Directories on google drive will
not be created.
}
\details{
This is helpful for when you are trying to keep some files tracked on GitHub
and others on Google Drive.

Before using the function all folders need to exist on Google drive and
locally and the tracker files needs to be created with a dummy row filled in.
Files will be created on Drive if they don't exist and updated if they do.
}
\examples{
# whole folders to sync
dirs <- c("data/", "../Manuscript/", "reports/", "figs/")

# individual files to sync
fls <- c("../README.docx")

tracker <- "data/interim/track_file_modtime.csv"

gd_dir <- "SAR_climate_change/3_SAR_CCVI_CAN_USA"

gd_dir_local <- "C:/Users/endicotts/Documents/Ilona/SAR_CCVI_CAN_USA"

# This should only need to be done once
#googledrive::drive_auth()

#look for files on google drive
#googledrive::drive_find(q = c("starred = true"))

update_gd(c("data/", "../Manuscript/"), c("../README.docx"),
          "data/interim/track_file_modtime.csv",
          "SAR_climate_change/3_SAR_CCVI_CAN_USA",
          "C:/Users/endicotts/Documents/Ilona/SAR_CCVI_CAN_USA")

}

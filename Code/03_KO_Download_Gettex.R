### KO: Download Gettex ###

#### 1. Preamble ####
library(data.table)
library(lubridate)

#Parameters
dates_to_download = seq(ymd("2021-01-01"), ymd("2026-07-31"), by = "day")
dates_to_download = dates_to_download[!(weekdays(dates_to_download) %in% c("Saturday", "Sunday"))]
dates_already_downloaded = list.files("Data/Gettex")
dates_already_downloaded = ymd(unique(substr(dates_already_downloaded, 4, 11)))
dates_to_download = dates_to_download[!(dates_to_download %in% dates_already_downloaded)]
#Note: dates_to_download can be adjusted if one does not wish to download data dating back until 2021 
#(which is not necessary for the replication of the results in the paper).
#For the replication, it suffices to download data starting March 20, 2023.

#### 2. Download and extract ####
for (i in 1:length(dates_to_download)) {
  d = dates_to_download[i]
  d_formatted = format(d, "%Y%m%d")
  
  url = paste0("https://www.gettex.de/fileadmin/rts27/MUNC-MUND/", 
               format(d, "%Y"), "/",
               format(d, "%m"), "/", 
               d_formatted, "_MUND.zip")
  
  zipfile = file.path(tempdir(), paste0(d_formatted, "_MUND.zip"))
  
  cat("Downloading", d_formatted, "...\n")
  
  success = tryCatch({
    download.file(url, zipfile, mode = "wb", quiet = TRUE)
    TRUE
  }, warning = function(w) {
    FALSE
  }, error = function(e) {
    FALSE
  })
  
  if (!success || !file.exists(zipfile)) {
    cat("  No data available.\n")
    Sys.sleep(runif(1, 2, 5))
    next
  }
  
  contents = unzip(zipfile, list = TRUE)
  
  files_to_extract = contents$Name[basename(contents$Name) %in% 
                                     c(paste0("BE_", d_formatted, "_MUND_02_0001.csv"), 
                                       paste0("BE_", d_formatted, "_MUND_06_0001.csv"))]
  
  if (length(files_to_extract) > 0) {
    unzip(
      zipfile,
      files = files_to_extract,
      exdir = "Data/Gettex"
    )
    cat("  Extracted", length(files_to_extract), "file(s).\n")
  } else {
    cat("  Desired files not found in ZIP.\n")
  }
  
  unlink(zipfile)
  
  Sys.sleep(runif(1, 2, 5))
}


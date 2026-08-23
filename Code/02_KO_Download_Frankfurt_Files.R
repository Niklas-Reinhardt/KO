### KO: Download Frankfurt Files ###

#### 1. Preamble ####
#Libraries
library(data.table)
library(lubridate)
library(httr2)
library(stringr)
library(curl)

#Parameters
user_agent = "Mozilla/5.0 (academic research scraper)"
max_num_downloads = 10000

#### 2. Links to download ####
#Import URL list
link_data = fread("Data/Frankfurt/Frankfurt_URLs.csv", header = FALSE, col.names = "URL")

#Extract file names from URL
link_data[, FileName := gsub(".*/", "", URL)]

#Extract file name types from file names
link_data[, FileNameType := gsub("\\.[0-9]{8}\\.xls$", "", FileName)]
print(unique(link_data$FileNameType)) #Check

#Extract dates from file names
link_data[, Date := ymd(gsub(".*\\.(\\d{8})\\.xls$", "\\1", FileName))]

#Check dates
summary(link_data$Date)
link_data[, N_Date := .N, by = Date]
print(setorder(link_data[N_Date > 1, .(Date, FileName)], Date))
#8 dates with 2 files each. 
#For each of these 8 dates, 
#there is one file under the "Scoach" naming regime and
#there is one file under the "BoerseFrankfurtZertifikate" naming regime.
#Download all files and then check afterwards if the data are identical.

#Do not download files that are already in the target folder
files_already_downloaded = list.files("Data/Frankfurt/DailyStatistics")
link_data = link_data[!(FileName %in% files_already_downloaded)]

#Only download at most max_num_downloads
num_to_download = min(nrow(link_data), max_num_downloads)
link_data = link_data[1:num_to_download]

#### 3. Download ####
files_not_downloaded = list()
error_counter = 1
for (i in 1:nrow(link_data)) {
  cat("Downloading", link_data[i, FileName], "...\n")
  success = tryCatch({
    temp_file = file.path(tempdir(), link_data[i, FileName])
    curl_download(link_data[i, URL], temp_file, quiet = TRUE,
                  handle = new_handle(useragent = user_agent))
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  if (!success || !file.exists(temp_file)) {
    files_not_downloaded[[error_counter]] = link_data[i, FileName]
    error_counter = error_counter + 1
    cat("  No data available.\n")
  } else {
    file.copy(temp_file, paste0("Data/Frankfurt/DailyStatistics/", link_data[i, FileName]), overwrite = TRUE)
    unlink(temp_file)
    cat("  Downloaded", i, "of", num_to_download, "\n")
  }
  Sys.sleep(runif(1, 2, 5))
}
print(unlist(files_not_downloaded))

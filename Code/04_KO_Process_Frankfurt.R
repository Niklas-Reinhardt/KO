### KO: Process Frankfurt ###

#### 1. Preamble ####
packages = c("zoo", "data.table", "lubridate", "readxl")
library("groundhog")
groundhog.library(packages, "2026-08-26")
options(scipen = 999)

#### 2. Data import ####
##### 2.1 Pre-import checks #####
#List of all downloaded files
filenames = data.table(FileName = list.files("Data/Frankfurt/DailyStatistics"))

#Check against list of downloadable files
downloadable_files = fread("Data/Frankfurt/Frankfurt_URLs.csv", header = FALSE, col.names = "URL")
downloadable_files[, FileName := gsub(".*/", "", URL)]
downloadable_files[, Check_Downloadable := 1]
downloadable_files = merge(x = downloadable_files, y = filenames[, .(FileName, Check_Downloaded = 1)], 
                           by = "FileName", all = TRUE)
summary(downloadable_files$Check_Downloadable)
summary(downloadable_files$Check_Downloaded)
#All downloadable files were downloaded.

##### 2.2 Duplicate dates #####
downloadable_files[, Date := ymd(gsub(".*\\.(\\d{8})\\.xls$", "\\1", FileName))]
downloadable_files[, N_Date := .N, by = Date]
duplicate_dates = downloadable_files[N_Date > 1, .(Date, FileName)]
print(setorder(duplicate_dates, Date, FileName))
#6 duplicate files in 2014 and 2 in 2021

for (i in seq_len(nrow(duplicate_dates)/2)) {
  temp_1 = as.data.table(read_xls(paste0("Data/Frankfurt/DailyStatistics/", duplicate_dates[i*2-1, FileName]), sheet = "Data", skip = 10))
  temp_2 = as.data.table(read_xls(paste0("Data/Frankfurt/DailyStatistics/", duplicate_dates[i*2, FileName]), sheet = "Data", skip = 10))

  print(temp_1[`Instrument Type` %like% "Knock", .(`Instrument Type`, `Client Order Book Turnover EUR`)])
  print(temp_2[`Instrument Type` %like% "Knock", .(`Instrument Type`, `Client Order Book Turnover EUR`)])
  print(sum(is.na(temp_1) != is.na(temp_2)))
  print(sum(temp_1 != temp_2, na.rm = TRUE))
  rm(temp_1, temp_2)
}
#Data for the two files on duplicate dates are exactly the same.
#Keep only the "BoerseFrankfurt..." duplicates and drop the "Scoach..." duplicates:
filenames = filenames[!(FileName %in% duplicate_dates[FileName %like% "Scoach", FileName])]
rm(duplicate_dates, downloadable_files)

##### 2.3 Excel sheet names #####
filenames[, SheetNames := as.character(NA)]
for (i in 1:nrow(filenames)) {
  filenames[i, SheetNames := paste(unlist(excel_sheets(paste0("Data/Frankfurt/DailyStatistics/", filenames[i, FileName]))), collapse = ", ")]
  if (i %% 100 == 0) {
    print(i)
  }
}
print(unique(filenames$SheetNames))
print(filenames[SheetNames != "Cover, Data"])
#Just one file with different sheet names in 2015

filenames_regular = filenames[SheetNames == "Cover, Data", FileName]
filename_extra = filenames[SheetNames != "Cover, Data", FileName]
rm(filenames)

##### 2.4 Import regular files #####
file_list = list()
i = 1
for (f in filenames_regular) {
  temp = as.data.table(read_xls(paste0("Data/Frankfurt/DailyStatistics/", f), sheet = "Data", skip = 10))
  
  f_without_extension = gsub(pattern = ".xls", replacement = "", x = f)
  temp[, DateFromFileName := ymd(substr(f_without_extension, start = nchar(f_without_extension) - 5, nchar(f_without_extension)))]
  date_check = read_xls(paste0("Data/Frankfurt/DailyStatistics/", f), 
                        sheet = "Cover", range = "B24:C24", col_names = c("Date_Type", "Date"))
  temp[, DateFromCoverSheetType := date_check$Date_Type]
  temp[, DateFromCoverSheet := dmy(date_check$Date)]
  
  file_list[[i]] = temp
  if (i %% 100 == 0) {
    print(i)
  }
  i = i + 1
  rm(f_without_extension, temp, date_check)
}
d = rbindlist(file_list, fill = TRUE)
rm(filenames_regular, file_list, i, f)

#Date check
print(d[, .N, by = DateFromCoverSheetType])
print(d[, mean(DateFromCoverSheet == DateFromFileName)])
print(d[DateFromCoverSheet != DateFromFileName, mean(DateFromCoverSheet < DateFromFileName)])
print(unique(d[DateFromCoverSheet != DateFromFileName, .(DateFromCoverSheet, DateFromFileName)]))
#DateFromCoverSheet is the trading date, 
#DateFromFileName in those cases where they differ either a weekend or holiday.

#Keep only relevant variables and rename them
d = d[, .(Date = DateFromCoverSheet,
          Group = `Instrument Group`,
          InstrumentType = `Instrument Type`, 
          Volume = `Client Order Book Turnover EUR`)]

##### 2.5 Import extra file #####
if (length(filename_extra) > 0) {
  temp = as.data.table(read_xls(paste0("Data/Frankfurt/DailyStatistics/", filename_extra), 
                                sheet = "Börse Frankfurt Zertifikate", skip = 13))
  temp[, Date := dmy(read_xls(paste0("Data/Frankfurt/DailyStatistics/", filename_extra), sheet = "Cover", range = "C25", col_names = "Date")$Date)]
  temp = temp[, .(Date,
                  Group = `Instrument Group`,
                  InstrumentType = `Instrument Type`, 
                  Volume = Euro)]
  temp[9, InstrumentType := "Total"]  #Total investment products
  temp[14, InstrumentType := "Total"] #Total leverage products
  temp[15, Group := "Total"] #Total products
  d = rbind(d, temp)
  rm(temp)
}
rm(filename_extra)

#### 3. Basic processing ####
#Check groups and types:
str(d)
print(setorder(d[, .N, by = InstrumentType], -N))
print(d[is.na(InstrumentType), .N, by = Group]) #Total across all products (not needed)
d = d[!(is.na(InstrumentType))] 

#Missing volume:
print(d[is.na(Volume)]) 
print(d[is.na(Volume), .N, by = InstrumentType]) 
d = d[!(is.na(Volume))]

#The first Total is for investment products, the second Total for leverage products.
#Fill the correct groups for each type:
d[, Group_Filled := na.locf(Group), by = Date]
print(setorder(d[, .N, by = .(Group_Filled, InstrumentType)], Group_Filled, -N))

#Only keep leverage products:
d = d[Group_Filled == "Leverage Products"]
d[, c("Group", "Group_Filled") := NULL]
print(d[, .N, by = InstrumentType])
print(summary(d[InstrumentType %like% "Knock", .N, by = Date]))

#Create 3 time series: KO, OtherLeverage, Total
d[, Type := "OtherLeverage"]
d[InstrumentType == "Total", Type := "Total"]
d[InstrumentType %like% "Knock", Type := "KO"]
print(setorder(d[, .N, by = .(Type, InstrumentType)], Type, -N))
data_fra = d[, .(Volume_Frankfurt = sum(Volume)), by = .(Type, Date)]
rm(d)

#Check that totals are correct:
check = merge(x = data_fra[Type != "Total", .(Volume_Frankfurt_Agg = sum(Volume_Frankfurt)), by = Date],
              y = data_fra[Type == "Total", .(Date, Volume_Frankfurt)], 
              by = "Date", all = TRUE)
print(check[, summary(Volume_Frankfurt_Agg - Volume_Frankfurt)]) #Check: no differences.
rm(check)
data_fra = data_fra[Type != "Total"]

#Check unique dates
print(summary(data_fra[, .N, by = Date])) #2 observations per date
unique_dates = unique(data_fra[, .(Date)])
setorder(unique_dates, Date)
unique_dates[, Date_Diff := Date - shift(Date)]
print(unique_dates[, min(Date_Diff, na.rm = TRUE)])
print(unique_dates[, max(Date_Diff, na.rm = TRUE)])
print(setorder(unique_dates[Date_Diff > days(3)], -Date_Diff))
rm(unique_dates)

#### 4. Export ####
saveRDS(data_fra, "Data/Processed_Data/Frankfurt.rds")


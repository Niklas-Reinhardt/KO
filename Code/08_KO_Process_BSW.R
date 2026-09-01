### KO: Process BSW ###

#### 0. Data requirements ####
#To run this script:
#The pdfs for "Börseumsätze" need to be downloaded from:
#https://www.derbsw.de/de/boersenumsaetze/
#and stored in the 4 respective sub-folders of "Data/BSW":
#Reports from January 2021 through June 2021 in "Data/BSW/Reports_202101_to_202106/"
#Reports from July 2021 through June 2024 in "Data/BSW/Reports_202107_to_202406/"
#Reports from July 2024 through January 2025 in "Data/BSW/Reports_202407_to_202501/"
#Reports from February 2025 through December 2025 in "Data/BSW/Reports_202502_to_202512/"

#### 1. Preamble ####
packages = c("data.table", "lubridate", "pdftools")
library("groundhog")
groundhog.library(packages, "2026-08-26")

#Function to split lines based on two or more whitespaces
split_line = function(x) {
  as.list(trimws(unlist(strsplit(x, "\\s{2,}"))))
}

#Month translation
german_months = c(
  Januar = "January",
  Februar = "February",
  März = "March",
  April = "April",
  Mai = "May",
  Juni = "June",
  Juli = "July",
  August = "August",
  September = "September",
  Oktober = "October",
  November = "November",
  Dezember = "December"
)

#### 2. Processing 202502_to_202512 ####
temp_folder = "Data/BSW/Reports_202502_to_202512/"
temp_file_list = list.files(temp_folder)
for (temp_file in temp_file_list) {
  #Read pdf
  txt = pdf_text(paste0(temp_folder, temp_file))
  
  #Page 4
  lines = strsplit(txt[4], "\n")[[1]]
  
  #Date line
  date = lines[1]
  date = gsub(" I ", "", date)
  date = gsub(" ", "", date)
  date = gsub("BörsenumsätzestrukturierterWertpapiere", "", date)
  for (m in names(german_months)) {
    date = gsub(m, german_months[[m]], date)
  }
  date = my(date)
  
  #Knock-out lines
  lines = lines[grepl("Hebelprodukte mit Knock-out", lines)]
  
  #Split lines based on two or more whitespaces and put together in a data.table
  table_vol = rbindlist(lapply(lines[[1]], split_line))
  table_num = rbindlist(lapply(lines[[2]], split_line))
  table_vol = table_vol[, c(2, 4, 6, 8)]
  table_num = table_num[, c(2, 4, 6, 8)]
  if (date == ymd("2025-03-01")) {
    setnames(table_vol, new = c("Stuttgart", "Frankfurt", "Gettex", "Total"))
    setnames(table_num, new = c("Stuttgart", "Frankfurt", "Gettex", "Total"))
  } else {
    setnames(table_vol, new = c("Total", "Stuttgart", "Frankfurt", "Gettex"))
    setnames(table_num, new = c("Total", "Stuttgart", "Frankfurt", "Gettex"))
  }
  
  table_vol[, Date := date]
  table_num[, Date := date]
  
  if (temp_file == temp_file_list[[1]]) {
    results_vol = copy(table_vol)
    results_num = copy(table_num)
  } else {
    results_vol = rbind(results_vol, table_vol)
    results_num = rbind(results_num, table_num)
  }
}

#### 3. Processing 202407_to_202501 ####
temp_folder = "Data/BSW/Reports_202407_to_202501/"
temp_file_list = list.files(temp_folder)
for (temp_file in temp_file_list) {
  #Read pdf
  txt = pdf_text(paste0(temp_folder, temp_file))
  
  #Page 4
  lines = strsplit(txt[4], "\n")[[1]]
  
  #Date line
  date = lines[1]
  date = gsub(" I ", "", date)
  date = gsub(" ", "", date)
  date = gsub("BörsenumsätzestrukturierterWertpapiere", "", date)
  for (m in names(german_months)) {
    date = gsub(m, german_months[[m]], date)
  }
  date = my(date)
  
  #Knock-out lines
  lines = lines[grepl("Hebelprodukte mit Knock-out", lines)]
  
  #Split lines based on two or more whitespaces and put together in a data.table
  table_vol = rbindlist(lapply(lines[[1]], split_line))
  table_num = rbindlist(lapply(lines[[2]], split_line))
  table_vol = table_vol[, c(2, 4, 6, 8)]
  table_num = table_num[, c(2, 4, 6, 8)]
  
  setnames(table_vol, new = c("Stuttgart", "Frankfurt", "Gettex", "Total"))
  setnames(table_num, new = c("Stuttgart", "Frankfurt", "Gettex", "Total"))
  
  table_vol[, Date := date]
  table_num[, Date := date]
  
  results_vol = rbind(results_vol, table_vol)
  results_num = rbind(results_num, table_num)
}

#### 4. Processing 202107_to_202406 ####
temp_folder = "Data/BSW/Reports_202107_to_202406/"
temp_file_list = list.files(temp_folder)
for (temp_file in temp_file_list) {
  #Read pdf
  txt = pdf_text(paste0(temp_folder, temp_file))
  
  #Page 5
  lines = strsplit(txt[5], "\n")[[1]]
  
  #Date line
  date = lines[1]
  date = gsub(" I ", "", date)
  date = gsub(" ", "", date)
  date = gsub("BörsenumsätzevonstrukturiertenWertpapieren", "", date)
  for (m in names(german_months)) {
    date = gsub(m, german_months[[m]], date)
  }
  date = my(date)
  
  #Knock-out lines
  lines = lines[grepl("Hebelprodukte mit Knock-Out", lines)]
  
  #Split lines based on two or more whitespaces and put together in a data.table
  table_vol = rbindlist(lapply(lines[[1]], split_line))
  table_num = rbindlist(lapply(lines[[2]], split_line))
  table_vol = table_vol[, c(2, 4, 6, 8)]
  table_num = table_num[, c(2, 4, 6, 8)]
  
  setnames(table_vol, new = c("Stuttgart", "Frankfurt", "Gettex", "Total"))
  setnames(table_num, new = c("Stuttgart", "Frankfurt", "Gettex", "Total"))
  
  table_vol[, Date := date]
  table_num[, Date := date]
  
  results_vol = rbind(results_vol, table_vol)
  results_num = rbind(results_num, table_num)
}

#### 5. Processing 202101_to_202106 ####
temp_folder = "Data/BSW/Reports_202101_to_202106/"
temp_file_list = list.files(temp_folder)
for (temp_file in temp_file_list) {
  #Read pdf
  txt = pdf_text(paste0(temp_folder, temp_file))
  
  #Page 5
  lines = strsplit(txt[5], "\n")[[1]]
  
  #Date line
  date = lines[1]
  date = gsub(" I ", "", date)
  date = gsub(" ", "", date)
  date = gsub("BörsenumsätzevonstrukturiertenWertpapieren", "", date)
  for (m in names(german_months)) {
    date = gsub(m, german_months[[m]], date)
  }
  date = my(date)
  
  #Knock-out lines
  lines = lines[grepl("Hebelprodukte mit Knock-Out", lines)]
  
  #Split lines based on two or more whitespaces and put together in a data.table
  table_vol = rbindlist(lapply(lines[[1]], split_line))
  table_num = rbindlist(lapply(lines[[2]], split_line))
  table_vol = table_vol[, c(2, 4, 6)]
  table_num = table_num[, c(2, 4, 6)]
  
  setnames(table_vol, new = c("Stuttgart", "Frankfurt", "Total"))
  setnames(table_num, new = c("Stuttgart", "Frankfurt", "Total"))
  
  table_vol[, Gettex := NA]
  table_num[, Gettex := NA]
  
  table_vol[, Date := date]
  table_num[, Date := date]
  
  results_vol = rbind(results_vol, table_vol)
  results_num = rbind(results_num, table_num)
}

#### 6. Export ####
#Numeric
results_vol[, Stuttgart := as.integer(gsub("\\.", "", Stuttgart))]
results_vol[, Frankfurt := as.integer(gsub("\\.", "", Frankfurt))]
results_vol[, Gettex := as.integer(gsub("\\.", "", Gettex))]
results_vol[, Total := as.integer(gsub("\\.", "", Total))]

results_num[, Stuttgart := as.integer(gsub("\\.", "", Stuttgart))]
results_num[, Frankfurt := as.integer(gsub("\\.", "", Frankfurt))]
results_num[, Gettex := as.integer(gsub("\\.", "", Gettex))]
results_num[, Total := as.integer(gsub("\\.", "", Total))]

#Check totals
results_vol[is.na(Gettex), summary(Total - Stuttgart - Frankfurt)]
results_vol[!is.na(Gettex), summary(Total - Stuttgart - Frankfurt - Gettex)]
results_num[is.na(Gettex), summary(Total - Stuttgart - Frankfurt)]
results_num[!is.na(Gettex), summary(Total - Stuttgart - Frankfurt - Gettex)]

#Check dates
setorder(results_vol, Date)
setorder(results_num, Date)
summary(results_vol$Date)
summary(results_num$Date)
sum(unique(results_vol[, Date]) != results_vol[, Date])
sum(unique(results_num[, Date]) != results_num[, Date])

#Melt and merge
results_vol = melt(results_vol, id.vars = "Date", variable.name = "Exchange", value.name = "Volume")
results_num = melt(results_num, id.vars = "Date", variable.name = "Exchange", value.name = "NumberTrades")
results = merge(x = results_vol, y = results_num, by = c("Exchange", "Date"), all = TRUE)

#Final processing
results[, Date := ceiling_date(Date, unit = "months") - days(1)]
results[, AverageTradeSize := 1000 * Volume / NumberTrades]

#Export
saveRDS(results, "Data/Processed_Data/BSW.rds")


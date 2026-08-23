### KO: Process Gettex ###

#### 1. Preamble ####
library(data.table)
library(lubridate)

#### 2. Data processing ####
dates_list = list.files("Data/Gettex")
dates_list = ymd(unique(substr(dates_list, 4, 11)))
dates_list = dates_list[dates_list > ymd("2023-03-17")]
#Until 2023-03-17 (including 2023-03-17), there are time periods, in which the 02-files are empty.

data_list = list()
for (i in 1:length(dates_list)) {
  #Import
  d = dates_list[[i]]
  d_formatted = format(d, "%Y%m%d")
  temp_table_2 = fread(paste0("Data/Gettex/BE_", d_formatted, "_MUND_02_0001.csv"))
  temp_table_6 = fread(paste0("Data/Gettex/BE_", d_formatted, "_MUND_06_0001.csv"))
  temp_table_6[, c("Orderloeschungen_Anzahl", "Orderaenderungen_Anzahl", "Kurswert_Median", 
                   "Ordereinstellungen_Anzahl", "Ordereinstellungen_WertMedian", "MarketMaker_Anzahl") := NULL]
  
  #Identify knock-out warrants and non-knock-out warrants
  temp_table_2[, Type := as.character(NA)]
  temp_table_2[substr(Finanzinstrument_Klassifizierung, 1, 2) == "RF" &
                 substr(Finanzinstrument_Klassifizierung, 4, 4) == "T", Type := "KO"]
  temp_table_2[substr(Finanzinstrument_Klassifizierung, 1, 2) == "RW" |
                 (substr(Finanzinstrument_Klassifizierung, 1, 2) == "RF" & 
                    substr(Finanzinstrument_Klassifizierung, 4, 4) == "M"), Type := "OtherLeverage"]
  
  #Filter
  temp_table_2 = temp_table_2[!is.na(Type), .(ISIN, Type)]
  temp_table_2[, Counter := 1:.N, by = ISIN] #In case of duplicate ISINs
  temp_table_2 = temp_table_2[Counter == 1, .(ISIN, Type)]
  
  #Merge
  temp_table_6 = merge(x = temp_table_6, y = temp_table_2, by = "ISIN", all.x = FALSE, all.y = FALSE)
  
  #Aggregate
  temp_agg = temp_table_6[, .(Date = d, 
                              Volume_Gettex = sum(Geschaefte_Kurswert),
                              NumTrades_Gettex = sum(Geschaefte_Anzahl)), by = Type]
  data_list[[i]] = temp_agg
  
  print(d)
  rm(temp_agg, temp_table_2, temp_table_6, d, d_formatted)
}
data_gettex = rbindlist(data_list)

#### 3. Export ####
saveRDS(data_gettex, "Data/Processed_Data/Gettex.rds")



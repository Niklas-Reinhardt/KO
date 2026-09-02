### KO: Process SIX ###

#### 0. Data requirements ####
#To run this script:
#"Monthly Trade Data" files need to be downloaded from:
#https://www.six-group.com/de/market-data/statistics/monthly-reports.html#tfl_XRzX2xpc3Q=/year/2026,/content/cq:tags/sixReference/six/content-type/report/monthly-trade-data
#and stored (as monthly .csv files) in "Data/SIX/TradeData".

#Additionally, "Monthly Trade Data" files need to be downloaded from:
#https://www.six-group.com/de/market-data/statistics/monthly-reports/archive.html#tfl_dHNfbGlzdA=/content/cq:tags/sixReference/six/content-type/report/monthly-trade-data
#and stored (as one unzipped folder per year) in "Data/SIX/Archive".
#Note: The replication requires data from 2023 through July 2026.

#### 1. Preamble ####
packages = c("data.table", "lubridate")
library("groundhog")
groundhog.library(packages, "2026-08-26")

#### 2. Data processing ####
##### 2.1 Import #####
filename_list = list.files("Data/SIX/", pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
results_list = list()
i = 1
for (f in filename_list) {
  temp = fread(f)
  setnames(temp, new = gsub(" ", "_", colnames(temp)))
  setnames(temp, old = "Isin", new = "ISIN", skip_absent = TRUE)
  if (f %like% "Monthly_Trade_Data_201908.csv") {
    temp[, Trade_Date := dmy(gsub("\\.00\\.", "\\.08\\.", Trade_Date))]
    temp[, Daily_Turnover_Chf := gsub(",", "", Daily_Turnover_Chf)]
  } else if (is.character(temp$Trade_Date)) {
    temp[, Trade_Date := dmy(Trade_Date)]
  } else {
    temp[, Trade_Date := as_date(Trade_Date)]
  }
  
  temp = temp[, .(Date = Trade_Date, 
                  Type = Instrument_Sub_Type, 
                  ISIN, 
                  Volume_CHF = as.numeric(Daily_Turnover_Chf), 
                  NumTrades = as.integer(Number_Of_Trades))]
  temp = temp[Type %in% c("Knock-Out Warrant", "Other Warrant")]
  temp_agg = temp[, .(Volume_SIX_CHF = sum(Volume_CHF), 
                      NumTrades_SIX = sum(NumTrades)), by = .(Type, Date)]
  results_list[[i]] = temp_agg
  print(i)
  i = i + 1
  rm(temp, temp_agg)
}
data_six = rbindlist(results_list)
rm(results_list, i, f, filename_list)

#Consistent naming of types with other datasets:
data_six[Type == "Knock-Out Warrant", Type := "KO"]
data_six[Type == "Other Warrant", Type := "OtherLeverage"] 

##### 2.2 Exclude non-trading days ####
print(setorder(data_six, NumTrades_SIX)[1:10])
print(data_six[Date == ymd("2019-02-03")]) #Just one trade for other warrants, none for knock outs.
data_six = data_six[Date != ymd("2019-02-03")]
print(data_six[, .N, by = Type]) #Check
print(summary(data_six[, .N, by = Date])) #Check

##### 2.3 Convert to EUR #####
eur_chf = as.data.table(read.csv("https://data-api.ecb.europa.eu/service/data/EXR/D.CHF.EUR.SP00.A?format=csvdata"))
eur_chf = eur_chf[, .(Date = as_date(TIME_PERIOD), CHF_per_EUR = OBS_VALUE)]
data_six = merge(x = data_six, y = eur_chf, by = "Date", all.x = TRUE, all.y = FALSE)
data_six[, summary(CHF_per_EUR)]
data_six[, Volume_SIX := Volume_SIX_CHF / CHF_per_EUR]
data_six[, c("Volume_SIX_CHF", "CHF_per_EUR") := NULL]
rm(eur_chf)

##### 2.4 Export #####
saveRDS(data_six, "Data/Processed_Data/SIX.rds")

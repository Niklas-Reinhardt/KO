### KO: Analyses Main ###

#### 1. Preamble ####
library(data.table)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(sandwich)
library(lmtest)
library(kableExtra)
options(scipen = 999)

#Function to output formatted coefficient estimates with t-stats in parentheses
est_tstat = function(regression, regressor, digits = 2) {
  estimate = format(round(regression[regressor, "Estimate"], digits), nsmall = digits, big.mark = ",")
  t_stat = paste0("(", format(round(regression[regressor, "t value"], digits), nsmall = digits, big.mark = ","), ")")
  all_together = c(estimate, t_stat)
  return(all_together)
}

#Parameters
intervention_date = ymd("2026-06-16")
start_event_window = ymd("2026-05-01")
end_event_window = ymd("2026-07-31")
alternative_start_event_window = ymd("2026-04-01")
announcement_date = ymd("2025-10-15")
announcement_start_event_window = ymd("2025-09-01")
announcement_end_event_window = ymd("2025-11-30")

#### 2. Import ####
data_fra = readRDS("Data/Processed_Data/Frankfurt.rds")
data_gettex = readRDS("Data/Processed_Data/Gettex.rds")
data_six = readRDS("Data/Processed_Data/SIX.rds")

#### 3. Merge, melt, and aggregaate data ####
data = merge(x = data_fra, y = data_gettex, by = c("Type", "Date"), all = FALSE)
data = merge(x = data, y = data_six, by = c("Type", "Date"), all = FALSE)
data = data[Date <= end_event_window]

#Melt volume data
data_long_vol = melt(data[, .(Type, Date, Volume_Frankfurt, Volume_Gettex, Volume_SIX)], 
                     id.vars = c("Type", "Date"), variable.name = "Exchange", value.name = "Volume")
data_long_vol[, Exchange := gsub(pattern = "Volume_", replacement = "", x = Exchange)]

#Melt number of trades data
data_long_num = melt(data[, .(Type, Date, NumTrades_Gettex, NumTrades_SIX)], 
                     id.vars = c("Type", "Date"), variable.name = "Exchange", value.name = "NumTrades")
data_long_num[, Exchange := gsub(pattern = "NumTrades_", replacement = "", x = Exchange)]

#Monthly data for Stuttgart:
data_stuttgart = data.table(Exchange = rep("Stuttgart", 2),
                            Month = ymd(c("2026-05-31", "2026-07-31")),
                            MeanDailyVolume = c(1567187590 / data_fra[, sum(Type == "KO" & year(Date) == 2026 & month(Date) == 5)], 
                                                1454409980 / data_fra[, sum(Type == "KO" & year(Date) == 2026 & month(Date) == 7)]))
#Note 1: Monthly volume data are from: 
#https://www.boerse-stuttgart.de/en/business-solutions/reports/euwax-monthly-report/
#Note 2: In May and July 2026, Stuttgart has the same trading days at Frankfurt, see:
#See also https://www.boerse-stuttgart.de/de-de/handel/handelszeiten/

#Monthly data for Frankfurt, Gettex, and SIX: 
data_long_vol[, Month := ceiling_date(Date, unit = "months") - days(1)]
data_monthly = data_long_vol[Type == "KO" & Month %in% ymd(c("2026-05-31", "2026-07-31")), 
                             .(MeanDailyVolume = mean(Volume)), by = .(Exchange, Month)]

#Monthly data together:
data_monthly = rbind(data_monthly, data_stuttgart)
setorder(data_monthly, Exchange, Month)
data_monthly[, Change := MeanDailyVolume / shift(MeanDailyVolume) - 1, by = Exchange]
data_monthly[, Change_Formatted := paste0(format(round(Change*100), nsmall = 0), "%")]
data_monthly = data_monthly[Month == ymd("2026-07-31")]
data_monthly[Exchange %in% c("Frankfurt", "Gettex", "Stuttgart"), Exchange := paste0("Germany:\n", Exchange)]
data_monthly[Exchange == "SIX", Exchange := paste0("Switzerland:\n", Exchange)]
rm(data_fra, data_gettex, data_six, data_stuttgart)

#### 4. Variables ####
data[, LogVolume_Frankfurt := log(Volume_Frankfurt)]
data[, LogVolume_Gettex := log(Volume_Gettex)]
data[, LogVolume_SIX := log(Volume_SIX)]
data[, LogNumTrades_Gettex := log(NumTrades_Gettex)]
data[, LogNumTrades_SIX := log(NumTrades_SIX)]
data[, DiffInLogVolume_Frankfurt_SIX := LogVolume_Frankfurt - LogVolume_SIX]
data[, DiffInLogVolume_Gettex_SIX := LogVolume_Gettex - LogVolume_SIX]
data[, DiffInLogNumTrades_Gettex_SIX := LogNumTrades_Gettex - LogNumTrades_SIX]
data[, Intervention := as.integer(Date >= intervention_date)]
data[, Announcement := as.integer(Date >= announcement_date)]
data_long_vol[, LogVolume := log(Volume)]
data_long_num[, LogNumTrades := log(NumTrades)]

#Different time periods:
data_whole_sample = copy(data)
data = data[Date >= start_event_window]

#For time trends:
setorder(data, Type, Date)
setorder(data_whole_sample, Type, Date)
data[, TimeIndex := 0:(.N - 1), by = Type]
data[, TimeIndex_Pre := pmin(0, TimeIndex - unique(data[Date == intervention_date, TimeIndex])), by = Type]
data[, TimeIndex_Post := pmax(0, TimeIndex - unique(data[Date == intervention_date, TimeIndex])), by = Type]
data_whole_sample[, TimeIndex := 0:(.N - 1), by = Type]
data_whole_sample[, TimeIndex_Pre := pmin(0, TimeIndex - unique(data[Date == intervention_date, TimeIndex])), by = Type]
data_whole_sample[, TimeIndex_Post := pmax(0, TimeIndex - unique(data[Date == intervention_date, TimeIndex])), by = Type]

#For placebo interventions:
num_days_pre = nrow(data[Type == "KO" & Date >= start_event_window & Date < intervention_date])
num_days_post = nrow(data[Type == "KO" & Date >= intervention_date])
data_whole_sample[, TimeIndex_Lagged_by_EventSpan := shift(TimeIndex, type = "lag", n = num_days_pre + num_days_post), by = Type]
placebo_final_start_timeindex = data_whole_sample[Type == "KO" & Date == intervention_date, TimeIndex_Lagged_by_EventSpan]

#### 5. Table: The effect of the intervention on trading activity ####
#Models
model_1 = lm(DiffInLogVolume_Frankfurt_SIX ~ Intervention, data[Type == "KO"])
coeftable_1 = coeftest(model_1, vcov. = NeweyWest(model_1, prewhite = FALSE, adjust = TRUE))
print(coeftable_1)

model_2 = lm(DiffInLogVolume_Gettex_SIX ~ Intervention, data[Type == "KO"])
coeftable_2 = coeftest(model_2, vcov. = NeweyWest(model_2, prewhite = FALSE, adjust = TRUE))
print(coeftable_2)

model_3 = lm(DiffInLogNumTrades_Gettex_SIX ~ Intervention, data[Type == "KO"])
coeftable_3 = coeftest(model_3, vcov. = NeweyWest(model_3, prewhite = FALSE, adjust = TRUE))
print(coeftable_3)

model_4 = lm(DiffInLogVolume_Frankfurt_SIX ~ Intervention, data[Type == "KO" & month(Date) == 6])
coeftable_4 = coeftest(model_4, vcov. = NeweyWest(model_4, prewhite = FALSE, adjust = TRUE))
print(coeftable_4)

model_5 = lm(DiffInLogVolume_Gettex_SIX ~ Intervention, data[Type == "KO" & month(Date) == 6])
coeftable_5 = coeftest(model_5, vcov. = NeweyWest(model_5, prewhite = FALSE, adjust = TRUE))
print(coeftable_5)

model_6 = lm(DiffInLogVolume_Frankfurt_SIX ~ Intervention, data[Type == "OtherLeverage"])
coeftable_6 = coeftest(model_6, vcov. = NeweyWest(model_6, prewhite = FALSE, adjust = TRUE))
print(coeftable_6)

model_7 = lm(DiffInLogVolume_Gettex_SIX ~ Intervention, data[Type == "OtherLeverage"])
coeftable_7 = coeftest(model_7, vcov. = NeweyWest(model_7, prewhite = FALSE, adjust = TRUE))
print(coeftable_7)

#Empty data.table
rnames = c("Intercept", "t_Intercept",
           "Post intervention", "t_Intervention",
           "Variable", "Exchanges", "Exchanges_2", "Time period",
           "$R^2$", "$N$")

colnames = c("Var", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)")
results = data.table(matrix(as.character(NA), nrow = length(rnames), ncol = length(colnames)))
setnames(results, colnames)
results[, Var := rnames]
rm(rnames, colnames)

#Fill model 1
results[results$Var %in% c("Intercept", "t_Intercept"), "(1)" := est_tstat(coeftable_1, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(1)" := est_tstat(coeftable_1, "Intervention")]
results[Var == "$N$", "(1)" := length(model_1$residuals)]
results[Var == "$R^2$", "(1)" := format(round(summary(model_1)$r.squared, 2), nsmall = 2)]
results[Var == "Exchanges", "(1)" := "Frankfurt"]
results[Var == "Exchanges_2", "(1)" := "vs.\\ SIX"]
results[Var == "Variable", "(1)" := "Volume"]
results[Var == "Time period", "(1)" := "May-July"]

#Fill model 2
results[results$Var %in% c("Intercept", "t_Intercept"), "(2)" := est_tstat(coeftable_2, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(2)" := est_tstat(coeftable_2, "Intervention")]
results[Var == "$N$", "(2)" := length(model_2$residuals)]
results[Var == "$R^2$", "(2)" := format(round(summary(model_2)$r.squared, 2), nsmall = 2)]
results[Var == "Exchanges", "(2)" := "Gettex"]
results[Var == "Exchanges_2", "(2)" := "vs.\\ SIX"]
results[Var == "Variable", "(2)" := "Volume"]
results[Var == "Time period", "(2)" := "May-July"]

#Fill model 3
results[results$Var %in% c("Intercept", "t_Intercept"), "(3)" := est_tstat(coeftable_3, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(3)" := est_tstat(coeftable_3, "Intervention")]
results[Var == "$N$", "(3)" := length(model_3$residuals)]
results[Var == "$R^2$", "(3)" := format(round(summary(model_3)$r.squared, 2), nsmall = 2)]
results[Var == "Exchanges", "(3)" := "Gettex"]
results[Var == "Exchanges_2", "(3)" := "vs.\\ SIX"]
results[Var == "Variable", "(3)" := "NumTrds"]
results[Var == "Time period", "(3)" := "May-July"]

#Fill model 4
results[results$Var %in% c("Intercept", "t_Intercept"), "(4)" := est_tstat(coeftable_4, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(4)" := est_tstat(coeftable_4, "Intervention")]
results[Var == "$N$", "(4)" := length(model_4$residuals)]
results[Var == "$R^2$", "(4)" := format(round(summary(model_4)$r.squared, 2), nsmall = 2)]
results[Var == "Exchanges", "(4)" := "Frankfurt"]
results[Var == "Exchanges_2", "(4)" := "vs.\\ SIX"]
results[Var == "Variable", "(4)" := "Volume"]
results[Var == "Time period", "(4)" := "June"]

#Fill model 5
results[results$Var %in% c("Intercept", "t_Intercept"), "(5)" := est_tstat(coeftable_5, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(5)" := est_tstat(coeftable_5, "Intervention")]
results[Var == "$N$", "(5)" := length(model_5$residuals)]
results[Var == "$R^2$", "(5)" := format(round(summary(model_5)$r.squared, 2), nsmall = 2)]
results[Var == "Exchanges", "(5)" := "Gettex"]
results[Var == "Exchanges_2", "(5)" := "vs.\\ SIX"]
results[Var == "Variable", "(5)" := "Volume"]
results[Var == "Time period", "(5)" := "June"]

#Fill model 6
results[results$Var %in% c("Intercept", "t_Intercept"), "(6)" := est_tstat(coeftable_6, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(6)" := est_tstat(coeftable_6, "Intervention")]
results[Var == "$N$", "(6)" := length(model_6$residuals)]
results[Var == "$R^2$", "(6)" := format(round(summary(model_6)$r.squared, 2), nsmall = 2)]
results[Var == "Exchanges", "(6)" := "Frankfurt"]
results[Var == "Exchanges_2", "(6)" := "vs.\\ SIX"]
results[Var == "Variable", "(6)" := "Volume"]
results[Var == "Time period", "(6)" := "May-July"]

#Fill model 7
results[results$Var %in% c("Intercept", "t_Intercept"), "(7)" := est_tstat(coeftable_7, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(7)" := est_tstat(coeftable_7, "Intervention")]
results[Var == "$N$", "(7)" := length(model_7$residuals)]
results[Var == "$R^2$", "(7)" := format(round(summary(model_7)$r.squared, 2), nsmall = 2)]
results[Var == "Exchanges", "(7)" := "Gettex"]
results[Var == "Exchanges_2", "(7)" := "vs.\\ SIX"]
results[Var == "Variable", "(7)" := "Volume"]
results[Var == "Time period", "(7)" := "May-July"]

#Export
for (i in colnames(results)) {
  results[is.na(get(i)), (i) := ""]
}
rm(i)
results[Var %like% "t_", Var := ""]
results[Var == "Exchanges_2", Var := ""]
setnames(results, old = "Var", new = "")

title = "The effect of the intervention on trading activity"
title_export = gsub(" ", "_", title)
footnote_text = paste0("The dependent variable is the difference in log trading activity ", 
                       "(measured by the indicated variable) of the indicated securities ", 
                       "(knock-out certificates in Columns 1-5 and ", 
                       "other leverage products, e.g.\\\\ plain vanilla warrants, in Columns 6-7) ", 
                       "between the indicated exchanges during the indicated time period in 2026. ",
                       "The independent variable (``Post intervention'') is a dummy that is 1 ",
                       "starting June 16, 2026 and 0 before. ",
                       "Frankfurt and Gettex are major German securities exchanges. ",
                       "SIX is the major Swiss securities exchange. ",
                       "Parentheses contain t-statistics calculated using Newey-West standard errors.")
writeLines(text = (kbl(results, caption = title, 
                       booktabs = TRUE, linesep = "",
                       align = c("l", rep("c", (ncol(results)-1))), format = "latex", escape = FALSE, label = title_export) %>% 
                     kable_styling(latex_options = "scale_down") %>% 
                     add_header_above(c(" " = 1, "Knock-out certificates" = 5, "Other leverage products" = 2), align = rep("c", 3), escape = FALSE) %>%
                     row_spec(4, extra_latex_after = "\\addlinespace[0.3em] \\hline ") %>%
                     row_spec(8, extra_latex_after = "\\addlinespace[0.3em] \\hline ") %>%
                     footnote(general_title = "", general = footnote_text, footnote_as_chunk = TRUE, threeparttable = TRUE, escape = FALSE)), 
           con = paste0("Output/", title_export, ".tex"))
rm(model_1, model_2, model_3, model_4, model_5, model_6, model_7,
   coeftable_1, coeftable_2, coeftable_3, coeftable_4, coeftable_5, coeftable_6, coeftable_7,
   results, footnote_text, title, title_export)

#### 6. Table: The effect of the intervention announcement on trading activity ####
#Models
model_1 = lm(DiffInLogVolume_Frankfurt_SIX ~ Announcement, data_whole_sample[Type == "KO" & 
                                                                         Date >= announcement_start_event_window & 
                                                                         Date <= announcement_end_event_window])
coeftable_1 = coeftest(model_1, vcov. = NeweyWest(model_1, prewhite = FALSE, adjust = TRUE))
print(coeftable_1)

model_2 = lm(DiffInLogVolume_Gettex_SIX ~ Announcement, data_whole_sample[Type == "KO" & 
                                                                            Date >= announcement_start_event_window & 
                                                                            Date <= announcement_end_event_window])
coeftable_2 = coeftest(model_2, vcov. = NeweyWest(model_2, prewhite = FALSE, adjust = TRUE))
print(coeftable_2)

model_3 = lm(DiffInLogNumTrades_Gettex_SIX ~ Announcement, data_whole_sample[Type == "KO" & 
                                                                               Date >= announcement_start_event_window & 
                                                                               Date <= announcement_end_event_window])
coeftable_3 = coeftest(model_3, vcov. = NeweyWest(model_3, prewhite = FALSE, adjust = TRUE))
print(coeftable_3)

#Empty data.table
rnames = c("Intercept", "t_Intercept",
           "Post Announcement", "t_Announcement",
           "Variable", "Exchanges", "Time period",
           "$R^2$", "$N$")

colnames = c("Var", "(1)", "(2)", "(3)")
results = data.table(matrix(as.character(NA), nrow = length(rnames), ncol = length(colnames)))
setnames(results, colnames)
results[, Var := rnames]
rm(rnames, colnames)

#Fill model 1
results[results$Var %in% c("Intercept", "t_Intercept"), "(1)" := est_tstat(coeftable_1, "(Intercept)")]
results[results$Var %in% c("Post Announcement", "t_Announcement"), "(1)" := est_tstat(coeftable_1, "Announcement")]
results[Var == "$N$", "(1)" := length(model_1$residuals)]
results[Var == "$R^2$", "(1)" := format(round(summary(model_1)$r.squared, 2), nsmall = 2)]
results[Var == "Exchanges", "(1)" := "Frankfurt vs.\\ SIX"]
results[Var == "Variable", "(1)" := "Volume"]
results[Var == "Time period", "(1)" := "September-November 2025"]

#Fill model 2
results[results$Var %in% c("Intercept", "t_Intercept"), "(2)" := est_tstat(coeftable_2, "(Intercept)")]
results[results$Var %in% c("Post Announcement", "t_Announcement"), "(2)" := est_tstat(coeftable_2, "Announcement")]
results[Var == "$N$", "(2)" := length(model_2$residuals)]
results[Var == "$R^2$", "(2)" := format(round(summary(model_2)$r.squared, 2), nsmall = 2)]
results[Var == "Exchanges", "(2)" := "Gettex vs.\\ SIX"]
results[Var == "Variable", "(2)" := "Volume"]
results[Var == "Time period", "(2)" := "September-November 2025"]

#Fill model 3
results[results$Var %in% c("Intercept", "t_Intercept"), "(3)" := est_tstat(coeftable_3, "(Intercept)")]
results[results$Var %in% c("Post Announcement", "t_Announcement"), "(3)" := est_tstat(coeftable_3, "Announcement")]
results[Var == "$N$", "(3)" := length(model_3$residuals)]
results[Var == "$R^2$", "(3)" := format(round(summary(model_3)$r.squared, 2), nsmall = 2)]
results[Var == "Exchanges", "(3)" := "Gettex vs.\\ SIX"]
results[Var == "Variable", "(3)" := "NumTrds"]
results[Var == "Time period", "(3)" := "September-November 2025"]

#Export
for (i in colnames(results)) {
  results[is.na(get(i)), (i) := ""]
}
rm(i)
results[Var %like% "t_", Var := ""]
setnames(results, old = "Var", new = "")

title = "The effect of the intervention announcement on trading activity"
title_export = gsub(" ", "_", title)
footnote_text = paste0("The dependent variable is the difference in log trading activity ", 
                       "(measured by the indicated variable) of knock-out certificates ", 
                       "between the indicated exchanges during the indicated time period. ",
                       "The independent variable (``Post announcement'') is a dummy that is 1 ",
                       "starting October 15, 2025 and 0 before. ",
                       "Frankfurt and Gettex are major German securities exchanges. ",
                       "SIX is the major Swiss securities exchange. ",
                       "Parentheses contain t-statistics calculated using Newey-West standard errors.")
writeLines(text = (kbl(results, caption = title, 
                       booktabs = TRUE, linesep = "",
                       align = c("l", rep("c", (ncol(results)-1))), format = "latex", escape = FALSE, label = title_export) %>% 
                     kable_styling(latex_options = c("scale_down", "HOLD_position")) %>% 
                     add_header_above(c(" " = 1, "Dependent variable: Log trading activity of knock-out certificates" = 3), align = rep("c", 2), escape = FALSE) %>%
                     row_spec(4, extra_latex_after = "\\addlinespace[0.3em] \\hline ") %>%
                     row_spec(7, extra_latex_after = "\\addlinespace[0.3em] \\hline ") %>%
                     footnote(general_title = "", general = footnote_text, footnote_as_chunk = TRUE, threeparttable = TRUE, escape = FALSE)), 
           con = paste0("Output/", title_export, ".tex"))
rm(model_1, model_2, model_3,
   coeftable_1, coeftable_2, coeftable_3,
   results, footnote_text, title, title_export)

#### 7. Table: Components of the DiD ####
#Models
model_1 = lm(LogVolume_Frankfurt ~ Intervention, data[Type == "KO"])
coeftable_1 = coeftest(model_1, vcov. = NeweyWest(model_1, prewhite = FALSE, adjust = TRUE))
print(coeftable_1)

model_2 = lm(LogVolume_Gettex ~ Intervention, data[Type == "KO"])
coeftable_2 = coeftest(model_2, vcov. = NeweyWest(model_2, lag = floor(bwNeweyWest(model_2)), prewhite = FALSE, adjust = TRUE))
print(coeftable_2)

model_3 = lm(LogVolume_SIX ~ Intervention, data[Type == "KO"])
coeftable_3 = coeftest(model_3, vcov. = NeweyWest(model_3, prewhite = FALSE, adjust = TRUE))
print(coeftable_3)

model_4 = lm(LogNumTrades_Gettex ~ Intervention, data[Type == "KO"])
coeftable_4 = coeftest(model_4, vcov. = NeweyWest(model_2, lag = floor(bwNeweyWest(model_4)), prewhite = FALSE, adjust = TRUE))
print(coeftable_4)

model_5 = lm(LogNumTrades_SIX ~ Intervention, data[Type == "KO"])
coeftable_5 = coeftest(model_5, vcov. = NeweyWest(model_5, prewhite = FALSE, adjust = TRUE))
print(coeftable_5)

#Empty data.table
rnames = c("Intercept", "t_Intercept",
           "Post intervention", "t_Intervention",
           "Variable", "Exchange", "Time period",
           "$R^2$", "$N$")

colnames = c("Var", "(1)", "(2)", "(3)", "(4)", "(5)")
results = data.table(matrix(as.character(NA), nrow = length(rnames), ncol = length(colnames)))
setnames(results, colnames)
results[, Var := rnames]
rm(rnames, colnames)

#Fill model 1
results[results$Var %in% c("Intercept", "t_Intercept"), "(1)" := est_tstat(coeftable_1, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(1)" := est_tstat(coeftable_1, "Intervention")]
results[Var == "$N$", "(1)" := length(model_1$residuals)]
results[Var == "$R^2$", "(1)" := format(round(summary(model_1)$r.squared, 2), nsmall = 2)]
results[Var == "Exchange", "(1)" := "Frankfurt"]
results[Var == "Variable", "(1)" := "Volume"]
results[Var == "Time period", "(1)" := "May-July"]

#Fill model 2
results[results$Var %in% c("Intercept", "t_Intercept"), "(2)" := est_tstat(coeftable_2, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(2)" := est_tstat(coeftable_2, "Intervention")]
results[Var == "$N$", "(2)" := length(model_2$residuals)]
results[Var == "$R^2$", "(2)" := format(round(summary(model_2)$r.squared, 2), nsmall = 2)]
results[Var == "Exchange", "(2)" := "Gettex"]
results[Var == "Variable", "(2)" := "Volume"]
results[Var == "Time period", "(2)" := "May-July"]

#Fill model 3
results[results$Var %in% c("Intercept", "t_Intercept"), "(3)" := est_tstat(coeftable_3, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(3)" := est_tstat(coeftable_3, "Intervention")]
results[Var == "$N$", "(3)" := length(model_3$residuals)]
results[Var == "$R^2$", "(3)" := format(round(summary(model_3)$r.squared, 2), nsmall = 2)]
results[Var == "Exchange", "(3)" := "SIX"]
results[Var == "Variable", "(3)" := "Volume"]
results[Var == "Time period", "(3)" := "May-July"]

#Fill model 4
results[results$Var %in% c("Intercept", "t_Intercept"), "(4)" := est_tstat(coeftable_4, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(4)" := est_tstat(coeftable_4, "Intervention")]
results[Var == "$N$", "(4)" := length(model_4$residuals)]
results[Var == "$R^2$", "(4)" := format(round(summary(model_4)$r.squared, 2), nsmall = 2)]
results[Var == "Exchange", "(4)" := "Gettex"]
results[Var == "Variable", "(4)" := "NumTrds"]
results[Var == "Time period", "(4)" := "May-July"]

#Fill model 5
results[results$Var %in% c("Intercept", "t_Intercept"), "(5)" := est_tstat(coeftable_5, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(5)" := est_tstat(coeftable_5, "Intervention")]
results[Var == "$N$", "(5)" := length(model_5$residuals)]
results[Var == "$R^2$", "(5)" := format(round(summary(model_5)$r.squared, 2), nsmall = 2)]
results[Var == "Exchange", "(5)" := "SIX"]
results[Var == "Variable", "(5)" := "NumTrds"]
results[Var == "Time period", "(5)" := "May-July"]

#Export
for (i in colnames(results)) {
  results[is.na(get(i)), (i) := ""]
}
rm(i)
results[Var %like% "t_", Var := ""]
setnames(results, old = "Var", new = "")

title = "Components of the DiD"
title_export = gsub(" ", "_", title)
footnote_text = paste0("The dependent variable is the log trading activity ", 
                       "(measured by the indicated variable) of knock-out certificates ", 
                       "on the indicated exchange during the indicated time period in 2026. ",
                       "The independent variable (``Post intervention'') is a dummy that is 1 ",
                       "starting June 16, 2026 and 0 before. ",
                       "Frankfurt and Gettex are major German securities exchanges. ",
                       "SIX is the major Swiss securities exchange. ",
                       "Parentheses contain t-statistics calculated using Newey-West standard errors.")

writeLines(text = (kbl(results, caption = title, 
                       booktabs = TRUE, linesep = "",
                       align = c("l", rep("c", (ncol(results)-1))), format = "latex", escape = FALSE, label = title_export) %>% 
                     kable_styling(latex_options = "HOLD_position", font_size = 10) %>%  
                     add_header_above(c(" " = 1, "Dependent variable: Log trading activity of knock-out certificates" = 5), align = rep("c", 2), escape = FALSE) %>%
                     column_spec(1, width = "3cm") %>%
                     column_spec(2:6, width = "2cm") %>%
                     row_spec(4, extra_latex_after = "\\addlinespace[0.3em] \\hline ") %>%
                     row_spec(7, extra_latex_after = "\\addlinespace[0.3em] \\hline ") %>%
                     footnote(general_title = "", general = footnote_text, footnote_as_chunk = TRUE, threeparttable = TRUE, escape = FALSE)), 
           con = paste0("Output/", title_export, ".tex"))
rm(model_1, model_2, model_3, model_4, model_5,
   coeftable_1, coeftable_2, coeftable_3, coeftable_4, coeftable_5,
   results, footnote_text, title, title_export)

#### 8. Table: Time trends ####
#Models
model_1 = lm(DiffInLogVolume_Frankfurt_SIX ~ Intervention + TimeIndex + TimeIndex_Post, data[Type == "KO"])
coeftable_1 = coeftest(model_1, vcov. = NeweyWest(model_1, prewhite = FALSE, adjust = TRUE))
print(coeftable_1)

model_2 = lm(DiffInLogVolume_Gettex_SIX ~ Intervention + TimeIndex + TimeIndex_Post, data[Type == "KO"])
coeftable_2 = coeftest(model_2, vcov. = NeweyWest(model_2, prewhite = FALSE, adjust = TRUE))
print(coeftable_2)

model_3 = lm(DiffInLogVolume_Frankfurt_SIX ~ Intervention + TimeIndex_Pre + TimeIndex_Post, data[Type == "KO"])
coeftable_3 = coeftest(model_3, vcov. = NeweyWest(model_3, prewhite = FALSE, adjust = TRUE))
print(coeftable_3)

model_4 = lm(DiffInLogVolume_Gettex_SIX ~ Intervention + TimeIndex_Pre + TimeIndex_Post, data[Type == "KO"])
coeftable_4 = coeftest(model_4, vcov. = NeweyWest(model_4, prewhite = FALSE, adjust = TRUE))
print(coeftable_4)

model_5 = lm(DiffInLogVolume_Frankfurt_SIX ~ Intervention, data_whole_sample[Type == "KO" & Date >= alternative_start_event_window])
coeftable_5 = coeftest(model_5, vcov. = NeweyWest(model_5, prewhite = FALSE, adjust = TRUE))
print(coeftable_5)

model_6 = lm(DiffInLogVolume_Gettex_SIX ~ Intervention, data_whole_sample[Type == "KO" & Date >= alternative_start_event_window])
coeftable_6 = coeftest(model_6, vcov. = NeweyWest(model_6, prewhite = FALSE, adjust = TRUE))
print(coeftable_6)

model_7 = lm(DiffInLogVolume_Frankfurt_SIX ~ Intervention + TimeIndex, data_whole_sample[Type == "KO"])
coeftable_7 = coeftest(model_7, vcov. = NeweyWest(model_7, prewhite = FALSE, adjust = TRUE))
print(coeftable_7)

model_8 = lm(DiffInLogVolume_Gettex_SIX ~ Intervention + TimeIndex, data_whole_sample[Type == "KO"])
coeftable_8 = coeftest(model_8, vcov. = NeweyWest(model_8, prewhite = FALSE, adjust = TRUE))
print(coeftable_8)

#Empty data.table
rnames = c("Intercept", "t_Intercept",
           "Post intervention", "t_Intervention",
           "$t$", "t_Time",
           "$min(t-T_{Int}, 0)$", "t_Time_Pre",
           "$max(t-T_{Int}, 0)$", "t_Time_Post",
           "Exchanges", "Exchanges_2", 
           "Time period", "Time period_2",
           "$R^2$", "$N$")

colnames = c("Var", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)")
results = data.table(matrix(as.character(NA), nrow = length(rnames), ncol = length(colnames)))
setnames(results, colnames)
results[, Var := rnames]
rm(rnames, colnames)

#Fill model 1
results[results$Var %in% c("Intercept", "t_Intercept"), "(1)" := est_tstat(coeftable_1, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(1)" := est_tstat(coeftable_1, "Intervention")]
results[results$Var %in% c("$t$", "t_Time"), "(1)" := est_tstat(coeftable_1, "TimeIndex", 3)]
results[results$Var %in% c("$max(t-T_{Int}, 0)$", "t_Time_Post"), "(1)" := est_tstat(coeftable_1, "TimeIndex_Post", 3)]
results[Var == "$N$", "(1)" := length(model_1$residuals)]
results[Var == "$R^2$", "(1)" := format(round(summary(model_1)$r.squared, 2), nsmall = 2)]
results[Var == "Exchanges", "(1)" := "Frankfurt"]
results[Var == "Exchanges_2", "(1)" := "vs.\\ SIX"]
results[Var == "Time period", "(1)" := "May 26-"]
results[Var == "Time period_2", "(1)" := "July 26"]

#Fill model 2
results[results$Var %in% c("Intercept", "t_Intercept"), "(2)" := est_tstat(coeftable_2, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(2)" := est_tstat(coeftable_2, "Intervention")]
results[results$Var %in% c("$t$", "t_Time"), "(2)" := est_tstat(coeftable_2, "TimeIndex", 3)]
results[results$Var %in% c("$max(t-T_{Int}, 0)$", "t_Time_Post"), "(2)" := est_tstat(coeftable_2, "TimeIndex_Post", 3)]
results[Var == "$N$", "(2)" := length(model_2$residuals)]
results[Var == "$R^2$", "(2)" := format(round(summary(model_2)$r.squared, 2), nsmall = 2)]
results[Var == "Exchanges", "(2)" := "Gettex"]
results[Var == "Exchanges_2", "(2)" := "vs.\\ SIX"]
results[Var == "Time period", "(2)" := "May 26-"]
results[Var == "Time period_2", "(2)" := "July 26"]

#Fill model 3
results[results$Var %in% c("Intercept", "t_Intercept"), "(3)" := est_tstat(coeftable_3, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(3)" := est_tstat(coeftable_3, "Intervention")]
results[results$Var %in% c("$min(t-T_{Int}, 0)$", "t_Time_Pre"), "(3)" := est_tstat(coeftable_3, "TimeIndex_Pre", 3)]
results[results$Var %in% c("$max(t-T_{Int}, 0)$", "t_Time_Post"), "(3)" := est_tstat(coeftable_3, "TimeIndex_Post", 3)]
results[Var == "$N$", "(3)" := length(model_3$residuals)]
results[Var == "$R^2$", "(3)" := format(round(summary(model_3)$r.squared, 2), nsmall = 2)]
results[Var == "Exchanges", "(3)" := "Frankfurt"]
results[Var == "Exchanges_2", "(3)" := "vs.\\ SIX"]
results[Var == "Time period", "(3)" := "May 26-"]
results[Var == "Time period_2", "(3)" := "July 26"]

#Fill model 4
results[results$Var %in% c("Intercept", "t_Intercept"), "(4)" := est_tstat(coeftable_4, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(4)" := est_tstat(coeftable_4, "Intervention")]
results[results$Var %in% c("$min(t-T_{Int}, 0)$", "t_Time_Pre"), "(4)" := est_tstat(coeftable_4, "TimeIndex_Pre", 3)]
results[results$Var %in% c("$max(t-T_{Int}, 0)$", "t_Time_Post"), "(4)" := est_tstat(coeftable_4, "TimeIndex_Post", 3)]
results[Var == "$N$", "(4)" := length(model_4$residuals)]
results[Var == "$R^2$", "(4)" := format(round(summary(model_4)$r.squared, 2), nsmall = 2)]
results[Var == "Exchanges", "(4)" := "Gettex"]
results[Var == "Exchanges_2", "(4)" := "vs.\\ SIX"]
results[Var == "Time period", "(4)" := "May 26-"]
results[Var == "Time period_2", "(4)" := "July 26"]

#Fill model 5
results[results$Var %in% c("Intercept", "t_Intercept"), "(5)" := est_tstat(coeftable_5, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(5)" := est_tstat(coeftable_5, "Intervention")]
results[Var == "$N$", "(5)" := length(model_5$residuals)]
results[Var == "$R^2$", "(5)" := format(round(summary(model_5)$r.squared, 2), nsmall = 2)]
results[Var == "Exchanges", "(5)" := "Frankfurt"]
results[Var == "Exchanges_2", "(5)" := "vs.\\ SIX"]
results[Var == "Time period", "(5)" := "April 26-"]
results[Var == "Time period_2", "(5)" := "July 26"]

#Fill model 6
results[results$Var %in% c("Intercept", "t_Intercept"), "(6)" := est_tstat(coeftable_6, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(6)" := est_tstat(coeftable_6, "Intervention")]
results[Var == "$N$", "(6)" := length(model_6$residuals)]
results[Var == "$R^2$", "(6)" := format(round(summary(model_6)$r.squared, 2), nsmall = 2)]
results[Var == "Exchanges", "(6)" := "Gettex"]
results[Var == "Exchanges_2", "(6)" := "vs.\\ SIX"]
results[Var == "Time period", "(6)" := "April 26-"]
results[Var == "Time period_2", "(6)" := "July 26"]

#Fill model 7
results[results$Var %in% c("Intercept", "t_Intercept"), "(7)" := est_tstat(coeftable_7, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(7)" := est_tstat(coeftable_7, "Intervention")]
results[results$Var %in% c("$t$", "t_Time"), "(7)" := est_tstat(coeftable_7, "TimeIndex", 3)]
results[Var == "$N$", "(7)" := length(model_7$residuals)]
results[Var == "$R^2$", "(7)" := format(round(summary(model_7)$r.squared, 2), nsmall = 2)]
results[Var == "Exchanges", "(7)" := "Frankfurt"]
results[Var == "Exchanges_2", "(7)" := "vs.\\ SIX"]
results[Var == "Time period", "(7)" := "March 23-"]
results[Var == "Time period_2", "(7)" := "July 26"]

#Fill model 8
results[results$Var %in% c("Intercept", "t_Intercept"), "(8)" := est_tstat(coeftable_8, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(8)" := est_tstat(coeftable_8, "Intervention")]
results[results$Var %in% c("$t$", "t_Time"), "(8)" := est_tstat(coeftable_8, "TimeIndex", 3)]
results[Var == "$N$", "(8)" := length(model_8$residuals)]
results[Var == "$R^2$", "(8)" := format(round(summary(model_8)$r.squared, 2), nsmall = 2)]
results[Var == "Exchanges", "(8)" := "Gettex"]
results[Var == "Exchanges_2", "(8)" := "vs.\\ SIX"]
results[Var == "Time period", "(8)" := "March 23-"]
results[Var == "Time period_2", "(8)" := "July 26"]

#Export
for (i in colnames(results)) {
  results[is.na(get(i)), (i) := ""]
}
rm(i)
results[Var %like% "t_", Var := ""]
results[Var == "Exchanges_2", Var := ""]
results[Var == "Time period_2", Var := ""]
setnames(results, old = "Var", new = "")

title = "Time trends"
title_export = gsub(" ", "_", title)
footnote_text = paste0("The dependent variable is the difference in log trading volume of ", 
                       "knock-out certificates between the indicated exchanges during the indicated time period. ",
                       "The independent variables are the following: ",
                       "``Post intervention'' is a dummy that is 1 starting June 16, 2026 and 0 before. ",
                       "\\$t = 0, 1, \\\\ldots, N-2, N-1\\$ counts the number of trading days since the first day of the indicated time period. ", 
                       "\\$T_{Int}\\$ is the value of $t$ on the day of the intervention (June 16, 2026). ",
                       "\\$min(t-T_{Int}, 0) = -T_{Int}, -T_{Int}+1, \\\\ldots, 0, 0\\$ is a piecewise linear variable, ", 
                       "which counts the number of trading days until the intervention and stays 0 after the intervention. ",
                       "\\$max(t-T_{Int}, 0) = 0, 0, \\\\ldots, N-2-T_{Int}, N-1-T_{Int}\\$ is a piecewise linear variable, ", 
                       "which is 0 before the intervention and counts the number of trading days since the intervention afterwards. ",
                       "Frankfurt and Gettex are major German securities exchanges. ",
                       "SIX is the major Swiss securities exchange. ",
                       "Parentheses contain t-statistics calculated using Newey-West standard errors.")
writeLines(text = (kbl(results, caption = title, 
                       booktabs = TRUE, linesep = "",
                       align = c("l", rep("c", (ncol(results)-1))), format = "latex", escape = FALSE, label = title_export) %>% 
                     kable_styling(latex_options = c("scale_down", "HOLD_position")) %>% 
                     add_header_above(c(" " = 1, "Dependent variable: Difference in log trading volume of knock-out certificates" = 8), align = rep("c", 2), escape = FALSE) %>%
                     row_spec(10, extra_latex_after = "\\addlinespace[0.3em] \\hline ") %>%
                     row_spec(14, extra_latex_after = "\\addlinespace[0.3em] \\hline ") %>%
                     footnote(general_title = "", general = footnote_text, footnote_as_chunk = TRUE, threeparttable = TRUE, escape = FALSE)), 
           con = paste0("Output/", title_export, ".tex"))
rm(model_1, model_2, model_3, model_4, model_5, model_6, model_7, model_8,
   coeftable_1, coeftable_2, coeftable_3, coeftable_4, coeftable_5, coeftable_6, coeftable_7, coeftable_8,
   results, footnote_text, title, title_export)

#### 9. Figure: Differences in log trading volume over time ####
##### 9.1 KO: Frankfurt vs. SIX #####
a_m1 = data[Type == "KO" & Date < intervention_date, mean(DiffInLogVolume_Frankfurt_SIX)]
a_m2 = data[Type == "KO" & Date >= intervention_date, mean(DiffInLogVolume_Frankfurt_SIX)]
a = ggplot(data[Type == "KO"], aes(x = Date, y = DiffInLogVolume_Frankfurt_SIX)) + 
  geom_line() + 
  geom_point() +
  scale_x_date(date_labels = "%m/%d", breaks = seq(start_event_window + days(4), end_event_window, by = "3 weeks")) +
  scale_y_continuous(limits = c(1, 1 + 2.15)) +
  geom_segment(aes(x = start_event_window, xend = intervention_date, y = a_m1, yend = a_m1), color = "blue") +
  geom_segment(aes(x = intervention_date, xend = end_event_window, y = a_m2, yend = a_m2), color = "red") +
  geom_vline(xintercept = intervention_date, linetype = "dashed", color = "red") +
  xlab(NULL) +
  ylab("log(VolumeFrankfurt) - log(VolumeSIX)") +
  ggtitle("A: Knock-out certificates:\nFrankfurt vs. SIX") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 

##### 9.2 KO: Gettex vs. SIX #####
b_m1 = data[Type == "KO" & Date < intervention_date, mean(DiffInLogVolume_Gettex_SIX)]
b_m2 = data[Type == "KO" & Date >= intervention_date, mean(DiffInLogVolume_Gettex_SIX)]
b = ggplot(data[Type == "KO"], aes(x = Date, y = DiffInLogVolume_Gettex_SIX)) + 
  geom_line() + 
  geom_point() +
  scale_x_date(date_labels = "%m/%d", breaks = seq(start_event_window + days(4), end_event_window, by = "3 weeks")) +
  scale_y_continuous(limits = c(2.15, 2.15 + 2.15)) +
  geom_segment(aes(x = start_event_window, xend = intervention_date, y = b_m1, yend = b_m1), color = "blue") +
  geom_segment(aes(x = intervention_date, xend = end_event_window, y = b_m2, yend = b_m2), color = "red") +
  geom_vline(xintercept = intervention_date, linetype = "dashed", color = "red") +
  xlab(NULL) +
  ylab("log(VolumeGettex) - log(VolumeSIX)") +
  ggtitle("B: Knock-out certificates:\nGettex vs. SIX") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 

##### 9.3 OtherLeverage: Frankfurt vs. SIX #####
c_m1 = data[Type == "OtherLeverage" & Date < intervention_date, mean(DiffInLogVolume_Frankfurt_SIX)]
c_m2 = data[Type == "OtherLeverage" & Date >= intervention_date, mean(DiffInLogVolume_Frankfurt_SIX)]
c = ggplot(data[Type == "OtherLeverage"], aes(x = Date, y = DiffInLogVolume_Frankfurt_SIX)) + 
  geom_line() + 
  geom_point() +
  scale_x_date(date_labels = "%m/%d", breaks = seq(start_event_window + days(4), end_event_window, by = "3 weeks")) +
  scale_y_continuous(limits = c(-1.5, -1.5 + 2.15)) +
  geom_segment(aes(x = start_event_window, xend = intervention_date, y = c_m1, yend = c_m1), color = "blue") +
  geom_segment(aes(x = intervention_date, xend = end_event_window, y = c_m2, yend = c_m2), color = "red") +
  geom_vline(xintercept = intervention_date, linetype = "dashed", color = "red") +
  xlab(NULL) +
  ylab("log(VolumeFrankfurt) - log(VolumeSIX)") +
  ggtitle("C: Other leverage products:\nFrankfurt vs. SIX") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 

##### 9.4 OtherLeverage: Gettex vs. SIX #####
d_m1 = data[Type == "OtherLeverage" & Date < intervention_date, mean(DiffInLogVolume_Gettex_SIX)]
d_m2 = data[Type == "OtherLeverage" & Date >= intervention_date, mean(DiffInLogVolume_Gettex_SIX)]
d = ggplot(data[Type == "OtherLeverage"], aes(x = Date, y = DiffInLogVolume_Gettex_SIX)) + 
  geom_line() + 
  geom_point() +
  scale_x_date(date_labels = "%m/%d", breaks = seq(start_event_window + days(4), end_event_window, by = "3 weeks")) +
  scale_y_continuous(limits = c(-1.5, -1.5 + 2.15)) +
  geom_segment(aes(x = start_event_window, xend = intervention_date, y = d_m1, yend = d_m1), color = "blue") +
  geom_segment(aes(x = intervention_date, xend = end_event_window, y = d_m2, yend = d_m2), color = "red") +
  geom_vline(xintercept = intervention_date, linetype = "dashed", color = "red") +
  xlab(NULL) +
  ylab("log(VolumeGettex) - log(VolumeSIX)") +
  ggtitle("D: Other leverage products:\nGettex vs. SIX") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 

##### 9.5 Export ####
out_plot = arrangeGrob(a, b, c, d, ncol = 2)
ggsave("Output/Diff.pdf", out_plot, width = 6, height = 6)
rm(a, b, c, d, a_m1, a_m2, b_m1, b_m2, c_m1, c_m2, d_m1, d_m2, out_plot)

#### 10. Figure: Log trading activity by exchange over time ####
##### 10.1 Volume #####
a = ggplot(data_long_vol[Type == "KO"], aes(x = Date, y = LogVolume, color = Exchange)) + 
  geom_line() + 
  scale_color_manual(values = c("black", "grey50", "grey80")) +
  geom_vline(xintercept = intervention_date, linetype = "dashed", color = "red") +
  ylab("log(Volume)") +
  ggtitle("A: Trading volume (in logs)") + 
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(cor(data_whole_sample[Type == "KO" & Date < intervention_date, .(LogVolume_Frankfurt, LogVolume_Gettex, LogVolume_SIX)]))
#Pre-intervention correlation of log volume between Frankfurt and SIX: 57%
#Pre-intervention correlation of log volume between Gettex and SIX: 38%

##### 10.2 Number of trades #####
b = ggplot(data_long_num[Type == "KO"], aes(x = Date, y = LogNumTrades, color = Exchange)) + 
  geom_line() + 
  scale_color_manual(values = c("grey50", "grey80")) +
  geom_vline(xintercept = intervention_date, linetype = "dashed", color = "red") +
  ylab("log(NumTrades)") +
  ggtitle("B: Number of trades (in logs)") + 
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(cor(data_whole_sample[Type == "KO" & Date < intervention_date, .(LogNumTrades_Gettex, LogNumTrades_SIX)]))
#Pre-intervention correlation of log number of trades between Gettex and SIX: 60%

##### 10.3 Export ####
out_plot = arrangeGrob(a, b, ncol = 1)
ggsave("Output/TradingByExchange.pdf", out_plot, width = 7, height = 7)
rm(a, b, out_plot)

#### 11. Figure: Placebo interventions ####
##### 11.1 Loop over all placebo time windows #####
coef_list_fra = list()
coef_list_get = list()
for (i in 0:placebo_final_start_timeindex) {
  temp_data = data_whole_sample[Type == "KO" & TimeIndex >= i & TimeIndex < i + num_days_pre + num_days_post]
  temp_data[, Intervention := as.integer(TimeIndex >= i + num_days_pre)]
  coef_list_fra[[i+1]] = lm(DiffInLogVolume_Frankfurt_SIX ~ Intervention, temp_data)$coefficients["Intervention"]
  coef_list_get[[i+1]] = lm(DiffInLogVolume_Gettex_SIX ~ Intervention, temp_data)$coefficients["Intervention"]
  rm(temp_data)
}
coefs_frankfurt = data.table(Coef = unlist(coef_list_fra))
coefs_gettex = data.table(Coef = unlist(coef_list_get))
rm(coef_list_fra, coef_list_get, i)

##### 11.2 Frankfurt vs. SIX #####
actual_coef = lm(DiffInLogVolume_Frankfurt_SIX ~ Intervention, data[Type == "KO"])$coefficients["Intervention"]
share_smaller_actual = paste0(round(coefs_frankfurt[, mean(Coef < actual_coef)]*100, 1), "%")
share_larger_actual = paste0(round(coefs_frankfurt[, mean(Coef >= actual_coef)]*100, 1), "%")

a = ggplot(coefs_frankfurt, aes(Coef)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 40) + 
  scale_x_continuous(breaks = seq(-1, 1, 0.2)) +
  coord_cartesian(xlim = c(-0.8, 0.8)) +
  scale_y_continuous(breaks = seq(0, 0.1, 0.02), limits = c(0, 0.1), labels = function(x) paste0(round(100*x), "%")) +
  geom_vline(xintercept = actual_coef, linetype = "dashed", color = "red") +
  annotate("text",
           x = actual_coef, y = Inf,
           label = paste0("Estimate for the actual\nintervention: ", round(actual_coef, 2)),
           color = "red",
           vjust = 1,
           fontface = "bold") +
  annotate("text",
           x = actual_coef - 0.02, y = Inf,
           label = share_smaller_actual,
           hjust = 1, vjust = 5) +
  annotate("text",
           x = actual_coef + 0.02, y = Inf,
           label = share_larger_actual,
           hjust = 0, vjust = 5) +
  labs(x = "Coefficient estimate", y = "Fraction of placebo tests") +
  ggtitle("A: Frankfurt vs. SIX") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
rm(actual_coef, share_smaller_actual, share_larger_actual)

##### 11.3 Gettex vs. SIX #####
actual_coef = lm(DiffInLogVolume_Gettex_SIX ~ Intervention, data[Type == "KO"])$coefficients["Intervention"]
share_smaller_actual = paste0(round(coefs_gettex[, mean(Coef < actual_coef)]*100, 1), "%")
share_larger_actual = paste0(round(coefs_gettex[, mean(Coef >= actual_coef)]*100, 1), "%")

b = ggplot(coefs_gettex, aes(Coef)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 40) + 
  scale_x_continuous(breaks = seq(-1, 1, 0.2)) +
  coord_cartesian(xlim = c(-0.8, 0.8)) +
  scale_y_continuous(breaks = seq(0, 0.1, 0.02), limits = c(0, 0.1), labels = function(x) paste0(round(100*x), "%")) +
  geom_vline(xintercept = actual_coef, linetype = "dashed", color = "red") +
  annotate("text",
           x = actual_coef, y = Inf,
           label = paste0("Estimate for the actual\nintervention: ", round(actual_coef, 2)),
           color = "red",
           vjust = 1,
           fontface = "bold") +
  annotate("text",
           x = actual_coef - 0.02, y = Inf,
           label = share_smaller_actual,
           hjust = 1, vjust = 5) +
  annotate("text",
           x = actual_coef + 0.02, y = Inf,
           label = share_larger_actual,
           hjust = 0, vjust = 5) +
  labs(x = "Coefficient estimate", y = "Fraction of placebo tests") +
  ggtitle("B: Gettex vs. SIX") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
rm(actual_coef, share_smaller_actual, share_larger_actual)

##### 11.4 Export #####
out_plot = arrangeGrob(a, b, ncol = 1)
ggsave("Output/Placebo.pdf", out_plot, width = 4, height = 6)

#Numbers for figure description:
print(num_days_pre) #29
print(num_days_post) #34
print(nrow(coefs_frankfurt)) #742
print(data_whole_sample[Type == "KO" & TimeIndex == 0 + num_days_pre, Date]) #2023-05-03
print(data_whole_sample[Type == "KO" & TimeIndex == 0, Date]) #2023-03-20
print(data_whole_sample[Type == "KO" & TimeIndex == 0 + num_days_pre + num_days_post - 1, Date]) #2023-06-21
print(data_whole_sample[Type == "KO" & TimeIndex == 1 + num_days_pre, Date]) #2023-05-04
print(data_whole_sample[Type == "KO" & TimeIndex == 1, Date]) #2023-03-21
print(data_whole_sample[Type == "KO" & TimeIndex == 1 + num_days_pre + num_days_post - 1, Date]) #2023-06-22
print(data_whole_sample[Type == "KO" & TimeIndex == placebo_final_start_timeindex + num_days_pre, Date]) #2023-04-24
print(data_whole_sample[Type == "KO" & TimeIndex == placebo_final_start_timeindex + num_days_pre + num_days_post - 1, Date]) #2023-06-15
rm(a, b, out_plot, coefs_frankfurt, coefs_gettex)

#### 12. Figure: Knock-out volume changes from May to July ####
setorder(data_monthly, Change)
data_monthly[, Exchange := factor(Exchange, levels = data_monthly$Exchange)]
out_plot = ggplot(data_monthly, aes(x = Exchange, y = Change, label = Change_Formatted, fill = Change)) + 
  geom_col() +
  geom_text(position = position_stack(vjust = 0.5), fontface = "bold") +
  scale_fill_gradient2(low = "red", mid = "white", high = "darkgreen", midpoint = 0) +
  geom_hline(yintercept = 0, color = "black") +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  xlab(NULL) +
  ylab("Volume change from May to July 2026") +
  theme_classic() + 
  theme(legend.position = "none")
ggsave(paste0("Output/Monthly.pdf"), width = 6, height = 4)
rm(out_plot)


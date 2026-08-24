### KO: Analyses Additional ###

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
est_tstat = function(regression, regressor, digits = 2, digits_t = 2) {
  estimate = format(round(regression[regressor, "Estimate"], digits), nsmall = digits, big.mark = ",")
  t_stat = paste0("(", format(round(regression[regressor, "t value"], digits_t), nsmall = digits_t, big.mark = ","), ")")
  all_together = c(estimate, t_stat)
  return(all_together)
}

#Parameters
intervention_date = ymd("2026-06-16")
start_event_window = ymd("2026-05-01")
end_event_window = ymd("2026-07-31")

#### 2. Import ####
data_gettex = readRDS("Data/Processed_Data/Gettex.rds")
data_gettex_namebased = readRDS("Data/Processed_Data/Gettex_NameBased.rds")
data_six = readRDS("Data/Processed_Data/SIX.rds")
data_bsw = readRDS("Data/Processed_Data/BSW.rds")

#### 3. Table: Gettex robustness: Name-based identification of knock-out certificates ####
setnames(data_gettex_namebased, "Volume_Gettex", "Volume_Gettex_NB")
setnames(data_gettex_namebased, "NumTrades_Gettex", "NumTrades_Gettex_NB")
data = merge(x = data_gettex, y = data_six, by = c("Type", "Date"), all = FALSE)
data = merge(x = data, y = data_gettex_namebased, by = c("Type", "Date"), all = FALSE)
data = data[Date >= start_event_window & Date <= end_event_window]

#Variables
data[, DiffInLogVolume_Gettex_SIX := log(Volume_Gettex) - log(Volume_SIX)]
data[, DiffInLogVolume_GettexNB_SIX := log(Volume_Gettex_NB) - log(Volume_SIX)]
data[, DiffInLogNumTrades_Gettex_SIX := log(NumTrades_Gettex) - log(NumTrades_SIX)]
data[, DiffInLogNumTrades_GettexNB_SIX := log(NumTrades_Gettex_NB) - log(NumTrades_SIX)]
data[, Intervention := as.integer(Date >= intervention_date)]

#Models
model_1 = lm(DiffInLogVolume_Gettex_SIX ~ Intervention, data[Type == "KO"])
coeftable_1 = coeftest(model_1, vcov. = NeweyWest(model_1, prewhite = FALSE, adjust = TRUE))
print(coeftable_1)

model_2 = lm(DiffInLogVolume_GettexNB_SIX ~ Intervention, data[Type == "KO"])
coeftable_2 = coeftest(model_2, vcov. = NeweyWest(model_2, prewhite = FALSE, adjust = TRUE))
print(coeftable_2)

model_3 = lm(DiffInLogVolume_Gettex_SIX ~ Intervention, data[Type == "KO" & month(Date) == 6])
coeftable_3 = coeftest(model_3, vcov. = NeweyWest(model_3, prewhite = FALSE, adjust = TRUE))
print(coeftable_3)

model_4 = lm(DiffInLogVolume_GettexNB_SIX ~ Intervention, data[Type == "KO" & month(Date) == 6])
coeftable_4 = coeftest(model_4, vcov. = NeweyWest(model_4, prewhite = FALSE, adjust = TRUE))
print(coeftable_4)

model_5 = lm(DiffInLogNumTrades_Gettex_SIX ~ Intervention, data[Type == "KO"])
coeftable_5 = coeftest(model_5, vcov. = NeweyWest(model_5, prewhite = FALSE, adjust = TRUE))
print(coeftable_5)

model_6 = lm(DiffInLogNumTrades_GettexNB_SIX ~ Intervention, data[Type == "KO"])
coeftable_6 = coeftest(model_6, vcov. = NeweyWest(model_6, prewhite = FALSE, adjust = TRUE))
print(coeftable_6)

model_7 = lm(DiffInLogVolume_Gettex_SIX ~ Intervention, data[Type == "OtherLeverage"])
coeftable_7 = coeftest(model_7, vcov. = NeweyWest(model_7, prewhite = FALSE, adjust = TRUE))
print(coeftable_7)

model_8 = lm(DiffInLogVolume_GettexNB_SIX ~ Intervention, data[Type == "OtherLeverage"])
coeftable_8 = coeftest(model_8, vcov. = NeweyWest(model_8, prewhite = FALSE, adjust = TRUE))
print(coeftable_8)

#Empty data.table
rnames = c("Intercept", "t_Intercept",
           "Post intervention", "t_Intervention",
           "Variable", "Gettex KO ident", "Time period",
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
results[Var == "Gettex KO ident", "(1)" := "CFI"]
results[Var == "Variable", "(1)" := "Volume"]
results[Var == "Time period", "(1)" := "May-July"]

#Fill model 2
results[results$Var %in% c("Intercept", "t_Intercept"), "(2)" := est_tstat(coeftable_2, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(2)" := est_tstat(coeftable_2, "Intervention")]
results[Var == "$N$", "(2)" := length(model_2$residuals)]
results[Var == "$R^2$", "(2)" := format(round(summary(model_2)$r.squared, 2), nsmall = 2)]
results[Var == "Gettex KO ident", "(2)" := "Name"]
results[Var == "Variable", "(2)" := "Volume"]
results[Var == "Time period", "(2)" := "May-July"]

#Fill model 3
results[results$Var %in% c("Intercept", "t_Intercept"), "(3)" := est_tstat(coeftable_3, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(3)" := est_tstat(coeftable_3, "Intervention")]
results[Var == "$N$", "(3)" := length(model_3$residuals)]
results[Var == "$R^2$", "(3)" := format(round(summary(model_3)$r.squared, 2), nsmall = 2)]
results[Var == "Gettex KO ident", "(3)" := "CFI"]
results[Var == "Variable", "(3)" := "Volume"]
results[Var == "Time period", "(3)" := "June"]

#Fill model 4
results[results$Var %in% c("Intercept", "t_Intercept"), "(4)" := est_tstat(coeftable_4, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(4)" := est_tstat(coeftable_4, "Intervention")]
results[Var == "$N$", "(4)" := length(model_4$residuals)]
results[Var == "$R^2$", "(4)" := format(round(summary(model_4)$r.squared, 2), nsmall = 2)]
results[Var == "Gettex KO ident", "(4)" := "Name"]
results[Var == "Variable", "(4)" := "Volume"]
results[Var == "Time period", "(4)" := "June"]

#Fill model 5
results[results$Var %in% c("Intercept", "t_Intercept"), "(5)" := est_tstat(coeftable_5, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(5)" := est_tstat(coeftable_5, "Intervention")]
results[Var == "$N$", "(5)" := length(model_5$residuals)]
results[Var == "$R^2$", "(5)" := format(round(summary(model_5)$r.squared, 2), nsmall = 2)]
results[Var == "Gettex KO ident", "(5)" := "CFI"]
results[Var == "Variable", "(5)" := "NumTrds"]
results[Var == "Time period", "(5)" := "May-July"]

#Fill model 6
results[results$Var %in% c("Intercept", "t_Intercept"), "(6)" := est_tstat(coeftable_6, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(6)" := est_tstat(coeftable_6, "Intervention")]
results[Var == "$N$", "(6)" := length(model_6$residuals)]
results[Var == "$R^2$", "(6)" := format(round(summary(model_6)$r.squared, 2), nsmall = 2)]
results[Var == "Gettex KO ident", "(6)" := "Name"]
results[Var == "Variable", "(6)" := "Volume"]
results[Var == "Time period", "(6)" := "May-July"]

#Fill model 7
results[results$Var %in% c("Intercept", "t_Intercept"), "(7)" := est_tstat(coeftable_7, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(7)" := est_tstat(coeftable_7, "Intervention")]
results[Var == "$N$", "(7)" := length(model_7$residuals)]
results[Var == "$R^2$", "(7)" := format(round(summary(model_7)$r.squared, 2), nsmall = 2)]
results[Var == "Gettex KO ident", "(7)" := "CFI"]
results[Var == "Variable", "(7)" := "Volume"]
results[Var == "Time period", "(7)" := "May-July"]

#Fill model 8
results[results$Var %in% c("Intercept", "t_Intercept"), "(8)" := est_tstat(coeftable_8, "(Intercept)")]
results[results$Var %in% c("Post intervention", "t_Intervention"), "(8)" := est_tstat(coeftable_8, "Intervention")]
results[Var == "$N$", "(8)" := length(model_8$residuals)]
results[Var == "$R^2$", "(8)" := format(round(summary(model_8)$r.squared, 2), nsmall = 2)]
results[Var == "Gettex KO ident", "(8)" := "Name"]
results[Var == "Variable", "(8)" := "Volume"]
results[Var == "Time period", "(8)" := "May-July"]

#Export
for (i in colnames(results)) {
  results[is.na(get(i)), (i) := ""]
}
rm(i)
results[Var %like% "t_", Var := ""]
setnames(results, old = "Var", new = "")

title = "Gettex robustness: Name-based identification of knock-out certificates"
title_export = gsub(" ", "_", title)
title_export = gsub("-", "_", title_export)
title_export = gsub(":", "", title_export)
footnote_text = paste0("The dependent variable is the difference in log trading activity ", 
                       "(measured by the indicated variable) of the indicated securities ", 
                       "(knock-out certificates in Columns 1-6 and ", 
                       "other leverage products, e.g.\\\\ plain vanilla warrants, in Columns 7-8) ", 
                       "between Gettex (a major German exchange) and SIX (the major Swiss exchange) ", 
                       "during the indicated time period in 2026. ",
                       "The independent variable (``Post intervention'') is a dummy that is 1 ",
                       "starting June 16, 2026 and 0 before. ",
                       "Odd-numbered columns identify knock-out certificates (other leverage products) ", 
                       "in the Gettex data using ISO 10962 Classification of Financial Instruments, CFI, ", 
                       "codes ``RFxTxx'' (``RWxxxx'' and ``RFxMxx''). ",
                       "Even-numbered columns start with the same identification but ", 
                       "also identify securites as knock-out certificates, ", 
                       "when the CFI code is ``RWxxxx'' and the security name contains any of the following: ",
                       "``TUR.'', ``TURBO'', ``MINI FUTURE'', ", 
                       "``TBULL'', ``TBEAR'', ``TOBULL'', ``TOBEAR'', ", 
                       "``MBULL'', ``MBEAR'', ``INLINE'', ``INLOS''. ",
                       "Parentheses contain t-statistics calculated using Newey-West standard errors.")
writeLines(text = (kbl(results, caption = title, 
                       booktabs = TRUE, linesep = "",
                       align = c("l", rep("c", (ncol(results)-1))), format = "latex", escape = FALSE, label = title_export) %>% 
                     kable_styling(latex_options = c("scale_down", "HOLD_position")) %>% 
                     add_header_above(c(" " = 1, "Knock-out certificates" = 6, "Other leverage products" = 2), align = rep("c", 3), escape = FALSE) %>%
                     row_spec(4, extra_latex_after = "\\addlinespace[0.3em] \\hline ") %>%
                     row_spec(7, extra_latex_after = "\\addlinespace[0.3em] \\hline ") %>%
                     footnote(general_title = "", general = footnote_text, footnote_as_chunk = TRUE, threeparttable = TRUE, escape = FALSE)), 
           con = paste0("Output/", title_export, ".tex"))
rm(data, data_gettex_namebased, data_six,
   model_1, model_2, model_3, model_4, model_5, model_6, model_7, model_8,
   coeftable_1, coeftable_2, coeftable_3, coeftable_4, coeftable_5, coeftable_6, coeftable_7, coeftable_8,
   results, footnote_text, title, title_export)

#### 4. Figure: Gettex : Aggregated vs. reported data ####
##### 4.1 Preparation #####
data_gettex[, Month := ceiling_date(Date, unit = "months") - days(1)]
data_gettex_monthly = data_gettex[Type == "KO" & Month > ymd("2023-03-31"), 
                             .(Vol_Aggregated = sum(Volume_Gettex) / 1000,
                               NumTrades_Aggregated = sum(NumTrades_Gettex)), by = Month]
data_gettex_monthly = merge(x = data_gettex_monthly, 
                            y = data_bsw[Exchange == "Gettex", .(Month = Date, Vol_Reported = Volume, NumTrades_Reported = NumberTrades)],
                            by = "Month", all = FALSE)

##### 4.2 Volume #####
plot_data = melt(data_gettex_monthly[, .(Month, Vol_Aggregated, Vol_Reported)], id.vars = "Month", variable.name = "Source", value.name = "Volume")
plot_data[, Source := gsub("Vol_", "", Source)]
plot_data[, Volume := Volume / 1000000]
a = ggplot(plot_data, aes(x = Month, y = Volume, color = Source)) + 
  geom_line() +
  scale_color_manual(values = c("black", "grey50")) +
  ylab("Volume (in billion euros)") +
  ggtitle("A: Trading volume") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
cor(data_gettex_monthly[, .(Vol_Aggregated, Vol_Reported)]) #99.6%
rm(plot_data)

##### 4.3 Number of trades #####
plot_data = melt(data_gettex_monthly[, .(Month, NumTrades_Aggregated, NumTrades_Reported)], id.vars = "Month", variable.name = "Source", value.name = "NumTrades")
plot_data[, Source := gsub("NumTrades_", "", Source)]
plot_data[, NumTrades := NumTrades / 1000000]
b = ggplot(plot_data, aes(x = Month, y = NumTrades, color = Source)) + 
  geom_line() +
  scale_color_manual(values = c("black", "grey50")) +
  ylab("Number of trades (in million trades)") +
  ggtitle("B: Number of trades") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
cor(data_gettex_monthly[, .(NumTrades_Aggregated, NumTrades_Reported)]) #99.8%
rm(plot_data, data_gettex_monthly, data_gettex)

##### 4.4 Export #####
out_plot = arrangeGrob(a, b, ncol = 1)
ggsave("Output/Gettex_AggregatedVsReported.pdf", out_plot, width = 6, height = 6)
rm(a, b, out_plot)

#### 5. Back-of-the-envelope calulation: The effect of the intervention on retail losses ####
##### 5.1 Bafin: Losses from 2021 until 2023 #####
#All information from Bafin study
bafin_total_sum_losses = 3.4*10^9 #Total losses from 2019 until 2023
bafin_yearly = data.table(Year = 2019:2023,
                          N_Transactions = c(
                            9593556,
                            21080313,
                            24847440,
                            31456143,
                            26270324))
bafin_yearly[, Share := N_Transactions / sum(N_Transactions)]
print(bafin_yearly[Year %in% 2021:2023, sum(Share)]) 
#73% of trading happening from 2021 to 2023.

#Distribute losses across years according to the share that
#the year's transactions make up of the total transactions from 2019 until 2023.
bafin_yearly[, Losses := bafin_total_sum_losses * Share]

#Average yearly losses 2021 until 2023:
bafin_avg_yearly_losses_2021_2023 = bafin_yearly[Year %in% 2021:2023, mean(Losses)]
print(bafin_avg_yearly_losses_2021_2023)
print(format(bafin_avg_yearly_losses_2021_2023, big.mark = ","))
#826,362,906 euros average retail losses per year from 2021 until 2023.

##### 5.2 Bafin x BSW: Losses in 2025 #####
bsw_average_volume_2021_2023 = data_bsw[Exchange == "Total" & year(Date) %in% 2021:2023, mean(Volume)*12]
print(bsw_average_volume_2021_2023)
print(format(bsw_average_volume_2021_2023*1000, big.mark = ","))
#26,801,201,486 euros average annual volume of knock-outs from 2021 to 2023.
#Note: Use mean times 12 instead of sum because July 2023 is missing.

bsw_volume_2025 = data_bsw[Exchange == "Total" & year(Date) == 2025, mean(Volume)*12]
print(format(bsw_volume_2025*1000, big.mark = ","))
#41,152,611,000 euros average trading volume of outstanding knock-outs in 2026.

bsw_volume_growth_from2123_to_26 = bsw_volume_2025 / bsw_average_volume_2021_2023
print(bsw_volume_growth_from2123_to_26)
#54% growth from Bafin to last pre-intervention observation

#Assume that losses grew proportional with the volume:
bafin_x_bsw_implied_yearly_losses_2025 = bafin_avg_yearly_losses_2021_2023 *bsw_volume_growth_from2123_to_26
print(bafin_x_bsw_implied_yearly_losses_2025)
print(format(bafin_x_bsw_implied_yearly_losses_2025, big.mark = ","))
#1,268,860,698 euros implied losses per year in 2026.

##### 5.3 Bafin x BSW x our estimate: Losses saved due to the intervention #####
#Conservatively use -0.31 from Frankfurt as lower estimate:
intervention_percentage_reduction = exp(-0.31) - 1 
print(intervention_percentage_reduction)
#27%

#Retail losses saved:
retail_losses_saved_yearly = intervention_percentage_reduction*bafin_x_bsw_implied_yearly_losses_2025
print(retail_losses_saved_yearly)
print(format(retail_losses_saved_yearly, big.mark = ","))
#338,218,681 euros in retail losses saved per year.


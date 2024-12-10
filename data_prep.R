#' [Author: Geoff Mayhew]
#' [Purpose: Prep of data for analyses]

library(data.table)   # for data wrangling
library(ggplot2)      # for plotting
library(lubridate)    # for easy manipulation of date/times

#======================================================================================================================#
# Subset and Format Data ####
#======================================================================================================================#

#' Load raw data: `hlbt_covar_pull` object
load("data/hlbt_covar_pull.rdata")

# This project will focus on A80 EFP decksort operations (program code R16). Remove irrelevant columns.
hlbt_dat <- hlbt_covar_pull[HAUL_PURPOSE_CODE == "R16", -c(
    "SSC_A", "SSC_C", "CONDITION_CODE", "PERCENT_OF_HOOKS_NOT_PRIMARY", 
    "A_TEMP", "W_TEMP", "RELEASE_CODE_A", "RELEASE_CODE_C",
    "IS_SORTING_BEGIN_TIME_EST", "IS_SORTING_END_TIME_EST", "IS_TIME_NET_LANDED_EST"
  )
]

# Duplicate rows based on number_of_animals, so each individual = 1 row
hlbt_dat <- hlbt_dat[rep(hlbt_dat[, .I], times = hlbt_dat[, NUMBER_OF_ANIMALS])] 
hlbt_dat[, NUMBER_OF_ANIMALS := NULL]

# Calculate assessment time in minutes. 
hlbt_dat[, ASSESSMENT_TIME := ASSESS_DAY * 1440 + ASSESS_HOUR * 60  + ASSESS_MIN + ASSESS_SEC / 60]

# Format trawl dataset. Remove unknown viablities (only 11 cases)
hlbt_dat <- hlbt_dat[
][VIABILITY != 'U'
][, VIABILITY := factor(VIABILITY, levels = c("E", "P", "D"), ordered = TRUE)
][, TOW_DUR := as.numeric(RETRV - DEPLOY, units = "hours")]

# Create an identifier for each haul by combing Cruise, Permit, and Haul_Seq
hlbt_dat[, CRUISE.PERMIT.HAUL := paste(CRUISE, PERMIT, HAUL_SEQ, sep = ".")]

#======================================================================================================================#
# Data Corrections ####
#======================================================================================================================#

#' There are *a lot* of data corrections because there were not validation rules created for any of the data in the
#' norpac.ATL_HALIBUT_ tables. Therefore, there are many keypunch errors in [ASSESSMENT_TIME], [TIME_NET_LANDED_ON_DECK],
#' and [SORTING_END_TIME]. I identify these errors by looking for obvious outliers or data that doesn't make sense 
#' (like sorts that start before retrieval). Some have obvious fixes whereas others will require checking the arhived
#' deckforms, or else throwing out the data. 

#==================#
# Helper Functions #
#==================#

#' Streamline the in-place corrections to hlbt_dat using some shorthand functions.

# This function creates the haul-level summary 'sort_time_check' by re-evaluating changes hlbt_dat. It also updates
# hlbt_dat via in-place modification of 'x'
recheck1 <- function(x = hlbt_dat) {
  x[
  ][, SORT_DUR := as.numeric(SORTING_END_TIME - TIME_NET_LANDED_ON_DECK, units = "mins")
  ][, LAST_HAL := max(ASSESSMENT_TIME), by = .(CRUISE.PERMIT.HAUL)]
  setorder(x, PERMIT, RETRV, ASSESSMENT_TIME)
  sort_time_check <- unique(x[, .(CRUISE.PERMIT.HAUL, VESSEL, DEPLOY, RETRV, TIME_NET_LANDED_ON_DECK, SORTING_END_TIME, SORT_DUR, LAST_HAL, PRESORTED_NUMBER)])
  sort_time_check[
  ][, RETRV_TO_TNLOD := as.numeric(TIME_NET_LANDED_ON_DECK - RETRV, units = "mins")
  ][, SORT_LAST := SORT_DUR - LAST_HAL][]
  assign(x = "sort_time_check", value = sort_time_check, pos = .GlobalEnv)
}
recheck1()

# This function allows me to add or subtract values to specified time column
fix_time <- function(dat = hlbt_dat, cph, columns, fun, val) {
  if(any(!sapply(dat[, ..columns], is.POSIXct))) stop("Not all 'columns' are POSIXct class!")
  if(length(val) > 1) stop("value has to be length 1.")
  if(nrow(dat[CRUISE.PERMIT.HAUL == cph]) == 0) stop("No records found!")
  
  # Modify in place. Changes to 'dat' will be reflected in 'hlbt_dat'
  dat[CRUISE.PERMIT.HAUL == cph, (columns) := lapply(.SD, function(x) x + do.call(fun, list(val))), .SDcols = columns]
}


#==========================================#
## Sorting Time/Time Net Landed on Deck ####
#==========================================#

#' Because the time between the retrieval and TIME_NET_LANDED_ON_DECK is important (essentially extra tow duration), 
#' these have to be corrected.

#===================#
### Missing Data ####
#===================#

#' 138 hauls are missing decksort start and end times, 955 halibut assessments. Unless we can find sort start and end,
#' we will have to remove them
#'* sort_time_check[is.na(TIME_NET_LANDED_ON_DECK)]  *
no_sort_time <- hlbt_dat[is.na(TIME_NET_LANDED_ON_DECK) | is.na(SORTING_END_TIME)]
hlbt_dat <- fsetdiff(hlbt_dat, no_sort_time)

# Some hauls are missing time, but not date, and therefore cannot be used. (10 halibut)
no_net_land_time <- format(sort_time_check[, TIME_NET_LANDED_ON_DECK], format = "%H:%M:%S")  == "00:00:00"
no_sort_end_time <- format(sort_time_check[, SORTING_END_TIME], format = "%H:%M:%S")  == "00:00:00"
no_sort_time2 <- hlbt_dat[CRUISE.PERMIT.HAUL %in% sort_time_check[no_net_land_time & no_sort_end_time]$CRUISE.PERMIT.HAUL]
hlbt_dat <- fsetdiff(hlbt_dat, no_sort_time2)


# Initialize the omit_dat which is where all omitted data will be placed
omit_dat <- rbind(no_sort_time, no_sort_time2)
recheck1()

#==================#
### Corrections ####
#==================#

# Check that sort times are after gear retrieval times
# 95% of decksorts take between 7 and 95 minutes, at least before corretions
quantile(sort_time_check$RETRV_TO_TNLOD, probs = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975))
sort_cols <- c("TIME_NET_LANDED_ON_DECK", "SORTING_END_TIME")

#' [Sort times wrong because they didn't account for the date change between retrieval and sort. Adding 1 day.]
if(T) {
  
  head(sort_time_check[order(RETRV_TO_TNLOD)], 6)
  head(sort_time_check[order(RETRV_TO_TNLOD)], 6)$CRUISE.PERMIT.HAUL
  
  # Sort teams clearly 1 day off (sort starts between 720 and 1440+ hours BEFORE retrieval)
  sort_time_add_1_day <- c(
    "21655.1879.526",  "21548.34249.50", "21774.2733.812",
    "21631.2110.274",  "21548.34249.154", "21548.34249.702", "21548.34249.664", "21631.2110.296",  "21548.34249.316",
    "21631.2110.280", "21655.1879.296", "21655.1879.490", "21821.34249.1014", "22087.2110.1258",  "21548.34249.678",
    "21579.3367.142",  "21783.3694.422",  "21548.34249.654", "22159.1996.1132", "21548.34249.640",
    "21548.34249.584", "21661.2800.272",  "21821.34249.1438", "21821.34249.810",  "21548.34249.346",  "22159.1996.950",
    "21783.3694.370",  "21786.1996.624",  "21548.34249.712", "21548.34249.228", "21774.2733.1024",
    "21821.34249.844", "21786.1996.784",  "22085.3367.1332", "21790.3369.654",  "21548.34249.666", "21548.34249.690",
    "21774.2733.732",  "21968.1610.1174", "21783.3694.276",  "21790.3369.640",  "22280.2733.2702", "21986.2733.1312",
    "22145.661.1766",  "21783.3694.364",  "21783.3694.428",  "22181.2733.2090", "21821.34249.798", "21661.2800.306",
    "21808.1879.1150", "21655.1879.582",  "22145.661.1682",  "21579.3367.332",  "21548.34249.440", "21786.1996.828",
    "21776.4635.550",  "21655.1879.510",  "21579.3367.180",  "21821.34249.742", "21692.2733.486",  "21783.3694.292",
    "21790.3369.402",  "22009.3694.1084", "22009.3694.1058", "21548.34249.724", "22142.4635.1644", "22280.2733.2790",
    "21616.661.6",     "21692.2733.440",  "21776.4635.516",  "22280.2733.2622", "21783.3694.246",  "21631.2110.312",
    "21579.3367.354",  "21878.5822.620",  "21808.1879.1148", "21808.1879.700",  "21579.3367.92",   "21860.2800.1008",
    "21836.3367.960",  "21968.1610.1114", "21790.3369.888",  "21968.1610.1354", "21774.2733.550",  "21836.3367.846", 
    "22182.4635.1828", "22182.4635.1870", "21808.1879.958",  "21156.4635.3088", "21156.4635.3236", "22146.1610.1538", 
    "22216.4092.1688", "21561.5822.356",  "21848.661.590",  "21968.1610.1128", "21579.3367.110",  "21848.661.1126", 
    "22046.4092.1546", "22182.4635.1842", "21968.1610.1404", "21655.1879.480", "21968.1610.1000", "21156.4635.3076", 
    "22215.5822.1846", "21741.661.482", "21824.1610.558", "22087.2110.1156", "22087.2110.1124", "21941.3369.1062", 
    "22168.3694.1456", "21679.4092.232", "21156.4635.2956", "21860.2800.1034", "22142.4635.1456", "22145.661.1544", 
    "21582.1610.374", "21582.1610.390", "22046.4092.1330", "21616.661.176", "21848.661.912", "21808.1879.872", 
    "21786.1996.630", "21962.661.1298", "21968.1610.1286", "21968.1610.1314", "21790.3369.686", "22168.3694.1396", 
    "21783.3694.578", "22064.4635.1192", "21561.5822.242", "21924.5822.1290", "21848.661.906", "22215.5822.1870", 
    "22146.1610.1606", "21655.1879.540", "22257.34249.2568", "21724.3694.126", "21832.4092.922", "21962.661.1242",
    "21776.4635.856", "21924.5822.966", "21924.5822.1018", "21808.1879.1090", "22215.5822.1832", "22009.3694.1164", 
    "22066.34249.1598", "22116.3369.1472", "21312.3694.1872", "21924.5822.900", "22009.3694.1144", "22009.3694.1220"
  )
  if(any(duplicated(sort_time_add_1_day))) message("You have duplicate HAUL_IDs in `sort_time_add_1_day`")
  
  # Apply the fixes to each of the above hauls
  for(i in sort_time_add_1_day) fix_time(cph = i, columns = sort_cols, fun = "days", val = 1)
  recheck1()
  
}

# [Sort times that appear to be 12 hours off (incorrectly writing the 'AM' version isntead of PM)]. *Verify these!*
if(T){
  # The hauls seem to be a case where the observer wrote the 'AM' date instead of PM. Will double-check these
  sort_time_add_12_hrs <- c(
    "22116.3369.1440", "22140.5822.1520", "21568.2123.276", "21924.5822.1044", "21631.2110.308",
    "21728.5822.522", "21921.3819.424", "21848.661.896",  "21713.4092.460", "21247.661.3634", "21776.4635.620",
    "22084.5822.1358", "21822.2123.372",  "22269.3367.1918", "22258.3367.1880", "21713.4092.470",  "22269.3367.1964",
    "22217.3369.1864", "22269.3367.1954", "22217.3369.1832", "21728.5822.542",  "21776.4635.684",  "22064.4635.1236"
  )
  # hlbt_dat <- copy(og_dat)   
  
  if(any(duplicated(sort_time_add_12_hrs))) message("You have duplicate HAUL_IDs in `sort_time_add_12_hrs`")
  # Apply the fixes to each of the above hauls
  for(i in sort_time_add_12_hrs) fix_time(cph = i, columns = sort_cols, fun = "hours", val = 12)
  recheck1()
  
}

#' Daylight savings at `02:00` In spring, 02:00 to 03:00, In Fall, 02:00 to 01:00`
#' [2016: Mar 13 - Nov 6] 
#' [2017: Mar 12 - Nov 5]

# Make a list of CPH to verify deckforms (no clear corrections. Will omit these unless we verify

#' *Find the deckforms*. These hauls are probably fine - They have a suspect RETRV time because the sort times make more 
#' sense with the next deploy, or the tow durations seem abnormally long and RETRV was probably recorded after the
#' decksort completed.
suspect_retrv_keep <- c(
  "21077.3694.1520", "21821.34249.1498", "21848.661.1140", "21817.2110.868", 
  "21923.2110.892", "21986.2733.1330", "21616.661.70",  "21616.661.332", "22181.2733.2340", "21548.34249.404",
  "22091.3835.1940", "21000.661.3256", "22085.3367.1126"
)

#' *Find the deckforms* Until then, omit.
suspect_sort_omit <- c(
  "21987.3835.1760", #' Sort end time makes sense with next haul deploy, but time net landed must be wrong, something around 12:55?
  "21579.3367.112"  #' Sort times are the same as the previous haul (21579.3367.110)
)
hlbt_dat <- hlbt_dat[!(CRUISE.PERMIT.HAUL %in% suspect_sort_omit)]

og_dat <- copy(hlbt_dat)

#' [Likely keypunch errors, such as sort times being off by 1 hour or reading a 9 as a 4 or vice versa.] *Verify these!*
#' Many of these are cases where the sort time makes more sense to fit with the next DEPLOY time.
if(T){
  
  # The code below was used to help identify and fix data with suspect fishing/sorting times
  # head(sort_time_check[order(RETRV_TO_TNLOD)], 6)
  # head(sort_time_check[order(RETRV_TO_TNLOD)], 6)$CRUISE.PERMIT.HAUL
  
  # This function returns the previous 2 and next 2 haul sequences for a given haul
  cph_check <- function(x) {
    # x <- "22116.3369.1548"
    cph_split <- unlist(strsplit(x, split = "[.]"))
    paste(cph_split[1], cph_split[2], as.numeric(cph_split[3]) + c(-4, -2, 0, 2, 4), sep = ".")
  }
  
  # cph <- "21821.34249.1498"
  # sort_time_check[CRUISE.PERMIT.HAUL %in% cph_check(cph)]
  # hlbt_dat[CRUISE.PERMIT.HAUL == cph]

  fix_time(cph = "21579.3367.240", columns = sort_cols, fun = "hours", val = 1)  # Daylight savings, observer was consistently 1 hour behind
  fix_time(cph = "21579.3367.238", columns = sort_cols, fun = "hours", val = 1)  # Daylight savings, observer was consistently 1 hour behind
  fix_time(cph = "21579.3367.236", columns = sort_cols, fun = "hours", val = 1)  # Daylight savings, observer was consistently 1 hour behind
  fix_time(cph = "21579.3367.234", columns = sort_cols, fun = "hours", val = 1)  # Daylight savings, observer was consistently 1 hour behind
  fix_time(cph = "21579.3367.232", columns = sort_cols, fun = "hours", val = 1)  # Daylight savings, observer was consistently 1 hour behind
  fix_time(cph = "21579.3367.230", columns = sort_cols, fun = "hours", val = 1)  # Daylight savings, observer was consistently 1 hour behind
  fix_time(cph = "21579.3367.226", columns = sort_cols, fun = "hours", val = 1)  # Daylight savings, observer was consistently 1 hour behind
  fix_time(cph = "21579.3367.224", columns = sort_cols, fun = "hours", val = 1)  # Daylight savings, observer was consistently 1 hour behind
  fix_time(cph = "21579.3367.220", columns = sort_cols, fun = "hours", val = 1)  # Daylight savings, observer was consistently 1 hour behind
  fix_time(cph = "21579.3367.218", columns = sort_cols, fun = "hours", val = 1)  # Daylight savings, observer was consistently 1 hour behind
  fix_time(cph = "21579.3367.216", columns = sort_cols, fun = "hours", val = 1)  # Daylight savings, observer was consistently 1 hour behind
  fix_time(cph = "21579.3367.214", columns = sort_cols, fun = "hours", val = 1)  # Daylight savings, observer was consistently 1 hour behind
  fix_time(cph = "21579.3367.212", columns = sort_cols, fun = "hours", val = 1)  # Daylight savings, observer was consistently 1 hour behind
  fix_time(cph = "21579.3367.208", columns = sort_cols, fun = "hours", val = 1)  # Daylight savings, observer was consistently 1 hour behind
  fix_time(cph = "22116.3369.1454", columns = "TIME_NET_LANDED_ON_DECK", fun = "minutes", val = 40)  # Changed sort start minutes from 01 to 41.
  fix_time(cph = "21544.4635.362", columns = sort_cols, fun = "hours", val = 1) # Vessel seemed to have little down time, so this correction makes the most sense.
  fix_time(cph = "21968.1610.984", columns = sort_cols, fun = "hours", val = 1)
  fix_time(cph = "21544.4635.310", columns = sort_cols, fun = "hours", val = 1) # Mar 12: Daylight Savings? observer didn't turn clock back?
  fix_time(cph = "22182.4635.1876", columns = sort_cols, fun = "hours", val = 1)
  fix_time(cph = "21582.1610.296", columns = "TIME_NET_LANDED_ON_DECK", fun = "hours", val = 1) # Mar 12: Daylight Savings? Changed start hour from 22 to 23
  fix_time(cph = "21474.34249.860", columns = sort_cols, fun = "hours", val = 1)
  fix_time(cph = "21561.5822.266", columns = sort_cols, fun = "hours", val = 1)
  fix_time(cph = "22046.4092.1048", columns = sort_cols, fun = "hours", val = 1)
  fix_time(cph = "22085.3367.1556", columns = "TIME_NET_LANDED_ON_DECK", fun = "hours", val = 1) # Changed sort start hour from 10 to 11
  fix_time(cph = "22101.2110.1586", columns = sort_cols, fun = "hours", val = 1)
  fix_time(cph = "21445.661.3830", columns = sort_cols, fun = "hours", val = 1)
  fix_time(cph = "22064.4635.1136", columns = sort_cols, fun = "hours", val = 1)
  fix_time(cph = "21661.2800.422", columns = sort_cols, fun = "hours", val = 1) 
  fix_time(cph = "21344.2110.2058", columns = sort_cols, fun = "hours", val = 1)
  fix_time(cph = "21544.4635.348", columns = sort_cols, fun = "hours", val = 1)
  fix_time(cph = "21579.3367.222", columns = sort_cols, fun = "hours", val = 1)
  fix_time(cph = "22116.3369.1594", columns = sort_cols, fun = "hours", val = 1)
  fix_time(cph = "21544.4635.366", columns = sort_cols, fun = "hours", val = 1)
  fix_time(cph = "21790.3369.612", columns = sort_cols, fun = "hours", val = 1) 
  fix_time(cph = "21544.4635.364", columns = sort_cols, fun = "hours", val = 1)
  fix_time(cph = "21790.3369.598", columns = "TIME_NET_LANDED_ON_DECK", fun = "hours", val = 1) # Changed hour 8 to 9
  fix_time(cph = "21345.3367.6448", columns = sort_cols, fun = "hours", val = 1)
  fix_time(cph = "21655.1879.456", columns = sort_cols, fun = "hours", val = 1)
  fix_time(cph = "21783.3694.226", columns = sort_cols, fun = "hours", val = 1)
  fix_time(cph = "22009.3694.1112", columns = sort_cols, fun = "hours", val = 2) # Fixing 24-hour conversion
  fix_time(cph = "21655.1879.342", columns = sort_cols, fun = "hours", val = 2) # Changing hour 13 to 15
  fix_time(cph = "22181.2733.2146", columns = sort_cols, fun = "hours", val = 2) # Changing hour 7 tp 9
  fix_time(cph = "21836.3367.826", columns = "TIME_NET_LANDED_ON_DECK", fun = "hours", val = 2) # Changing hour 13 to 15
  fix_time(cph = "22146.1610.1588", columns = "TIME_NET_LANDED_ON_DECK", fun = "hours", val = 2) # Fixing 24-hour conversion
  fix_time(cph = "21803.2800.544", columns = sort_cols, fun = "hours", val = 2) # Fixing 24-hour conversion
  fix_time(cph = "21305.4092.3396", columns = sort_cols, fun = "hours", val = 2) # Changing hour from 13 to 15
  fix_time(cph = "21548.34249.692", columns = sort_cols, fun = "hours", val = 2) # Changing hour from 4 to 9
  fix_time(cph = "21808.1879.1056", columns = sort_cols, fun = "hours", val = 2) # Fixing 24-hour conversion
  fix_time(cph = "21568.2123.220", columns = sort_cols, fun = "hours", val = 2) # Fixing 24-hour conversion
  fix_time(cph = "21631.2110.400", columns = sort_cols, fun = "hours", val = 2) # Changing hour from 3 to 5
  fix_time(cph = "21924.5822.1276", columns = sort_cols, fun = "hours", val = 2) # Fixing 24-hour conversion
  fix_time(cph = "22280.2733.2694", columns = sort_cols, fun = "hours", val = 2) # Fixing 24-hour conversion
  fix_time(cph = "21817.2110.782", columns = sort_cols, fun = "hours", val = 2) # Changing hour 3 to 5
  fix_time(cph = "21481.2733.2724", columns = sort_cols, fun = "hours", val = 2) # Fixing 24-hour conversion
  fix_time(cph = "21561.5822.250", columns = sort_cols, fun = "hours", val = 2) # Fixing 24-hour conversion
  fix_time(cph = "22116.3369.1548", columns = sort_cols, fun = "hours", val = 2) # Fixing 24-hour conversion
  fix_time(cph = "21817.2110.582", columns = sort_cols, fun = "hours", val = 2) # Fixing 24-hour conversion
  fix_time(cph = "21741.661.574", columns = sort_cols, fun = "hours", val = 4) # Changing hour from 15 to 19

}


## Check Sort Durations ####

# In cases where SORTING_END_TIME is very long, and no obvious correction exists, we can still leave these in because
# we will simply truncate the sort durations of all long sorts to 35 minutes.

#' *Check these deckforms!* The sort times are quite long, probably the SORTING_END_TIME, and may either be data entry
#' errors or real.
deckform_check_sort_end_time <- c(
  "22085.3367.1572",   #' Super long sort time, probably a data entry error
  "21445.661.3798"    #' Long sort time? 61 min, last halibut at 14.68
)
#sort_time_check[!(CRUISE.PERMIT.HAUL %in% deckform_check_sort_end_time)][order(-SORT_DUR)][36:41]
#sort_time_check[CRUISE.PERMIT.HAUL %in% cph_check("21445.661.3798") ]

#' *Check these deckforms*
fix_time(cph = "21655.1879.284", columns = "SORTING_END_TIME", fun = "minutes", val = -50) # Assuming sort duration was 11 min, not 1 hour 1 min. Subtracting 11 min from 61
fix_time(cph = "21077.3694.1136", columns = "SORTING_END_TIME", fun = "minutes", val = -47) # Assuming sort duration was 20:38 min, not sorting end time of 20:38. Subtracting 21 from 68
fix_time(cph = "22066.34249.1598", columns = "SORTING_END_TIME", fun = "minutes", val = -72) # Assuming sort duration was 12:60min, not sorting end time of 1:26. Subtracting 13 from 85
fix_time(cph = "21848.661.802", columns = "SORTING_END_TIME", fun = "minutes", val = -163) # Assuming sort duration was 11:15, not SORTING_END_TIME. Subtracting 12 min from 175
fix_time(cph = "21344.2110.1676", columns = "SORTING_END_TIME", fun = "minutes", val = -282) # Assuming sort duration was 12:54, not the sort end time. Subtracting 13min from 295
fix_time(cph = "21924.5822.802", columns = "SORTING_END_TIME", fun = "minutes", val = -206) # Sort time was same as last halibut. Fixing the sort end time as 20.1 min - SORT_DUR
fix_time(cph = "21692.2733.404", columns = "SORTING_END_TIME", fun = "minutes", val = -45) # Sort time was probably 10:54, not the time end. Subtracting the difference (11min from 56)
fix_time(cph = "21248.3369.3426", columns = "SORTING_END_TIME", fun = "minutes", val = -50) # Sort end time added 1 hour 1 min instead of 11 min
fix_time(cph = "21553.1610.270", columns = "SORTING_END_TIME", fun = "minutes", val = -45) # Sort end time (13:36) was last halibut time. Subtracting the difference (14 min from 59)
fix_time(cph = "21247.661.3592", columns = "SORTING_END_TIME", fun = "hours", val = -1) # Sort end time hours was 21 instead of 10
fix_time(cph = "22181.2733.2410", columns = "SORTING_END_TIME", fun = "hours", val = -1) # Sort end time hours was 23 instead of 22
fix_time(cph = "21203.1610.1052", columns = "SORTING_END_TIME", fun = "hours", val = -1) # Sort end time hours was 06 instead of 15
fix_time(cph = "21803.2800.646", columns = "SORTING_END_TIME", fun = "hours", val = -1) # Sort end time hours was 03 instead of 02
fix_time(cph = "21203.1610.1288", columns = "SORTING_END_TIME", fun = "hours", val = -1) # Sort end time hours was 11 instead of 10
fix_time(cph = "21924.5822.996", columns = "SORTING_END_TIME", fun = "hours", val = -1) # Sort end time hours was 20 instead of 19
fix_time(cph = "21962.661.1270", columns = "SORTING_END_TIME", fun = "hours", val = -1) # Sort end time hours was 23 instead of 22
fix_time(cph = "22146.1610.1596", columns = "SORTING_END_TIME", fun = "hours", val = -1) # Sort end time hours was 23 instead of 22
fix_time(cph = "22140.5822.1552", columns = "SORTING_END_TIME", fun = "hours", val = -1) # Sort end time hours was 15 instead of 14
fix_time(cph = "21077.3694.1178", columns = "SORTING_END_TIME", fun = "hours", val = -1) # Sort end time hours was 05 instead of 04
fix_time(cph = "21408.1996.142", columns = "SORTING_END_TIME", fun = "hours", val = -1) # Sort end time hours was 14 instead of 13
fix_time(cph = "21344.2110.1836", columns = "SORTING_END_TIME", fun = "hours", val = -2) # Sort end time hours was 23 instead of 21
fix_time(cph = "21783.3694.500", columns = "SORTING_END_TIME", fun = "hours", val = -2) # Sort end time hours was 08 instead of 06
fix_time(cph = "21305.4092.3396", columns = "SORTING_END_TIME", fun = "hours", val = -2) # Sort end time hours was 18 instead of 16
fix_time(cph = "21790.3369.842", columns = "SORTING_END_TIME", fun = "hours", val = -7) # Sort end time hours was 19 instead of 12
fix_time(cph = "22181.2733.2202", columns = "SORTING_END_TIME", fun = "hours", val = -9) # Sort end time hours was 23 instead of 14
fix_time(cph = "21808.1879.828", columns = "SORTING_END_TIME", fun = "hours", val = -10) # Sort end time hours was 15 instead of 5
fix_time(cph = "22066.34249.1598", columns = "SORTING_END_TIME", fun = "hours", val = -10) # Sort end time hours was 11 instead of 1
fix_time(cph = "21561.5822.334", columns = "SORTING_END_TIME", fun = "hours", val = -12) # Sort end time hours was 19 instead of 7
fix_time(cph = "21247.661.3626", columns = "SORTING_END_TIME", fun = "hours", val = -18) # Sort end time hours was 20 instead of 2
fix_time(cph = "22066.34249.1700", columns = "SORTING_END_TIME", fun = "days", val = -1) # Sort end time day was 23 isntead of 22
fix_time(cph = "22146.1610.1606", columns = "SORTING_END_TIME", fun = "days", val = -1) # Sort end time day was 28 instead of 27
fix_time(cph = "22064.4635.1138", columns = "SORTING_END_TIME", fun = "days", val = -18) # Sort end time day was 31 instead of 13
fix_time(cph = "21808.1879.1092", columns = "SORTING_END_TIME", fun = "days", val = -20) # Sort end time day was 22 instead of 02
fix_time(cph = "21548.34249.592", columns = "SORTING_END_TIME", fun = "days", val = -21) # Sort end time day was 23 instead of 02
fix_time(cph = "21808.1879.1102", columns = "SORTING_END_TIME", fun = "days", val = -27) # Sort end time day was 30 instead of 03
fix_time(cph = "22116.3369.1334", columns = "SORTING_END_TIME", fun = "months", val = -1) # Sort end time month was 09 instead of 08
fix_time(cph = "22181.2733.2212", columns = "SORTING_END_TIME", fun = "years", val = -2) # Sort end time year was 2019 instead of 2017
fix_time(cph = "21924.5822.934", columns = "SORTING_END_TIME", fun = "years", val = -403) # Sort end time year was 2420 instead of 2017
fix_time(cph = "20917.3369.2736", columns = "SORTING_END_TIME", fun = "years", val = -1000) # Sort end time year was 3016 instead of 2016
fix_time(cph = "22168.3694.1656", columns = "SORTING_END_TIME", fun = "minutes", val = 30) # Sort end time minutes changed from 24 to 54
fix_time(cph = "20917.3369.2736", columns = "SORTING_END_TIME", fun = "days", val = 1) # Sort end time did not account for date change
fix_time(cph = "21548.34249.592", columns = "SORTING_END_TIME", fun = "days", val = 1) # Sort end time did not account for date change
fix_time(cph = "21924.5822.934", columns = "SORTING_END_TIME", fun = "days", val = 138) # Sort end date was 02-06 instead of 06-24


recheck1()
sort_time_check[order(SORT_DUR)]  
#' Some sort durations are very long, which won't matter when we truncate decksorts to 35 minutes. Others have
#' questionable SORTING_END_TIME, in which cases we will replace with LAST_HAL time.

# For hauls with Assessment Times greater than sort time, look for corrections
sort_time_check[order(SORT_LAST)] #' 
hlbt_cols <- c("CRUISE.PERMIT.HAUL", "LENGTH_SIZE", "VIABILITY", "ASSESS_MIN", "ASSESS_SEC", "ASSESSMENT_TIME", "SORT_DUR")

hlbt_dat[CRUISE.PERMIT.HAUL == "21987.3835.1804", ..hlbt_cols]  # Probably intended 16 minutes instead of 46 min?
hlbt_dat[CRUISE.PERMIT.HAUL == "22168.3694.1384", ..hlbt_cols]  # Probably intended 3 min 2 sec instead of 30 min 2 sec?


#=====================#
## Assessment Time ####
#=====================#
hlbt_dat[order(ASSESSMENT_TIME)] # only 1 halibut with a crazy high assessment time

# Assessment Time of 8 min 41 was entered as 8 hours 41 min
hlbt_dat[
  CRUISE.PERMIT.HAUL == "21474.34249.878" & ASSESSMENT_TIME == 521, 
  ':=' (ASSESS_HOUR = 0, ASSESS_MIN = 8, ASSESS_SEC = 41, ASSESSMENT_TIME = 8 + 41/60)]


#=====================#
#' *MOVE ME*
## More Sort Times ####
#=====================#

cph <- "21077.3694.1520"
sort_time_check[CRUISE.PERMIT.HAUL %in% cph_check(cph)]
hlbt_dat[CRUISE.PERMIT.HAUL == cph]

#' TODO What is the difference between `deckform_check_sort_end_time` and `suspect_retrv_keep`? Latter is just to help with some outlier searching but probably okay?


# SORT Long after RETRV

suspect_sort_omit <- c(
  suspect_sort_omit,
  "21803.2800.538",   #' Sort times are definitely wrong, Probably should be 6:30 and 6:40 instead of 19:30 and 19:40 but a 13 hour difference is strange.
  "21579.3367.202",   #' Recorded the sorting times for previous haul, 21579.3367.198. Have to find deckforms
  "21803.2800.758",   #' Sort time very likely wrong, as it is before previous decksort. No obvious fix.
  "21582.1610.432"    #' Sort times with no obvious fix, perhaps 14:00+, but a 45 min difference between RETRV and sorting start is uncharacteristic
)

fix_time(cph = "21848.661.1138", columns = sort_cols, fun = "hours", val = -10) # Incorrectly, added 10 hours (19 instead of 9())
fix_time(cph = "22258.3367.1864", columns = sort_cols, fun = "hours", val = -12) # Incorrectly converted times from AM to PM
fix_time(cph = "22258.3367.1840", columns = sort_cols, fun = "hours", val = -12) # Incorrectly converted times from AM to PM
fix_time(cph = "22258.3367.1856", columns = sort_cols, fun = "hours", val = -12) # Incorrectly converted times from AM to PM
fix_time(cph = "21432.1610.1894", columns = sort_cols, fun = "hours", val = -12) # Incorrectly converted times from AM to PM
fix_time(cph = "22269.3367.1936", columns = sort_cols, fun = "hours", val = -12) # Incorrectly converted times from AM to PM
fix_time(cph = "22269.3367.1934", columns = sort_cols, fun = "hours", val = -12) # Incorrectly converted times from AM to PM
fix_time(cph = "21000.661.3216", columns = sort_cols, fun = "hours", val = -12) # Incorrectly converted times from AM to PM
fix_time(cph = "21655.1879.234", columns = sort_cols, fun = "hours", val = -12) # Incorrectly converted times from AM to PM
fix_time(cph = "21878.5822.616", columns = sort_cols, fun = "hours", val = -6) # Wrote 2 pm as 20:00 instead of 14:00
fix_time(cph = "21878.5822.614", columns = sort_cols, fun = "hours", val = -12) # Incorrectly converted times from AM to PM
fix_time(cph = "22009.3694.1180", columns = sort_cols, fun = "days", val = -1) # Sort times switched with "22009.3694.1178".
fix_time(cph = "22009.3694.1178", columns = sort_cols, fun = "days", val = -1) # Sort times switched with "22009.3694.1180"
fix_time(cph = "21579.3367.212", columns = sort_cols, fun = "days", val = -1) # Sort date was ahead by 1 day
fix_time(cph = "21679.4092.316", columns = sort_cols, fun = "days", val = -1) # Sort date was ahead by 1 day
fix_time(cph = "22181.2733.2278", columns = sort_cols, fun = "days", val = -1) # Sort date was ahead by 1 day
#' Sort times for ["21745.3367.542"] and ["21745.3367.544"] were switched
hlbt_dat[CRUISE.PERMIT.HAUL == "21745.3367.542", ":=" (TIME_NET_LANDED_ON_DECK = as.POSIXct("2017-05-02 23:28:00", tz = "GMT"), SORTING_END_TIME = as.POSIXct("2017-05-02 23:47:00", tz = "GMT"))]
hlbt_dat[CRUISE.PERMIT.HAUL == "21745.3367.544", ":=" (TIME_NET_LANDED_ON_DECK = as.POSIXct("2017-05-03 04:31:00", tz = "GMT"), SORTING_END_TIME = as.POSIXct("2017-05-03 04:51:00", tz = "GMT"))]
#' Sort times for ["22009.3694.1178"] and ["22009.3694.1180"] were switched
hlbt_dat[CRUISE.PERMIT.HAUL == "22009.3694.1178", ":=" (TIME_NET_LANDED_ON_DECK = as.POSIXct("2017-08-15 20:34:00", tz = "GMT"), SORTING_END_TIME = as.POSIXct("2017-08-15 20:39:00", tz = "GMT"))]
hlbt_dat[CRUISE.PERMIT.HAUL == "22009.3694.1180", ":=" (TIME_NET_LANDED_ON_DECK = as.POSIXct("2017-08-16 00:16:00", tz = "GMT"), SORTING_END_TIME = as.POSIXct("2017-08-16 00:25:00", tz = "GMT"))]

#======================#
# How are we doing? ####
#======================#

hlbt_dat <- hlbt_dat[!(CRUISE.PERMIT.HAUL %in% suspect_sort_omit)]


recheck1()
test1 <- copy(hlbt_dat)
test1[, TIME_IN_WATER := as.numeric(TIME_NET_LANDED_ON_DECK - DEPLOY, units = "mins")]
test1[, TIME_IN_WATER_HR := TIME_IN_WATER/60] # in hours
test1[, RETRV_TO_SORT := as.numeric(TIME_NET_LANDED_ON_DECK - RETRV, units = "mins")  ] # in hours

ggplot(test1, aes(x = ASSESSMENT_TIME, fill = VIABILITY)) + geom_density(alpha = 0.8)
ggplot(test1, aes(x = TIME_IN_WATER, fill = VIABILITY)) + geom_density(alpha = 0.8)
ggplot(test1, aes(x = ASSESSMENT_TIME + TIME_IN_WATER, fill = VIABILITY)) + geom_density(alpha = 0.8) + xlim(0, 500)
ggplot(test1, aes(x = ASSESSMENT_TIME * TIME_IN_WATER, fill = VIABILITY)) + geom_density(alpha = 0.8) + xlim(0, 10000)  # Product seems to do better

# Larger hauls have higher mortality, could also be from tow duration?
ggplot(test1, aes(x = HAUL_MT, fill = VIABILITY)) + geom_density(alpha = 0.8) 
# Correlation isn't that tight
ggplot(unique(test1[, .(CRUISE.PERMIT.HAUL, TOW_DUR, HAUL_MT)]), aes(x = HAUL_MT, y = TOW_DUR)) + geom_point(alpha = 0.8) 
unique(test1[, .(CRUISE.PERMIT.HAUL, TOW_DUR, HAUL_MT)])[, cor(TOW_DUR, HAUL_MT)] # wow, not tightly correlated at all!
ggplot(unique(test1[, .(CRUISE.PERMIT.HAUL, TOW_DUR, RETRV_TO_SORT)]), aes(x = TOW_DUR, y = RETRV_TO_SORT)) + geom_point()

library(ordinal)

mod0 <- clm(VIABILITY ~ 1, data = test1)
mod1 <- clm(VIABILITY ~ ASSESSMENT_TIME, data = test1)
mod2 <- clm(VIABILITY ~ ASSESSMENT_TIME + TOW_DUR, data = test1)
mod3 <- clm(VIABILITY ~ ASSESSMENT_TIME + TIME_IN_WATER_HR, data = test1)
mod4 <- clm(VIABILITY ~ ASSESSMENT_TIME + TOW_DUR + RETRV_TO_SORT, data = test1)

AIC(mod0, mod1, mod2, mod3, mod4) # Using TIME_IN_WATER Is better than just TOW_DUR. Separately is still better
diff(AIC(mod0, mod1, mod2, mod3, mod4)$AIC)

# Mixed models?
if(F){
  mod1.m <- clmm(VIABILITY ~ ASSESSMENT_TIME + (1|CRUISE.PERMIT.HAUL), data = test1)  # took like 20 minutes
  mod2.m <- clmm(VIABILITY ~ ASSESSMENT_TIME + TOW_DUR + (1|CRUISE.PERMIT.HAUL), data = test1)
  mod3.m <- clmm(VIABILITY ~ ASSESSMENT_TIME + TIME_IN_WATER_HR + (1|CRUISE.PERMIT.HAUL), data = test1)
  mod4.m <- clmm(VIABILITY ~ ASSESSMENT_TIME + TOW_DUR + RETRV_TO_SORT + (1|CRUISE.PERMIT.HAUL), data = test1)
}

AIC(mod0, mod1, mod2, mod3, mod4, mod1.m, mod2.m, mod3.m, mod4.m)  # Mixed model lowers AIC a bunch!
diff(AIC(mod0, mod1, mod2, mod3, mod4, mod1.m, mod2.m, mod3.m, mod4.m)$AIC)
# Having tow duration and retrv to sort doesnt seem to change much in terms of AIC (250 points but )


diff(AIC(mod3, mod3.m)$AIC)  # mod3 (TIME_IN_WATER) gets more of a decrease with the mixed model than for TOW_DUR + RETRV_TO_SORT
diff(AIC(mod4, mod4.m)$AIC)

coef(mod3.m)
coef(mod4.m)

# through model building, would likely find TIME_IN_WATER lowers AIC more ethan either TOW_DUR or RETRV_TO_SRT alone

AIC(
  clm(VIABILITY ~ ASSESSMENT_TIME, data = test1),                    #' [198230.8]
  clm(VIABILITY ~ ASSESSMENT_TIME + TOW_DUR, data = test1),          #'  196352.9
  clm(VIABILITY ~ ASSESSMENT_TIME + RETRV_TO_SORT, data = test1),    #'  195474.0
  clm(VIABILITY ~ ASSESSMENT_TIME + TIME_IN_WATER_HR, data = test1), #' *194659.4*
  clm(VIABILITY ~ ASSESSMENT_TIME * TIME_IN_WATER_HR, data = test1), #' *194642.6* # interaction throws warnings and hardly affects AIC
  clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT, data = test1)           #'  195895.3 time in water has lower AIC than HAUL_MT, RETRV_TO_SORT comparable. Last time HAUL_MT came out first
) 

#' TODO Does accounting for `VESSEL` or `OBSERVER` effects help? Have not yet pulled observer_seq but we can...

coef(mod1)
coef(mod1.m) # coefficients are quite different. How this translate to mortality will be interesting.


coef(mod3)
coef(mod3.m) # assessment time and time in water are both stronger, curious to see how it plays out using model modes

# coef for assessment time is consistent here
coef(mod2)
coef(mod3) 
coef(mod4)

summary(mod1)
summary(mod1.m)

summary(mod2)
summary(mod3)
summary(mod3.m)
summary(mod4)

#=============================#
## EVERYTHING BELOW IS OLD ####





# List of HAUL_IDs to omit due to not having good TIME_NET_LANDED or SORTING_END_TIME duration

hlbt_dat[HAUL_ID == 1946] # previous HAUL_SEW 430 retrieved at 04-03 11:05. TIME_NET_LANDED and SORTING_TIME must have wrong hours.

hlbt_dat[RETRV > TIME_NET_LANDED_ON_DECK & RETRV > SORTING_END_TIME]



hlbt_dat[CRUISE==22181 & PERMIT== 2733 & HAUL_SEQ==2202,] # No clear correction. Last halibut was  9.52 min. Find deck sheet. 2202 retrieved 12:45, 2204 deployed 15:05 and 2206 deployed 14:35. Vessel fishes with 2 nets?
# RETRV is 12:45, TNLOD = 14:01, SET = 23:32
# Possible that SET should be 13:32, not 23:32? And then TNLOD and SET were flipped?
# This would put RETRV: 12:45, TNLOD: 13:32, SET: 14:01 (~ 30 minute decksort time)



hlbt_dat[CRUISE==21344 & PERMIT== 2110 & HAUL_SEQ==1676,] # No clear correction. Last halibut was  2.13 min. Find deck sheet.
hlbt_dat[CRUISE==21924 & PERMIT== 5822 & HAUL_SEQ== 802,] # No clear correction. Last halibut was 20.02 min. Find deck sheet.
hlbt_dat[CRUISE==21987 & PERMIT== 3835 & HAUL_SEQ==1760,] # No clear correction. Last halibut was  7.62 min. Find deck sheet. TIME_NET_LANDED < RETRV
hlbt_dat[CRUISE==22168 & PERMIT== 3694 & HAUL_SEQ==1526,] # Probably correct - All assessed halibut were 3+ hours on deck,
hlbt_dat[CRUISE==21848 & PERMIT==  661 & HAUL_SEQ== 802,] # No clear correction. Last halibut was  9.13 min. Find deck sheet.
hlbt_dat[CRUISE==22085 & PERMIT== 3367 & HAUL_SEQ==1572,] # No clear correction. Last halibut was 11.60 min. Find deck sheet.
hlbt_dat[CRUISE==21081 & PERMIT== 4092 & HAUL_SEQ==3210,] # Probably correct - Last halibut assessed ~2.5 hours on deck.
hlbt_dat[CRUISE==21305 & PERMIT== 4092 & HAUL_SEQ==3396,] # TIME_NET_LANDED < RETRV. Find deck sheet. TIME_NET_LANDED probably 15:58 instead of 13:58
hlbt_dat[CRUISE==22146 & PERMIT== 1610 & HAUL_SEQ==1588,] # No clear correction. Last halibut was 06.15 min. Find deck sheet. Hours may be 22 instead of 20?
hlbt_dat[CRUISE==21783 & PERMIT== 3694 & HAUL_SEQ== 500,] # No clear correction. Last halibut was 08.72 min. Find deck sheet. Hours may be 06 instead of 08?



#' TODO *Air Temperature data pull?*

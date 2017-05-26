############################################################################################################################################################
# PROJECT : In day patient predictions

# DETAILS : Holt winters was used for forecasting daily patient admissions
#           based on previous data.
#           Split data for financial year 16/17 into quartiles based on number of admissions. 
#           Then split by day. Found average of admissions per hour for each quartile and found the weightings. 
#           The 1st prediction is broken down hourly based on the weightings from the quartile the prediction falls under. 
#           The actual figures are fed in at certain times in the day and weightings may be adjusted based on where the actual cumulative figure now lies.

# AUTHOR  : Amir Munaf

# DATE    : 24/05/17
############################################################################################################################################################

setwd ("C:/Users/AMunaf/Desktop/In day predictions/A & E hourly prediction adjustments")

#install.packages("forecast")
#install.packages("RODBC")

library("forecast")
library("RODBC")

############################################################################################################################################################
# IMPORT DATA FROM SQL
############################################################################################################################################################

# IMPORT LIVE ACTUAL ADMISSIONS

dbhandle <-
  odbcDriverConnect('driver={SQL Server};server=SRHTDW2DB;database=inquire;trusted_connection=true')
actuals <- sqlQuery(
  dbhandle,
  "
  ;with h as (
  select DATEPART(hh,AdmissionDTExternal) AdmissionHour
  , COUNT(*) Total
  from inquire.[dbo].[CurrentIP]
  where MethodOfAdmission in ('AE','GP')
  and CAST(AdmissionDTExternal AS DATE) = CAST(GETDATE() AS DATE)
  GROUP BY DATEPART(hh,AdmissionDTExternal)
  )
  SELECT hour_val AS AdmissionHour
  , DATEPART(dw, GETDATE()) AS DayOfWeek
  , ISNULL(Total,0) AS Total
  FROM inquire.[dbo].HourCount
  LEFT OUTER JOIN h
  ON hour_val = AdmissionHour
  ORDER BY hour_val
  "
)

dbhandle <-
  odbcDriverConnect(
    'driver={SQL Server};server=SRHTDW2DB;database=srft_dwsources;trusted_connection=true'
  )
forecasted <- sqlQuery(
  dbhandle,
  "
  select AdmissionDate
  , datepart(dw, AdmissionDate) DayOfWeek
  , count(*) Total
  from srft_dwsources.pas.ProviderSpells
  where AdmissionMethod in ('AE','EM')
  and AdmissionDate >='20160408'
  group by AdmissionDate
  , datepart(dw, AdmissionDate)
  order by AdmissionDate"
)

dw <- format(Sys.time(), "%u")
current_hour <- as.numeric(format(Sys.time(), "%H"))

#################################################################################################################
# SUBSET EACH DAY - HOLT WINTERS FOR FORECASTED VALUES
################################################################################################################
if (dw == 1) {
      pa <-
      holt(
      subset(forecasted, DayOfWeek == 1)[, 3],
      alpha = 0.2,
      beta = 0.2,
      initial = "simple",
      h = 1,
      l.start = 104,
      b.start = 1
      )
      day_actuals <- subset(actuals, DayOfWeek == 1)
      day <- NULL
      day$lq_hour <- c(3.857143,3.142857,3.142857,1.285714,1.428571,1.142857,0.857143,1.285714,1.428571,2,2.428571,2.428571,3.285714,4.428571,4.857143,3.857143,4.285714,4.285714,4,4.714286,5.857143,5.285714,4.571429,4.285714)
      day$iqr_hour <- c(4.08,3.92,2.8,2.88,2.16,1.92,1.88,1.28,1.76,1.88,2.48,2.6,4.36,4.76,5.28,6.32,5.88,4.52,5.52,4.2,6.28,5.48,4.64,5.52)
      day$uq_hour <- c(4,3.772727,3.090909,3.272727,2.863636,2.454545,2.090909,1.454545,1.590909,2.181818,2.636364,4,5.181818,6.090909,7.590909,6.863636,7.681818,7.409091,6.409091,6.363636,7.181818,8.136364,6.727273,7)
      day_actuals <- data.frame(day_actuals,day)  
  
} else if (dw == 2) {
      pa <-
      holt(
      subset(forecasted, DayOfWeek == 2)[, 3],
      alpha = 0.2,
      beta = 0.8,
      initial = "simple",
      h = 1,
      l.start = 93,
      b.start = 9
    )
      day_actuals <- subset(actuals, DayOfWeek == 2)
      
      day <- NULL
      day$lq_hour <- c(3.833333,3.5,5.333333,1.833333,1.666667,1.833333,1.333333,1.5,1,1.5,1.333333,2,3,4.333333,3.666667,6,4.333333,5.5,4.666667,4.833333,3.666667,4.666667,4.166667,5.833333)
      day$iqr_hour <- c(4.736842,4.052632,3.473684,2.473684,2.631579,2.263158,1.684211,1.315789,1.105263,1.473684,2.315789,2.631579,3.842105,4.368421,5.947368,5.421053,5.736842,5.263158,5.315789,5.105263,5.473684,5.789474,4.631579,5.947368)
      day$uq_hour <- c(4.793103,4.827586,4.344828,3.586207,2.586207,2.068966,1.862069,1.482759,1.62069,2.62069,2.62069,3.724138,4.206897,4.827586,6,6.344828,6.896552,5.275862,5.344828,5.448276,6.655172,6.896552,6.310345,5.689655)
      day_actuals <- data.frame(day_actuals,day)
      
} else if (dw == 3) {
      pa <-
      holt(
      subset(forecasted, DayOfWeek == 3)[, 3],
      alpha = 0.4,
      beta = 0.2,
      initial = "simple",
      h = 1,
      l.start = 85,
      b.start = 5
      )
    day_actuals <- subset(actuals, DayOfWeek == 3)
    day <- NULL
    day$lq_hour <- c(4,3.222222,2.777778,1.888889,2.333333,1.777778,1.444444,2.222222,1.666667,1.333333,1.444444,2.777778,3,3.666667,4.777778,5.333333,4.111111,5,4.444444,3.111111,5.333333,4.555556,4.444444,4.444444)
    day$iqr_hour <- c(4.08,4.84,3.72,3.04,2.6,1.72,1.44,1.44,1.36,1.52,2.2,3.24,3.68,3.96,4.92,5.2,5.4,5.48,4.56,4.92,5.24,5.04,5.28,5.84)
    day$uq_hour <- c(4.842105,4.210526,4.473684,3.157895,3.368421,2.105263,1.315789,1.368421,2.105263,2,2.684211,3.263158,4.684211,4.421053,5.421053,6.578947,6.789474,5.526316,6.368421,5.789474,6.526316,6.263158,6.526316,5.789474)
    day_actuals <- data.frame(day_actuals,day)
  
} else if (dw == 4) {
      pa <-
      holt(
      subset(forecasted, DayOfWeek == 3)[, 3],
      alpha = 0.4,
      beta = 0.2,
      initial = "simple",
      h = 1,
      l.start = 82,
      b.start = 9
      )
     day_actuals <- subset(actuals, DayOfWeek == 4)
     day <- NULL
     day$lq_hour <- c(4.75,3.625,2.75,3.375,2.625,1.5,1,2.125,0.875,1.625,2.375,3.125,3.25,3.5,4.875,3.625,4,4.25,6.375,2.25,4.25,3.75,5.25,5)
     day$iqr_hour <- c(4.529412,3.676471,3.823529,2.941176,2.5,1.882353,1.911765,1.029412,1.441176,2,2.529412,3.323529,3.088235,4.411765,5.294118,5.411765,5.558824,4.911765,4.647059,4.794118,5.558824,5.352941,5.147059,5.235294)
     day$uq_hour <- c(4.5,4.666667,4,4.166667,2.166667,2.583333,2.25,1.5,2,1.416667,2.25,3.5,4.666667,5,5.916667,6.5,6.25,5.916667,7.666667,5.25,6.5,7.583333,6,5.583333)
     day_actuals <- data.frame(day_actuals,day)
     
} else if (dw == 5) 
        {holt(subset(forecasted, DayOfWeek == 5)[,3], 
        alpha = 0.4, 
        beta = 0.2, 
        initial = "simple", 
        h = 1, l.start = 95, 
        b.start = -7
        )
       day_actuals <- subset(actuals, DayOfWeek == 5)
  
        day <- NULL
        day$lq_hour <- c(2.714286,4,3.285714,1.857143,2,1.142857,1.142857,0.857143,1,1.714286,2.428571,1.714286,4.142857,3.285714,5,4.571429,5.142857,4.285714,5.285714,5,3.285714,6.571429,4.857143,4.571429)
        day$iqr_hour <- c(4.676471,3.529412,2.882353,3.352941,2.205882,1.852941,1.647059,1.352941,1.176471,1.558824,1.852941,2.5,3.647059,4.676471,5.088235,6.088235,5.529412,5.323529,6.264706,5.323529,5.794118,5.617647,5.147059,5.117647)
        day$uq_hour <- c(4.071429,4.714286,3.857143,2.928571,3.071429,2.857143,2.142857,1.571429,2.071429,1.571429,1.928571,2.857143,3.214286,4.928571,5.642857,7.785714,7.5,7.142857,6.5,6.285714,6.428571,6.928571,7.214286,6.571429)
        day_actuals <- data.frame(day_actuals,day)
  
}else if (dw == 6)
        {holt(subset(forecasted, DayOfWeek == 6)[,3], 
        alpha = 0.4, 
        beta = 0.2, 
        initial = "simple", 
        h = 1, 
        l.start = 99, 
        b.start = -10
        )
    day_actuals <- subset(actuals, DayOfWeek == 6)
  
    day <- NULL
    day$lq_hour <- c(3.807692,3.653846,3.038462,2.653846,2.884615,2.230769,1.807692,1.884615,1.461538,1.423077,2.076923,2.230769,2.538462,2.807692,3.923077,4.384615,4.269231,3.961538,3.615385,4.230769,4.576923,3.961538,4.038462,4.538462)
    day$iqr_hour <- c(4.875,4.416667,3.833333,3.416667,2.75,2.416667,2.333333,1.791667,1.458333,1.916667,2.416667,3.083333,2.666667,4.041667,4.791667,5.125,5.375,5.291667,4.291667,4.208333,5.041667,5.125,5.291667,5.125)
    day$uq_hour <- c(4,3.4,5.2,4.6,3,3.4,1.8,2,0.8,3.4,2,4.8,3.4,5,6.4,5.4,6.6,5.8,5.2,4.6,6.2,6,4.6,6)
    day_actuals <- data.frame(day_actuals,day)
  
  } else if (dw == 7)
    {holt(subset(forecasted, DayOfWeek == 7)[,3],
    alpha = 0.4, 
    beta = 0.2, 
    initial = "simple", 
    h = 1,  
    l.start = 106, 
    b.start = -26)
    
    day_actuals <- subset(actuals, DayOfWeek == 7)
    day <- NULL
    day$lq_hour <- c(3.043478,4.217391,2.782609,2.521739,2.173913,1.434783,1.347826,1.478261,1.73913,1.434783,2.086957,2.434783,3.086957,4.043478,4.521739,4.130435,4.73913,3.782609,4.391304,3.434783,3.956522,4.478261,3.782609,4.391304)
    day$iqr_hour <- c(4.310345,4.758621,3.62069,3.034483,2.965517,1.862069,2.689655,1.172414,1.931034,1.931034,2.517241,2.724138,3.241379,3.931034,4.896552,5.586207,5.551724,5.896552,4.931034,3.931034,4.931034,4.689655,4.517241,5.137931)
    day$uq_hour <- c(5.333333,4.333333,2.333333,5.333333,3.333333,2,3,1.666667,2.333333,2.666667,2.666667,3.333333,2.666667,5,8.333333,4,5.666667,6.333333,4.666667,6,5,6.666667,4,6.333333)
    day_actuals <- data.frame(day_actuals,day) 
    }

pa <- data.frame(pa)[, 1] 
pa_matrix <- matrix(rep(pa, 24)) # TAKES THE FORECASTED VALUE AND REPLICATES IT 24 TIMES SO THAT IT CAN  BE MERGED LATER ON

#########################################################################################
# Calculate hourly weights
day_actuals$lq_weight_hour <-(day_actuals$lq_hour / sum(day_actuals$lq_hour))
day_actuals$iqr_weight_hour <-(day_actuals$iqr_hour / sum(day_actuals$iqr_hour))
day_actuals$uq_weight_hour <-(day_actuals$uq_hour / sum(day_actuals$uq_hour))

# MERGE FORECASTED 
day_actuals <- data.frame(day_actuals, pa_matrix)
day_actuals$first_prediction <-(day_actuals$iqr_weight_hour * as.numeric(day_actuals$pa_matrix))


# Cumulatives

day_actuals$cum_lq <- cumsum(day_actuals[, 4])
day_actuals$cum_iqr <- cumsum(day_actuals[, 5])
day_actuals$cum_uq <- cumsum(day_actuals[, 6])
day_actuals$cum_actual <- cumsum(day_actuals[, 3])
day_actuals$cum_actual <- cumsum(day_actuals[, 3])
day_actuals$cum_first_prediction <- cumsum(day_actuals[, 11])


#####################################################################################################################################
# Hourly predictions
#####################################################################################################################################

hour_prediction_day <- day_actuals[day_actuals$AdmissionHour %in% (0:current_hour),] # 1/. data upto ADMISSION HOUR
xhour_prediction_day <- day_actuals[day_actuals$AdmissionHour %in% (0:current_hour + 1),] 

# tail of above is then used to obtain the cumulative values for the last hourly timepoint before the prediction

a <- tail(hour_prediction_day$cum_actual, n = 1)
b <- tail(hour_prediction_day$cum_lq, n = 1)
c <- tail(hour_prediction_day$cum_iqr, n = 1)
d <- tail(hour_prediction_day$cum_uq, n = 1)
e <- tail(hour_prediction_day$cum_first_prediction, n = 1)
xxx <- tail(xhour_prediction_day$first_prediction, n = 1)


dat_hour_prediction_day <- day_actuals[day_actuals$AdmissionHour %in% (0:current_hour + 1),] # 2/.

x <- tail(dat_hour_prediction_day$lq_weight_hour, n = 1)
y <- tail(dat_hour_prediction_day$iqr_weight_hour, n = 1)
z <- tail(dat_hour_prediction_day$uq_weight_hour, n = 1)
m <- as.numeric(pa_matrix)


day_hour <- NULL
day_hour$AdmissionHour <- current_hour + 1 # 3/. Prediction hour
day_hour$DayOfWeek <- dw
day_hour$prediction <- NA
day_hour$totalforday <- NA # Total for day is based on conditional logic below
day_hour$first_forecast <- head(day_actuals$pa_matrix, n = 1)


# compare cumulative actual of (prediction hour - 1) -
# if actual is equal/bigger than Cum UQ then prediction = UQweight per hour * forecasted value for the day
# if actual is between UQ and LQ then prediction = IQweight per hour * forecasted value for the day
# if actual is equal/smaller than Cum LQ then prediction = LQweight per hour * forecasted value for the day


if (a >= d) {
  day_hour$prediction <-
    z * m
} else if # if cumulative actual is bigger or equal to UQ
(a < d &
 a > b) {
  day_hour$prediction <-
    y * m
} else if # if cumulative actual is between UQ and LQ
(a < b) {
  day_hour$prediction <-
    x * m
} else if # if cumulative actual is smaller or equal to IQ
(a <= (e + 2) &
 a >= (e - 2)) {
  day_hour$prediction <-
    xxx
} # if cumulative actual is +-1 value away from first prediction then keep the first prediction values
# Threshold of + or - 2 for the first prediction

day_hour <- data.frame(day_hour)# Convert to dataframe
day_hour <-
  data.frame(head(day_hour, n = 1)) # this was done as the output was replicated 24 times, and only needed 1 row

# If else logic split by quartile

if (a < b) {
  # if actual is in LQ range then do
  lq_day_hour <-
    day_actuals[day_actuals$AdmissionHour > current_hour + 1,] # 2/. This is all the hours after the prediction hour
  lq_day_hour$pa_matrix <- as.numeric(lq_day_hour$pa_matrix)
  lq_day_hour$prediction <- lq_day_hour$lq_weight_hour * lq_day_hour$pa_matrix
  lq_day_hour$totalforday <- cumsum(lq_day_hour$prediction)
  day_hour$totalforday <-
    (
      sum(hour_prediction_day$Total) + day_hour$prediction + tail(lq_day_hour$totalforday, n = 1)
    )
} else if (a < d & a > b) {
  iqr_day_hour <- day_actuals[day_actuals$AdmissionHour > current_hour + 1,] # 2/.
  iqr_day_hour$pa_matrix <- as.numeric(iqr_day_hour$pa_matrix)
  iqr_day_hour$prediction <-
    iqr_day_hour$iqr_weight_hour * iqr_day_hour$pa_matrix
  iqr_day_hour$totalforday <- cumsum(iqr_day_hour$prediction)
  day_hour$totalforday <-
    (
      sum(hour_prediction_day$Total) + day_hour$prediction + tail(iqr_day_hour$totalforday, n = 1)
    )
} else if (a >= d) {
  uq_day_hour <- day_actuals[day_actuals$AdmissionHour > current_hour + 1,] # 2/.
  uq_day_hour$pa_matrix <- as.numeric(uq_day_hour$pa_matrix)
  uq_day_hour$prediction <- uq_day_hour$uq_weight_hour * uq_day_hour$pa_matrix
  uq_day_hour$totalforday <- cumsum(uq_day_hour$prediction)
  day_hour$totalforday <-
    (
      sum(hour_prediction_day$Total) + day_hour$prediction + tail(uq_day_hour$totalforday, n = 1)
    )
} else if (a <= (e + 2) & a >= (e - 2)) {
  first_day_hour <- day_actuals[day_actuals$AdmissionHour > current_hour + 1, ]
  first_day_hour$pa_matrix <- as.numeric(first_day_hour$pa_matrix)
  first_day_hour$prediction <- first_day_hour$first_prediction
  first_day_hour$totalforday <- cumsum(first_day_hour$prediction)
  day_hour$totalforday <-
    (
      sum(hour_prediction_day$Total) + day_hour$prediction + tail(first_day_hour$totalforday, n = 1)
    )
}

# Round up

day_hour$totalforday <- ceiling(day_hour$totalforday) # Rounding everything up
day_hour$prediction <- ceiling(day_hour$prediction)


 
##############################################################################################
# Export to SQL
##############################################################################################

varTypes = c(run_date="datetime")

dbhandle <- odbcDriverConnect('driver={SQL Server};server=SRHTDW2DB;database=nhs_import;trusted_connection=true')
sqlDrop(dbhandle, sqtable = "r.MovingPredictedValue", errors = FALSE)
sqlSave(dbhandle, day_hour, tablename = "r.MovingPredictedValue", append=TRUE, varTypes = varTypes)

odbcCloseAll()
#' temp_risk
#'
#' compute the risk associated with high temperatures vs age
#' @param threshold temp threshold (C) (or extreme temp)
#' @param temperature (C)
#' @param age_threshold_med (years) (default 5)
#' @param age_threshold_high (years) (default 8)
#' @param temperature_threshold (C) (default 10)
#' @return risk (high,med, low), mean nutrient concentration (mg/L)



temp_risk = function(age, temperature, age_threshold_med=50,
                         age_threshold_high=80, temperature_threshold=40) {

  # start with some error checking
  if (length(age) < 5)
    return("Not enough measurements, at least 5 are needed")

  if (length(temperature) < 5)
    return("Not enough temperature measurements, at least 5 are needed")


  # a while loop is useful here
  num = 0
  i=1
  # we use while here because we want to exit our loop any time we get more than 5 days with
  # air temperature greater than the threshold

  while ( (num< 5) && (i <= length(temperature))) {
    if (temperature[i] > temperature_threshold)
      # we have another day with greater than 10 so accumulate
      num = num +1
    else
      # we have to start over
      num = 0
    # remember with while loops we need to increment our counter
    i = i+1

  }

  # compute the mean nutrient
  mean_age = mean(age)

  # only high or med if temperature has been 10 degrees for more than 5 days
  if (num >= 5) {
    risk= case_when ( mean_age < age_threshold_med ~ "low",
                      mean_age >= age_threshold_med &
                        mean_age< age_threshold_high ~ "med",
                      mean_age >= age_threshold_high ~ "high" )
  } else
    risk = "low"


  return(list(risk=risk, mean_age=mean_age))
}


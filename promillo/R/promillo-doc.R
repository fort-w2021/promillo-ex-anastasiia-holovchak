#' Tell me how drunk
#'
#' Computes the blood alcohol concentration (BAC) using the Widmark and Whatson
#' combination
#'
#' @param age age in years
#' @param sex sex as a character ("m", "w", "male", "female"; lower/upper case both
#' acceptable)
#' @param height height in cm
#' @param weight weight in kg
#' @param drinking_time start and end time points of alcohol consumption as a
#' POSIXct vector (sorted ascended)
#' @param drinks vector or list with number of consumed drinks
#' ("massn", "hoibe", "wein", "schnaps" allowed)
#' @return blood alcohol concentration in per-mille, adjusted for the reduction
#' of the blood alcohol level at the end of drinking time
#' @references https://web.archive.org/web/20150123143123/http://promille-rechner.org/erlaeuterung-der-promille-berechnung/
#' @author Anastasiia Holovchak
#' @export
#' @import checkmate

tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight,
                              drinking_time, drinks) {
  if ((age < 16 && drinks > 0) ||
      (age < 18 && isTRUE("schnaps" %in% names(drinks)) && drinks[["schnaps"]] != 0)) {
    warning("illegal drinking age!")
  }
  alcohol_drunk <- get_alcohol(drinks)
  bodywater <- get_bodywater(sex, age, height, weight)
  get_permille(alcohol_drunk, bodywater, drinking_time)
}


#' Show me how drunk
#'
#' Plots per-mille blood alcohol concentration (BAC) computed using [tell_me_how_drunk()]
#' at 5 mins interval
#'
#' @inheritParams tell_me_how_drunk
#' @export
#' @import checkmate
#' @importFrom ggplot2 qplot

show_me_how_drunk <- function(age, sex = c("male", "female"), height, weight,
                              drinking_time, drinks) {
  # check drinking_time
  assert_posixct(drinking_time, any.missing = FALSE, sorted = TRUE, len = 2)
  # generate time sequence at 5 min interval
  drink_sequence <- seq(drinking_time[1], drinking_time[2], by = "5 mins")
  # compute blood alcohol concentration for the sequence time points & end point
  permille <- sapply(drink_sequence, function(t) {
    tell_me_how_drunk(age = age, sex = sex, height = height, weight = weight,
                      drinking_time = c(drinking_time[1], t), drinks = drinks)
  })
  # generate the plot
  qplot(x = drink_sequence, y = permille, xlab = "Drinking Time (at 5 mins interval)",
        ylab = "Per-mille")
}

# utilities --------------------------------------------------------------------
#' check legality of drinking age
#'
#' @param age age in years
#' @param drinks possible drinks
#' @return total mass of alcohol consumed
#' @import checkmate

check_illegal_drink <- function(age, drinks) {
  if ((age < 16 && drinks > 0) ||
      (age < 18 && isTRUE("schnaps" %in% names(drinks)) && drinks[["schnaps"]] != 0)) {
    warning("illegal drinking age.")
  }
}

#' compute total mass of alcohol consumed
#'
#' @param drinks possible drinks
#' @return total mass of alcohol consumed
#' @import checkmate

get_alcohol <- function(drinks) {
  # homogenize inputs:
  drinks <- unlist(drinks)
  assert_subset(names(drinks), choices = c("massn", "hoibe", "wein", "schnaps"),
    empty.ok = FALSE)
  assert_numeric(drinks, lower = 0)

  volume <- c(
    "massn"    = 1000,
    "hoibe"   = 500,
    "wein"    = 200,
    "schnaps" = 40)
  alcohol_concentration <- c(
    "massn"    = 0.06,
    "hoibe"   = 0.06,
    "wein"    = 0.11,
    "schnaps" = 0.4)
  alcohol_density <- 0.8

  sum(drinks * volume[names(drinks)] *
      alcohol_concentration[names(drinks)] * alcohol_density)
}

#' compute total amount of water in the body
#'
#' @param sex sex as a character ("male" or "female")
#' @param age age in years
#' @param height height in cm
#' @param weight weight in kg
#' @return total amount of water in the body
#' @import checkmate
get_bodywater <- function(sex = c("male", "female"), age, height, weight) {
  sex <- tolower(sex)
  sex <- match.arg(sex)

  assert_number(age, lower = 10, upper = 110)
  if (age > 90) {
    warning("...ts ts ts, this at your age!")
  }

  assert_number(height, lower = 100, upper = 230)
  assert_number(weight, lower = 40, upper = 300)

  coef <- if (sex == "male") {
    c(2.447, -0.09516, 0.1074, 0.3362)
  } else {
    c(0.203, -0.07, 0.1069, 0.2466)
  }
  t(coef) %*% c(1, age, height, weight)
}

#' compute BAC per-mille
#'
#' @param alcohol_drunk total mass of alcohol consumed
#' @param bodywater total amount of water in the body
#' @param drinking_time duration of drinking time in hours
#' @return blood alcohol concentration (BAC) per-mille
#' @import checkmate

get_permille <- function(alcohol_drunk, bodywater, drinking_time){
  assert_posixct(drinking_time, any.missing = FALSE, sorted = TRUE, len = 2)

  alcohol_density <- 0.8
  blood_density <- 1.055
  permille <- alcohol_density * alcohol_drunk / (blood_density * bodywater)

  partylength <- difftime(drinking_time[2], drinking_time[1], units = "hours")
  sober_per_hour <- 0.15
  # sobering up starts only after one hour & you can't be more sober than 0:
  max(0, permille  - (max(0, partylength - 1) * sober_per_hour))
}

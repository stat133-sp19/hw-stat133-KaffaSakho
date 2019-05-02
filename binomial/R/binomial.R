#---
#  title: "workout03-kaffa-sakho"
#output: html_document
#---

#1) R Funtions

#1.1) Private Checker Functions

#CHECK IF ROXYGEN COMMENTS FORMAT IS CORRECT
#Title: validity check of the probability
#Description: checks whether the inputted probability is a valid possibility of a probability.
#Param: prob a number between 0 and 1 with 0 and 1 included, for example: 0.4
#Return: returns TRUE if the proability is a correct probability i.e a number between 0 and 1 included; and returns an error message otherwise.

check_prob <- function (prob) {
  if (prob <= 1 & prob >= 0) {
    return(TRUE)
  }
  stop("Invalid Probability Value")
}

#Function check_trials()

#Title: validity check of the number of trials
#Description: checks whether the inputted number of trials is valid i.e if it is a non-negative integer.
#Param: trials
#Return: returns TRUE when trials is valid and an error message otherwise.

check_trials <- function (trials) {
  if (trials > 0 & round(trials)==trials) {
    return(TRUE)
  }
  stop("Invalid Trials Value")
}

#Function check_success()

#Title: validity check of the vector success
#Description: checks whether the inputted vector success is a vector of non-negative integers and that it is less than inputted number of trials.
#Param: success, trials. success can be either a non-negative integer or a vector of non-negative integers with length greater than 1. trials is a non-negative integer greater than success; example: success=3, trials=6
#Return: returns TRUE if success is both a vector of non-negative integers that are all less than trials and the length of success is less than or equal to trials. It returns an error message if success>trials or any other condition mentioned previously is not met.

check_success <- function (success,trials) {
  if (all(success>0) & all(round(success)==success) & all(success<=trials)) { #verify the last "&"
    if (i>0 & round(i)==success) # add check_trials() to the condition?
      return(TRUE)
  }
  else if (success>trials) {
    stop("success cannot be greater than trials")
  }
  stop("Invalid Success Value")
}


#1.2) Private Auxiliary Functions

#Title: mean of a binomial distribution
#Description: calculates the mean i.e expected value of a binomial distribution with a known number of trials and probability of success.
#Param: trials number of trials, prob the probability of success in one trial. trials is a non-negative integer (e.g: 5) and prob is a number between 0 and 1 included (eg 0.6).
#Return: returns the calculated expected value of a binomial distribution.

aux_mean <- function(trials, prob) {
  x_mean <- trials * prob
  x_mean
}

#Title: variance of a binomial distribution
#Description: calculates the variance of a binomial distribution with a known number of trials and probability of success.
#Param: trials number of trials, prob the probability of success in one trial. trials is a non-negative integer (e.g: 5) and prob is a number between 0 and 1 included (eg 0.6).
#Return: returns the calculated variance of a binomial distribution. The output is a non-negative integer.

aux_variance <- function(trials, prob) {
  x_variance <- trials*prob*(1-prob)
  x_variance
}

#are we supposed to explain what variance is?


#Title: mode of a binomial distribution
#Description: calculates the mode of a binomial distribution, which is the most likely number of success in a given number of independent trials rounded to the greatest integer less than or equal to it.
#Param: trials number of trials, prob the probability of success in one trial. trials is a non-negative integer (e.g: 5) and prob is a number between 0 and 1 included (eg 0.6).
#Return: returns the calculated mode of a binomial distribution. The output is a non-negative integer.

aux_mode <- function(trials, prob) {
  x_mode <- floor((trials*prob) + prob)
  x_mode
}


#Title: skewness of a binomial distribution
#Description: calculates the skewness of a binomial distribution, which measures the asymmetry of the probability distribution of a random variable about its mean.
#Param: trials number of trials, prob the probability of success in one trial. trials is a non-negative integer (e.g: 5) and prob is a number between 0 and 1 included (eg 0.6).
#Return: returns the calculated skewness of a binomial distribution; the output can either be  positive, negative or undefined

aux_skewness <- function(trials, prob) {
  x_skewness <- (1-2*prob)/sqrt(trials*prob*(1-prob))
  x_skewness
}


#Title: kurtosis of a binomial distribution
#Description: calculates the kurtosis of a binomial distribution, which measures the tailedness of the probability distribution of random variable.
#Param: trials number of trials, prob the probability of success in one trial. trials is a non-negative integer (e.g: 5) and prob is a number between 0 and 1 included (eg 0.6).
#Return: returns the calculated kurtosis of a binomial distribution. The output can be positive, negative or 0.

aux_kurtosis <- function(trials, prob) {
  x_kurtosis <- (1-6*prob*(1-prob))/(trials*prob*(1-prob))
  x_kurtosis
}

#1.3) Function bin_choose() CHECK IF THIS FUNCTION IS CORRECT

#' @title number of possible combinations in which a number of successes occurs
#' @description calculates the number of possible combination of successes in a given number of trials
#' @param n the number of trials, a non-negative integer
#' @param k the number of successes, a non-negative integer less than n
#' @return computed number of possible combinations in which k successes can occur in n trials
#' @export
#' @examples

bin_choose <- function(n,k) {
  check_trials(n) #doesn't this make the function end here?
  check_success(n,k)
  k_successes <- factorial(n)/(factorial(k)*factorial(n-k))
  k_successes
}

#include code tests? and to auxiliary functions too?
#what is the format of the examples?
#what is @export?

#1.4) Function bin_probability()

#' @title probability of a number of successes in a number of trials
#' @description calculates the probability of getting a given number of successes in a given number of trials
#' @param success
#' @param trials
#' @param prob
#' @return the calculated probability of getting success number of successes in trials number of trials
#' @export
#' @examples

bin_probability <- function(success,trials,prob) {
  check_trials(trials) #is this the correct way to invok it ?
  check_prob(prob)
  check_success(success, trials)
  probability <- bin_choose(trials,success)*(prob**success)*((1-prob)**(trials-success))
  probability
}


#1.5) Function bin_distribution

#' @title probability distribution of a random variable in a given number of trials under a given probability
#' @description calculates and stores in a data frame the probabilities of getting different number of successes in a given number of trials
#' @param trials the number of trials, a non-negative integer
#' @param prob the probability of success, a number between 0 and 1 both included.
#' @return a data frame of primary class bindis and secondary class data.frame with two columns and that contains the probabilities of getting different numbers of successes under a given number of trials
#' @export
#' @examples

bin_distribution <- function(trials, prob) {
  df <- data.frame(success=0:trials, probability=bin_probability(0:trials,trials,prob))
  class(df) <- c("bindis", "data.frame")
  df
}
#can roxygen comments include punctuation?


#Function plot.bindis #check if this is correct

#' @export

plot.bindis <- function(distrib1) {
  barplot(distrib1[,2], names.arg=distrib1[,1], xlab= "successes", ylab="probability")
}



#1.6) Function bin_cumulative()

#' @title binomial probability distribution and cumulative distribution
#' @description calculates and stores in a data frame the probabilities of getting different number of successes in a given number of trials and the cumulative probabilities
#' @param trials the number of trials, a non-negative integer
#' @param prob the probability of success, a number between 0 and 1 both included.
#' @return a data frame of primary class bincum and secondary class data.frame with three columns and that contains the probabilities of getting different numbers of successes under a given number of trials and the cumulative probabilities
#' @export
#' @examples

bin_cumulative <- function(trials,prob) {
  df_cumulative <- data.frame(success = 0:trials, probability = bin_probability(0:trials,trials,prob))
  df_cumulative <- mutate(df_cumulative, cumulative=cumsum(probability))
  class(df_cumulative) <- c("bincum","data.frame")
  df_cumulative
}


#Function plot.bincum()

#' @export

plot.bincum <- function(distrib2) {
  plot(distrib2[,1], distrib2[,3])
  lines(distrib2[,1], distrib2[,3])
}

#1.7) Function bin_variable()

#' @title a random variable with a given a number of trials and probability of success
#' @description
#' @param trials
#' @param prob
#' @return
#' @export
#' @examples

bin_variable <- function (trials,prob) {
  check_trials(trials)
  check_prob(prob)

  variab <- list("trials" = trials,
                 "prob" = prob)
  class(variab) <- "binvar"
  variab
}
#stuck on the description part


#Methods


#' @export

print.binvar <- function(variable) {
  cat('"Binomial variable"')
  cat("\n\n")
  cat("Parameters")
  cat("\n", "- number of trials: ", variable[['trials']])
  cat("\n", "- prob of success: ", variable[['success']])
  invisible(variable)
}


#' @export

summary.binvar <- function(variable) {
  output_list <- list("trials" = variable[[trials]],
                      "prob" = variable[[prob]],
                      "mean" = aux_mean(variable[[trials]],variable[[prob]]),
                      "variance" = aux_variance(variable[[trials]],variable[[prob]]),
                      "mode" = aux_mode(variable[[trials]],variable[[prob]]),
                      "skewness"= aux_skewness(variable[[trials]],variable[[prob]]),
                      "kurtosis"= aux_kurtosis(variable[[trials]],variable[[prob]])
  )
  output_list
}

#' @export

print.summary.binvar <- function(summary) {
  cat('"Summary Binomial"')
  cat("\n\n")
  cat("Parameters")
  cat("\n", "- number of trials: ", summary[['trials']])
  cat("\n", "- prob of success: ", summary[['success']])
  cat("\n\n")
  cat("Measures")
  cat("\n", "- mean: ", summary[['mean']])
  cat("\n", "- variance: ", summary[['variance']])
  cat("\n", "- mode: ", summary[['mode']])
  cat("\n", "- skewness: ", summary[['skewness']])
  cat("\n", "- kurtosis: ", summary[['kurtosis']])
  invisible(summary)
}

#1.8) Functions of measures

#' @title
#' @description
#' @param trials
#' @param prob
#' @return the mean of the
#' @export
#' @examples

bin_mean <- function(trials,prob) {
  check_trials(trials)
  check_prob(prob)
  aux_mean(trials,prob)
}

#' @title
#' @description
#' @param trials
#' @param prob
#' @return
#' @export
#' @examples

bin_variance <- function(trials,prob) {
  check_trials(trials)
  check_prob(prob)
  aux_variance(trials,prob)
}


#' @title
#' @description
#' @param trials
#' @param prob
#' @return
#' @export
#' @examples

bin_mode <- function(trials,prob) {
  check_trials(trials)
  check_prob(prob)
  aux_mode(trials,prob)
}


#' @title
#' @description
#' @param trials
#' @param prob
#' @return
#' @export
#' @examples

bin_skewness <- function(trials,prob) {
  check_trials(trials)
  check_prob(prob)
  aux_skewness(trials,prob)
}

#' @title
#' @description
#' @param trials
#' @param prob
#' @return
#' @export
#' @examples

bin_kurtosis <- function(trials,prob) {
  check_trials(trials)
  check_prob(prob)
  aux_kurtosis(trials,prob)
}

#include roxygen comments?






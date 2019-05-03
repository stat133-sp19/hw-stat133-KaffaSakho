
### Overview

"binomial" is a package that allows the creation of a random variable as well as the measurements and visualization of its features. 

-   bin\_variable() creates a random variable object of class "binvar"

-   print() can print the content of a random variable.

-   summary() gives a comprehensive summary of a binomial random variable.

-   print() can print the content of the summary of a binomial random variable.

-   bin\_mean() calculates the expected value of binomial random variable.

-   bin\_variance() calculates the variance of binomial random variable.

-   bin\_mode() calculates the mode of binomial random variable.

-   bin\_skewness() calculates the skewness of binomial random variable.

-   bin\_kurtosis() calculates the kurtosis of binomial random variable.

-   bin\_choose() calculates the different possible combinations of a given number of successes in a given number of trials.

-   bin\_probability() calculates the probability of obtaining a given number of successes in a given number of trials.

-   bin\_distribution() calculates and stores in a data frame the distribution of the probabilities over different number of successes.

-   bin\_cumulative() calculates and stores in a data frame the distribution of the cumulative probabilities over different number of successes.

-   plot() can plot either the distribution or the cumulative distribution depending on whether its input is of class "bindis" or "bincum".


### Motivation

This package was created as a practice problem in R package creation.

### Installation

\# development version from GitHub: \#install.packages("devtools")

\# install "cointoss" (without vignettes) devtools::install\_github("KaffaSakho/binomial")

\# install "cointoss" (with vignettes) devtools::install\_github("KaffaSakho/binomial", build\_vignettes = TRUE)

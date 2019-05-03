
### Overview

"binomial" is a package that allows the creation and computations on a random variable.

-   bin\_variable() creates a random variable object of class "binvar"

-   print() can print the content of a binomial object.

-   summary() gives a comprehensive summary of a binomial object.

-   print() can print the content of the summary of a binomial object.

-   bin\_mean()

-   bin\_variance()

-   bin\_mode()

-   bin\_skewness()

-   bin\_kurtosis()

-   bin\_choose()

-   bin\_probability()

-   plot() can plot either the distribution or the cumulative distribution depending on whether its input is of class "bindis" or "bincum".

-   bin\_distribution()

-   bin\_cumulative()

### Motivation

This package was created as a practice problem in R package creation.

### Installation

\# development version from GitHub: \#install.packages("devtools")

\# install "cointoss" (without vignettes) devtools::install\_github("KaffaSakho/binomial")

\# install "cointoss" (with vignettes) devtools::install\_github("KaffaSakho/binomial", build\_vignettes = TRUE)

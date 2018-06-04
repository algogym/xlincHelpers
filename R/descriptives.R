#' Bootstrapped mean and confidence interval
#'
#' The function returns the mean and the confidence interval for a numeric vector
#'
#' @param data A numeric vector containing the values to be summarized
#' @param repeats An integer indicating the number of bootstrap samples to be used
#' @importFrom dplyr %>%
#'
#' @return A data.frame containing a column each for mean, lower 95% confidence interval
#'  and upper 95% confidence interval.
#' @export
#'
#' @examples
#'
#' library(dplyr)
#'
#' test <- data.frame(group = rep(letters[1:2], each = 30),
#'                                dv = c(rnorm(30, 40, 4), rnorm(30,70,4)))
#' test %>%
#'    group_by(group) %>%
#'    do(bootstrapped_summary(.$dv))

bootstrapped_summary <- function(data, repeats = 1000){

    mean_boot <- function(data, id){
        resampled_data <- data[id]
        data_mean <- mean(resampled_data, na.rm = TRUE)
        params <- c(data_mean)
        return(params)}

    return_statistics <- function(boot_object){
        boot_unlist <- unlist(boot_object)
        statistic <- boot_unlist$t0
        lower <- boot_unlist$normal2
        upper <- boot_unlist$normal3
        data.frame(statistic, lower, upper)
    }

    data %>%
        boot::boot(statistic = mean_boot, R = repeats) %>%
        boot::boot.ci(index=1, conf = 0.95, type = "norm") %>%
        return_statistics()
}



#' Title
#'
#' @param outcome
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#' library(dplyr)
#'
#' df <- data.frame(group = rep(c("a", "b"), each = 20), outcome = c(rbinom(20,1, 0.7), rbinom(20,1,0.2)))
#'
#' df %>%
#' group_by(group) %>%
#' do(binom_summary(.$outcome))

binom_summary <- function(outcome){
    trials <- length(outcome)
    sucesses <- sum(outcome)
    tibble::as.tibble(binom::binom.agresti.coull(sucesses, trials)) %>%
        dplyr::ungroup() %>%
        dplyr::select(sucesses=x, trials = n, mean, lower, upper)
}




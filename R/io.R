#' Batch read files from a directory
#'
#' @param path
#' @param files
#' @param extension
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
multiple_file_reader <- function(path=".", files, extension, ...){
   paths <- fs::path(fs::path_abs(path), files, ext=extension)
   purrr::map_dfr(paths, readr::read_csv)
   # TODO: add different readers for different types
   # IDEA: Add support for base_r readers
}


#' model_writer
#'
#' A function to save models from the list of models fitted with the fit_with()-function.
#' The function saves one RDS file for each model to the provided path
#'
#'
#' @param model_container A list of model objects.
#' @param save_path File destination for the RDS-files to be saved in
#' @param prefix Custom Prefix to the file names.
#'
#' @return
#'
#' @export
#'
#' @examples
#'
#' models <- modelr::fit_with(mtcars, lm, "mpg ~ hp")
#' model_writer(models)
#'

model_writer <- function(model_container, save_path = "./", prefix = "" ){
    if(is.null(names(model_container))){
        names(model_container) <- letters[1:length(model_container)]
    }

    save_path <- ifelse(stringr::str_detect(save_path, "/$"), save_path, stringr::str_glue(save_path, "/"))
    prefix <- ifelse(stringr::str_length(prefix) == 0, "", stringr::str_glue(prefix, "_"))
    output_path <- stringr::str_glue(save_path, prefix, "{names(model_container)}",".rds")
    purrr::walk2(model_container, output_path, ~ saveRDS(.x, .y))
}

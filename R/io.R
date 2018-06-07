#' Batch read files from a directory
#'
#' The function reads a batch of files within one directory and concatenates them into
#' one data.frame (or equivalent structure). This is useful, when your data of each participant is
#' stored in a separate file.
#'
#' @param path A character string indicating the path to the files to read
#' @param files Usually, a vector of character strings with the names of the files to read in
#' @param extension A character string for the file extension like csv, tsv. Only csv supported right now.
#' @param reader_function The function to use for reading individual files
#' @param ... Additional parameters for the reader function, like sep or dec
#'
#' @return A data frame (or equivalent structure) with the concatenated files
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' files_names <- letters[1:3]
#'
#' # Reads three files with .csv extension in the working directory using readr::read_csv
#'
#' my_data <- multiple_file_reader(file_names, ".csv", reader::read_csv)
#'
#'
#' }

multiple_file_reader <- function(files, extension, reader_function, path=".", ...){
    extension = stringr::str_extract(extension, "\\w+")
   paths <- fs::path(fs::path_abs(path), files, ext=extension)
   purrr::map_dfr(paths, reader_function, ...)
}

#' Write a list of models to RDS files
#'
#' A function to save models from the list of models fitted with the fit_with()-function.
#' The function saves one RDS file for each model to the provided path
#'
#'
#' @param model_container A list of model objects.
#' @param save_path File destination for the RDS-files to be saved in
#' @param prefix Custom Prefix to the file names.
#'
#' @return Nothing
#'
#' @export
#'
#' @examples
#' \dontrun{
#' models <- modelr::fit_with(mtcars, lm, "mpg ~ hp")
#' model_writer(models)
#' }
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

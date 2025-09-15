#' Validate Wide Matrix Data Format
#'
#' This function validates that a data frame conforms to the expected wide
#' matrix format for forest inventory data. It checks for the presence of
#' required columns and ensures matrix columns follow the expected naming
#' convention and data types.
#'
#' @param df A data frame to validate. Expected to contain a 'PlotID'
#' column and matrix columns with names following the pattern 'S##_D##'
#' (e.g., S01_D01, S02_D15), where ## represents two-digit numbers.
#'
#' @returns A character vector of error messages. If the data frame is
#' valid, returns an empty character vector (length 0). If validation
#' fails, returns a vector containing descriptive error messages for each
#' issue found.
#'
#' @export
#'
#' @examples
#' # Valid data frame
#' valid_df <- data.frame(
#'   PlotID = c("Plot1", "Plot2"),
#'   S01_D01 = c(1.2, 1.5),
#'   S01_D02 = c(2.1, 2.3),
#'   S02_D01 = c(0.8, 1.1)
#' )
#' validate_wide_matrix(valid_df)  # Returns character(0)
#'
#' # Invalid data frame - missing PlotID
#' invalid_df1 <- data.frame(
#'   Site = c("A", "B"),
#'   S01_D01 = c(1.2, 1.5)
#' )
#' validate_wide_matrix(invalid_df1)  # Returns error message
#'
#' # Invalid data frame - no matrix columns
#' invalid_df2 <- data.frame(
#'   PlotID = c("Plot1", "Plot2"),
#'   SomeCol = c("a", "b")
#' )
#' validate_wide_matrix(invalid_df2)  # Returns error message
#'
#' # Invalid data frame - non-numeric matrix columns
#' invalid_df3 <- data.frame(
#'   PlotID = c("Plot1", "Plot2"),
#'   S01_D01 = c("1.2", "1.5"),  # character instead of numeric
#'   S01_D02 = c(2.1, 2.3)
#' )
#' validate_wide_matrix(invalid_df3)  # Returns error message
validate_wide_matrix <- function(df) {
  errs <- c()
  if (!"PlotID" %in% names(df)) errs <- c(errs, "Missing required column:
                                          PlotID")
  mat_cols <- grep("^S[0-9]{2}_D[0-9]{2}$", names(df), value = TRUE)
  if (length(mat_cols) == 0) errs <- c(errs, "No matrix columns found.
                                       Expect names like S01_D01, S01_D02,
                                       ...")
  if (length(errs)) return(errs)
  non_num <- mat_cols[!sapply(df[mat_cols], is.numeric)]
  if (length(non_num)) errs <- c(errs,
                                 paste0("Non-numeric matrix columns: ",
                                        paste(non_num, collapse=", ")))
  errs
}  # keep a lightweight DBH reference packaged

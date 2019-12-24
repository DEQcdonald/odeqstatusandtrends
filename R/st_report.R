#' Create Status and Trends Report
#'
#' Creates a Status and Trends Report with the given parameter summary table, Basin name, and output directory.
#' @param format The output format for the rmarkdown document
#' @param table_format The format for kable tables. "pandoc" is the default. See knitr::kable() for more options
#' @param file_name The name of the output .html file
#' @param out_path The output path
#' @return A report located in the out_path
#' @export
#' @examples
#' st_report(format = "word_document", table_format = "pandoc", file_name = "Oregon_SandT_report",
#' out_path = "N:/Status_and_Trend_Reports/2019/Statewide Report")

st_report <- function(format = "word_document", table_format = "pandoc", file_name = "Oregon_SandT_report",
                      out_dir = "N:/Status_and_Trend_Reports/2019/Statewide Report"){

  rmarkdown::render(input = "N:/Status_and_Trend_Reports/Report_Files/st_report_files/state_summary.Rmd",
                    output_format = format,
                    output_file = file_name,
                    output_dir = out_dir,
                    envir = globalenv())
}

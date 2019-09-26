#' Create Status and Trends Report
#'
#' Creates a Status and Trends Report with the given parameter summary table, Basin name, and output directory.
#' @param basin The name of the basin for reporting
#' @param param_summary A parameter summary table as returned from odeqstatusandtrends::parameter_summary().
#' @param format The output format for the rmarkdown document
#' @param file_name The name of the output .html file
#' @param out_path The output path
#' @param complete_years A list of complete years within analysis
#' @param hucs A list of huc8s within analysis area
#' @return An html report located in the output path
#' @export
#' @examples
#' st_report(basin = "Willamette", param_summary = param_sum_Willamette, out_path = ".../your-output-path")

st_report <- function(basin, param_summary, format = "html_document", file_name, out_path, complete_years, hucs){
  out_path <- gsub("\\\\", "/", out_path)

  table_format <- if(format == "html_document" ){"html"} else {"markdown"}

  rmarkdown::render(input = "N:/Status_and_Trend_Reports/Report_Files/basin.Rmd",
                    params = list(
                      basin = basin,
                      param_sum = param_summary,
                      complete_years = complete_years,
                      hucs = hucs,
                      table_format = table_format),
                    output_format = format,
                    output_file = file_name,
                    # output_dir = "C:/workspace/StatusAndTrends",
                    output_dir = out_path,
                    # intermediates_dir = "C:/workspace/StatusAndTrends",
                    # intermediates_dir = out_path,
                    envir = globalenv())
}

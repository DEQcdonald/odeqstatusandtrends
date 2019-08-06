#' Create Status and Trends Report
#' 
#' Creates a Status and Trends Report with the given parameter summary table, Basin name, and output directory.
#' @param basin The name of the basin for reporting
#' @param param_summary A parameter summary table as returned from odeqstatusandtrends::parameter_summary().
#' @param file_name The name of the output .html file
#' @param out_path The output path
#' @return An html report located in the output path
#' @export
#' @examples
#' st_report(basin = "Willamette", param_summary = param_sum_Willamette, out_path = ".../your-output-path")

st_report <- function(basin, param_summary, file_name, out_path){ 
  out_path <- gsub("\\\\", "/", out_path)
  rmarkdown::render("C:/workspace/basin.Rmd",
                  params = list(
                    basin = basin,
                    param_sum = param_summary),
                  output_file = file_name,
                  output_dir = out_path)
}
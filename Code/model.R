# PM25 Health Impact Calc Core
# By Yifan LIU 2021/05/15
# Modify by Yifan LIU 2022/04/09
# Modified By yifan liu on 23/12/19
# Improved By Yifan LIU With Deepseek on 260610

library(tidyverse)
library(writexl)
library(readxl)

#' detect CRF model name and generate a name for file output
#'
#' @return formatted string of CRF name
#' @export
#'
tell_Model <- function() {
  return(
    if (.CR_Model %>% str_detect('IER')) {
      str_c(.CR_Model)
    } else if (.CR_Model %in% c('5COD', 'NCD+LRI')) {
      str_c('PM2.5_GEMM', .CR_Model, sep = '_')
    } else if (.CR_Model == 'MRBRT') {
      str_c("PM2.5_", .CR_Model)
    } else if (.CR_Model == 'O3') {
      #新增
      str_c(.CR_Model)
    } else if (.CR_Model == 'NO2') {
      str_c(.CR_Model)
    } #新增
  )
}

#' set CRF for calculation
#'
#' @param Model string, one of `IER`, `NCD+LRI`, `5COD`, `MRBRT`, `O3` or `NO2`
#'
#' @export
set_Model <- function(Model) {
  assign(".CR_Model", Model, envir = globalenv())

  if (str_detect(Model, "IER|NCD\\+LRI|5COD|MRBRT|O3|NO2")) {
    cat(str_glue(
      "C-R Model \"{tell_Model()}\" is set as the default methodology"
    ))
  } else {
    warning(str_glue(
      "\"{Model}\" is not a bnuilt-in CR model, \\
      please provide a corrosponding `RR_table` after `read_file()`"
    ))
  }
}

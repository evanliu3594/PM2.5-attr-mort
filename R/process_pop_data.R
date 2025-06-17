#' get fishnet grids from a grid file
#'
#' @param filenm file path of the target raster
#' @param x name of the 
#' @param y 
#' @param crs 
#' @param digit 
#'
#' @returns
#' @export
#'
#' @examples
getGrid <- function(filenm, crs = 4326, digit = 4) {
  
  distance <- \(x) head(x, -1) - tail(x, -1)
  
  getMinMax <- \(x) quantile(x, probs = c(0, 1))
  
  mode <- \(x) sort(table(x), decreasing = T) %>% names() %>% .[1] %>% as.numeric()
  
  if (str_detect(filenm, "csv$")) {
    if (is.na(x) | is.na(y)) {
      stop("Please specify lon/lat column of the file using `x = ..., y = ...`")
    }
    
    pollu <- read_csv(filenm)
    
    if (max(pull(pollu, {{x}})) > 180) {
      stop("The provided file dose not coordinated by WGS 84, please specify the CRS using `crs = ...`")
    }
    
    xy <- names(pollu) %>% str_subset("x|y|lon|lat|Lon|Lat") %>% set_names(\(x) case_when(
      str_detect(x, "x|lon|Lon") ~ "x", 
      str_detect(x, "y|lat|Lat") ~ "y",
      TRUE ~ "other"
    ))
    
    pollu <- pollu %>% mutate(across(!!xy, as.numeric))
    
    bound <- c(getMinMax(pull(pollu, !!xy["x"])), getMinMax(pull(pollu, !!xy["y"]))) %>%
      set_names(c("xmin", "xmax", "ymin", "ymax")) %>% st_bbox(crs = crs)
    
    dx <- pull(pollu, {{x}}) %>% round(digit) %>% unique() %>% sort() %>% distance() %>% mode()
    dy <- pull(pollu, {{y}}) %>% round(digit) %>% unique() %>% sort() %>% distance() %>% mode()
    
    return(st_as_stars(bound, dx = dx, dy = dy))
    
  } else if (str_detect(filenm, "xlsx?$")) {
    if (is.na(x) | is.na(y)) {
      stop("Please specify lon/lat column of the file using `x = ..., y = ...`")
    }
    
    pollu <- readxl::read_excel(filenm) %>% 
      mutate(across(where(is.character) & c({{x}},{{y}}), as.numeric))
    
    if (max(pull(pollu, {{x}})) > 180) {
      stop("The provided file dose not coordinated by WGS 84, please specify the CRS using `crs = ...`")
    }
    
    bound <- c(getMinMax(pull(pollu, {{x}})), getMinMax(pull(pollu, {{y}}))) %>%
      set_names(c("xmin", "xmax", "ymin", "ymax")) %>% st_bbox(crs = crs)
    
    dx <- pull(pollu, {{x}}) %>% round(digit) %>% unique() %>% sort() %>% distance() %>% mode()
    dy <- pull(pollu, {{y}}) %>% round(digit) %>% unique() %>% sort() %>% distance() %>% mode()
    
    return(st_as_stars(bound, dx = dx, dy = dy))
    
  } else if (str_detect(filenm, "nc$")) {
    xy <- ncmeta::nc_dims(filenm)$name %>% set_names(\(x) case_when(
      str_detect(x, "x|lon|Lon") ~ "x", str_detect(x, "y|lat|Lat") ~ "y", TRUE ~ "other"
    ))
    
    nc <- ncdf4::nc_open(filenm)
    nx <- ncdf4::ncvar_get(nc, xy["x"]) %>% round(digit)
    ny <- ncdf4::ncvar_get(nc, xy["y"]) %>% round(digit)
    dx <- nx %>% sort(decreasing = T) %>% distance() %>% mode()
    dy <- ny %>% sort(decreasing = T) %>% distance() %>% mode()
    ncdf4::nc_close(nc)
    
    if (abs(nx[1] - nx[2]) < dx) {
      nx[1] <- nx[2] - dx
    } else if (abs(nx[length(nx)] - nx[length(nx) - 1]) < dx) {
      nx[length(nx)] <- nx[length(nx) - 1] + dx
    } else if (abs(ny[1] - ny[2]) < dy) {
      ny[1] <- ny[2] - dy
    } else if (abs(nx[length(ny)] - nx[length(ny) - 1]) < dy) {
      ny[length(ny)] <- ny[length(ny) - 1] + dy
    }
    
    return(expand_grid(x = nx, y = ny, values = 0) %>% st_as_stars(coords = 1:2) %>% st_set_crs(crs))
    
  } else if (str_detect("tif$|tiff$|asc$")) {
    pollu <- read_stars(filenm)
    xy <- st_dimensions(pollu) %>% names() %>% set_names(\(x) case_when(
      str_detect(x, "x|lon|Lon") ~ "x", 
      str_detect(x, "y|lat|Lat") ~ "y"
    ))
    
    nx <- st_get_dimension_values(pollu, xy["x"]) %>% round(digit)
    ny <- st_get_dimension_values(pollu, xy["x"]) %>% round(digit)
    dx <- nx %>% sort(decreasing = T) %>% distance() %>% mode()
    dy <- ny %>% sort(decreasing = T) %>% distance() %>% mode()
    
    if (abs(nx[1] - nx[2]) < dx) {
      nx[1] <- nx[2] - dx
    } else if (abs(nx[length(nx)] - nx[length(nx) - 1]) < dx) {
      nx[length(nx)] <- nx[length(nx) - 1] + dx
    } else if (abs(ny[1] - ny[2]) < dy) {
      ny[1] <- ny[2] - dy
    } else if (abs(nx[length(ny)] - nx[length(ny) - 1]) < dy) {
      ny[length(ny)] <- ny[length(ny) - 1] + dy
    }
    
    return(expand_grid(x = nx, y = ny, values = 0) %>% st_as_stars(coords = 1:2) %>% st_set_crs(crs))
    
  }
  
}

getGrid(filenm)

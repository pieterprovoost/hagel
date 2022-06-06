knmi_api_url <- "https://api.dataplatform.knmi.nl/open-data"
knmi_api_version <- "v1"

#' Get KNMI image filenames.
#'
#' @param dataset_name dataset name.
#' @param dataset_version dataset version.
#' @param after return images dated after this time (POSIXct), if not provided then defaults to one hour ago.
#' @param max_keys maximum number of keys to return.
#' @return Filenames.
#' @export
knmi_filenames <- function(dataset_name, dataset_version, after = NULL, max_keys = 6) {
  if (is.null(after)) {
    after <- lubridate::now("UTC") - lubridate::minutes(30)
  }
  url <- glue("{knmi_api_url}/{knmi_api_version}/datasets/{dataset_name}/versions/{dataset_version}/files")
  res_sample_file <- httr::GET(
    url,
    httr::add_headers(Authorization = Sys.getenv("KNMI_API_KEY")),
    query = list("maxKeys" = 1)
  )
  if (res_sample_file$status_code != 200) {
    stop(glue("Received status code {res_sample_file$status_code} for {url}"))
  }
  sample_file <- httr::content(res_sample_file, "parsed")$files[[1]]$filename
  filename_base <- stringr::str_extract(sample_file, ".*_")
  timestamp <- format(after, "%Y%m%d%H%M")
  start_after = glue::glue("{filename_base}{timestamp}.h5")
  url <- glue("{knmi_api_url}/{knmi_api_version}/datasets/{dataset_name}/versions/{dataset_version}/files")
  res_files <- httr::GET(
    url,
    httr::add_headers(Authorization = Sys.getenv("KNMI_API_KEY")),
    query = list("maxKeys" = max_keys, "startAfterFilename" = start_after)
  )
  if (res_files$status_code != 200) {
    stop(glue("Received status code {res_files$status_code} for {url}"))
  }
  files <- httr::content(res_files, "parsed")$files
  filenames <- sapply(files, function(x) { x$filename })
  filenames
}

#' Get KNMI file URLs.
#'
#' @param dataset_name dataset name.
#' @param dataset_version dataset version.
#' @param after return images dated after this time (POSIXct), if not provided then defaults to one hour ago.
#' @param max_keys maximum number of keys to return.
#' @return Image URLs.
#' @export
knmi_urls <- function(dataset_name = "radar_reflectivity_composites", dataset_version = "2.0", after = NULL, max_keys = 12) {
  filenames <- knmi_filenames(dataset_name, dataset_version, after, max_keys)
  sapply(filenames, function(filename) {
    url <- glue("{knmi_api_url}/{knmi_api_version}/datasets/{dataset_name}/versions/{dataset_version}/files/{filename}/url")
    res <- httr::GET(
      url,
      httr::add_headers(Authorization = Sys.getenv("KNMI_API_KEY"))
    )
    if (res$status_code != 200) {
      stop(glue("Received status code {res$status_code} for {url}"))
    }
    httr::content(res, "parsed")$temporaryDownloadUrl
  })
}

#' Read KNMI radar file.
#'
#' @param filename local filename.
#' @param radar_proj proj string of the radar file.
#' @return A stars object.
#' @export
read_knmi_file <- function(filename, radar_proj = "+proj=stere +lat_0=90 +lon_0=0 +lat_ts=60 +a=6378140 +b=6356750 +x_0=0 y_0=0") {
  attr <- rhdf5::h5readAttributes(filename, "geographic")
  proj <- rhdf5::h5readAttributes(filename, "geographic/map_projection")
  nc <- ncdf4::nc_open(filename)
  m <- ncdf4::ncvar_get(nc, "image1/image_data")
  r <- raster::raster(t(m))
  sw <- st_transform(st_sfc(st_point(c(attr$geo_product_corners[1], attr$geo_product_corners[2])), crs = 4326), radar_proj)
  nw <- st_transform(st_sfc(st_point(c(attr$geo_product_corners[3], attr$geo_product_corners[4])), crs = 4326), radar_proj)
  ne <- st_transform(st_sfc(st_point(c(attr$geo_product_corners[5], attr$geo_product_corners[6])), crs = 4326), radar_proj)
  se <- st_transform(st_sfc(st_point(c(attr$geo_product_corners[7], attr$geo_product_corners[8])), crs = 4326), radar_proj)
  ext <- raster::extent(st_coordinates(sw)[1], st_coordinates(se)[1], st_coordinates(sw)[2], st_coordinates(nw)[2])
  raster::extent(r) <- ext
  raster::crs(r) <- radar_proj
  rs <- st_as_stars(r)
  rs[rs == 255] <- NA
  return(rs)
}

#' Read radar images from KNMI.
#'
#' @param dataset_name dataset name.
#' @param dataset_version dataset version.
#' @param after return images dated after this time (POSIXct), if not provided then defaults to one hour ago.
#' @param max_keys maximum number of keys to return.
#' @return List of stars objects.
#' @export
radar_knmi <- function(dataset_name = "radar_reflectivity_composites", dataset_version = "2.0", after = NULL, max_keys = 12) {
  download_urls <- knmi_urls(dataset_name, dataset_version, after, max_keys)
  lapply(download_urls, function(download_url) {
    temp <- tempfile()
    download.file(download_url, temp)
    read_knmi_file(temp)
  })
}

#' List of KNMI datasets.
#'
#' @return Tibble with dataset names and versions.
#' @export
knmi_datasets <- tibble::tibble(
  dataset_name = c("radar_reflectivity_composites", "radar_hail_warning_5min"),
  dataset_version = c("2.0", "1.0")
)

#' Plot a KNMI radar image.
#'
#' @param knmi_raster KNMI image.
#' @return ggplot object.
#' @export
plot_knmi <- function(knmi_raster) {
  netherlands <- st_crop(ne_countries(country = "netherlands", type = "countries", returnclass = "sf", scale = "large"), c(xmin = 0, xmax = 10, ymin = 40, ymax = 60))
  belgium <- ne_countries(country = "belgium", type = "countries", returnclass = "sf", scale = "large")
  knmi_raster <- knmi_raster %>% st_transform(4326)
  background <- knmi_raster
  background[is.na(background)] <- 255
  background[background < 255] <- NA
  ggplot() +
    geom_sf(data = netherlands, fill = NA) +
    geom_sf(data = belgium, fill = NA) +
    geom_stars(data = knmi_raster, color = NA, alpha = 0.8) +
    geom_stars(data = background, fill = "#eeeeee", color = NA) +
    scale_fill_viridis(na.value = "#eeeeee", limits = c(0, 255)) +
    theme_void()
}

#' Convert KNMI radar image to sfc polygons.
#'
#' @param knmi_raster KNMI image.
#' @param method smoothing method.
#' @return sfc.
#' @export
knmi_to_polygon <- function(knmi_raster, threshold = 50, method = "chaikin", ...) {
  knmi_raster[knmi_raster <= threshold] <- NA
  pols <- raster::rasterToPolygons(as(knmi_raster, "Raster"), dissolve = TRUE)
  if (!is.null(pols)) {
    pols %>%
      st_as_sf() %>%
      st_union() %>%
      smoothr::smooth(method = method, ...)
  }
}

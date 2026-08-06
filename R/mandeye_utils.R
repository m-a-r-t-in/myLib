#' Read and subsample an HD map trajectory
#'
#' Reads a trajectory from a CSV file, removes invalid records,
#' sorts the points by time, and subsamples them by time interval,
#' distance, or row position. The result is converted to a point
#' [terra::SpatVector] object.
#'
#' The input file must not contain a header and must contain columns
#' in the following order:
#' `time_ns`, `x`, `y`, `z`, `qx`, `qy`, `qz`, `qw`.
#' The `time_ns` value is expected to be a non-negative integer
#' representing nanoseconds since the Unix epoch.
#'
#' @param file A character string specifying the path to the input CSV file.
#' @param method Subsampling method. One of `"time"`, `"distance"`,
#'   or `"nth"`.
#' @param every_sec A positive numeric value specifying the time interval
#'   in seconds. Used only when `method = "time"`.
#' @param min_dist A positive numeric value specifying the minimum distance
#'   between retained points. Used only when `method = "distance"`.
#'   The unit corresponds to the unit of the input coordinates.
#' @param nth An integer greater than or equal to 1. When
#'   `method = "nth"`, every `nth` point is retained.
#' @param distance_3d A logical value. If `TRUE`, distances are calculated
#'   using the `x`, `y`, and `z` coordinates. If `FALSE`, only the
#'   `x` and `y` coordinates are used.
#' @param crs The coordinate reference system passed to the `crs` argument
#'   of [terra::vect()]. For example, `"EPSG:5514"`.
#' @param verbose A logical value indicating whether to print information
#'   about the original and retained number of points.
#'
#' @return A point object of class [terra::SpatVector]. The geometry is
#'   created from the `x` and `y` columns. The attribute table contains:
#'   \describe{
#'     \item{time}{Absolute UTC time in ISO 8601 format with nanoseconds.}
#'     \item{time_s}{Relative time in seconds from the start of the trajectory.}
#'     \item{x}{X coordinate.}
#'     \item{y}{Y coordinate.}
#'     \item{z}{Z coordinate.}
#'   }
#'
#' @details
#' When subsampling by time, the first point from each time interval
#' of length `every_sec` is retained.
#'
#' When subsampling by distance, the first point is always retained.
#' Each subsequent point is retained when its distance from the last
#' retained point is at least `min_dist`.
#'
#' Regardless of the selected subsampling method, the final point
#' of the trajectory is always retained.
#'
#' @examples
#' \dontrun{
#' trajectory <- read_hdmap_trajectory(
#'   file = "trajectory.csv",
#'   method = "time",
#'   every_sec = 0.1,
#'   crs = "EPSG:5514"
#' )
#'
#' trajectory_distance <- read_hdmap_trajectory(
#'   file = "trajectory.csv",
#'   method = "distance",
#'   min_dist = 0.5,
#'   distance_3d = FALSE
#' )
#'
#' trajectory_nth <- read_hdmap_trajectory(
#'   file = "trajectory.csv",
#'   method = "nth",
#'   nth = 10L
#' )
#' }
#'
#' @export


read_hdmap_trajectory <- function(
    file,
    method = c("time", "distance", "nth"),
    every_sec = 0.05,
    min_dist = 0.05,
    nth = 10L,
    distance_3d = TRUE,
    crs = "",
    verbose = TRUE
) {
  method <- match.arg(method)
  
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Nainstaluj balíček terra: install.packages('terra')")
  }
  
  if (!file.exists(file)) {
    stop("Soubor neexistuje: ", file)
  }
  
  # Soubor má sloupce:
  # time_ns, x, y, z, qx, qy, qz, qw
  #
  # Čas čteme jako character, aby nedošlo ke ztrátě
  # přesnosti u nanosekundového timestampu.
  d <- utils::read.csv(
    file,
    header = FALSE,
    col.names = c(
      "time_ns", "x", "y", "z",
      "qx", "qy", "qz", "qw"
    ),
    colClasses = c(
      "character",
      "numeric", "numeric", "numeric",
      "NULL", "NULL", "NULL", "NULL"
    ),
    check.names = FALSE
  )
  
  if (nrow(d) == 0L) {
    stop("Soubor neobsahuje žádné body.")
  }
  
  # Odstranění neplatných řádků
  valid <- complete.cases(d[, c("time_ns", "x", "y", "z")])
  d <- d[valid, , drop = FALSE]
  
  if (nrow(d) == 0L) {
    stop("Po odstranění neplatných řádků nezůstal žádný bod.")
  }
  
  if (!all(grepl("^[0-9]+$", d$time_ns))) {
    stop("První sloupec neobsahuje očekávaný čas v nanosekundách.")
  }
  
  # Rozdělení timestampu na celé sekundy a nanosekundovou část.
  # Předejde se tím převodu celého 19místného čísla na double.
  nch <- nchar(d$time_ns)
  long_timestamp <- nch > 9L
  
  seconds <- numeric(nrow(d))
  nanoseconds <- numeric(nrow(d))
  
  seconds[long_timestamp] <- as.numeric(
    substr(
      d$time_ns[long_timestamp],
      1L,
      nch[long_timestamp] - 9L
    )
  )
  
  nanoseconds[long_timestamp] <- as.numeric(
    substr(
      d$time_ns[long_timestamp],
      nch[long_timestamp] - 8L,
      nch[long_timestamp]
    )
  )
  
  nanoseconds[!long_timestamp] <- as.numeric(
    d$time_ns[!long_timestamp]
  )
  
  # Relativní čas v sekundách od začátku trajektorie
  d$time_s <- (
    seconds - seconds[1L]
  ) + (
    nanoseconds - nanoseconds[1L]
  ) / 1e9
  
  # Seřazení podle času, pokud by vstup nebyl seřazený
  if (is.unsorted(d$time_s)) {
    ord <- order(d$time_s)
    d <- d[ord, , drop = FALSE]
    seconds <- seconds[ord]
    nanoseconds <- nanoseconds[ord]
    
    d$time_s <- (
      seconds - seconds[1L]
    ) + (
      nanoseconds - nanoseconds[1L]
    ) / 1e9
  }
  
  # Čitelný absolutní čas v UTC.
  # Nanosekundová část zůstává zachována jako text.
  d$time <- paste0(
    format(
      as.POSIXct(
        seconds,
        origin = "1970-01-01",
        tz = "UTC"
      ),
      format = "%Y-%m-%dT%H:%M:%S",
      tz = "UTC"
    ),
    ".",
    sprintf("%09.0f", nanoseconds),
    "Z"
  )
  
  n <- nrow(d)
  
  # ----------------------------------
  # Subsampling
  # ----------------------------------
  
  if (method == "time") {
    if (
      length(every_sec) != 1L ||
      !is.finite(every_sec) ||
      every_sec <= 0
    ) {
      stop("'every_sec' musí být kladné číslo.")
    }
    
    time_group <- floor(d$time_s / every_sec)
    keep <- which(!duplicated(time_group))
  }
  
  if (method == "nth") {
    nth <- as.integer(nth)
    
    if (length(nth) != 1L || is.na(nth) || nth < 1L) {
      stop("'nth' musí být celé číslo větší nebo rovno 1.")
    }
    
    keep <- seq.int(
      from = 1L,
      to = n,
      by = nth
    )
  }
  
  if (method == "distance") {
    if (
      length(min_dist) != 1L ||
      !is.finite(min_dist) ||
      min_dist <= 0
    ) {
      stop("'min_dist' musí být kladné číslo.")
    }
    
    coordinate_columns <- c("x", "y")
    
    if (isTRUE(distance_3d)) {
      coordinate_columns <- c(coordinate_columns, "z")
    }
    
    xyz <- as.matrix(d[, coordinate_columns, drop = FALSE])
    
    keep <- integer(n)
    keep[1L] <- 1L
    number_kept <- 1L
    last_kept <- 1L
    
    if (n > 1L) {
      for (i in 2L:n) {
        point_distance <- sqrt(
          sum((xyz[i, ] - xyz[last_kept, ])^2)
        )
        
        if (point_distance >= min_dist) {
          number_kept <- number_kept + 1L
          keep[number_kept] <- i
          last_kept <- i
        }
      }
    }
    
    keep <- keep[seq_len(number_kept)]
  }
  
  # Vždy zachovat poslední bod
  keep <- sort(unique(c(keep, n)))
  
  result <- d[
    keep,
    c("time", "time_s", "x", "y", "z"),
    drop = FALSE
  ]
  
  # Bodový SpatVector:
  # geometrie = x, y
  # atributy = time, time_s, x, y, z
  v <- terra::vect(
    result,
    geom = c("x", "y"),
    crs = crs,
    keepgeom = TRUE
  )
  
  if (isTRUE(verbose)) {
    message(
      sprintf(
        "Trajektorie: %s -> %s bodů (%.1f %%)",
        format(n, big.mark = " "),
        format(nrow(v), big.mark = " "),
        100 * nrow(v) / n
      )
    )
  }
  
  v
}

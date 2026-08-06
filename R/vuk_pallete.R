# ---- VUKOZ palette definition ----
vuk_palette <- c(
  '#d73526', '#efd621', '#dbce10', '#95c11f', '#3aaa35','#85cee4', '#bce4fa', '#6a509d', '#140302'
)
names(vuk_palette) <- c(
  'VUK_cervena',
  'VUK_zluta_1',
  'VUK_zluta_2',
  'VUK_zelena_1',
  'VUK_zelena_2',
  'VUK_modra_2',
  'VUK_modra_1',
  'VUK_fialova',
  'VUK_RichBlack'
)



#' get_vuk_palette
#'
#' Return a color palette using VÚKOZ visual identity.
#'
#' The function selects colors in CIE Lab space to maximize mutual contrast.
#' - If `n = 2` and `color = NULL`: returns the globally most distant pair.
#' - If `n = 2` and `color` is given: returns this color and the palette color
#'   with maximum distance to it.
#' - If `n > 2`: selection starts with either the user-provided color or the
#'   most distant pair, then continues with farthest-first.
#'
#' @param n integer. Number of colors to return (>= 1).
#' @param color optional. Either a palette color name (from `names(vuk_palette)`)
#'   or a valid hex code (e.g., "#123456"). If given, it will be the first color.
#'   The rest are selected from the official palette.
#' @return Named character vector with `n` hex colors.
#' @keywords VUK palette, color, hex
#' @export
#' @examples
#' get_vuk_palette()
#' get_vuk_palette(2)
#' get_vuk_palette(2, color = "VUK_modra_1")
#' get_vuk_palette(3, color = "#112233")
get_vuk_palette <- function(n = 9, color = NULL) {

  # ---- Input validation ----
  if (!is.numeric(n) || length(n) != 1 || n < 1 || n %% 1 != 0) {
    stop("Parameter 'n' must be a positive integer.")
  }
  
  # Helper: convert colors (hex or names) to Lab coordinates
  to_lab <- function(hex_vec) {
    grDevices::convertColor(
      t(grDevices::col2rgb(hex_vec)) / 255,
      from = "sRGB", to = "Lab", scale.in = 1
    )
  }
  
  # Helper: normalize arbitrary color input to hex
  normalize_hex <- function(col) {
    val <- try(grDevices::col2rgb(col), silent = TRUE)
    if (inherits(val, "try-error")) {
      stop("Unknown color specification: '", col,
           "'. Use a palette name or a valid hex code.")
    }
    paste0("#", toupper(sprintf("%02x%02x%02x", val[1], val[2], val[3])))
  }
  
  # ---- Determine starting set ----
  selected_hex   <- character(0)
  selected_names <- character(0)
  candidates     <- vuk_palette
  
  if (!is.null(color)) {
    # If color is a palette name
    if (color %in% names(vuk_palette)) {
      selected_hex   <- unname(vuk_palette[color])
      selected_names <- color
      candidates     <- candidates[names(candidates) != color]
    } else {
      # Otherwise treat it as arbitrary hex
      first_hex <- normalize_hex(color)
      match_idx <- which(toupper(vuk_palette) == toupper(first_hex))
      if (length(match_idx) == 1) {
        selected_hex   <- unname(vuk_palette[match_idx])
        selected_names <- names(vuk_palette)[match_idx]
        candidates     <- candidates[-match_idx]
      } else {
        selected_hex   <- first_hex
        selected_names <- "custom"
      }
    }
  }
  
  # ---- Special case: n = 2 ----
  if (n == 2 && is.null(color)) {
    # Find the most distant pair among all palette colors
    lab <- to_lab(vuk_palette)
    dist_mat <- as.matrix(dist(lab))
    diag(dist_mat) <- -Inf
    ij <- which(dist_mat == max(dist_mat), arr.ind = TRUE)[1, ]
    out <- vuk_palette[c(ij[1], ij[2])]
    names(out) <- names(vuk_palette)[c(ij[1], ij[2])]
    return(out)
  }
  if (n == 2 && !is.null(color)) {
    # Already have first color; find farthest candidate
    lab_cand <- to_lab(candidates)
    lab_sel  <- to_lab(selected_hex)
    scores <- apply(lab_cand, 1, function(row) {
      sqrt(sum((row - lab_sel)^2))
    })
    k <- which.max(scores)
    selected_hex   <- c(selected_hex, unname(candidates[k]))
    selected_names <- c(selected_names, names(candidates)[k])
    return(setNames(selected_hex, selected_names))
  }
  
  # ---- General case: n > 2 ----
  if (length(selected_hex) == 0) {
    # If no custom start: pick the globally most distant pair
    lab_all <- to_lab(candidates)
    dist_mat <- as.matrix(dist(lab_all))
    diag(dist_mat) <- -Inf
    ij <- which(dist_mat == max(dist_mat), arr.ind = TRUE)[1, ]
    selected_hex   <- unname(c(candidates[ij[1]], candidates[ij[2]]))
    selected_names <- names(candidates)[c(ij[1], ij[2])]
    candidates     <- candidates[-unique(c(ij[1], ij[2]))]
  }
  
  # Farthest-first selection until we have n colors
  while (length(selected_hex) < n) {
    lab_cand <- to_lab(candidates)
    lab_sel  <- to_lab(selected_hex)
    # Score each candidate by its minimum distance to already selected colors
    scores <- sapply(seq_len(nrow(lab_cand)), function(i) {
      min(apply(lab_sel, 1, function(sr) sqrt(sum((lab_cand[i, ] - sr)^2))))
    })
    k <- which.max(scores)
    selected_hex   <- c(selected_hex, unname(candidates[k]))
    selected_names <- c(selected_names, names(candidates)[k])
    candidates     <- candidates[-k]
  }
  
  out <- selected_hex
  names(out) <- selected_names
  out
}


#' show_vuk_palette
#'
#' Display and return the full VÚKOZ palette.
#'
#' Produces a bar plot with color names and returns the palette
#' as a named character vector (invisible).
#'
#' @keywords VUK palette show
#' @export
#' @return Named character vector with all palette colors (invisible).
#' @examples
#' show_vuk_palette()
show_vuk_palette <- function() {
  
  graphics::barplot(rep(1, length(vuk_palette)),
                    col = vuk_palette,
                    border = NA,
                    axes = FALSE,
                    names.arg = names(vuk_palette),
                    las = 2, cex.names = 0.7)
  
  invisible(vuk_palette)
}

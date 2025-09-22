#' get_vuk_palette
#'
#' Function to return color pallete using VUK visual identity, maximum is 8 colors
#' @param int n colors to return
#' @keywords hex color pallete
#' @export
#' @examples
#' get_vuk_palette()
get_vuk_palette <- function(n) {
  palette_hex <- c('#d73526', '#efd621', '#dbce10', '#95c11f', '#3aaa35', '#85cee4', '#85cee4', '#85cee4')
  if (n > length(palette_hex)) {
    stop("Požadovaný počet barev je větší než počet dostupných ve vzorníku!")
  }
  
  # Převod do Lab pro výpočet kontrastu
  lab <- convertColor(col2rgb(palette_hex)/255, from='sRGB', to='Lab', scale.in=1)
  
  # 1) Vybereme první barvu: ta s nejvyšší průměrnou vzdáleností k ostatním (v Lab)
  pairdist <- function(i, j) {
    sqrt(sum((lab[,i] - lab[,j])^2))
  }
  avg_d <- sapply(seq_len(ncol(lab)), function(i) {
    mean(sapply(setdiff(seq_len(ncol(lab)), i), function(j) pairdist(i, j)))
  })
  chosen <- c(which.max(avg_d))
  
  # 2) Farthest-first: pokaždé přidáme barvu s maximální minimální vzdáleností k dosud zvoleným
  while (length(chosen) < n) {
    cand_scores <- sapply(setdiff(seq_len(ncol(lab)), chosen), function(i) {
      min(sapply(chosen, function(j) pairdist(i, j)))
    })
    next_id <- setdiff(seq_len(ncol(lab)), chosen)[which.max(cand_scores)]
    chosen <- c(chosen, next_id)
  }
  
  palette_hex[chosen]
}

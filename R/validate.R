validate_dna <- function(seq) {
  seq <- toupper(seq)
  grepl("^[ATCGN]+$", seq)
}

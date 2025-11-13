kmer_freq <- function(seq, k = 3) {
  seq <- toupper(seq)
  n <- nchar(seq)
  kmers <- substring(seq, 1:(n-k+1), k:(n))
  table(kmers)
}

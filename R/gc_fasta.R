gc_content <- function(seq) {
  seq <- toupper(gsub("[^ATGC]", "", seq))
  (sum(strsplit(seq, "")[[1]] %in% c("G", "C")) / nchar(seq)) * 100
}

gc_fasta <- function(filepath) {
  linhas <- readLines(filepath)

  seqs <- list()
  atual <- ""

  for (linha in linhas) {
    if (startsWith(linha, ">")) {
      if (nchar(atual) > 0) {
        seqs <- c(seqs, list(atual))
      }
      atual <- ""
    } else {
      atual <- paste0(atual, linha)
    }
  }

  seqs <- c(seqs, list(atual))

  gc_vals <- sapply(seqs, gc_content)
  return(gc_vals)
}

# Em seguida rode:
gc_fasta("fake.fasta")

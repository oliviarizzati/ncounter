dna_translate <- function(seq) {

  # Deixar sempre maiúscula
  seq <- toupper(seq)

  # Remover espaços e caracteres inválidos
  seq <- gsub("[^ATCG]", "", seq)

  # Se o comprimento não for múltiplo de 3, corta o excesso
  if (nchar(seq) %% 3 != 0) {
    warning("O comprimento da sequência não é múltiplo de 3. Cortando o excesso final.")
    seq <- substr(seq, 1, nchar(seq) - (nchar(seq) %% 3))
  }

  # Separar a sequência em códons
  codons <- substring(seq, seq(1, nchar(seq)-2, 3), seq(3, nchar(seq), 3))

  # Tabela genética padrão
  tabela <- c(
    "TTT"="F","TTC"="F","TTA"="L","TTG"="L",
    "CTT"="L","CTC"="L","CTA"="L","CTG"="L",
    "ATT"="I","ATC"="I","ATA"="I","ATG"="M",
    "GTT"="V","GTC"="V","GTA"="V","GTG"="V",
    "TCT"="S","TCC"="S","TCA"="S","TCG"="S",
    "CCT"="P","CCC"="P","CCA"="P","CCG"="P",
    "ACT"="T","ACC"="T","ACA"="T","ACG"="T",
    "GCT"="A","GCC"="A","GCA"="A","GCG"="A",
    "TAT"="Y","TAC"="Y","TAA"="*","TAG"="*",
    "CAT"="H","CAC"="H","CAA"="Q","CAG"="Q",
    "AAT"="N","AAC"="N","AAA"="K","AAG"="K",
    "GAT"="D","GAC"="D","GAA"="E","GAG"="E",
    "TGT"="C","TGC"="C","TGA"="*","TGG"="W",
    "CGT"="R","CGC"="R","CGA"="R","CGG"="R",
    "AGT"="S","AGC"="S","AGA"="R","AGG"="R",
    "GGT"="G","GGC"="G","GGA"="G","GGG"="G"
  )

  # Traduzir
  aa <- tabela[codons]

  # Se tiver algum codon inválido
  if (any(is.na(aa))) {
    warning("Alguns códons não foram reconhecidos e serão marcados como 'X'.")
    aa[is.na(aa)] <- "X"
  }

  paste(aa, collapse = "")
}



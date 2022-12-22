# Extract sequence ID
extract_accession <- function(sequence_id) {
  full_info <- xmlParse(entrez_fetch(db = "nucleotide", id = sequence_id, rettype = "xml"))
  seq_accid <- xmlToList(full_info)[["GBSeq"]][["GBSeq_accession-version"]]

  return(seq_accid)
}



# Extract NCBI information about a specie
genomic_info <- function(sp_name) {
  # Search for SRA accessions
  sra_nacc <- entrez_search(db = "sra", term = paste0(sp_name, "[ORGN]"), retmax = 10000)$count

  # Mitochondrial genome accession
  mt_flags <- "[ORGN] AND mitochondrion AND complete genome"
  mt_cts   <- entrez_search(db = "nucleotide", term = paste0(sp_name, mt_flags), retmax = 10000)$count

  if (mt_cts > 0) {
    mt_accid <- entrez_search(db = "nucleotide", term = paste0(sp_name, mt_flags), retmax = 10000)$ids
    mt_seqid <- unlist(lapply(X = mt_accid, FUN = extract_accession))
    mt_seqnc <- str_detect(mt_seqid, pattern = "^NC_")

    if (any(mt_seqnc)) {
      mt_seqid <- mt_seqid[mt_seqnc][1]
    } else {
      mt_seqid <- mt_seqid[1]
    }
  } else {
    mt_seqid <- as.character(NA)
  }

  # Nuclear genome accession
  geno_search <- entrez_search(db = "genome", term = paste0(sp_name, "[ORGN]"), maxret = 10000)
  if (geno_search$count > 0) {
    geno_info <- entrez_summary(db = "genome", id = geno_search$ids[1])
    geno_accs <- geno_info["assembly_accession"]
    geno_stat <- geno_info["status"]
  } else {
    geno_accs <- as.character(NA)
    geno_stat <- as.character(NA)
  }

  # Build output table
  if (is.na(geno_accs)) {
    sp_info <- tibble("SPNAME" = sp_name, "NSRA" = sra_nacc, "MTGENOME" = mt_seqid, "GENOME" = NA, "STATUS" = NA)
  } else {
    sp_info <- tibble("SPNAME" = sp_name, "NSRA" = sra_nacc, "MTGENOME" = mt_seqid, "GENOME" = geno_accs[[1]][1], "STATUS" = geno_stat[[1]][1])
  }

  return(sp_info)
}



# Download genome sequence
fetch_genome <- function(assembly, out_file) {
  assembly_id   <- entrez_search(db = "assembly", term = assembly)$ids
  assembly_accs <- entrez_link(dbfrom = "assembly", db = "nuccore", id = assembly_id)$links["assembly_nuccore"][[1]]

  for (CHR in assembly_accs) {
    rec <- entrez_fetch(db = "nuccore", id = CHR, rettype = "fasta")
    cat(rec, file = out_file, append = TRUE)
  }
}



# Download mitogenome sequence
fetch_mitogenome <- function(accession, out_file) {
  mitogenome_id  <- entrez_search(db = "nuccore", term = accession)$ids
  mitogenome_rec <- entrez_fetch(db = "nuccore", id = mitogenome_id, rettype = "fasta")

  cat(mitogenome_rec, file = out_file, append = FALSE)
}





#' @export
get_bioproject_id <- function(geo_id) {
  message("Retrieving BioProject id for GEO accession: ", geo_id)
  readLines(paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", geo_id)) %>%
    paste(., collapse = "") %>%
    str_extract(., "PRJNA\\d+")
}

#' @export
get_bioproject_info <- function(bioproject_id) {
  message("Retrieving info for BioProject accession: ", bioproject_id)
  paste0("https://www.ebi.ac.uk/ena/data/warehouse/filereport?accession=", bioproject_id, "&result=read_run") %>%
    read_tsv(., col_types = cols(fastq_bytes = col_character()))
}

#' @export
get_geo_info <- function(geo_ids) {
  tibble(geo_id = geo_ids) %>%
    mutate(
      bioproject_id = map(geo_id, get_bioproject_id),
      bioproject_info = map(bioproject_id, get_bioproject_info)
    ) %>%
    unnest(cols = c(bioproject_id, bioproject_info))
}

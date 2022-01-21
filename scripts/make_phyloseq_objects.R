# make_phyloseq_objects.R -------------------------------------------------

# Make phyloseq objects for each run

# Setup -------------------------------------------------------------------

pacman::p_load(tidyverse, phyloseq, ape)

# Read input files --------------------------------------------------------

# Define helper function
read_phylo_files <- function(pattern) {
  
  x <- list.files("./data", pattern = pattern, full.names = TRUE)
  
  if (pattern == "tree") {
    files <- purrr::map(.x = x, .f = ~ ape::read.tree(.x))
    } else {
      files <- purrr::map(.x = x, .f = ~ readr::read_tsv(.x, show_col_types = FALSE))
    }
  
  names(files) <- paste0(pattern, c("_run1", "_run2"))
  return(files)
}

# Read files
files <- purrr::map(c("OTU", "taxonomy", "tree"), ~ read_phylo_files(.x))

# Read metadata-- include this as sample data in the phyloseq object
sample_metadata <- readr::read_tsv("./metadata/sample_metadata.txt", show_col_types = FALSE) %>% 
  dplyr::select(-c(8:9)) %>% 
  dplyr::slice_head(n = 9) %>% 
  purrr::set_names("library_prep_id", "dna_extract_id", "pm_number", "appt_date", "sample_type", 
                   "swab_buffer", "re_reviewed") %>% 
  tibble::column_to_rownames("library_prep_id") # need to match column names in OTU table
  

# Tidy tables -------------------------------------------------------------

# For OTU tables, clean up field names and remove taxonomy label
otu_tables <- purrr::map(
  .x = files[[1]],
  .f = ~ dplyr::rename(.x, "feature_id" = 1) %>% 
    dplyr::select(-taxonomy) %>% 
    tibble::column_to_rownames("feature_id") %>% 
    as.matrix()
)

# For taxonomy tables, add field names
taxa_tables <- purrr::map(
  .x = files[[2]], 
  .f = ~ purrr::set_names(.x, "feature_id", "kingdom", "phylum", "class", 
                          "order", "family", "genus", "species", "confidence") %>% 
    tibble::column_to_rownames("feature_id") %>% 
    dplyr::select(-confidence) %>% 
    as.matrix()
)

trees <- files[[3]]

# Build phyloseq objects --------------------------------------------------

# Build
ps <- purrr::pmap(
  .l = list(otu_tables, taxa_tables, trees), 
  .f = ~ phyloseq::phyloseq(
    phyloseq::sample_data(sample_metadata), 
    phyloseq::otu_table(..1, taxa_are_rows = TRUE), 
    phyloseq::tax_table(..2), 
    phyloseq::phy_tree(..3)
  ))

names(ps) <- c("run1", "run2")

# Check
purrr::map(
  .x = ps, 
  .f = ~ if(class(.x) == "phyloseq") {
    message("Successfully built phyloseq object")
  } else {
    stop("Error building phyloseq object")
  }
)

# Save
filenames <- paste0("./data/", names(ps), "_phyloseq_obj.Rds")
purrr::map2(
  .x = ps, 
  .y = filenames, 
  .f = ~ saveRDS(.x, file = .y)
)

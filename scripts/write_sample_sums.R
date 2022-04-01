# write_sample_sums.R -----------------------------------------------------

# Compute sample abundances from unfiltered data and write to tsv file

# Setup -------------------------------------------------------------------

pacman::p_load(tidyverse, phyloseq)

# Compute sample sums -----------------------------------------------------

# Prepare input data
files <- list.files("./data", ".Rds", full.names = TRUE)
ps <- purrr::map(files, ~ readRDS(.x))

map(ps, ~ data.frame(reads = phyloseq::sample_sums(.x)))


# Get sample sums
ps_sums <- purrr::map2(ps, c(1, 2), ~ phyloseq::sample_sums(.x) %>% 
                        data.frame() %>% 
                        purrr::set_names("abundance") %>% 
                        tibble::rownames_to_column("sample_id") %>% 
                        dplyr::mutate(run = .y)) %>% 
  dplyr::bind_rows()

# Save to tsv file
readr::write_tsv(ps_sums, file = "./output/sample_sums.tsv")

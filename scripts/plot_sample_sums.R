# plot_sample_sums.R ------------------------------------------------------

# Quick script to plot sample sums for each run

# Setup -------------------------------------------------------------------

# Libraries
pacman::p_load(tidyverse, scales)

# Input file
ps_sums <- readr::read_tsv("./output/sample_sums.tsv", show_col_types = FALSE)

# Plot --------------------------------------------------------------------

barplot <- ps_sums %>% 
  dplyr::mutate(sample_no = as.numeric(stringr::str_extract(sample_id, "[0-9]{1,}$"))) %>% 
  ggplot2::ggplot(ggplot2::aes(forcats::fct_reorder(sample_id, sample_no, .desc = FALSE), abundance, fill = as.character(run))) + 
  ggplot2::geom_col(position = "dodge") + 
  ggplot2::scale_y_continuous(labels = scales::label_comma()) +
  labs(x = "Sample ID", y = "Abundance", fill = "Run", 
       title = "Abundance of microbial taxa across lung samples and Illumina iSeq 100 runs")

# Save
ggplot2::ggsave(
  filename = "sample_sums.png", 
  plot = barplot, 
  device = "png", 
  path = "./figures", 
  height = 5, 
  width = 5)

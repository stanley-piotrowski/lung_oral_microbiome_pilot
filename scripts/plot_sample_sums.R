# plot_sample_sums.R ------------------------------------------------------

# Quick script to plot sample sums for each run

# Setup -------------------------------------------------------------------

# Libraries
pacman::p_load(tidyverse, scales)

# Input files
ps_sums <- readr::read_tsv("./output/sample_sums.tsv", show_col_types = FALSE)
patient_disease_metadata <- readr::read_tsv("./metadata/disease_status_metadata.txt", show_col_types = FALSE)
sample_metadata <- readr::read_tsv("./metadata/sample_metadata.txt", show_col_types = FALSE)

# Plot --------------------------------------------------------------------

# Bind disease metadata with sample metadata
barplot_input <- ps_sums %>% 
  dplyr::left_join(sample_metadata, by = c("sample_id" = "library_prep_id")) %>% 
  dplyr::left_join(patient_disease_metadata, by = "pm_id") %>% 
  dplyr::mutate(sample_no = as.numeric(stringr::str_extract(sample_id, "[0-9]{1,}$")),
                sample_type = factor(sample_type, levels = c("Oral", "Left Lung", "Right Lung")))

sample_id_levels <- barplot_input %>% 
  dplyr::select(sample_id, sample_no) %>% 
  dplyr::arrange(sample_no) %>% 
  dplyr::pull("sample_id") %>% 
  unique()

barplot_input$sample_id <- factor(barplot_input$sample_id, levels = sample_id_levels)

barplot <- barplot_input %>% 
  ggplot2::ggplot(ggplot2::aes(sample_type, abundance, fill = as.character(run))) +
  #ggplot2::ggplot(ggplot2::aes(forcats::fct_reorder(sample_id, sample_no, .desc = FALSE), abundance, fill = as.character(run))) + 
  ggplot2::geom_col(position = "dodge") + 
  ggplot2::scale_y_continuous(labels = scales::label_comma()) +
  ggplot2::facet_wrap(~ pm_id, scales = "free_y") +
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

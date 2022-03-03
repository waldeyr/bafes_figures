library(readr)
library(dplyr)
library(stringr)
library(here)
# Gu Z, Eils R, Schlesner M (2016). “Complex heatmaps reveal patterns and correlations in multidimensional genomic data.” Bioinformatics.
library(ComplexHeatmap)

# get the data from biochemical and physiological tests
response_to_biochemical_and_physiological_tests <- read_delim(paste0(here(), "/response_to_biochemical_and_physiological_tests.tsv"), "\t", escape_double = FALSE, trim_ws = TRUE)
response_to_biochemical_and_physiological_tests$SDF <- str_pad(response_to_biochemical_and_physiological_tests$SDF, 4, pad = "0")

# get the selected group (B. paenibacillus like)
group_tax_b_paenibacillus <- read_delim(paste0(here(),"/group_tax_b_paenibacillus.tsv"), "\t", escape_double = FALSE, trim_ws = TRUE)

# Filtering the results for the selected group 
b_paenibacillus_filtered <- response_to_biochemical_and_physiological_tests %>%
  filter(as.numeric(SDF) %in% as.numeric(group_tax_b_paenibacillus$SDF))

# concatenating the species names and their isolates id (SDF9999)
rownames <- as.character(paste0(b_paenibacillus_filtered$Taxonomy, " (SDF", b_paenibacillus_filtered$SDF, ")"))

# builds a list of labels for the heatmap
colnames <- paste(c( 
  "Citrate utilization",
  "Propionate utilization",
  "NaCl 7%",	
  "NaCl 10%",
  "Lysozyme",
  "45 °C",	
  "65 °C",	
  "pH 5.7",	
  "Anaerobiosis",
  "Catalase",
  "Oxidase",
  "Hemolysis",
  "Nitrate Reduction",
  "Casein",
  "Gelatin",
  "Esculin",
  "Starch",
  "Phenylalanine",
  "Tyrosine Degradation",
  "Arginine Dihydrolase",
  "Lysine Decarboxylase",
  "Ornithine Decarboxylase",
  "Indole Production",
  "D-Glucose",
  "L-Arabinose",
  "Lactose",
  "Mannitol",
  "Sucrose",
  "D-Xylose",
  "Voges-Proskauer"))

# removing unused columns from the dataset
b_paenibacillus_filtered <- dplyr::select(b_paenibacillus_filtered, -c(NCBI, SDF)) 
View(b_paenibacillus_filtered)

# Converting the dataset into a numeric matrix omitting the first (string) column
matrix <- data.matrix(b_paenibacillus_filtered[,-1])
# View(matrix)

# assigning row names for the plot
rownames(matrix) <- rownames
# assigning column names for the plot
colnames(matrix) <- colnames

# margins
par(mar=c(1,1,1,1))

# heatmap
ht = Heatmap(
  matrix,
  #name = 'Bacillus_paenibacillus_group',
  #heatmap_legend_param = list(direction = "horizontal", title_position = "topcenter"),
  # clustering_distance_rows = function(x, y) 1 - cor(x, y),  #cluster and dendogram with a function that calculates pairwise distance
  clustering_distance_rows = "pearson",
  row_title = "SDF strains", # rows
  row_dend_side = "left", # side of rows dendogram
  row_names_side = "right", # side of row names
  row_dend_width = unit(4, "cm"),# high dendogram lines
  row_dend_gp = gpar(col = "gray20"), # color dendogram lines
  row_dend_reorder = TRUE,
  heatmap_legend_param = list(labels = c("Positive","Negative"), 
                              title = substitute(paste("Response to biochemical and physiological tests")),
                              direction = "horizontal",
                              ncol = 2),
  column_dend_height = unit(2, "cm"),# high dendogram lines
  column_dend_gp = gpar(col = "gray20"), # color dendogram lines
  column_title_side = "top",
  column_dend_side = "top",
  column_dend_reorder = TRUE,
  column_names_rot = 60,
  row_names_gp = gpar(fontsize = 8, fontface=3),
  column_names_gp = gpar(fontsize = 8),
  rect_gp = gpar(col = "white", lwd = 2),
  col = c( "#e5383b","#f8961e")
)
draw(ht, heatmap_legend_side = "bottom")


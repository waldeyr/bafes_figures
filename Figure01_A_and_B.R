library(ggplot2)
library(readr)
library(dplyr)
library(ggpubr)
library(stringr)
######################################################
## Raw data load
######################################################
raw_data <- read_delim("Figure01_A_and_B.tsv", 
                       delim = "\t", escape_double = FALSE, 
                       trim_ws = TRUE)
######################################################
## Genera filtered data
######################################################
genera_df <- raw_data %>% group_by(Genera) %>% summarize(Number = sum(Number))
genera_df <- genera_df %>% mutate(Percentage = Number/sum(Number)*100)
genera_df$Percentage <- round(genera_df$Percentage, digits=2)
genera_df = genera_df %>% arrange(Genera, desc(Number))

#######################################################
## Species filtered data
#######################################################
species_df <- filter(raw_data, Level == "Species")
species_df <- species_df %>% mutate(Percentage = Number/sum(Number)*100)
species_df$Percentage <- round(species_df$Percentage, digits=2)
species_df = species_df %>% arrange(Genera, Number)
species_df$id <- seq(1, nrow(species_df))
number_of_bar <- nrow(species_df)
#substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
angle <- 90 - 360 * (species_df$id-0.5) /number_of_bar 
species_df$hjust <- ifelse( angle < -90, 1, 0)
species_df$angle <- ifelse(angle < -90, angle+180, angle)
######################################################
## Colors palette
######################################################
my_palette = c("#001219", "#005F73", "#0BAAAD", "#EE9B00", "#FF5733", "#581845")

#######################################################
## Genera plot
#######################################################
genera_plot <- ggplot(genera_df, aes(x = reorder(Genera, Number), y = Number, fill = Genera)) + 
  geom_bar(
    width = 0.5, 
    stat="identity", 
    alpha = 0.75,
    color = "#ECECEC") +    
  coord_polar(theta = "y") +    
  xlab("") + 
  ylab("") +
  ylim(0, 238) + 
  geom_text(
    data = genera_df, 
    fontface="bold.italic",
    hjust = 1.05, 
    size = 2.5, 
    alpha = 0.6,
    aes(x = Genera, 
        y = 0, 
        label = paste0(Genera, ": ", Number, " (",Percentage, "%)"))
  ) +  
  scale_fill_manual(values = my_palette) + 
  theme_minimal() +
  labs(title = "A", subtitle = "Genera level (238  SDF strains)")+
  theme(
    legend.position = "none",
    # legend.position = c(1, .95),
    # legend.direction = "horizontal",
    # legend.text = element_text(face = "bold.italic", colour="#566573", size=8),
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),          
    axis.line = element_blank(),
    axis.text.y = element_blank(),          
    axis.text.x = element_blank(), 
    #axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "italic"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic")
  )

######################################################
## Colors palette adjusted for items supressed in this dataset
######################################################
my_palette = c("#001219", "#005F73", "#0BAAAD", "#EE9B00", "#581845")
#####################################################
## Species plot
#####################################################
species_plot <- ggplot(species_df, aes(x = as.factor(id), y = Number, fill = Genera )) +
  geom_bar(
           stat="identity", 
           alpha = 0.75,
           color = "#ECECEC"
           ) +
  scale_fill_manual(values = my_palette)+
  ylim(-20, 62) +
  coord_polar() +
  labs(title = "B", subtitle = "Species level (224 SDF strains)")+
  geom_text(
    data = species_df,
    aes(
      x=id,
      y=Number+1,
      hjust=hjust,
      label = paste0(Species, ": ", Number, " (",Percentage, "%)")
    ),
    color="black",
    fontface="bold.italic",
    alpha = 0.6,
    size = 2.5,
    angle = species_df$angle,
    inherit.aes = FALSE
  )+
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust=0.5, face = "italic"),
    plot.subtitle = element_text(hjust=0.5, face = "italic"),
    panel.spacing = unit(-1, "lines")
  )
####################################################
## Plot grid
####################################################
library(patchwork)
genera_plot | species_plot + plot_layout(ncol = 1)
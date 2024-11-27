library(ggplot2)
library(dplyr)
library(readr)
library(ggpubr)
library(extrafont)
library(svglite)
R0 <- read_csv('R0.csv')
Re <- read_csv('Reff.csv')
R0 <- R0 %>%
  mutate(
    `95%CI Upper` = as.numeric(`95%CI Upper`),
    `95%CI Lower` = as.numeric(`95%CI Lower`),
    `CI Range` = `95%CI Upper` - `95%CI Lower`
  )

custom_palette <- rev(c("#F9FF4D", "#00A9A0", "#0074D9", "#003D99", "#0000FF"))
deep_palette <- rev(c("#003B3F", "#006D6C", "#004B7D", "#00266B", "#0000A0")) 

p1 <- ggplot(R0, aes(x = `Median`, y = Coronavirus, fill = Coronavirus)) +
  geom_violin(trim = FALSE, color = "gray80", linewidth = 0.5, alpha = 0.6) + 
  geom_jitter(aes(color = Coronavirus), size = 1.5, width = 0.00001, alpha = 0.25) +  
  geom_boxplot(width = 0.1, color = deep_palette[1], outlier.shape = NA, alpha = 1) + 
  scale_fill_manual(values = custom_palette) + 
  scale_color_manual(values = custom_palette) + 
  labs(
    title = expression(paste("Violin Plot of ", R[0])), 
    x = "Median " ~ R[0],
    y = NULL  
  ) +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Arial Black"), 
    plot.title = element_text(size = 16, hjust = 0.5), 
    axis.title = element_text(size = 18), 
    axis.title.y = element_text(size = 18), 
    axis.text.y = element_text(size = 14, face = "bold"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none" 
  )+
  annotate("text", x = Inf, y = Inf, label = "A", hjust = 1.5, vjust = 2, size = 6, fontface = "bold") 
custom_palette_1 <- rev(c("#F9FF4D", "#00A9A0", "#0074D9", "#003D99"))
deep_palette_1 <- rev(c("#003B3F", "#006D6C", "#004B7D", "#00266B")) 
p2 <- ggplot(Re, aes(x = `Median`, y = Coronavirus, fill = Coronavirus)) +
  geom_violin(trim = FALSE, color = "gray80", linewidth = 0.5, alpha = 0.6) +
  geom_jitter(aes(color = Coronavirus), size = 1.5, width = 0.00001, alpha = 0.25) + 
  geom_boxplot(width = 0.1, color = deep_palette_1[1], outlier.shape = NA, alpha = 1) +
  scale_fill_manual(values = custom_palette_1) + 
  scale_color_manual(values = custom_palette_1) + 
  labs(
    title = expression(paste("Violin Plot of ", R[eff])),
    x = "Median " ~ R[eff],
    y = "Variant"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Arial Black"), 
    plot.title = element_text(size = 16, hjust = 0.5), 
    axis.title = element_text(size = 18), 
    axis.title.y = element_blank(), 
    axis.text.y = element_text(size = 14, face = "bold"), 
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none" 
  )+
  annotate("text", x = Inf, y = Inf, label = "B", hjust = 1.5, vjust = 2, size = 6, fontface = "bold") 


p1_legend <- get_legend(
  p1 + theme(
    legend.position = "bottom",     
    legend.direction = "horizontal",  
    legend.justification = "center",    
    legend.box.just = "center",  
    legend.margin = margin(t = 2, b = 2, l = 2, r = 0), 
    legend.text = element_text(family = "Arial Black", size = 12),
    legend.title = element_text(family = "Arial Black", size = 12) 
  )
)


p3 <- as_ggplot(p1_legend)

ggsave(
  filename = "p1.svg",  
  plot = p1,   
  width = 800 / 72,      
  height = 600 / 72,   
  units = "in",              
  dpi = 72                 
)
ggsave(
  filename = "p2.svg", 
  plot = p2,   
  width = 800 / 72,   
  height = 500 / 72,           
  units = "in",                
  dpi = 72                  
)
ggsave(
  filename = "p3.svg",  
  plot = p3,      
  width = 800 / 72,        
  height = 300 / 72,     
  units = "in",            
  dpi = 72                    
)


combined_plot <- ggarrange(
  p1, p2, p3,               
  ncol = 3,                
  nrow = 1,                    
  widths = c(3, 3, 1),          
  labels = c("A", "B", ""),     
  label.x = 0.05, label.y = 1.05 
)

plot(combined_plot)
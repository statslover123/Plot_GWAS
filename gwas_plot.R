library(tidyverse)
library(ggplot2)
library(dplyr)

myGM<-read.csv("genotypicmap.csv")
resultt<-read.csv("gwas_results.csv")

plot<-dplyr::left_join(resultt,myGM)
don <- plot %>% 
  
  # Compute chromosome size
  group_by(Chromosome) %>% 
  summarise(chr_len=max(Position)) %>% 
  
  # Calculate cumulative position of each chromosome
  mutate(tot=cumsum(as.numeric(chr_len))-chr_len) %>%
  dplyr::select(-chr_len) %>%
  
  # Add this info to the initial dataset
  left_join(plot, ., by=c("Chromosome"="Chromosome")) %>%
  
  # Add a cumulative position of each SNP
  arrange(Chromosome, Position) %>%
  mutate( BPcumulative=Position+tot)
rownumber<-length(row_number(don))
axisdf = don %>% group_by(Chromosome) %>% summarize(center=( max(BPcumulative) + min(BPcumulative) ) / 2 ) 
ggplot(don, aes(x=BPcumulative, y=-log10(PVALUE))) +
  
  # Show all points
  geom_point( aes(color=as.factor(Chromosome)), alpha=0.8, size=1.3)+geom_abline(intercept = -log10(.05/rownumber), slope = 0) +
  scale_color_manual(values = rep(c("grey", "dodgerblue4","orange2","chartreuse1","grey0","magenta2","gold"), 22 )) +
  
  # custom X axis:
  scale_x_continuous( label = axisdf$Chromosome, breaks= axisdf$center ) +
  scale_y_continuous(expand = c(0, 0) ) +     # remove space between plot area and x axis
  
  # Custom the theme:
  theme_bw() +
  theme( 
    legend.position="none",
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

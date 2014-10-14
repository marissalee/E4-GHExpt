#mytheme.R
#my theme for ggplots

mytheme <- theme_bw(base_size = 16, base_family = "Helvetica") +
  theme(panel.border = element_rect(colour = "black", size=1),      #put a black box around the plotting area
        axis.line = element_line(colour = "black"),                 #axis lines are in black
        panel.grid.major = element_blank(),                         #turn off the gridlines
        panel.grid.minor = element_blank()) +
  theme(strip.text.x = element_text(face='italic', hjust=0.05),         #turn off the x axis facet labels
        strip.text.y = element_text(face='italic', hjust=0.05)) +      #make y axis facet labels be italic and top justified
  theme(legend.key = element_blank(),                               #turn off box around legend
        plot.title=element_text(hjust=-0.1, vjust=2, face='bold')) +#style and position of the panel label
  theme(axis.title.x=element_text(vjust=-1.5),                        #make more room for the x and y axis labels
        axis.title.y=element_text(vjust=2),
        plot.margin=unit(c(1,1,1,1),"cm"))

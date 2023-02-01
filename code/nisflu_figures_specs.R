
min_theme <-  theme(#axis.text.x = element_blank(),
                    #axis.text.y = element_blank(),
                    axis.text.x = element_text(hjust=0, vjust=0, size = 5, color = "black", face = "plain", angle = 0),
                    axis.text.y = element_text(hjust=0, vjust=0, size = 5, color = "black", face = "plain"),
                    axis.ticks = element_blank(),
                    axis.title.x = element_blank(),
                    axis.title.y = element_text(hjust = 0, size = 6, face = "bold"),
                    panel.background =  element_blank(),
                    panel.border = element_rect(colour="black", fill=NA, size=0.01), # element_blank(),
                    strip.background =element_blank(),
                    strip.text.x = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.grid.major.y = element_blank(), # element_line(colour="gray90", size=0.1),
                    panel.grid.major.x = element_blank(), #  element_line(colour="gray90", size=0.1),
                    # squeezes panels closer together (margins may help even more)
                    panel.spacing.x = unit(0.02, "lines"),
                    panel.spacing.y = unit(0.02, "lines"),
                    plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
                    plot.title = element_text(size = 6, face = "bold", hjust=0),
                    text = element_text(family = "Arial"),
                    legend.position="none") 

min_theme_fig4 <-  theme(
                    axis.text.x = element_text(hjust=0, vjust=0, size = 10, color = "black", face = "plain", angle = 0),
                    axis.text.y = element_text(hjust=0, vjust=0, size = 10, color = "black", face = "plain"),
                    axis.ticks = element_blank(),
                    axis.title.x = element_text(hjust = 0.5, size = 10),
                    axis.title.y = element_text(hjust = 0.5, size = 10),
                    panel.background =  element_blank(),
                    panel.border = element_blank(), #element_rect(colour="black", fill=NA, size=0.01), # element_blank(),
                    strip.background =element_blank(),
                   # strip.text.x = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.grid.major.y = element_line(colour="black", size=0.05),
                    panel.grid.major.x = element_blank(), #  element_line(colour="gray90", size=0.1),
                    # squeezes panels closer together (margins may help even more)
                    panel.spacing.x = unit(0.02, "lines"),
                    panel.spacing.y = unit(0.02, "lines"),
                    plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
                    plot.title = element_text(size = 12,  hjust=0),
                    text = element_text(family = "Arial"),
                    legend.position="right") 

min_theme_legend <-  theme(#axis.text.x = element_blank(),
  #axis.text.y = element_blank(),
  axis.text.x = element_text(hjust=0, vjust=0, size = 5, color = "black", face = "plain", angle = 0),
  axis.text.y = element_text(hjust=0, vjust=0, size = 5, color = "black", face = "plain"),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_text(hjust = 0, size = 6, face = "bold"),
  panel.background =  element_blank(),
  panel.border = element_rect(colour="black", fill=NA, size=0.01), # element_blank(),
  strip.background =element_blank(),
  strip.text.x = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_blank(), # element_line(colour="gray90", size=0.1),
  panel.grid.major.x = element_blank(), #  element_line(colour="gray90", size=0.1),
  # squeezes panels closer together (margins may help even more)
  panel.spacing.x = unit(0.02, "lines"),
  panel.spacing.y = unit(0.02, "lines"),
  plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
  plot.title = element_text(size = 6, face = "bold", hjust=0),
  text = element_text(family = "Arial"),
  legend.position="bottom") 

mycols = c(brewer.pal(name="Set1", n = 8), brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 8))

mycols2 = c("slateblue1", "purple", "purple3","brown",
            "turquoise2", "steelblue", "blue2", "navyblue", "violet", "mediumpurple1", 
          "orange", "tomato", "coral2", "palevioletred", "violetred", "red2",
          "springgreen2", "yellowgreen", "palegreen4",
          "wheat2", "tan", "tan2", "tan3", "brown",  
          "grey70", "grey50", "grey30")

fct_names <- c("6 Months - 4 Years_2010.5", "6 Months - 4 Years_2011.5", "6 Months - 4 Years_2012.5", "6 Months - 4 Years_2013.5", "6 Months - 4 Years_2014.5", 
              "6 Months - 4 Years_2015.5", "6 Months - 4 Years_2016.5", "6 Months - 4 Years_2017.5", "6 Months - 4 Years_2018.5", "6 Months - 4 Years_2019.5", 
              "6 Months - 4 Years_2020.5", "6 Months - 4 Years_2021.5",
              "5-12 Years_2010.5", "5-12 Years_2011.5", "5-12 Years_2012.5", "5-12 Years_2013.5", "5-12 Years_2014.5", "5-12 Years_2015.5", "5-12 Years_2016.5", 
              "5-12 Years_2017.5" ,"5-12 Years_2018.5", "5-12 Years_2019.5", "5-12 Years_2020.5", "5-12 Years_2021.5", 
              "13-17 Years_2010.5", "13-17 Years_2011.5", "13-17 Years_2012.5", "13-17 Years_2013.5", "13-17 Years_2014.5",       
              "13-17 Years_2015.5", "13-17 Years_2016.5", "13-17 Years_2017.5", "13-17 Years_2018.5", "13-17 Years_2019.5", "13-17 Years_2020.5", "13-17 Years_2021.5", 
              "18-49 Years_2010.5", "18-49 Years_2011.5", "18-49 Years_2012.5", "18-49 Years_2013.5", "18-49 Years_2014.5", "18-49 Years_2015.5", 
              "18-49 Years_2016.5", "18-49 Years_2017.5", "18-49 Years_2018.5", "18-49 Years_2019.5", "18-49 Years_2020.5", "18-49 Years_2021.5",   
              "50-64 Years_2010.5", "50-64 Years_2011.5", "50-64 Years_2012.5", "50-64 Years_2013.5", "50-64 Years_2014.5", "50-64 Years_2015.5",
              "50-64 Years_2016.5", "50-64 Years_2017.5", "50-64 Years_2018.5", "50-64 Years_2019.5", "50-64 Years_2020.5", "50-64 Years_2021.5", 
              "65+ Years_2010.5", "65+ Years_2011.5", "65+ Years_2012.5", "65+ Years_2013.5", "65+ Years_2014.5",         
              "65+ Years_2015.5", "65+ Years_2016.5", "65+ Years_2017.5", "65+ Years_2018.5", "65+ Years_2019.5", "65+ Years_2020.5", "65+ Years_2021.5" )

age_ylab <- c("  65+ years         50-64 years       18-49 years      13-17 years      5-12 years         0.5-4 years  \n")
re_ylab <- c("       White NH              Other*                    Hispanic                   Black NH  \n")


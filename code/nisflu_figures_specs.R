
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
                    # panel title (state names)
                    # strip.text.x = element_text(hjust=0, vjust=-1, size = 5, color = "black", face = "plain"),
                    legend.position="none") 

mycols = c(brewer.pal(name="Set1", n = 8), brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 8))

mycols2 = c("slateblue1", "purple", "purple3","brown",
            "turquoise2", "steelblue", "blue2", "navyblue", "violet", "mediumpurple1", 
          "orange", "tomato", "coral2", "palevioletred", "violetred", "red2",
          "springgreen2", "yellowgreen", "palegreen4",
          "wheat2", "tan", "tan2", "tan3", "brown",  
          "grey70", "grey50", "grey30")

fct_names <- c("6 Months - 4 Years_2010.5", "6 Months - 4 Years_2011.5", "6 Months - 4 Years_2012.5", "6 Months - 4 Years_2013.5", "6 Months - 4 Years_2014.5", 
              "6 Months - 4 Years_2015.5", "6 Months - 4 Years_2016.5", "6 Months - 4 Years_2017.5", "6 Months - 4 Years_2018.5", "6 Months - 4 Years_2019.5", 
              "6 Months - 4 Years_2020.5",
              "5-12 Years_2010.5", "5-12 Years_2011.5", "5-12 Years_2012.5", "5-12 Years_2013.5", "5-12 Years_2014.5", "5-12 Years_2015.5", "5-12 Years_2016.5", 
              "5-12 Years_2017.5" ,"5-12 Years_2018.5", "5-12 Years_2019.5", "5-12 Years_2020.5", 
              "13-17 Years_2010.5", "13-17 Years_2011.5", "13-17 Years_2012.5", "13-17 Years_2013.5", "13-17 Years_2014.5",       
              "13-17 Years_2015.5", "13-17 Years_2016.5", "13-17 Years_2017.5", "13-17 Years_2018.5", "13-17 Years_2019.5", "13-17 Years_2020.5", 
              "18-49 Years_2010.5", "18-49 Years_2011.5", "18-49 Years_2012.5", "18-49 Years_2013.5", "18-49 Years_2014.5", "18-49 Years_2015.5", 
              "18-49 Years_2016.5", "18-49 Years_2017.5", "18-49 Years_2018.5", "18-49 Years_2019.5", "18-49 Years_2020.5",   
              "50-64 Years_2010.5", "50-64 Years_2011.5", "50-64 Years_2012.5", "50-64 Years_2013.5", "50-64 Years_2014.5", "50-64 Years_2015.5",
              "50-64 Years_2016.5", "50-64 Years_2017.5", "50-64 Years_2018.5", "50-64 Years_2019.5", "50-64 Years_2020.5", 
              "65+ Years_2010.5", "65+ Years_2011.5", "65+ Years_2012.5", "65+ Years_2013.5", "65+ Years_2014.5",         
              "65+ Years_2015.5", "65+ Years_2016.5", "65+ Years_2017.5", "65+ Years_2018.5", "65+ Years_2019.5", "65+ Years_2020.5" )

age_ylab <- c("  65+ years         50-64 years       18-49 years      13-17 years      5-12 years         0.5-4 years  \n")
re_ylab <- c("       White NH              Other*                    Hispanic                   Black NH  \n")

nis_figfun_preddiff <- function(preddat, var1, var2){
  
  preddat %>%
    mutate(pred_diff = est_data- pred.lm) %>%
    mutate(pred_diff_ll = est_ll - pred.lm) %>%
    mutate(pred_diff_ul = est_ul - pred.lm) %>%
    pivot_longer(cols = starts_with("pred_diff"), names_to = "difference", values_to = "estimate")  %>%
    filter(get(var1)!= "All") %>%
    ggplot()+
    geom_point(aes(x=estimate, y=reorder(get(var2), rank(biden_voteperc)), color = difference), size = 1.5) +
    geom_segment(aes(x=estimate, xend=0, y=reorder(get(var2), rank(biden_voteperc)), yend = reorder(get(var2), rank(biden_voteperc))), size = 0.5) +
    scale_colour_manual(values = c("black", "#7FC97F", "#7FC97F")) +
    geom_vline(xintercept=0) +
    xlab("Observed-predicted") +
    ylab("Descending order of vote share for Biden") +
    facet_wrap(~get(var1), ncol = 6) + 
    theme_minimal_vgrid() +
    theme(legend.position = "none")   -> p
  
  return(p)
}

nis_figfun_preddiff_v2 <- function(preddat, var1, var2){
  
  preddat %>%
    mutate(pred_diff = est_data- pred.lm) %>%
    mutate(pred_diff_ll = est_ll - pred.lm) %>%
    mutate(pred_diff_ul = est_ul - pred.lm) %>%
 #   pivot_longer(cols = starts_with("pred_diff"), names_to = "difference", values_to = "estimate")  %>%
    filter(get(var1)!= "All") %>%
    ggplot()+
    geom_pointrange(aes(x=pred_diff, xmin = pred_diff_ll, xmax = pred_diff_ul,
                          y=reorder(get(var2), rank(democrat_voteperc))), size = 0.5, fatten = 1, shape = 1, colour = "black") +
 #   geom_segment(aes(x=estimate, xend=0, y=reorder(get(var2), rank(biden_voteperc)), yend = reorder(get(var2), rank(biden_voteperc))), size = 0.5) +
    geom_point(aes(x=pred_diff_ll, y=reorder(get(var2), rank(democrat_voteperc)), color = difference), size = 1, colour = "#7FC97F") +
    geom_point(aes(x=pred_diff_ul, y=reorder(get(var2), rank(democrat_voteperc)), color = difference), size = 1, colour = "#7FC97F") +
    scale_colour_manual(values = c("black", "#7FC97F", "#7FC97F")) +
    geom_vline(xintercept=0) +
    xlab("Observed-predicted") +
    ylab("Ordered by vote share for Democrats") +
    facet_wrap(~get(var1),   ncol = 6) + #scales = "free_x",
    theme_minimal_vgrid() +
    theme(legend.position = "none")   -> p
  
  return(p)
}


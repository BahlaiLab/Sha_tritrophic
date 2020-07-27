#in this script we're going to actually analyse the data we cleaned in the other file, then deposited
#in the folder so we'd have data in an intermediate state


# first load bad breakup script
source_github <- function(u) {
  # load package
  require(RCurl)
  
  # read script lines from website
  script <- getURL(u, ssl.verifypeer = FALSE)
  
  # parase lines and evaluate in the global environment
  eval(parse(text = script))
}

source("https://raw.githubusercontent.com/BahlaiLab/bad_breakup_2/master/R_model/bad_breakup_script.R")

#now, we conduct the same analysis we did on the firefly case study in the algorithm paper

#######################################
#let's start with the data from Santa Barbara Costal, using the kelp_napl producer data
# this is where the work starts with this specific dataset- we'll want to do this with each 

kelp_napl<-read.csv(file="cleaned_data/SBC_producer_kelp_napl.csv", header=TRUE)


#let's make a time series plot

#get averages by treatment


#panel 1 with complete dataset
kelp_napl_time<-ggplot(kelp_napl, aes(YEAR, biomass))+
  geom_smooth(method="loess", colour="black", se=F)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  theme(legend.key=element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  xlab("\nYear")+
  ylab("biomass \n")
kelp_napl_time


#here goes nothing

output_frame<-multiple_breakups(kelp_napl)
output_frame
pyramid_plot(kelp_napl, rsq_points=TRUE)
stability_time(kelp_napl, min_percent = 95, error_multiplyer = 1)
relative_range(kelp_napl)
relative_range_after_stability(kelp_napl)
proportion_significant(kelp_napl)
proportion_wrong(kelp_napl)
proportion_wrong_before_stability(kelp_napl)

wrongness_plot(kelp_napl)

#number of these figures will depend on the total length of the dataset

broken_stick_plot(kelp_napl, window_length = 3)
broken_stick_plot(kelp_napl, window_length = 4)
broken_stick_plot(kelp_napl, window_length = 5)
broken_stick_plot(kelp_napl, window_length = 6)
broken_stick_plot(kelp_napl, window_length = 7)
broken_stick_plot(kelp_napl, window_length = 8)
broken_stick_plot(kelp_napl, window_length = 9)
broken_stick_plot(kelp_napl, window_length = 10)
broken_stick_plot(kelp_napl, window_length = 11)
broken_stick_plot(kelp_napl, window_length = 12)
broken_stick_plot(kelp_napl, window_length = 13)
broken_stick_plot(kelp_napl, window_length = 14)
broken_stick_plot(kelp_napl, window_length = 15)
broken_stick_plot(kelp_napl, window_length = 16)
broken_stick_plot(kelp_napl, window_length = 17)
broken_stick_plot(kelp_napl, window_length = 18)
broken_stick_plot(kelp_napl, window_length = 19)




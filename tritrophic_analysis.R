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
#let's start with the data from Santa Barbara Costal, using the algae_napl producer data
# this is where the work starts with this specific dataset- we'll want to do this with each 


#Kelp napl
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
kelp_napl_time+ggtitle("Kelp, NAPL site")


#here goes nothing

output_frame<-multiple_breakups(kelp_napl)
#output_frame
pyramid_plot(kelp_napl, rsq_points=TRUE)+ggtitle("Kelp, NAPL site")
stability_time(kelp_napl, min_percent = 95, error_multiplyer = 1)
relative_range(kelp_napl)
relative_range_after_stability(kelp_napl)
proportion_significant(kelp_napl)
proportion_wrong(kelp_napl)
proportion_wrong_before_stability(kelp_napl)

wrongness_plot(kelp_napl)+ggtitle("Kelp, NAPL site")

#number of these figures will depend on the total length of the dataset
# 
# broken_stick_plot(kelp_napl, window_length = 3)
# broken_stick_plot(kelp_napl, window_length = 4)
# broken_stick_plot(kelp_napl, window_length = 5)
# broken_stick_plot(kelp_napl, window_length = 6)
# broken_stick_plot(kelp_napl, window_length = 7)
# broken_stick_plot(kelp_napl, window_length = 8)
# broken_stick_plot(kelp_napl, window_length = 9)
# broken_stick_plot(kelp_napl, window_length = 10)
# broken_stick_plot(kelp_napl, window_length = 11)
# broken_stick_plot(kelp_napl, window_length = 12)
# broken_stick_plot(kelp_napl, window_length = 13)
# broken_stick_plot(kelp_napl, window_length = 14)
# broken_stick_plot(kelp_napl, window_length = 15)
# broken_stick_plot(kelp_napl, window_length = 16)
# broken_stick_plot(kelp_napl, window_length = 17)
# broken_stick_plot(kelp_napl, window_length = 18)
# broken_stick_plot(kelp_napl, window_length = 19)

#create new result frame so we can bring things together

#create the variables we need
trophic<-rep("producer", length(output_frame$start_year))
site<-rep("sbc", length(output_frame$start_year))
taxon<-rep("kelp", length(output_frame$start_year))
treat<-rep("napl", length(output_frame$start_year))

kelp_napl_output<-cbind(site, trophic, taxon, treat, output_frame)

#################################

#kelp carp is the next dataset
kelp_carp<-read.csv(file="cleaned_data/SBC_producer_kelp_carp.csv", header=TRUE)


#let's make a time series plot

#get averages by treatment


#panel 1 with complete dataset
kelp_carp_time<-ggplot(kelp_carp, aes(YEAR, biomass))+
  geom_smooth(method="loess", colour="black", se=F)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  theme(legend.key=element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  xlab("\nYear")+
  ylab("biomass \n")
kelp_carp_time+ggtitle("Kelp, CARP site")


#here goes nothing

output_frame<-multiple_breakups(kelp_carp)
#output_frame
pyramid_plot(kelp_carp, rsq_points=TRUE)+ggtitle("Kelp, CARP site")
stability_time(kelp_carp, min_percent = 95, error_multiplyer = 1)
relative_range(kelp_carp)
relative_range_after_stability(kelp_carp)
proportion_significant(kelp_carp)
proportion_wrong(kelp_carp)
proportion_wrong_before_stability(kelp_carp)

wrongness_plot(kelp_carp)+ggtitle("Kelp, CARP site")

#number of these figures will depend on the total length of the dataset
# 
# broken_stick_plot(kelp_carp, window_length = 3)
# broken_stick_plot(kelp_carp, window_length = 4)
# broken_stick_plot(kelp_carp, window_length = 5)
# broken_stick_plot(kelp_carp, window_length = 6)
# broken_stick_plot(kelp_carp, window_length = 7)
# broken_stick_plot(kelp_carp, window_length = 8)
# broken_stick_plot(kelp_carp, window_length = 9)
# broken_stick_plot(kelp_carp, window_length = 10)
# broken_stick_plot(kelp_carp, window_length = 11)
# broken_stick_plot(kelp_carp, window_length = 12)
# broken_stick_plot(kelp_carp, window_length = 13)
# broken_stick_plot(kelp_carp, window_length = 14)
# broken_stick_plot(kelp_carp, window_length = 15)
# broken_stick_plot(kelp_carp, window_length = 16)
# broken_stick_plot(kelp_carp, window_length = 17)
# broken_stick_plot(kelp_carp, window_length = 18)
# broken_stick_plot(kelp_carp, window_length = 19)

#create new result frame so we can bring things together

#create the variables we need
trophic<-rep("producer", length(output_frame$start_year))
site<-rep("sbc", length(output_frame$start_year))
taxon<-rep("kelp", length(output_frame$start_year))
treat<-rep("carp", length(output_frame$start_year))

kelp_carp_output<-cbind(site, trophic, taxon, treat, output_frame)


##############################
#now it's algae time!


#algae napl
algae_napl<-read.csv(file="cleaned_data/SBC_producer_algae_napl.csv", header=TRUE)


#let's make a time series plot

#get averages by treatment


#panel 1 with complete dataset
algae_napl_time<-ggplot(algae_napl, aes(YEAR, biomass))+
  geom_smooth(method="loess", colour="black", se=F)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  theme(legend.key=element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  xlab("\nYear")+
  ylab("biomass \n")
algae_napl_time+ggtitle("Algae, NAPL site")


#here goes nothing

output_frame<-multiple_breakups(algae_napl)
#output_frame
pyramid_plot(algae_napl, rsq_points=TRUE)+ggtitle("Algae, NAPL site")
stability_time(algae_napl, min_percent = 95, error_multiplyer = 1)
relative_range(algae_napl)
relative_range_after_stability(algae_napl)
proportion_significant(algae_napl)
proportion_wrong(algae_napl)
proportion_wrong_before_stability(algae_napl)

wrongness_plot(algae_napl)+ggtitle("Algae, NAPL site")

#number of these figures will depend on the total length of the dataset
# 
# broken_stick_plot(algae_napl, window_length = 3)
# broken_stick_plot(algae_napl, window_length = 4)
# broken_stick_plot(algae_napl, window_length = 5)
# broken_stick_plot(algae_napl, window_length = 6)
# broken_stick_plot(algae_napl, window_length = 7)
# broken_stick_plot(algae_napl, window_length = 8)
# broken_stick_plot(algae_napl, window_length = 9)
# broken_stick_plot(algae_napl, window_length = 10)
# broken_stick_plot(algae_napl, window_length = 11)
# broken_stick_plot(algae_napl, window_length = 12)
# broken_stick_plot(algae_napl, window_length = 13)
# broken_stick_plot(algae_napl, window_length = 14)
# broken_stick_plot(algae_napl, window_length = 15)
# broken_stick_plot(algae_napl, window_length = 16)
# broken_stick_plot(algae_napl, window_length = 17)
# broken_stick_plot(algae_napl, window_length = 18)
# broken_stick_plot(algae_napl, window_length = 19)

#create new result frame so we can bring things together

#create the variables we need
trophic<-rep("producer", length(output_frame$start_year))
site<-rep("sbc", length(output_frame$start_year))
taxon<-rep("algae", length(output_frame$start_year))
treat<-rep("napl", length(output_frame$start_year))

algae_napl_output<-cbind(site, trophic, taxon, treat, output_frame)

#################################

#algae carp is the next dataset
algae_carp<-read.csv(file="cleaned_data/SBC_producer_algae_carp.csv", header=TRUE)


#let's make a time series plot

#get averages by treatment


#panel 1 with complete dataset
algae_carp_time<-ggplot(algae_carp, aes(YEAR, biomass))+
  geom_smooth(method="loess", colour="black", se=F)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  theme(legend.key=element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  xlab("\nYear")+
  ylab("biomass \n")
algae_carp_time+ggtitle("Algae, CARP site")


#here goes nothing

output_frame<-multiple_breakups(algae_carp)
#output_frame
pyramid_plot(algae_carp, rsq_points=TRUE)+ggtitle("Algae, CARP site")
stability_time(algae_carp, min_percent = 95, error_multiplyer = 1)
relative_range(algae_carp)
relative_range_after_stability(algae_carp)
proportion_significant(algae_carp)
proportion_wrong(algae_carp)
proportion_wrong_before_stability(algae_carp)

wrongness_plot(algae_carp)+ggtitle("Algae, CARP site")

#number of these figures will depend on the total length of the dataset
# 
# broken_stick_plot(algae_carp, window_length = 3)
# broken_stick_plot(algae_carp, window_length = 4)
# broken_stick_plot(algae_carp, window_length = 5)
# broken_stick_plot(algae_carp, window_length = 6)
# broken_stick_plot(algae_carp, window_length = 7)
# broken_stick_plot(algae_carp, window_length = 8)
# broken_stick_plot(algae_carp, window_length = 9)
# broken_stick_plot(algae_carp, window_length = 10)
# broken_stick_plot(algae_carp, window_length = 11)
# broken_stick_plot(algae_carp, window_length = 12)
# broken_stick_plot(algae_carp, window_length = 13)
# broken_stick_plot(algae_carp, window_length = 14)
# broken_stick_plot(algae_carp, window_length = 15)
# broken_stick_plot(algae_carp, window_length = 16)
# broken_stick_plot(algae_carp, window_length = 17)
# broken_stick_plot(algae_carp, window_length = 18)
# broken_stick_plot(algae_carp, window_length = 19)

#create new result frame so we can bring things together

#create the variables we need
trophic<-rep("producer", length(output_frame$start_year))
site<-rep("sbc", length(output_frame$start_year))
taxon<-rep("algae", length(output_frame$start_year))
treat<-rep("carp", length(output_frame$start_year))

algae_carp_output<-cbind(site, trophic, taxon, treat, output_frame)

###################################
#moving on to inverts

#minvert napl
minvert_napl<-read.csv(file="cleaned_data/SBC_consumer_minvert_napl.csv", header=TRUE)


#let's make a time series plot

#get averages by treatment


#panel 1 with complete dataset
minvert_napl_time<-ggplot(minvert_napl, aes(YEAR, biomass))+
  geom_smooth(method="loess", colour="black", se=F)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  theme(legend.key=element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  xlab("\nYear")+
  ylab("biomass \n")
minvert_napl_time+ggtitle("Minvert, NAPL site")


#here goes nothing

output_frame<-multiple_breakups(minvert_napl)
#output_frame
pyramid_plot(minvert_napl, rsq_points=TRUE)+ggtitle("Minvert, NAPL site")
stability_time(minvert_napl, min_percent = 95, error_multiplyer = 1)
relative_range(minvert_napl)
relative_range_after_stability(minvert_napl)
proportion_significant(minvert_napl)
proportion_wrong(minvert_napl)
proportion_wrong_before_stability(minvert_napl)

wrongness_plot(minvert_napl)+ggtitle("Minvert, NAPL site")

#number of these figures will depend on the total length of the dataset
# 
# broken_stick_plot(minvert_napl, window_length = 3)
# broken_stick_plot(minvert_napl, window_length = 4)
# broken_stick_plot(minvert_napl, window_length = 5)
# broken_stick_plot(minvert_napl, window_length = 6)
# broken_stick_plot(minvert_napl, window_length = 7)
# broken_stick_plot(minvert_napl, window_length = 8)
# broken_stick_plot(minvert_napl, window_length = 9)
# broken_stick_plot(minvert_napl, window_length = 10)
# broken_stick_plot(minvert_napl, window_length = 11)
# broken_stick_plot(minvert_napl, window_length = 12)
# broken_stick_plot(minvert_napl, window_length = 13)
# broken_stick_plot(minvert_napl, window_length = 14)
# broken_stick_plot(minvert_napl, window_length = 15)
# broken_stick_plot(minvert_napl, window_length = 16)
# broken_stick_plot(minvert_napl, window_length = 17)
# broken_stick_plot(minvert_napl, window_length = 18)
# broken_stick_plot(minvert_napl, window_length = 19)

#create new result frame so we can bring things together

#create the variables we need
trophic<-rep("consumer", length(output_frame$start_year))
site<-rep("sbc", length(output_frame$start_year))
taxon<-rep("minvert", length(output_frame$start_year))
treat<-rep("napl", length(output_frame$start_year))

minvert_napl_output<-cbind(site, trophic, taxon, treat, output_frame)

#################################

#minvert carp is the next dataset
minvert_carp<-read.csv(file="cleaned_data/SBC_consumer_minvert_carp.csv", header=TRUE)


#let's make a time series plot

#get averages by treatment


#panel 1 with complete dataset
minvert_carp_time<-ggplot(minvert_carp, aes(YEAR, biomass))+
  geom_smooth(method="loess", colour="black", se=F)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  theme(legend.key=element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  xlab("\nYear")+
  ylab("biomass \n")
minvert_carp_time+ggtitle("Minvert, CARP site")


#here goes nothing

output_frame<-multiple_breakups(minvert_carp)
#output_frame
pyramid_plot(minvert_carp, rsq_points=TRUE)+ggtitle("Minvert, CARP site")
stability_time(minvert_carp, min_percent = 95, error_multiplyer = 1)
relative_range(minvert_carp)
relative_range_after_stability(minvert_carp)
proportion_significant(minvert_carp)
proportion_wrong(minvert_carp)
proportion_wrong_before_stability(minvert_carp)

wrongness_plot(minvert_carp)+ggtitle("Minvert, CARP site")

#number of these figures will depend on the total length of the dataset
# 
# broken_stick_plot(minvert_carp, window_length = 3)
# broken_stick_plot(minvert_carp, window_length = 4)
# broken_stick_plot(minvert_carp, window_length = 5)
# broken_stick_plot(minvert_carp, window_length = 6)
# broken_stick_plot(minvert_carp, window_length = 7)
# broken_stick_plot(minvert_carp, window_length = 8)
# broken_stick_plot(minvert_carp, window_length = 9)
# broken_stick_plot(minvert_carp, window_length = 10)
# broken_stick_plot(minvert_carp, window_length = 11)
# broken_stick_plot(minvert_carp, window_length = 12)
# broken_stick_plot(minvert_carp, window_length = 13)
# broken_stick_plot(minvert_carp, window_length = 14)
# broken_stick_plot(minvert_carp, window_length = 15)
# broken_stick_plot(minvert_carp, window_length = 16)
# broken_stick_plot(minvert_carp, window_length = 17)
# broken_stick_plot(minvert_carp, window_length = 18)
# broken_stick_plot(minvert_carp, window_length = 19)

#create new result frame so we can bring things together

#create the variables we need
trophic<-rep("consumer", length(output_frame$start_year))
site<-rep("sbc", length(output_frame$start_year))
taxon<-rep("minvert", length(output_frame$start_year))
treat<-rep("carp", length(output_frame$start_year))

minvert_carp_output<-cbind(site, trophic, taxon, treat, output_frame)




#sinvert napl
sinvert_napl<-read.csv(file="cleaned_data/SBC_consumer_sinvert_napl.csv", header=TRUE)


#let's make a time series plot

#get averages by treatment


#panel 1 with complete dataset
sinvert_napl_time<-ggplot(sinvert_napl, aes(YEAR, biomass))+
  geom_smooth(method="loess", colour="black", se=F)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  theme(legend.key=element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  xlab("\nYear")+
  ylab("biomass \n")
sinvert_napl_time+ggtitle("Sinvert, NAPL site")


#here goes nothing

output_frame<-multiple_breakups(sinvert_napl)
#output_frame
pyramid_plot(sinvert_napl, rsq_points=TRUE)+ggtitle("Sinvert, NAPL site")
stability_time(sinvert_napl, min_percent = 95, error_multiplyer = 1)
relative_range(sinvert_napl)
relative_range_after_stability(sinvert_napl)
proportion_significant(sinvert_napl)
proportion_wrong(sinvert_napl)
proportion_wrong_before_stability(sinvert_napl)

wrongness_plot(sinvert_napl)+ggtitle("Sinvert, NAPL site")

#number of these figures will depend on the total length of the dataset
# 
# broken_stick_plot(sinvert_napl, window_length = 3)
# broken_stick_plot(sinvert_napl, window_length = 4)
# broken_stick_plot(sinvert_napl, window_length = 5)
# broken_stick_plot(sinvert_napl, window_length = 6)
# broken_stick_plot(sinvert_napl, window_length = 7)
# broken_stick_plot(sinvert_napl, window_length = 8)
# broken_stick_plot(sinvert_napl, window_length = 9)
# broken_stick_plot(sinvert_napl, window_length = 10)
# broken_stick_plot(sinvert_napl, window_length = 11)
# broken_stick_plot(sinvert_napl, window_length = 12)
# broken_stick_plot(sinvert_napl, window_length = 13)
# broken_stick_plot(sinvert_napl, window_length = 14)
# broken_stick_plot(sinvert_napl, window_length = 15)
# broken_stick_plot(sinvert_napl, window_length = 16)
# broken_stick_plot(sinvert_napl, window_length = 17)
# broken_stick_plot(sinvert_napl, window_length = 18)
# broken_stick_plot(sinvert_napl, window_length = 19)

#create new result frame so we can bring things together

#create the variables we need
trophic<-rep("consumer", length(output_frame$start_year))
site<-rep("sbc", length(output_frame$start_year))
taxon<-rep("sinvert", length(output_frame$start_year))
treat<-rep("napl", length(output_frame$start_year))

sinvert_napl_output<-cbind(site, trophic, taxon, treat, output_frame)

#################################

#sinvert carp is the next dataset
sinvert_carp<-read.csv(file="cleaned_data/SBC_consumer_sinvert_carp.csv", header=TRUE)


#let's make a time series plot

#get averages by treatment


#panel 1 with complete dataset
sinvert_carp_time<-ggplot(sinvert_carp, aes(YEAR, biomass))+
  geom_smooth(method="loess", colour="black", se=F)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  theme(legend.key=element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  xlab("\nYear")+
  ylab("biomass \n")
sinvert_carp_time+ggtitle("Sinvert, CARP site")


#here goes nothing

output_frame<-multiple_breakups(sinvert_carp)
#output_frame
pyramid_plot(sinvert_carp, rsq_points=TRUE)+ggtitle("Sinvert, CARP site")
stability_time(sinvert_carp, min_percent = 95, error_multiplyer = 1)
relative_range(sinvert_carp)
relative_range_after_stability(sinvert_carp)
proportion_significant(sinvert_carp)
proportion_wrong(sinvert_carp)
proportion_wrong_before_stability(sinvert_carp)

wrongness_plot(sinvert_carp)+ggtitle("Sinvert, CARP site")

#number of these figures will depend on the total length of the dataset
# 
# broken_stick_plot(sinvert_carp, window_length = 3)
# broken_stick_plot(sinvert_carp, window_length = 4)
# broken_stick_plot(sinvert_carp, window_length = 5)
# broken_stick_plot(sinvert_carp, window_length = 6)
# broken_stick_plot(sinvert_carp, window_length = 7)
# broken_stick_plot(sinvert_carp, window_length = 8)
# broken_stick_plot(sinvert_carp, window_length = 9)
# broken_stick_plot(sinvert_carp, window_length = 10)
# broken_stick_plot(sinvert_carp, window_length = 11)
# broken_stick_plot(sinvert_carp, window_length = 12)
# broken_stick_plot(sinvert_carp, window_length = 13)
# broken_stick_plot(sinvert_carp, window_length = 14)
# broken_stick_plot(sinvert_carp, window_length = 15)
# broken_stick_plot(sinvert_carp, window_length = 16)
# broken_stick_plot(sinvert_carp, window_length = 17)
# broken_stick_plot(sinvert_carp, window_length = 18)
# broken_stick_plot(sinvert_carp, window_length = 19)

#create new result frame so we can bring things together

#create the variables we need
trophic<-rep("consumer", length(output_frame$start_year))
site<-rep("sbc", length(output_frame$start_year))
taxon<-rep("sinvert", length(output_frame$start_year))
treat<-rep("carp", length(output_frame$start_year))

sinvert_carp_output<-cbind(site, trophic, taxon, treat, output_frame)

#######################################

#predators

#fish napl
fish_napl<-read.csv(file="cleaned_data/SBC_predator_fish_napl.csv", header=TRUE)


#let's make a time series plot

#get averages by treatment


#panel 1 with complete dataset
fish_napl_time<-ggplot(fish_napl, aes(YEAR, biomass))+
  geom_smooth(method="loess", colour="black", se=F)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  theme(legend.key=element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  xlab("\nYear")+
  ylab("biomass \n")
fish_napl_time+ggtitle("Fish, NAPL site")


#here goes nothing

output_frame<-multiple_breakups(fish_napl)
#output_frame
pyramid_plot(fish_napl, rsq_points=TRUE)+ggtitle("Fish, NAPL site")
stability_time(fish_napl, min_percent = 95, error_multiplyer = 1)
relative_range(fish_napl)
relative_range_after_stability(fish_napl)
proportion_significant(fish_napl)
proportion_wrong(fish_napl)
proportion_wrong_before_stability(fish_napl)

wrongness_plot(fish_napl)+ggtitle("Fish, NAPL site")

#number of these figures will depend on the total length of the dataset
# 
# broken_stick_plot(fish_napl, window_length = 3)
# broken_stick_plot(fish_napl, window_length = 4)
# broken_stick_plot(fish_napl, window_length = 5)
# broken_stick_plot(fish_napl, window_length = 6)
# broken_stick_plot(fish_napl, window_length = 7)
# broken_stick_plot(fish_napl, window_length = 8)
# broken_stick_plot(fish_napl, window_length = 9)
# broken_stick_plot(fish_napl, window_length = 10)
# broken_stick_plot(fish_napl, window_length = 11)
# broken_stick_plot(fish_napl, window_length = 12)
# broken_stick_plot(fish_napl, window_length = 13)
# broken_stick_plot(fish_napl, window_length = 14)
# broken_stick_plot(fish_napl, window_length = 15)
# broken_stick_plot(fish_napl, window_length = 16)
# broken_stick_plot(fish_napl, window_length = 17)
# broken_stick_plot(fish_napl, window_length = 18)
# broken_stick_plot(fish_napl, window_length = 19)

#create new result frame so we can bring things together

#create the variables we need
trophic<-rep("predator", length(output_frame$start_year))
site<-rep("sbc", length(output_frame$start_year))
taxon<-rep("fish", length(output_frame$start_year))
treat<-rep("napl", length(output_frame$start_year))

fish_napl_output<-cbind(site, trophic, taxon, treat, output_frame)

#################################

#fish carp is the next dataset
fish_carp<-read.csv(file="cleaned_data/SBC_predator_fish_carp.csv", header=TRUE)


#let's make a time series plot

#get averages by treatment


#panel 1 with complete dataset
fish_carp_time<-ggplot(fish_carp, aes(YEAR, biomass))+
  geom_smooth(method="loess", colour="black", se=F)+
  geom_point(colour="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  theme(legend.key=element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  xlab("\nYear")+
  ylab("biomass \n")
fish_carp_time+ggtitle("Fish, CARP site")


#here goes nothing

output_frame<-multiple_breakups(fish_carp)
#output_frame
pyramid_plot(fish_carp, rsq_points=TRUE)+ggtitle("Fish, CARP site")
stability_time(fish_carp, min_percent = 95, error_multiplyer = 1)
relative_range(fish_carp)
relative_range_after_stability(fish_carp)
proportion_significant(fish_carp)
proportion_wrong(fish_carp)
proportion_wrong_before_stability(fish_carp)

wrongness_plot(fish_carp)+ggtitle("Fish, CARP site")

#number of these figures will depend on the total length of the dataset
# 
# broken_stick_plot(fish_carp, window_length = 3)
# broken_stick_plot(fish_carp, window_length = 4)
# broken_stick_plot(fish_carp, window_length = 5)
# broken_stick_plot(fish_carp, window_length = 6)
# broken_stick_plot(fish_carp, window_length = 7)
# broken_stick_plot(fish_carp, window_length = 8)
# broken_stick_plot(fish_carp, window_length = 9)
# broken_stick_plot(fish_carp, window_length = 10)
# broken_stick_plot(fish_carp, window_length = 11)
# broken_stick_plot(fish_carp, window_length = 12)
# broken_stick_plot(fish_carp, window_length = 13)
# broken_stick_plot(fish_carp, window_length = 14)
# broken_stick_plot(fish_carp, window_length = 15)
# broken_stick_plot(fish_carp, window_length = 16)
# broken_stick_plot(fish_carp, window_length = 17)
# broken_stick_plot(fish_carp, window_length = 18)
# broken_stick_plot(fish_carp, window_length = 19)

#create new result frame so we can bring things together

#create the variables we need
trophic<-rep("predator", length(output_frame$start_year))
site<-rep("sbc", length(output_frame$start_year))
taxon<-rep("fish", length(output_frame$start_year))
treat<-rep("carp", length(output_frame$start_year))

fish_carp_output<-cbind(site, trophic, taxon, treat, output_frame)




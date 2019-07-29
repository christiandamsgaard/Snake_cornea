# Load packages
require(MASS)
require(bayou)
require(ape)
require(readxl)
require(geiger)
require(phytools)
require(ggplot2)
require(RColorBrewer)
require(cowplot)
library(writexl)
library(nlme)
require(MCMCglmm)

# Custom function
keep.tip                 <- function(tree,tip) {
  drop.tip(tree,setdiff(tree$tip.label,tip))
}

# Import data
setwd("/Users/christiandamsgaard/Dropbox/Projects/Snake cornea evolution/") # Set working directory
cols<-brewer.pal(4,"Set1") # Set color scheme
cornea<-read_excel("Data.xlsx", sheet="Sheet1") # Load all data points
cornea<-as.data.frame(cornea) # Make it a data frame

# Load tree
tree<-read.tree(text="(Anilius_scytale:92.7,(((Cylindrophis_ruffus:44.4,Uropeltis_melanogaster:44.4):23.3,((Xenopeltis_unicolor:52.4,(Morelia_amethistina:18.2,Broghammerus_reticulatus:18.2):34.4):10.5,(Acrantophis_madagascarensis:44.1,(Eryx_jaculus:39.7,(Boa_constrictor:32.1,(Corallus_hortulanus:27.7,(Chilabothrus_angulifer:24,Eunectes_murinus:24):3.7):4.4):7.6):4.4):18.8):4.8):19.8,(Acrochordus_javanicus:80.6,(((Trimeresurus_albolabris:27.4,Agkistrodon_piscivorus:27.4):15.4,(Vipera_berus:36.3,(Atheris_ceratophora:31.6,Bitis_arietans:31.6):4.7):6.5):18.6,(Fordonia_leucobalia:54.8,(((Plagiopholis_nuchalis:47.1,(Thamnophis_sirtalis:37,(Natriciteres_olivacea:33.7,Rhabdophis_subminiatus:33.7):3.3):10.1):1.6,(Ahaetulla_nasuta:44.9,(Dispholidus_typus:36.5,((Boiga_irregularis:29.2,Crotaphopeltis_hotamboeia:29.2):4.1,((Pantherophis_guttata:14.7,Pituophis_melanoleucus:14.7):3.1,(Lampropeltis_getula:7.35,Lampropeltis_triangulum:7.35):10.45):15.5):3.2):8.4):3.8):4.3,(((Dendroaspis_angusticeps:31.6,Naja_naja:31.6):3.9,((Acanthophis_antarcticus:26.8,Aspidomorphus_muelleri:26.8):0.8,(Hydrophis_ornatus:6.3,Hydrophis_platurus:6.3):21.3):7.9):12.4,(Rhamphiophis_rostratus:46.8,(Gonionotophis_poensis:35.4,(Lycophidion_capense:32.6,((Lamprophis_fuliginosus:8.3,Boaedon_olivaceus:8.3):18.4,(Lamprophis_aurora:22.2,Lycodonomorphus_bicolor:22.2):4.5):5.9):2.8):11.4):1.1):5.1):1.8):6.6):19.2):6.9):5.2):10;")

## Compute mean values per individual

  # Make empty data vectors
cornea$ct<-rep(NA,dim(cornea)[1])
cornea$st<-rep(NA,dim(cornea)[1])
cornea$sd<-rep(NA,dim(cornea)[1])

  # Find corneal thickness, spectacle thickness and spectacle diameter for each individual. 
for (i in 1:dim(cornea)[1]){
  if(is.na(cornea$ctl[i])&is.na(cornea$ctr[i])){cornea$ct[i]<-NA} # if corneal thickness was not measure on either eye, return "NA"
  if(is.na(cornea$ctl[i])|is.na(cornea$ctr[i])){cornea$ct[i]<-mean(c(cornea$ctr[i],cornea$ctl[i]), na.rm=TRUE)} # if corneal thickness is only measured on one eye, return that value
  if(is.na(cornea$ctl[i])==F&is.na(cornea$ctr[i])==F){cornea$ct[i]<-mean(c(cornea$ctr[i],cornea$ctl[i]))} # If corneal thickness is measured on both eyes, return the mean
} 

for (i in 1:dim(cornea)[1]){
  if(is.na(cornea$stl[i])&is.na(cornea$str[i])){cornea$st[i]<-NA}
  if(is.na(cornea$stl[i])|is.na(cornea$str[i])){cornea$st[i]<-mean(c(cornea$str[i],cornea$stl[i]), na.rm=TRUE)}
  if(is.na(cornea$stl[i])==F&is.na(cornea$str[i])==F){cornea$st[i]<-mean(c(cornea$str[i],cornea$stl[i]))}
} 

for (i in 1:dim(cornea)[1]){
  if(is.na(cornea$sdl[i])&is.na(cornea$sdr[i])){cornea$sd[i]<-NA}
  if(is.na(cornea$sdl[i])|is.na(cornea$sdr[i])){cornea$sd[i]<-mean(c(cornea$sdr[i],cornea$sdl[i]), na.rm=TRUE)}
  if(is.na(cornea$sdl[i])==F&is.na(cornea$sdr[i])==F){cornea$sd[i]<-mean(c(cornea$sdr[i],cornea$sdl[i]))}
} 

  # Remove individuals with incomplete data
data<-cornea[c("Scientific name","svl","bm", "ct","sd","st")]
colnames(data)[1]<-"species"
data<-data[complete.cases(data),]

## Compute mean values per species

  # This sets up a data frame
data_sp <-data.frame(species = unique(data$species),
                     svl = rep(NA,length(unique(data$species))),
                     ct = rep(NA,length(unique(data$species))),
                     st = rep(NA,length(unique(data$species))),
                     sd = rep(NA,length(unique(data$species))),
                     svl_m = rep(NA,length(unique(data$species))),
                     svl_sd = rep(NA,length(unique(data$species))),
                     ct_m = rep(NA,length(unique(data$species))),
                     ct_sd = rep(NA,length(unique(data$species))),
                     st_m = rep(NA,length(unique(data$species))),
                     st_sd = rep(NA,length(unique(data$species))),
                     sd_m = rep(NA,length(unique(data$species))),
                     sd_sd = rep(NA,length(unique(data$species))))


for (i in 1:length(data_sp$species)){
  
  data_1<-subset(data,species == unique(species)[i]) # subset the dataset to a species "i"
  data_sp$svl[i]<-median(data_1$svl) # median svl of that species
  
  # if more than 1 individual for that species
  if (length(data_1$species)>1){
    # CT at median
    fit<-(summary(lm(log10(data_1$ct)~log10(data_1$svl))));fit    # fit log10(ct) vs. log10(svl)
    slo<-fit$coefficients[2,1];slo # extract slope
    int<-fit$coefficients[1,1];int # extract intercepts
    data_sp$ct[i]<-10^(log10(data_sp$svl[i])*slo+int) # interpolate ct to median svl
    
    # SD at median
    fit<-(summary(lm(log10(data_1$sd)~log10(data_1$svl))));fit
    slo<-fit$coefficients[2,1];slo
    int<-fit$coefficients[1,1];int
    data_sp$sd[i]<-10^(log10(data_sp$svl[i])*slo+int)
    
    # ST at median
    fit<-(summary(lm(log10(data_1$st)~log10(data_1$svl))));fit
    slo<-fit$coefficients[2,1];slo
    int<-fit$coefficients[1,1];int
    data_sp$st[i]<-10^(log10(data_sp$svl[i])*slo+int)
    
  }
  
  # if only 1 individual within species "i"
  if (length(data_1$species)==1){
    data_sp$ct[i]<-data_1$ct
    data_sp$st[i]<-data_1$st
    data_sp$sd[i]<-data_1$sd
  }
      # CT mean and standard deviation
    data_sp$ct_m[i]<-mean(data_1$ct)
    data_sp$ct_sd[i]<-sd(data_1$ct)
    data_sp$st_m[i]<-mean(data_1$st)
    data_sp$st_sd[i]<-sd(data_1$st)
    data_sp$sd_m[i]<-mean(data_1$sd)
    data_sp$sd_sd[i]<-sd(data_1$sd)
    data_sp$svl_m[i]<-mean(data_1$svl)
    data_sp$svl_sd[i]<-sd(data_1$svl)
}


## INCLUDE HABITAT AND DAP IN DATA SET
classification<-read_excel("Data.xlsx", sheet="Sheet2") # Load all data points
data_sp$Habitat<-classification$Habitat
data_sp$Period<-classification$Period


## PRUNE PHYLOGENY
data_sp$species<-sub("\\ ", "_", data_sp$species)                      #  separate genus and species by underscore
row.names(data_sp)<-data_sp$species
tree_pgls<-keep.tip(tree,row.names(data_sp)) 


##################################################
### BODY MASS CORRECTION AND PANELS FOR FIG. 2 ###
##################################################

## CT 
# pgls fits using bm and ou models
pgls_ct_bm               <- gls(model = ct~svl, 
                                correlation = corBrownian(phy = tree_pgls),
                                method = "ML",
                                data = data_sp);summary(pgls_ct_bm)

pgls_ct_ou               <- gls(model = ct~svl, 
                                correlation = corMartins(1,phy = tree_pgls, fixed = F),
                                method = "ML",
                                data = data_sp);summary(pgls_ct_ou)

aicw(c(AIC(pgls_ct_bm),AIC(pgls_ct_ou)))
# OU model is best

# Plot scatter plot
ggplot(data_sp, aes(x=svl,y=ct,col=Habitat,shape=Period))+
  geom_point(size = 3)+
  theme_classic()+
  theme(legend.position = "none",
        panel.grid.major.y   = element_line(colour = "grey90",size = 0.2),
        axis.text = element_text(size=6),
        axis.title = element_text(size=8))+
  scale_color_manual  (values=cols,
                       name="Habitat",
                       breaks=c("AQ","F","T","A"),
                       labels=c("Aquatic", "Fossorial", "Terrestrial", "Aboreal"))+
  geom_abline(intercept = unname(pgls_ct_ou$coefficients[1]),slope = unname(pgls_ct_ou$coefficients[2]))+
  labs(y = expression(bold("Corneal thickness (µm)")),
       x = expression(bold("Snout-vent length (mm)")))->p1;p1

pdf("/Users/christiandamsgaard/Dropbox/Projects/Snake cornea evolution/CT vs SVL.pdf",width = 3, height=3,useDingbats = F)
p1
dev.off()




## SD
# pgls fits using bm and ou models
pgls_sd_bm               <- gls(model = sd~svl, 
                                correlation = corBrownian(phy = tree_pgls),
                                method = "ML",
                                data = data_sp);summary(pgls_sd_bm)

pgls_sd_ou               <- gls(model = sd~svl, 
                                correlation = corMartins(1,phy = tree_pgls, fixed = F),
                                method = "ML",
                                data = data_sp);summary(pgls_sd_ou)


aicw(c(AIC(pgls_sd_bm),AIC(pgls_sd_ou)))
# BM model is best


# Plot scatter plot
ggplot(data_sp, aes(x=svl,y=sd,col=Habitat,shape=Period))+
  geom_point(size = 3)+
  theme_classic()+
  theme(legend.position = "none",
        panel.grid.major.y   = element_line(colour = "grey90",size = 0.2),
        axis.text = element_text(size=6),
        axis.title = element_text(size=8))+
  scale_color_manual  (values=cols,
                       name="Habitat",
                       breaks=c("AQ","F","T","A"),
                       labels=c("Aquatic", "Fossorial", "Terrestrial", "Aboreal"))+
  geom_abline(intercept = unname(pgls_sd_bm$coefficients[1]),slope = unname(pgls_sd_bm$coefficients[2]))+
  labs(y = expression(bold("Spectacle diameter (mm)")),
       x = expression(bold("Snout-vent length (mm)")))->p2

pdf("/Users/christiandamsgaard/Dropbox/Projects/Snake cornea evolution/SD vs SVL.pdf",width = 3, height=3,useDingbats = F)
p2
dev.off()



## ST
# pgls fits using bm and ou models

pgls_st_bm               <- gls(model = st~svl, 
                                correlation = corBrownian(phy = tree_pgls),
                                method = "ML",
                                data = data_sp);summary(pgls_st_bm)

pgls_st_ou               <- gls(model = st~svl, 
                                correlation = corMartins(1,phy = tree_pgls, fixed = F),
                                method = "ML",
                                data = data_sp);summary(pgls_st_ou)


aicw(c(AIC(pgls_st_bm),AIC(pgls_st_ou)))
# BM model is best



# Plot scatter plot
ggplot(data_sp, aes(x=svl,y=st,col=Habitat,shape=Period))+
  geom_point(size = 3)+
  theme_classic()+
  theme(legend.position = "none",
        panel.grid.major.y   = element_line(colour = "grey90",size = 0.2),
        axis.text = element_text(size=6),
        axis.title = element_text(size=8))+
  scale_color_manual  (values=cols,
                       name="Habitat",
                       breaks=c("AQ","F","T","A"),
                       labels=c("Aquatic", "Fossorial", "Terrestrial", "Aboreal"))+
  geom_abline(intercept = unname(pgls_st_bm$coefficients[1]),slope = unname(pgls_st_bm$coefficients[2]))+
  labs(y = expression(bold("Spectacle thickness (µm)")),
       x = expression(bold("Snout-vent length (mm)")))->p3;p3

pdf("/Users/christiandamsgaard/Dropbox/Projects/Snake cornea evolution/ST vs SVL.pdf",width = 3, height=3,useDingbats = F)
p3
dev.off()




##########################################################################
### CALCULATE RESIDUALS, TEST EFFECT OF HABITAT+DAP, PANELS FOR FIG. 3 ###
##########################################################################

# Calculate residuals
data_sp$ct_r<-residuals(pgls_ct_ou)
data_sp$st_r<-residuals(pgls_st_bm)
data_sp$sd_r<-residuals(pgls_sd_bm)

# Generate named vectors 
ct_r<-setNames(as.numeric(data_sp$ct_r),data_sp$species) # named vector for residual corneal thickness
st_r<-setNames(as.numeric(data_sp$st_r),data_sp$species) # named vector for residual spectacle thickness
sd_r<-setNames(as.numeric(data_sp$sd_r),data_sp$species) # named vector for residual spectacle diameter
hab<-setNames(data_sp$Habitat,data_sp$species)  # named vector for habitat
per<-setNames(data_sp$Period,data_sp$species)  # named vector for periode (i.e. diurnal activity pattern)
ct<-setNames(as.numeric(data_sp$ct),data_sp$species) # named vector for absolute corneal thickness

# Test for a phylogenetic signal in residuals
phylosig(tree,ct_r,method = "lambda",test = T) # No signal
phylosig(tree,st_r,method = "lambda",test = T) # Signal
phylosig(tree,sd_r,method = "lambda",test = T) # Signal


## CORNEAL THICKNESS ##

# Does diurnal activity pattern explain the variation in ABSOLUTE corneal thickness?
aggregate(ct~Period,data_sp,mean) # mean values
aggregate(ct~Period,data_sp,sd) # standard deviation
phylANOVA(tree,per,ct,nsim = 10000)
# Answer: No


# Does diurnal activity pattern explain the variation in RESIDUAL corneal thickness?
aggregate(ct_r~Period,data_sp,mean)
aggregate(ct_r~Period,data_sp,sd)
phylANOVA(tree,per,ct_r,nsim = 10000)
# No


# Does habitat explain the variation in residual corneal thickness?
phylANOVA(tree,hab,ct_r,nsim = 10000) # No


# Does diurnal activity pattern explain the variation in residual corneal thickness among species in same habitat?
within.phylANOVA<-function (Habitat) {
  data<-data_sp[which(data_sp$Habitat==Habitat),];data
  red.tree<-keep.tip(tree,rownames(data))
  print(phylANOVA(red.tree,data$Period,data$ct_r,nsim=10000))
}
within.phylANOVA("AQ") # Yes
within.phylANOVA("F") # No
within.phylANOVA("T") # No
within.phylANOVA("A") # No

# Jitter plot
p4<-ggplot(data_sp, aes(y=ct_r, x=Habitat, color=Habitat, shape = Period)) +
  geom_jitter(position=position_jitter(0.2))+
  theme_classic()+
  theme(legend.position = "none",
        panel.grid.major.y   = element_line(colour = "grey90",size = 0.2),
        axis.text = element_text(size=6),
        axis.title = element_text(size=8))+
  scale_x_discrete(limits=c("AQ","F","T","A"),
                   labels=c("Aquatic", "Fossorial", "Terrestrial", "Aboreal"))+
  scale_color_manual  (values=cols,
                       name="Habitat",
                       breaks=c("AQ","F","T","A"),
                       labels=c("Aquatic", "Fossorial", "Terrestrial", "Aboreal"))+
  labs(y = expression(bold("residual Corneal Thickness (µm)")),
       x = expression(bold("Habitat")));p4

pdf("/Users/christiandamsgaard/Dropbox/Projects/Snake cornea evolution/CT_r.pdf",width = 3, height=3,useDingbats = F)
p4
dev.off()



## SPECTACLE THICKNESS ##

# Does diurnal activity pattern explain the variation in RESIDUAL spectacle thickness?
phylANOVA(tree,per,st_r,nsim = 10000) # No
aggregate(st_r~Period,data_sp,mean)
aggregate(st_r~Period,data_sp,sd)
# No


# Does habitat explain the variation in residual spectacle thickness?
phylANOVA(tree,hab,st_r,nsim = 10000) # No

# Does diurnal activity pattern explain the variation in residual spectacle thickness among species in same habitat?
within.phylANOVA<-function (Habitat) {
  data<-data_sp[which(data_sp$Habitat==Habitat),];data
  red.tree<-keep.tip(tree,rownames(data))
  print(phylANOVA(red.tree,data$Period,data$st_r,nsim=10000))
}
within.phylANOVA("AQ") # No
within.phylANOVA("F") # No
within.phylANOVA("T") # Yes
within.phylANOVA("A") # No

# Jitter plot
p5<-ggplot(data_sp, aes(y=st_r, x=Habitat, color=Habitat, shape = Period)) +
  geom_jitter(position=position_jitter(0.2))+
  theme_classic()+
  theme(legend.position = "none",
        panel.grid.major.y   = element_line(colour = "grey90",size = 0.2),
        axis.text = element_text(size=6),
        axis.title = element_text(size=8))+
  scale_x_discrete(limits=c("AQ","F","T","A"),
                   labels=c("Aquatic", "Fossorial", "Terrestrial", "Aboreal"))+
  scale_color_manual  (values=cols,
                       name="Habitat",
                       breaks=c("AQ","F","T","A"),
                       labels=c("Aquatic", "Fossorial", "Terrestrial", "Aboreal"))+
  labs(y = expression(bold("residual Spectacle Thickness (µm)")),
       x = expression(bold("Habitat")));p5

pdf("/Users/christiandamsgaard/Dropbox/Projects/Snake cornea evolution/ST_r.pdf",width = 3, height=3,useDingbats = F)
p5
dev.off()







## SPECTACLE DIAMETER ##

# Does diurnal activity pattern explain the variation in RESIDUAL spectacle diameter?
phylANOVA(tree,per,sd_r,nsim = 10000) # No
aggregate(sd_r~Period,data_sp,mean)
aggregate(sd_r~Period,data_sp,sd)



# Does habitat explain the variation in residual spectacle diameter?
phylANOVA(tree,hab,sd_r,nsim = 10000) # Yes

# Does diurnal activity pattern explain the variation in residual spectacle diameter among species in same habitat?
within.phylANOVA<-function (Habitat) {
  data<-data_sp[which(data_sp$Habitat==Habitat),];data
  red.tree<-keep.tip(tree,rownames(data))
  print(phylANOVA(red.tree,data$Period,data$sd_r,nsim=10000))
}
within.phylANOVA("AQ") # No
within.phylANOVA("F") # No
within.phylANOVA("T") # No
within.phylANOVA("A") # No

# Jitter plot
p6<-ggplot(data_sp, aes(y=sd_r, x=Habitat, color=Habitat, shape = Period)) +
  geom_jitter(position=position_jitter(0.2))+
  theme_classic()+
  theme(legend.position = "none",
        panel.grid.major.y   = element_line(colour = "grey90",size = 0.2),
        axis.text = element_text(size=6),
        axis.title = element_text(size=8))+
  scale_x_discrete(limits=c("AQ","F","T","A"),
                   labels=c("Aquatic", "Fossorial", "Terrestrial", "Aboreal"))+
  scale_color_manual  (values=cols,
                       name="Habitat",
                       breaks=c("AQ","F","T","A"),
                       labels=c("Aquatic", "Fossorial", "Terrestrial", "Aboreal"))+
  labs(y = expression(bold("residual Spectacle diameter (µm)")),
       x = expression(bold("Habitat")));p6

pdf("/Users/christiandamsgaard/Dropbox/Projects/Snake cornea evolution/SD_r.pdf",width = 3, height=3,useDingbats = F)
p6
dev.off()






##################################################
### INTERRELATIONSHIPTS BETWEEEN CT, ST AND SD ###
###        TEST EFFECT OF HABITAT+DAP          ###
###            PANELS FOR FIG. 4               ###
##################################################




## CT VS SD ##
# pgls fits using bm and ou models
pgls_ct.sd_bm               <- gls(model = ct~sd, 
                                correlation = corBrownian(phy = tree_pgls),
                                method = "ML",
                                data = data_sp);summary(pgls_ct.sd_bm)

pgls_ct.sd_ou               <- gls(model = ct~sd, 
                                correlation = corMartins(1,phy = tree_pgls, fixed = F),
                                method = "ML",
                                data = data_sp);summary(pgls_ct.sd_ou)

aicw(c(AIC(pgls_ct.sd_bm),AIC(pgls_ct.sd_ou)))
# OU model is best

summary(pgls_ct.sd_ou)$tTable


# Plot scatter plot
ggplot(data_sp, aes(x=sd,y=ct,col=Habitat,shape=Period))+
  geom_point(size = 3)+
  theme_classic()+
  theme(legend.position = "none",
        panel.grid.major.y   = element_line(colour = "grey90",size = 0.2),
        axis.text = element_text(size=6),
        axis.title = element_text(size=8))+
  scale_color_manual  (values=cols,
                       name="Habitat",
                       breaks=c("AQ","F","T","A"),
                       labels=c("Aquatic", "Fossorial", "Terrestrial", "Aboreal"))+
  geom_abline(intercept = unname(pgls_ct.sd_ou$coefficients[1]),slope = unname(pgls_ct.sd_ou$coefficients[2]))+
  labs(y = expression(bold("Corneal thickness (µm)")),
       x = expression(bold("Spectacle diameter (mm)")))->p7;p7

pdf("/Users/christiandamsgaard/Dropbox/Projects/Snake cornea evolution/CT vs SD.pdf",width = 3, height=3,useDingbats = F)
p7
dev.off()


# Calculate ct.sd residuals
data_sp$ct.sd<-residuals(pgls_ct.sd_ou)

# Generate named vector
ct.sd<-setNames(as.numeric(data_sp$ct.sd),data_sp$species)


# Does diurnal activity pattern explain the variation ct.sd?
phylANOVA(tree,per,ct.sd,nsim = 10000) # No


# Does habitat explain the variation in ct.sd?
phylANOVA(tree,hab,ct.sd,nsim = 10000) # No

# Does diurnal activity pattern explain the variation in ct.sd among species in same habitat?
within.phylANOVA<-function (Habitat) {
  data<-data_sp[which(data_sp$Habitat==Habitat),];data
  red.tree<-keep.tip(tree,rownames(data))
  print(phylANOVA(red.tree,data$Period,data$ct.sd,nsim=10000))
}
within.phylANOVA("AQ") # No
within.phylANOVA("F") # No
within.phylANOVA("T") # No
within.phylANOVA("A") # No

# Jitter plot
p8<-ggplot(data_sp, aes(y=ct.sd, x=Habitat, color=Habitat, shape = Period)) +
  geom_jitter(position=position_jitter(0.2))+
  theme_classic()+
  theme(legend.position = "none",
        panel.grid.major.y   = element_line(colour = "grey90",size = 0.2),
        axis.text = element_text(size=6),
        axis.title = element_text(size=8))+
  scale_x_discrete(limits=c("AQ","F","T","A"),
                   labels=c("Aquatic", "Fossorial", "Terrestrial", "Aboreal"))+
  scale_color_manual  (values=cols,
                       name="Habitat",
                       breaks=c("AQ","F","T","A"),
                       labels=c("Aquatic", "Fossorial", "Terrestrial", "Aboreal"))+
  labs(y = expression(bold("residual corneal thickness (µm)")),
       x = expression(bold("Habitat")));p8

pdf("/Users/christiandamsgaard/Dropbox/Projects/Snake cornea evolution/CT.SD.pdf",width = 3, height=3,useDingbats = F)
p8
dev.off()







## ST VS SD ##
# pgls fits using bm and ou models
pgls_st.sd_bm               <- gls(model = st~sd, 
                                correlation = corBrownian(phy = tree_pgls),
                                method = "ML",
                                data = data_sp);summary(pgls_st.sd_bm)

pgls_st.sd_ou               <- gls(model = st~sd, 
                                correlation = corMartins(1,phy = tree_pgls, fixed = F),
                                method = "ML",
                                data = data_sp);summary(pgls_st.sd_ou)

aicw(c(AIC(pgls_st.sd_bm),AIC(pgls_st.sd_ou)))
# BM model is best

summary(pgls_st.sd_bm)$tTable


# Plot scatter plot
ggplot(data_sp, aes(x=sd,y=st,col=Habitat,shape=Period))+
  geom_point(size = 3)+
  theme_classic()+
  theme(legend.position = "none",
        panel.grid.major.y   = element_line(colour = "grey90",size = 0.2),
        axis.text = element_text(size=6),
        axis.title = element_text(size=8))+
  scale_color_manual  (values=cols,
                       name="Habitat",
                       breaks=c("AQ","F","T","A"),
                       labels=c("Aquatic", "Fossorial", "Terrestrial", "Aboreal"))+
  geom_abline(intercept = unname(pgls_st.sd_bm$coefficients[1]),slope = unname(pgls_st.sd_bm$coefficients[2]))+
  labs(y = expression(bold("Spectacle thickness (µm)")),
       x = expression(bold("Spectacle diameter (mm)")))->p9;p9

pdf("/Users/christiandamsgaard/Dropbox/Projects/Snake cornea evolution/ST vs SD.pdf",width = 3, height=3,useDingbats = F)
p9
dev.off()


# Calculate ct.sd residuals
data_sp$st.sd<-residuals(pgls_st.sd_bm)

# Generate named vector
st.sd<-setNames(as.numeric(data_sp$st.sd),data_sp$species)


# Does diurnal activity pattern explain the variation ct.sd?
phylANOVA(tree,per,st.sd,nsim = 10000) # No


# Does habitat explain the variation in ct.sd?
phylANOVA(tree,hab,st.sd,nsim = 10000) # No

# Does diurnal activity pattern explain the variation in ct.sd among species in same habitat?
within.phylANOVA<-function (Habitat) {
  data<-data_sp[which(data_sp$Habitat==Habitat),];data
  red.tree<-keep.tip(tree,rownames(data))
  print(phylANOVA(red.tree,data$Period,data$st.sd,nsim=10000))
}
within.phylANOVA("AQ") # No
within.phylANOVA("F") # No
within.phylANOVA("T") # No
within.phylANOVA("A") # No

# Jitter plot
p10<-ggplot(data_sp, aes(y=st.sd, x=Habitat, color=Habitat, shape = Period)) +
  geom_jitter(position=position_jitter(0.2))+
  theme_classic()+
  theme(legend.position = "none",
        panel.grid.major.y   = element_line(colour = "grey90",size = 0.2),
        axis.text = element_text(size=6),
        axis.title = element_text(size=8))+
  scale_x_discrete(limits=c("AQ","F","T","A"),
                   labels=c("Aquatic", "Fossorial", "Terrestrial", "Aboreal"))+
  scale_color_manual  (values=cols,
                       name="Habitat",
                       breaks=c("AQ","F","T","A"),
                       labels=c("Aquatic", "Fossorial", "Terrestrial", "Aboreal"))+
  labs(y = expression(bold("residual spectacle thickness (µm)")),
       x = expression(bold("Habitat")));p10

pdf("/Users/christiandamsgaard/Dropbox/Projects/Snake cornea evolution/ST.SD.pdf",width = 3, height=3,useDingbats = F)
p10
dev.off()







## CT VS ST ##
# pgls fits using bm and ou models
pgls_ct.st_bm               <- gls(model = ct~st, 
                                correlation = corBrownian(phy = tree_pgls),
                                method = "ML",
                                data = data_sp);summary(pgls_ct.st_bm)

pgls_ct.st_ou               <- gls(model = ct~st, 
                                correlation = corMartins(1,phy = tree_pgls, fixed = F),
                                method = "ML",
                                data = data_sp);summary(pgls_ct.st_ou)

aicw(c(AIC(pgls_ct.st_bm),AIC(pgls_ct.st_ou)))
# BM model is best

summary(pgls_ct.st_bm)$tTable


# Plot scatter plot
ggplot(data_sp, aes(x=st,y=ct,col=Habitat,shape=Period))+
  geom_point(size = 3)+
  theme_classic()+
  theme(legend.position = "none",
        panel.grid.major.y   = element_line(colour = "grey90",size = 0.2),
        axis.text = element_text(size=6),
        axis.title = element_text(size=8))+
  scale_color_manual  (values=cols,
                       name="Habitat",
                       breaks=c("AQ","F","T","A"),
                       labels=c("Aquatic", "Fossorial", "Terrestrial", "Aboreal"))+
  geom_abline(intercept = unname(pgls_ct.st_bm$coefficients[1]),slope = unname(pgls_ct.st_bm$coefficients[2]))+
  labs(y = expression(bold("Corneal thickness (µm)")),
       x = expression(bold("Spectacle thickness (µm)")))->p11;p11

pdf("/Users/christiandamsgaard/Dropbox/Projects/Snake cornea evolution/CT vs ST.pdf",width = 3, height=3,useDingbats = F)
p11
dev.off()


# Calculate ct.sd residuals
data_sp$ct.st<-residuals(pgls_ct.st_bm)

# Generate named vector
ct.st<-setNames(as.numeric(data_sp$ct.st),data_sp$species)


# Does diurnal activity pattern explain the variation ct.sd?
phylANOVA(tree,per,ct.st,nsim = 10000) # Yes


# Does habitat explain the variation in ct.sd?
phylANOVA(tree,hab,ct.st,nsim = 10000) # Yes

# Does diurnal activity pattern explain the variation in ct.sd among species in same habitat?
within.phylANOVA<-function (Habitat) {
  data<-data_sp[which(data_sp$Habitat==Habitat),];data
  red.tree<-keep.tip(tree,rownames(data))
  print(phylANOVA(red.tree,data$Period,data$ct.st,nsim=10000))
}
within.phylANOVA("AQ") # Yes
within.phylANOVA("F") # No
within.phylANOVA("T") # No
within.phylANOVA("A") # No

# Jitter plot
p12<-ggplot(data_sp, aes(y=ct.st, x=Habitat, color=Habitat, shape = Period)) +
  geom_jitter(position=position_jitter(0.2))+
  theme_classic()+
  theme(legend.position = "none",
        panel.grid.major.y   = element_line(colour = "grey90",size = 0.2),
        axis.text = element_text(size=6),
        axis.title = element_text(size=8))+
  scale_x_discrete(limits=c("AQ","F","T","A"),
                   labels=c("Aquatic", "Fossorial", "Terrestrial", "Aboreal"))+
  scale_color_manual  (values=cols,
                       name="Habitat",
                       breaks=c("AQ","F","T","A"),
                       labels=c("Aquatic", "Fossorial", "Terrestrial", "Aboreal"))+
  labs(y = expression(bold("residual corneal thickness (µm)")),
       x = expression(bold("Habitat")));p12

pdf("/Users/christiandamsgaard/Dropbox/Projects/Snake cornea evolution/CT.ST.pdf",width = 3, height=3,useDingbats = F)
p12
dev.off()







## EXPORT DATA
write.table(data_sp,"data_sp.txt")


###################
### OUwie model ###
###################

### The code below fits an OUwie model to identify selective regieme shift in residual corneal thickness, as was kindly proposed by a peer-reviewer.
### As the model did not detect regieme shift, this was not included in the manuscript, but the code is included here. 
### The code below was modified from Schmitz and Higham (2018) "Non-uniform evolutionary response of gecko eye size to changes in diel activity patterns"


res <- as.vector(pgls_ct_bm$residuals)
names(res) <- names(pgls_ct_bm$residuals)


prior <- make.prior(tree, dists=list(dalpha="dlnorm", 
                                          dsig2="dlnorm",
                                          dsb="dsb", 
                                          dk="cdpois", 
                                          dtheta="dnorm",
                                          dloc="dunif"),
                    param=list(dk=list(lambda=20, kmax=44),
                               dtheta=list(mean=mean(res), sd=5), 
                               dsig2=list(meanlog=2, sdlog=1),
                               dalpha=list(meanlog=-1, sdlog=1)
                    )
)

dev.off()
fit1 <- bayou.makeMCMC(tree = tree, 
                       dat = res, 
                       SE = 0.001, 
                       model = "OU", 
                       prior = prior, 
                       plot.freq=100000, 
                       ticker.freq=100000)
fit1$run(2000000)
chain <- fit1$load()
chain <- set.burnin(chain, 0.3)
out <- summary(chain);out


write.csv(out$statistics, "./bayou_results/bayOU_a_stats.csv")
write.csv(out$branch.posteriors, "./bayou_results/bayOU_a_posteriors.csv")

pdf("./bayou_results/bayOUpar_a.pdf")
par(mfrow=c(1,2))
truehist(chain$alpha, col="blue", xlab="alpha", ylab="density")
curve(dlnorm(x, -1, 1), col ="red", add=T)
truehist(chain$sig2, col="blue", xlab="sig2", ylab="density")
curve(dlnorm(x, 2, 1), col ="red", add=T)
dev.off()

pdf("./bayou_results/bayOUpar_a2.pdf")
par(mfrow=c(1,2))
truehist(chain$alpha, col="blue", xlab="alpha", ylab="density", xlim=c(0,10), ylim=c(0,1))
curve(dlnorm(x, -1, 1), col ="red", add=T)
truehist(chain$sig2, col="blue", xlab="sig2", ylab="density", xlim=c(0,6), ylim=c(0,1))
curve(dlnorm(x, 2, 1), col ="red", add=T)
dev.off()

pdf("./bayou_results/bayOU_a.pdf")
par(mfrow=c(1,1))
plotSimmap.mcmc(chain, burnin=0.3, lwd=2, edge.type="theta", pal=colorRampPalette(c("blue", "red")), show.tip.label=T, circle.col="black", cex=0.7)
dev.off()



fit2 <- bayou.makeMCMC(tree = tree, 
                               dat = res, 
                               SE = 0.001, 
                               model = "OU", 
                               prior = prior, 
                               plot.freq=100000, 
                               ticker.freq=100000)
fit2$run(2000000)
chain2 <- fit2$load()
chain2 <- set.burnin(chain2, 0.3)
out2 <- summary(chain2)

write.csv(out2$statistics, "./bayou_results/bayOU_b_stats.csv")
write.csv(out2$branch.posteriors, "./bayou_results/bayOU_b_posteriors.csv")

pdf("./bayou_results/bayOUpar_b.pdf")
par(mfrow=c(1,2))
truehist(chain2$alpha, col="blue", xlab="alpha", ylab="density")
curve(dlnorm(x, -1, 1), col ="red", add=T)
truehist(chain2$sig2, col="blue", xlab="sig2", ylab="density")
curve(dlnorm(x, 2, 1), col ="red", add=T)
dev.off()

pdf("./bayou_results/bayOUpar_b2.pdf")
par(mfrow=c(1,2))
truehist(chain2$alpha, col="blue", xlab="alpha", ylab="density", xlim=c(0,10), ylim=c(0,1))
curve(dlnorm(x, -1, 1), col ="red", add=T)
truehist(chain2$sig2, col="blue", xlab="sig2", ylab="density", xlim=c(0,10), ylim=c(0,1))
curve(dlnorm(x, 2, 1), col ="red", add=T)
dev.off()

pdf("./bayou_results/bayOU_b.pdf")
par(mfrow=c(1,1))
plotSimmap.mcmc(chain2, burnin=0.3, lwd=2, edge.type="theta", pal=colorRampPalette(c("blue", "red")), show.tip.label=T, circle.col="black", cex=0.7)
dev.off()

out2 <- summary(chain2)

write.csv(out2$statistics, "./bayou_results/bayOU_b_stats.csv")
write.csv(out2$branch.posteriors, "./bayou_results/bayOU_b_posteriors.csv")


plotSimmap.mcmc(chain2, burnin = 0.3, pp.cutoff = 0.3)



RlnL <- gelman.R("lnL", chain1=chain, chain2=chain2, plot=TRUE, type="n", ylim=c(0, 2))
abline(v=666666.67, lty=2, col="red")
Ralpha <- gelman.R("alpha", chain1=chain, chain2=chain2, plot=TRUE, type="n", ylim=c(0.9, 2))
abline(v=666666.67, lty=2, col="red")
Rsig2 <- gelman.R("sig2", chain1=chain, chain2=chain2, plot=TRUE, type="n", ylim=c(0.9, 2))
abline(v=666666.67, lty=2, col="red")
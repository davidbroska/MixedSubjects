library(data.table)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)

# default theme for ggplot2
theme_set(theme_bw())


# Custom functions
get_filepath = function(.fname){
  
  files = list.dirs() %>% 
    lapply(., function(l) list.files(l,full.names = T)) %>% 
    unlist() 
  
  idx = grep(paste0(.fname,"$"), files)[1]
  if(length(idx) > 1) warning("More than one file detected.")
  
  filepath = files[idx]
  return(filepath)
}


# Functions by Awad et al. (2018) -----------------------------------------------------------
PreprocessProfiles <- function(profiles){
  profiles[,Saved := as.numeric(Saved)]
  profiles[,ScenarioType := as.factor(ScenarioType)]
  profiles[,AttributeLevel := factor(AttributeLevel, 
                                     levels=c("Rand",
                                              "Male","Female",
                                              "Fat","Fit",
                                              "Low","High",
                                              "Old","Young",
                                              "Less","More",
                                              "Pets","Hoomans"))]
  profiles[,Barrier := factor(Barrier, levels=c(1,0))]
  profiles[,CrossingSignal := factor(CrossingSignal, levels=c(0,2,1))]
  profiles[,ScenarioType := as.factor(ScenarioType)]
  profiles[,ScenarioTypeStrict := as.factor(ScenarioTypeStrict)]
  return(profiles)
}



calcWeightsActual <- function(Tr, X){
  T10 <- ifelse(Tr==levels(factor(Tr))[2],1,0)
  d <- as.numeric(ave(X, X, T10, FUN = length))
  w <- max(d)/d
  return(w)
}

calcWeightsTheoretical <- function(profiles){
  p <- apply(profiles,1,CalcTheoreticalInt)
  return(1/p)
}

CalcTheoreticalInt <- function(X){
  if (X["Intervention"]==0){
    if (X["Barrier"]==0){
      if (X["PedPed"] == 1) p <- 0.48
      else p <- 0.32
      
      if (X["CrossingSignal"]==0) p <- p*0.48
      else if (X["CrossingSignal"]==1) p <- p*0.2
      else p <- p * 0.32
    }
    else p <- 0.2
  }
  else {
    if (X["Barrier"]==0){
      if (X["PedPed"] == 1) {
        p <- 0.48
        if (X["CrossingSignal"]==0) p <- p*0.48
        else if (X["CrossingSignal"]==1) p <- p*0.32
        else p <- p * 0.2
      }
      else {
        p <- 0.2
        if (X["CrossingSignal"]==0) p <- p*0.48
        else if (X["CrossingSignal"]==1) p <- p*0.2
        else p <- p * 0.32
      }
    }
    else p <- 0.32
  }
  return(p)
}


########################################
############ Main Effects ##############
########################################
# Main effect sizes
GetMainEffectSizes <- function(profiles,savedata,r){
  Coeffs <- matrix(NA,r,2)
  AttLevels <- levels(profiles$AttributeLevel)
  lev <- levels(profiles$ScenarioType)
  if (levels(profiles$ScenarioType)[1]=="") lev <- levels(profiles$ScenarioType)[2:8]
  lev <- lev[c(3,2,5,1,7,6)]
  
  # For intervention
  profiles$BC.weights <- calcWeightsTheoretical(profiles)
  lm.Int <- lm(Saved ~as.factor(Intervention), data=profiles, weights = BC.weights)
  lm.Int.se <- coeftest(lm.Int, cluster.vcov(lm.Int, cluster = profiles$UserID))[,2]
  Coeffs[1,1] <- lm.Int$coefficients[[2]]
  Coeffs[1,2] <- lm.Int.se[[2]]
  
  # For relationship to vehicle 
  ## Consider only 'no legality' (CrossingSignal==0) and 'passengers vs. pedestrians' (PedPed==0)
  profile.Relation <- profiles[which(profiles$CrossingSignal==0 & profiles$PedPed==0),]
  profile.Relation$BC.weights <- calcWeightsTheoretical(profile.Relation)
  lm.Rel <- lm(Saved ~as.factor(Barrier), data=profile.Relation, weights = BC.weights)
  lm.Rel.se <- coeftest(lm.Rel, cluster.vcov(lm.Rel, cluster = profile.Relation$UserID))[,2]
  Coeffs[2,1] <- lm.Rel$coefficients[[2]]
  Coeffs[2,2] <- lm.Rel.se[[2]]
  
  # Legality 
  ## Exclude 'no legality' (CrossingSignal!=0) and consider only 'pedestrians vs. pedestrians' (PedPed==1)
  profile.Legality <- profiles[which(profiles$CrossingSignal!=0 & profiles$PedPed==1),]
  profile.Legality$CrossingSignal <- factor(profile.Legality$CrossingSignal, levels=levels(profiles$CrossingSignal)[2:3])
  profile.Legality$BC.weights <- calcWeightsTheoretical(profile.Legality)
  lm.Leg <- lm(Saved ~as.factor(CrossingSignal), data=profile.Legality, weights = BC.weights)
  lm.Leg.se <- coeftest(lm.Leg, cluster.vcov(lm.Leg, cluster = profile.Legality$UserID))[,2]
  Coeffs[3,1] <- lm.Leg$coefficients[[2]]
  Coeffs[3,2] <- lm.Leg.se[[2]]
  
  # Six factors (gender, fitness, Social Status, age, utilitarianism, age, and species)
  ## Extract data subsets and run regression for each
  for(i in 1:6){
    Temp <- profiles[which(profiles$ScenarioType==lev[i] & profiles$ScenarioTypeStrict==lev[i]),]
    Temp$AttributeLevel <- factor(Temp$AttributeLevel, levels=AttLevels[(i*2):(i*2+1)])
    Temp$BC.weights <- calcWeightsTheoretical(Temp)    
    lm.Temp <- lm(Saved ~ as.factor(AttributeLevel), data=Temp, weights = BC.weights)
    lm.Temp.se <- coeftest(lm.Temp, cluster.vcov(lm.Temp, cluster = Temp$UserID))[,2]
    Coeffs[i+3,1] <- lm.Temp$coefficients[[2]]
    Coeffs[i+3,2] <- lm.Temp.se[[2]]
    
    # Save to a data frame
    if(savedata){
      var.name <- paste("profile",gsub(" ","",lev[i]),sep=".")
      assign(var.name,Temp)
    }
  }
  return(Coeffs)
}

# Prepare plotted dataset
GetPlotData <- function(Coeffs,isMainFig,r){
  # Convert to dataframe and add labels
  plotdata <- as.data.frame(Coeffs)
  colnames(plotdata)=c("Estimates","se")
  plotdata$Label <- c("Preference for action -> \n Preference for inaction",
                      "Sparing Passengers -> \n Sparing Pedestrians",
                      "Sparing the Unlawful -> \n Sparing the Lawful",
                      "Sparing Males -> \n Sparing Females",
                      "Sparing the Large -> \n Sparing the Fit",
                      "Sparing Lower Status -> \n Sparing Higher Status",
                      "Sparing the Elderly -> \n Sparing the Young",
                      "Sparing Fewer Characters -> \n Sparing More Characters",
                      "Sparing Pets -> \n Sparing Humans") 
  if(isMainFig)
    plotdata$Label <- c("Intervention",
                        "Relation to AV",
                        "Law",
                        "Gender",
                        "Fitness",
                        "Social Status",
                        "Age",
                        "No. Characters",
                        "Species") 
  
  
  plotdata$Label <- factor(plotdata$Label,as.ordered(plotdata$Label[match(sort(plotdata$Estimates[1:r]),plotdata$Estimates[1:r])]))
  plotdata$Label <- factor(plotdata$Label,levels = rev(levels(plotdata$Label)))
  
  plotdata$Estimates <- as.numeric(as.character(plotdata$Estimates))
  plotdata$se <- as.numeric(as.character(plotdata$se))
  
  return(plotdata)
}

# Effect of difference in number of characters within Utilitarian dimension
## Subclass by diff in number of characters
GetMainEffectSizes.Util <- function(profiles){
  Coeffs <- matrix(NA,4,2)
  AttLevels <- levels(profiles$AttributeLevel)
  for(i in 1:4){
    Temp <- profiles[which(profiles$ScenarioType== "Utilitarian" & profiles$ScenarioTypeStrict== "Utilitarian" & profiles$DiffNumberOFCharacters==i),]
    Temp$AttributeLevel <- factor(Temp$AttributeLevel, levels=AttLevels[10:11])
    Temp$BC.weights <- calcWeightsTheoretical(Temp)
    lm.Signed.NoC.Util <- lm(Saved ~as.factor(AttributeLevel), data=Temp, weights = BC.weights)
    lm.Signed.NoC.Util.se <- coeftest(lm.Signed.NoC.Util, cluster.vcov(lm.Signed.NoC.Util, cluster = Temp$UserID))[,2]
    Coeffs[i,1] <- lm.Signed.NoC.Util$coefficients[[2]]
    Coeffs[i,2] <- lm.Signed.NoC.Util.se[[2]]
  }
  return(Coeffs)
}

GetPlotData.Util <- function(Coeffs){
  plotdata <- as.data.frame(Coeffs)
  colnames(plotdata)=c("Estimates","se")
  plotdata$Variant <- c(1:4)
  plotdata$Variant <- factor(plotdata$Variant,levels=rev(plotdata$Variant))
  plotdata$Label <- as.factor(rep("No. Characters",4))
  return(plotdata)
}

PlotAndSave <- function(plotdata.main,isMainFig,filename,plotdata.Util){
  if(isMainFig){
    yticks <- seq(-0.2,0.8,0.2)
    util.x <- which(order(plotdata.main$Label)==8)+0.2
    alphas <- rep(.7,9)
    alphas[which(order(plotdata.main$Label)==8)] <- 0
    gg <- ggplot(plotdata.main,aes(x=Label, y=Estimates))+
      geom_hline(yintercept=0, color="black", size=0.7)+
      geom_col(width = .5, fill = "#0077ad", alpha = alphas)+
      geom_col(data = plotdata.util, width = .5, fill = "#0077ad", alpha = .7,aes(y=max(Estimates)))+
      geom_point(data = plotdata.util, color = "#49c6ff" , size = 5.5, shape = 21, fill = "white", stroke = 2)+  
      geom_point(size = 5.5, shape = 21, color = "#0077ad", fill = "white", stroke = 2)+
      geom_text(data=plotdata.util, aes(label=as.character(c(1:4)),y=Estimates),size=3,color="#37a2ef")+
      coord_flip()+
      labs(title="Preference in favor of the choice on the right side")+
      xlab("")+
      ylab(expression(paste("\n ",Delta," Pr")))+
      scale_y_continuous(limits = c(-0.2,0.9),breaks=yticks,labels=sapply(yticks,function(z) return(ifelse(z<0,"",ifelse(z>0,paste0("+",as.character(z)),"no change")))))+
      theme_bw()+
      theme_ipsum_rc(grid="Y",axis_title_just="m")+
      theme(text=element_text(size=20),
            legend.position="right",
            aspect.ratio=1/2, 
            axis.title.x = element_text(size=20),
            axis.text.y = element_text(hjust=0,color="black"),
            legend.text=element_text(size=8),
            panel.border = element_rect(colour = "black", fill=NA, size=1.5))
    
    ggsave(file=paste0(filename,".pdf"), width = 12, height = 6)
  }
  else{ 
    gg <- ggplot(plotdata.main,aes(Label, Estimates))+
      geom_hline(yintercept=0, color="black", size=0.7)+
      geom_crossbar(aes(ymin = Estimates - 1.96*se, ymax=Estimates + 1.96*se), 
                    position=position_dodge(.9), size=0.7, fill="blue",
                    fatten=1.5, width=.8)+
      coord_flip()+
      labs(title="Effect of attributes on decision for AV to spare")+ 
      xlab("")+ ylab(expression(paste("\n ",Delta," Pr")))+ ylim(0,0.6)+
      scale_color_brewer(palette="Set1")+
      guides(fill = guide_legend(reverse = TRUE))+
      theme_bw()+
      theme(text=element_text(size=15),legend.position="right",
            aspect.ratio=1/2, axis.text.y = element_text(hjust=0,color="black"),
            legend.text=element_text(size=8),
            panel.border = element_rect(colour = "black", fill=NA, size=1.5)) 
    ggsave(file=paste0(filename,"SI.pdf"), width = 12, height = 6)
  }
}



## Name characters in each dimension
characters.random <- c("Old Man", "Old Woman", "Boy", "Girl",
                       "Large Man", "Large Woman", "Male Athlete", "Female Athlete",
                       "Male Doctor","Female Doctor","Male Executive","Female Executive",
                       "Pregnant", "Stroller", "Homeless","Criminal","Dog", "Cat")
characters.age <- characters.random[1:4]
characters.fitness <- characters.random[5:8]
#characters.socialvalue <- characters.random[c(9:13,15:16)]
characters.socialstatus <- characters.random[c(11:12,15)]
characters.species <- characters.random[17:18]
characters.gender <- characters.random[1:12]
characters.all <- list(Age = characters.age,
                       Fitness = characters.fitness,
                       `Social Status` = characters.socialstatus,
                       Species = characters.species,
                       Gender = characters.gender,
                       Random = characters.random)

characterTypes <- c("Age","Fitness","Social Status","Species","Gender","Random")

# Effects of characters within each dimension
GetMainEffectSizes.Characters <- function(profiles,charType,k){
  c <- length(characters.all[charType][[1]])
  Coeffs <- matrix(NA,k*2,c)
  for(i in 1:k){
    Temp <- profiles[which(profiles$ScenarioType== charType & profiles$ScenarioTypeStrict== charType & profiles$NumberOfCharacters==i),]
    Temp[,(gsub(" ","",characters.all[charType][[1]])):=lapply(.SD,as.factor),.SDcols=gsub(" ","",characters.all[charType][[1]])]
    Temp$BC.weights <- calcWeightsTheoretical(Temp)    
    lm.Temp <- lm(as.formula(paste("Saved", paste(gsub(" ","",characters.all[charType][[1]]), collapse=" + "), sep=" ~ ")),
                  data=Temp, weights = BC.weights)
    lm.Temp.se <- coeftest(lm.Temp, cluster.vcov(lm.Temp, cluster = Temp$UserID))[,2]
    for(j in 1:c){
      Coeffs[i,j] <- lm.Temp$coefficients[[i*j+1]]
      Coeffs[i+k,j] <- lm.Temp.se[[i*j+1]]
    }
  }
  return(Coeffs)
}
#profiles[,(gsub(" ","",characters.all["Age"][[1]])):=lapply(.SD,as.factor),.SDcols=gsub(" ","",characters.all["Age"][[1]])]
#profiles[,lapply(sapply(gsub(" ","",characters.all["Age"][[1]]),function(x) eval(parse(text = x))),as.factor)]

GetPlotData.Characters <- function(Coeffs,charType,k){
  plotdata <- as.data.frame(Coeffs)
  colnames(plotdata)=characters.all[charType][[1]]
  plotdata$Measure <- rep(c("Estimates","se"),each=k)
  plotdata$`No. of Characters` <- rep(1:k,times=2)
  plotdata <- plotdata %>% 
    gather(CharacterType, Value, -Measure, -`No. of Characters`) %>%
    spread(Measure, Value)
  plotdata$`No. of Characters` <- as.factor(plotdata$`No. of Characters`)
  plotdata$CharacterType <- factor(plotdata$CharacterType, 
                                   levels=unique(plotdata$CharacterType[
                                     order(plotdata$Estimates[
                                       which(plotdata$`No. of Characters`==1)])]))
  return(plotdata)
}

PlotAndSave.Characters <- function(plotdata,charType,isMainFig,filename){
  if(isMainFig){
    yticks <- seq(-0.2,0.2,0.1)
    plotdata[nrow(plotdata) + c(1:unique(plotdata$`No. of Characters`)), 
             "No. of Characters"] <- unique(plotdata$`No. of Characters`)
    
    gg <- ggplot(plotdata,aes(CharacterType, Estimates,color=Estimates>0,fill=Estimates>0))+
      geom_hline(yintercept=0, color="black", size=0.7)+
      geom_col(width = .5, alpha = 0.7)+
      geom_point(size = 5, shape = 21, fill = "white", stroke = 2)+
      theme_bw()+ coord_flip()+
      labs(title="Preference in favor of sparing characters")+ xlab("")+
      ylab(expression(paste("\n ",Delta," Pr")))+ 
      scale_x_discrete(labels=c(levels(plotdata$CharacterType),""))+
      scale_y_continuous(limits = c(-0.2,0.2),breaks=yticks,labels=sapply(yticks,function(z) return(ifelse(z<0,paste0("-  ",as.character(-z)),ifelse(z>0,paste0("+",as.character(z)),"no change")))))+
      scale_color_manual(values=c("#984EA3","#0077ad"))+
      scale_fill_manual(values=c("#984EA3","#0077ad"))+
      theme_ipsum_rc(grid="XY",axis_title_just="m")+
      theme(text=element_text(size=16), axis.text=element_text(size=18),
            aspect.ratio=1.5/1, axis.text.y = element_text(hjust=0,color="black"),
            legend.position="none",axis.title.x = element_text(size=20),
            panel.border = element_rect(colour = "black", fill=NA, size=1.5))
    
    ggsave(file=paste0(filename,charType,".pdf"), width = 9, height = 9)
    
  } else{
    gg <- ggplot(plotdata,aes(CharacterType, Estimates,color=`No. of Characters`, fill=`No. of Characters`))+
      geom_hline(yintercept=0, color="black", size=0.7)+
      geom_crossbar(aes(ymin = Estimates - 1.96*se, ymax=Estimates + 1.96*se), 
                    position=position_dodge(.9), size=0.7, alpha=.4,
                    fatten=1.5, width=.8, fill="white",color="#0077cc")+
      theme_bw()+ coord_flip()+
      labs(title=paste0("Effect of characters within ",charType," dimension"))+ xlab("")+
      ylab(expression(paste("\n ",Delta," Pr")))+ ylim(-0.7,0.7)+
      scale_color_brewer(palette="Blues", direction=-1)+
      scale_fill_brewer(palette="Blues", direction=-1)+
      theme(text=element_text(size=16), axis.text=element_text(size=18), 
            aspect.ratio=1/1, axis.text.y = element_text(hjust=0,color="black"),
            legend.text=element_text(size=6), legend.position="right",
            panel.border = element_rect(colour = "black", fill=NA, size=1.5))
    ggsave(file=paste0(filename,charType,"SI.pdf"), width = 9, height = 9)
  }
  
}

########################################
####### Interaction Effects ############
########################################

# Split on attributes
GetEffectSizes.Inter.Att <- function(profiles,r,Attr){
  k <- 2
  if(Attr=="CrossingSignal") k=3
  Coeffs <- matrix(NA,r*k,2)
  AttLevels <- levels(profiles$AttributeLevel)
  lev <- levels(profiles$ScenarioType)
  if (levels(profiles$ScenarioType)[1]=="") lev <- levels(profiles$ScenarioType)[2:8]
  lev <- lev[c(3,2,5,1,7,6)]
  
  for(j in 0:(k-1)){
    if (Attr != "Intervention"){
      # For intervention
      if (Attr == "Barrier")    
        profile.Int <- profiles[which(profiles$Barrier==j & profiles$CrossingSignal==0 & profiles$PedPed==0),]
      if (Attr == "CrossingSignal")    
        profile.Int <- profiles[which(profiles$CrossingSignal==j & profiles$Barrier==0 & profiles$PedPed==0),]
      profile.Int$BC.weights <- calcWeightsTheoretical(profile.Int)
      lm.Int <- lm(Saved ~as.factor(Intervention), data=profile.Int, weights = BC.weights)
      lm.Int.se <- coeftest(lm.Int, cluster.vcov(lm.Int, cluster = profile.Int$UserID))[,2]
      Coeffs[r*j+1,1] <- lm.Int$coefficients[[2]]
      Coeffs[r*j+1,2] <- lm.Int.se[[2]]
      l=1
    } else{
      # For relationship to vehicle
      profile.Relation <- profiles[which(profiles$CrossingSignal==0  & profiles$PedPed==0 & profiles$Intervention==j),]
      profile.Relation$BC.weights <- calcWeightsTheoretical(profile.Relation)
      lm.Rel <- lm(Saved ~as.factor(Barrier), data=profile.Relation, weights = BC.weights)
      lm.Rel.se <- coeftest(lm.Rel, cluster.vcov(lm.Rel, cluster = profile.Relation$UserID))[,2]
      Coeffs[r*j+1,1] <- lm.Rel$coefficients[[2]]
      Coeffs[r*j+1,2] <- lm.Rel.se[[2]]
      
      # Legality
      profile.Legality <- profiles[which(profiles$CrossingSignal!=0  & profiles$PedPed==1 & profiles$Intervention==j),]
      profile.Legality$CrossingSignal <- factor(profile.Legality$CrossingSignal, levels=levels(profiles$CrossingSignal)[2:3])
      profile.Legality$BC.weights <- calcWeightsTheoretical(profile.Legality)
      lm.Leg <- lm(Saved ~as.factor(CrossingSignal), data=profile.Legality, weights = BC.weights)
      lm.Leg.se <- coeftest(lm.Leg, cluster.vcov(lm.Leg, cluster = profile.Legality$UserID))[,2]
      Coeffs[r*j+2,1] <- lm.Leg$coefficients[[2]]
      Coeffs[r*j+2,2] <- lm.Leg.se[[2]]
      l=2
    }
    
    for(i in 1:6){
      if(Attr == "Intervention")
        Temp <- profiles[which(profiles$ScenarioType==lev[i] & profiles$ScenarioTypeStrict==lev[i] & 
                                 profiles$Intervention==j),]
      if(Attr == "Barrier")
        Temp <- profiles[which(profiles$ScenarioType==lev[i] & profiles$ScenarioTypeStrict==lev[i] &
                                 profiles$Barrier==j & profiles$CrossingSignal==0 & profiles$PedPed==0),]
      if(Attr == "CrossingSignal")
        Temp <- profiles[which(profiles$ScenarioType==lev[i] & profiles$ScenarioTypeStrict==lev[i] & 
                                 profiles$CrossingSignal==j & profiles$Barrier==0  & profiles$PedPed==0),]
      
      Temp$AttributeLevel <- factor(Temp$AttributeLevel, levels=AttLevels[(i*2):(i*2+1)])
      Temp$BC.weights <- calcWeightsTheoretical(Temp)  
      lm.Temp <- lm(Saved ~as.factor(AttributeLevel),data=Temp, weights = BC.weights)
      lm.Temp.se <- coeftest(lm.Temp, cluster.vcov(lm.Temp, cluster = Temp$UserID))[,2]
      Coeffs[l+r*j+i,1] <- lm.Temp$coefficients[[2]]
      Coeffs[l+r*j+i,2] <- lm.Temp.se[[2]]
      
    }
  }
  return(Coeffs)
}



attrSI <- list(Intervention = c("Sparing Passengers -> \n Sparing Pedestrians",
                                "Sparing the Unlawful -> \n Sparing the Lawful"),
               Barrier = c("Preference for action -> \n Preference for inaction"),
               CrossingSignal = c("Preference for action -> \n Preference for inaction"))
attr <- list(Intervention = c("Relation to AV","Law"),
             Barrier = c("Intervention"),
             CrossingSignal = c("Intervention"))


# Prepare plotted dataset
GetPlotData.Inter.Attr <- function(Coeffs,isMainFig,r,Attr){
  k <- 2
  if(Attr=="CrossingSignal") k=3
  plotdata <- as.data.frame(Coeffs)
  colnames(plotdata)=rep(c("Estimates","se"))
  # plotdata$Label <- rep(c(attrSI[Attr][[1]],
  #                         "Gender [Male -> Female]",
  #                         "Fitness [Large -> Fit]",
  #                         "Social Status [Low -> High]",
  #                         "Age [Elderly -> Young]",
  #                         "No. Characters [Less -> More]",
  #                         "Species [Pets -> Humans]"),times=k)
  plotdata$Label <- rep(c(attrSI[Attr][[1]],
                          "Sparing Males -> \n Sparing Females",
                          "Sparing the Large -> \n Sparing the Fit",
                          "Sparing Lower Status -> \n Sparing Higher Status",
                          "Sparing the Elderly -> \n Sparing the Young",
                          "Sparing Fewer Characters -> \n Sparing More Characters",
                          "Sparing Pets -> \n Sparing Humans"),times=k) 
  if(isMainFig){
    plotdata$Label <- rep(c(attr[Attr][[1]],
                            "Gender",
                            "Fitness",
                            "Social Status",
                            "Age",
                            "No. Characters",
                            "Species"),times=k)
  }
  if(Attr=="Intervention")
    plotdata$Attribute <- rep(c("Omission","Commission"),each=r)
  if(Attr=="Barrier")
    plotdata$Attribute <- rep(c("Pedestrians","Passengers"),each=r)
  if(Attr=="CrossingSignal")
    plotdata$Attribute <- rep(c("No Legality", "Legal Crossing", "Ilegal Crossing"),each=r)
  
  plotdata$Label <- factor(plotdata$Label,as.ordered(plotdata$Label[match(sort(plotdata$Estimates[1:r]),plotdata$Estimates[1:r])]))
  plotdata$Label <- factor(plotdata$Label,levels = rev(levels(plotdata$Label)))
  
  plotdata$Estimates <- as.numeric(as.character(plotdata$Estimates))
  plotdata$se <- as.numeric(as.character(plotdata$se))
  
  return(plotdata)
}



PlotAndSave.Split <- function(plotdata,AttrLabel,isMainFig,filename){
  gg <- ggplot(plotdata,aes(Label, Estimates, color=Attribute))+
    geom_hline(yintercept=0, color="black", size=0.7)+
    geom_crossbar(aes(ymin = Estimates - 1.96*se, ymax=Estimates + 1.96*se), 
                  position=position_dodge(.9), size=0.7, 
                  fatten=1.5, width=.8)+
    coord_flip()+
    labs(title="Effect of attributes on decision for AV to spare",
         subtitle=paste0("Split on \'",AttrLabel,"\'"), color= AttrLabel)+ 
    xlab("")+ ylab(expression(paste("\n ",Delta," Pr")))+ ylim(0,0.8)+
    scale_color_brewer(palette="Set1")+
    guides(fill = guide_legend(reverse = TRUE))+
    theme_bw()+
    theme(text=element_text(size=15),legend.position="right",
          aspect.ratio=1/2, axis.text.y = element_text(hjust=0,color="black"),
          legend.text=element_text(size=8),
          panel.border = element_rect(colour = "black", fill=NA, size=1.5))
  
  if(isMainFig)
    ggsave(file=paste0(filename,gsub(" ","",AttrLabel),".pdf"), width = 12, height = 6)
  else
    ggsave(file=paste0(filename,gsub(" ","",AttrLabel),"SI.pdf"), width = 12, height = 6)
}

########################################
####### Additional Weighting ###########
########################################
GetAddWeightedMainEffectSizes <- function(profiles,weightCol,savedata,r){
  Coeffs <- matrix(NA,r,2)
  AttLevels <- levels(profiles$AttributeLevel)    
  lev <- levels(profiles$ScenarioType)
  if (levels(profiles$ScenarioType)[1]=="") lev <- levels(profiles$ScenarioType)[2:8]
  lev <- lev[c(3,2,5,1,7,6)]
  
  # For intervention
  profiles$BC.weights <- calcWeightsTheoretical(profiles) * calcWeightsActual(as.factor(profiles$Intervention), as.character(profiles[[weightCol]]))
  lm.Int <- lm(Saved ~as.factor(Intervention), data=profiles, weights = BC.weights)
  lm.Int.se <- coeftest(lm.Int, cluster.vcov(lm.Int, cluster = profiles$UserID))[,2]
  Coeffs[1,1] <- lm.Int$coefficients[[2]]
  Coeffs[1,2] <- lm.Int.se[[2]]
  
  # For relationship to vehicle
  ## Consider only 'no legality' (CrossingSignal==0) and 'passengers vs. pedestrians' (PedPed==0)
  profile.Relation <- profiles[which(profiles$CrossingSignal==0 & profiles$PedPed==0),]
  profile.Relation$BC.weights <- calcWeightsTheoretical(profile.Relation) * calcWeightsActual(as.factor(profile.Relation$Barrier),as.character(profile.Relation[[weightCol]]))
  lm.Rel <- lm(Saved ~as.factor(Barrier), data=profile.Relation, weights = BC.weights)
  lm.Rel.se <- coeftest(lm.Rel, cluster.vcov(lm.Rel, cluster = profile.Relation$UserID))[,2]
  Coeffs[2,1] <- lm.Rel$coefficients[[2]]
  Coeffs[2,2] <- lm.Rel.se[[2]]
  
  # Legality
  ## Exclude 'no legality' (CrossingSignal!=0) and consider only 'pedestrians vs. pedestrians' (PedPed==1)
  profile.Legality <- profiles[which(profiles$CrossingSignal!=0 & profiles$PedPed==1),]
  profile.Legality$CrossingSignal <- factor(profile.Legality$CrossingSignal, levels=levels(profiles$CrossingSignal)[2:3])
  profile.Legality$BC.weights <- calcWeightsTheoretical(profile.Legality) * calcWeightsActual(as.factor(profile.Legality$CrossingSignal),as.character(profile.Legality[[weightCol]]))
  lm.Leg <- lm(Saved ~as.factor(CrossingSignal), data=profile.Legality, weights = BC.weights)
  lm.Leg.se <- coeftest(lm.Leg, cluster.vcov(lm.Leg, cluster = profile.Legality$UserID))[,2]
  Coeffs[3,1] <- lm.Leg$coefficients[[2]]
  Coeffs[3,2] <- lm.Leg.se[[2]]
  
  # Six factors (gender, fitness, Social Status, age, utilitarianism, age, and species)
  ## Extract data subsets and run regression for each
  for(i in 1:6){
    Temp <- profiles[which(profiles$ScenarioType==lev[i] & profiles$ScenarioTypeStrict==lev[i]),]
    Temp$AttributeLevel <- factor(Temp$AttributeLevel, levels=AttLevels[(i*2):(i*2+1)])
    Temp$BC.weights <- calcWeightsTheoretical(Temp) * calcWeightsActual(as.factor(Temp$AttributeLevel), as.character(Temp[[weightCol]]))   
    lm.Temp <- lm(Saved ~ as.factor(AttributeLevel), data=Temp, weights = BC.weights)
    lm.Temp.se <- coeftest(lm.Temp, cluster.vcov(lm.Temp, cluster = Temp$UserID))[,2]
    Coeffs[i+3,1] <- lm.Temp$coefficients[[2]]
    Coeffs[i+3,2] <- lm.Temp.se[[2]]
    
    # Save to a data frame
    if(savedata){
      var.name <- paste("profile",gsub(" ","",lev[i]),sep=".")
      assign(var.name,Temp)
    }
  }
  return(Coeffs)
}

########################################
########## Stratification ##############
########################################

# Split on others
GetEffectSizes.Inter.Others <- function(profiles,r,filter,vals){
  k=length(vals)
  Coeffs <- matrix(NA,k,r*2)
  
  # Extract levels. 
  AttLevels <- levels(profiles$AttributeLevel)    
  lev <- levels(profiles$ScenarioType)
  if (levels(profiles$ScenarioType)[1]=="") lev <- levels(profiles$ScenarioType)[2:8]
  lev <- lev[c(3,2,5,1,7,6)]
  
  for(j in 0:(k-1)){
    # For intervention
    profile.Int <- profiles[which(filter==vals[j+1]),]
    profile.Int$BC.weights <- calcWeightsTheoretical(profile.Int)
    lm.Int <- lm(Saved ~as.factor(Intervention), data=profile.Int, weights = BC.weights)
    lm.Int.se <- coeftest(lm.Int, cluster.vcov(lm.Int, cluster = profile.Int$UserID))[,2]
    Coeffs[j+1,1] <- lm.Int$coefficients[[2]]
    Coeffs[j+1,r+1] <- lm.Int.se[[2]]  
    
    # For relationship to vehicle
    profile.Relation <- profiles[which(profiles$CrossingSignal==0 & profiles$PedPed==0 & filter==vals[j+1]),]
    profile.Relation$BC.weights <- calcWeightsTheoretical(profile.Relation)
    lm.Rel <- lm(Saved ~as.factor(Barrier), data=profile.Relation, weights = BC.weights)
    lm.Rel.se <- coeftest(lm.Rel, cluster.vcov(lm.Rel, cluster = profile.Relation$UserID))[,2]
    Coeffs[j+1,2] <- lm.Rel$coefficients[[2]]
    Coeffs[j+1,r+2] <- lm.Rel.se[[2]]
    
    # Legality
    profile.Legality <- profiles[which(profiles$CrossingSignal!=0 & profiles$PedPed==1 & filter==vals[j+1]),]
    profile.Legality$CrossingSignal <- factor(profile.Legality$CrossingSignal, levels=levels(profiles$CrossingSignal)[2:3])
    profile.Legality$BC.weights <- calcWeightsTheoretical(profile.Legality)
    lm.Leg <- lm(Saved ~as.factor(CrossingSignal), data=profile.Legality, weights = BC.weights)
    lm.Leg.se <- coeftest(lm.Leg, cluster.vcov(lm.Leg, cluster = profile.Legality$UserID))[,2]
    Coeffs[j+1,3] <- lm.Leg$coefficients[[2]]
    Coeffs[j+1,r+3] <- lm.Leg.se[[2]]
    
    
    for(i in 1:6){
      Temp <- profiles[which(profiles$ScenarioType==lev[i] & profiles$ScenarioTypeStrict==lev[i] & filter==vals[j+1]),]
      Temp$AttributeLevel <- factor(Temp$AttributeLevel, levels=AttLevels[(i*2):(i*2+1)])
      Temp$BC.weights <- calcWeightsTheoretical(Temp)
      lm.Temp <- lm(Saved ~as.factor(AttributeLevel),data=Temp,weights = BC.weights)
      lm.Temp.se <- coeftest(lm.Temp, cluster.vcov(lm.Temp, cluster = Temp$UserID))[,2]
      Coeffs[j+1,3+i] <- lm.Temp$coefficients[[2]]
      Coeffs[j+1,r+3+i] <- lm.Temp.se[[2]]
      
    }
  }
  return(Coeffs)
}

GetFilteredList <- function(profiles,filter,N){
  a <- profiles %>% group_by_(filter) %>% summarise(count = length(unique(UserID)))
  vals <- a[which(a$count>=N),filter][[1]]
  if ("" %in% vals) vals <- vals[-which(vals=="")]
  if (NA %in% vals) vals <- vals[-which(is.na(vals))]
  if (", " %in% vals) vals <- vals[-which(vals==", ")]
  return(vals)
}

# Convert to dataframe and add labels
GetFinalDF <- function(Coeffs,vals){
  plotdata <- as.data.frame(Coeffs,row.names = as.character(vals))
  facs <- c("[Omission -> Commission]",
            "[Passengers -> Pedestrians]",
            "Law [Illegal -> Legal]",
            "Gender [Male -> Female]",
            "Fitness [Large -> Fit]",
            "Social Status [Low -> High]",
            "Age [Elderly -> Young]",
            "No. Characters [Less -> More]",
            "Species [Pets -> Humans]")
  
  colnames(plotdata)=paste0("", levels(interaction(rep(factor(facs,levels = ordered(facs)),2),rep(c("Estimates","se"),each=9), sep = ": ")))
  return(plotdata)
  
}

GetPlotData.Inter.Others <- function(Coeffs,vals,isMainFig,fixedOrder){
  plotdata <- as.data.frame(Coeffs)
  facs <- c("Preference for inaction\n [Interventionism]",
            "Sparing pedestrians\n [Relation to AV]",
            "Sparing the lawful\n[Law]",
            "Sparing females\n [Gender]",
            "Sparing the fit\n [Fitness]",
            "Sparing higher status\n [Social Status]",
            "Sparing the younger\n [Age]",
            "Sparing more characters\n [No. Characters]",
            "Sparing humans\n [Species]") 
  colnames(plotdata)=paste0("", levels(interaction(rep(factor(facs,levels = ordered(facs)),2),rep(c("Estimates","se"),each=9), sep = ":")))
  plotdata$Attribute <- as.character(vals)
  plotdata <- plotdata %>% gather(key, value, -grep(':',names(plotdata),invert = T))
  plotdata <- plotdata %>%  extract(key, c("Label", "Measure"), "([a-zA-Z\\W]+):([a-zA-Z.]+)")
  plotdata <- plotdata %>% spread(Measure, value)
  
  if(fixedOrder) plotdata$Label <- factor(plotdata$Label,as.ordered(facs))
  else plotdata$Label <- factor(plotdata$Label,as.ordered(plotdata$Label[match(sort(plotdata$Estimates[1:r]),plotdata$Estimates[1:r])]))
  
  plotdata$Label <- factor(plotdata$Label,levels = rev(levels(plotdata$Label)))
  
  plotdata$Estimates <- as.numeric(as.character(plotdata$Estimates))
  plotdata$se <- as.numeric(as.character(plotdata$se))
  
  return(plotdata)
  
}


PlotAndSave.Split.Order <- function(plotdata,AttrLabel,isMainFig,filename){
  gg <- ggplot(plotdata,aes(Attribute, Estimates))+
    geom_hline(yintercept=0, color="black", size=0.7)+
    geom_crossbar(aes(ymin = Estimates - 1.96*se, ymax=Estimates + 1.96*se), 
                  position=position_dodge(.9), size=0.7, 
                  fatten=1.5, width=.8,color="black")+
    coord_flip()+
    facet_wrap(~Label)+
    labs(title="Effect of attributes on decision for AV to spare",subtitle=paste0("Split on \'",AttrLabel,"\'"))+ 
    xlab("Scenario Order")+ ylab(expression(paste("\n ",Delta," Pr")))+ ylim(0,0.95)+
    scale_fill_brewer(palette="Set1")+
    guides(fill = guide_legend(reverse = TRUE))+
    theme_bw()+
    theme(text=element_text(size=13),legend.position="right",
          aspect.ratio=0.85/1, axis.text.y = element_text(color="black"),
          legend.text=element_text(size=7),
          panel.border = element_rect(colour = "black", fill=NA, size=1.5))
  if(isMainFig)
    ggsave(file=paste0(filename,gsub(" ","",AttrLabel),".pdf"), width = 12, height = 9)
  else
    ggsave(file=paste0(filename,gsub(" ","",AttrLabel),"SI.pdf"), width = 12, height = 9)
  
}



# Setup for survey data
AddUserColumns <- function(profiles.S){
  profiles.S[,UserGender := as.factor(Review_gender)]
  
  Q <- quantile(profiles.S$Review_political,c(0.25,0.75))
  profiles.S[,UserPolitical := as.factor(ifelse(Review_political<=Q[[1]],"Conservative",ifelse(Review_political>=Q[[2]],"Progressive","Neutral")))]
  
  Q <- quantile(profiles.S$Review_religious,c(0.25,0.75))
  profiles.S[,UserReligious := as.factor(ifelse(Review_religious<=Q[[1]],"Not Religious",ifelse(Review_religious>=Q[[2]],"Very Religious","Neutral")))]
  
  profiles.S[,Review_age.nontroll := ifelse(Review_age>15 & Review_age<75, Review_age, NA)]
  Q <- quantile(profiles.S$Review_age.nontroll,c(0.25,0.75),na.rm = T)
  profiles.S[,UserAge := as.factor(ifelse(Review_age.nontroll<=Q[[1]],"Younger",ifelse(Review_age.nontroll>=Q[[2]],"Older","Neutral")))]
  
  
  profiles.S[,UserIncome := as.factor(ifelse(Review_income %in% c("under5000","5000"),"Lower Income",
                                             ifelse(Review_income %in% c("10000","15000","25000","35000","50000"),"Middle Income",
                                                    "Higher Income")))]
  
  profiles.S[,UserEducation := as.factor(ifelse(Review_education %in% c("underHigh","high","vocational"),"Less Educated",
                                                ifelse(Review_education %in% c("college","bachelor"),"Medium Educated",
                                                       "More Educated")))]
  return(profiles.S)
}

gender.val <- c("female","male")
political.val <- c("Conservative","Progressive")
religious.val <- c("Not Religious","Very Religious")
age.val <- c("Younger","Older")
income.val <- c("Lower Income","Higher Income")
education.val <- c("Less Educated","More Educated")

User.all <- list(UserGender = gender.val,
                 UserPolitical = political.val,
                 UserReligious = religious.val,
                 UserAge = age.val,
                 UserIncome = income.val,
                 UserEducation = education.val)




PlotAndSave.Survey <- function(plotdata.2.c,AttrLabel,isMainFig,filename){
  plotdata.2.c$Label <- factor(plotdata.2.c$Label,levels = rev(levels(plotdata.2.c$Label)))
  d = plotdata.2.c %>% 
    mutate(Trait = case_when(.$Group2 == "Higher\n Income" ~ "Higher earners",
                             .$Group2 == "Male" ~ "Men",
                             .$Group2 == "More\n Educated" ~ "More educated",
                             .$Group2 == "Older" ~ "Older",
                             .$Group2 == "Progres." ~ "Progressive",
                             .$Group2 == "Very\n Religious" ~ "More religious"),
           Start = 0,
           Stop = Difference) %>% 
    gather(Change, Score, Start:Stop)
  gg <- ggplot(d, aes(Change, Score, group = 1, color = Difference))+
    geom_segment(y = 0, yend = 0, x = 1, xend = 2, linetype = "dotted", color = "grey65")+
    scale_x_discrete(name = "", labels = element_blank())+
    annotate(geom = "text", x = 2.1, y = 0, label = "0", size = 2, hjust = 0, color = "grey30")+
    annotate(geom = "text", x = 2.1, y = .15, label = "+0.15", size = 2, hjust = 0, color = "grey30")+
    annotate(geom = "text", x = 2.1, y = -.15, label = "-0.15", size = 2, hjust = 0, color = "grey30")+
    geom_path(arrow = arrow(type = "closed", angle = 25, length = unit(0.18, "cm")))+
    scale_y_continuous(position = "right", labels = NULL, name=expression(paste("\n ",Delta, "(",Delta," Pr)")))+
    scale_color_gradient2(low="#AE1C3E", mid="grey70", high="#3D52A1")+
    facet_grid(Label ~ Trait, switch = "y") +
    theme_minimal(base_family="Roboto Condensed", base_size = 10) +
    theme(strip.text.y = element_blank(),
          strip.text.x = element_text(hjust = 0.5, size = 9),
          legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          strip.background = element_rect(fill = "grey95", color = "transparent"),
          panel.grid.major.x = element_line(color = "#3d3d3d"))
  
  
  if(isMainFig)
    ggsave(gg, file = paste0(filename,gsub(" ","",AttrLabel),".pdf"), height = 5, width = 8)
  else
    ggsave(gg, file = paste0(filename,gsub(" ","",AttrLabel),"SI.pdf"), height = 5, width = 8)
}


PlotAndSave.Survey.old <- function(plotdata.2.c,AttrLabel,isMainFig,filename){
  d = plotdata.2.c %>% mutate(Trait = gsub("User's","",UserAttribute))
  d[nrow(d) + c(1:6), "Trait"] <- unique(d$Trait)
  yticks <- c(-0.2, -0.1, 0, 0.1, 0.2)
  gg <- ggplot(d,aes(y =Difference,x=Label,color=Difference>0,fill=Difference>0))+
    geom_hline(yintercept=0, color="black", size=0.7)+
    geom_col(width = .47, alpha = 0.7)+
    coord_flip()+
    facet_grid(.~Trait)+
    labs(title="Effect of attributes on decision for AV to spare",subtitle=paste0("Split on respondent's attributes"))+
    xlab("")+ ylab(expression(paste("\n ",Delta, "(",Delta," Pr)")))+
    #ylim(-0.26,0.26)+
    guides(fill = guide_legend(reverse = TRUE))+
    theme_bw()+
    # annotate("rect",ymin=-0.25,ymax=0,xmin=0,xmax=10,fill="#984EA3",#"#e41a1c",
    #          color="transparent",alpha=.6)+
    # annotate("rect",ymin=0,ymax=0.25,xmin=0,xmax=10,fill="#377eb8",
    #          color="transparent",alpha=.6)+
    geom_point(size = 4.5, shape = 21, fill = "white", stroke = 1)+
    geom_label(x=9.9,y=-0.14,aes(label=Group1),size=3,color="#183c37", family = "Roboto Condensed", fill="white")+
    geom_label(x=9.9,y=+0.14,aes(label=Group2), size=3, color = "#1c1807", family = "Roboto Condensed", fill="white")+
    scale_y_continuous(limits = c(-0.26,0.26),breaks=yticks,labels=sapply(yticks,function(z) return(ifelse(z<0,paste0("-  ",as.character(-z)),ifelse(z>0,paste0("+",as.character(z)),"0")))))+
    scale_color_manual(values=c("#984EA3","#0077ad"))+
    scale_fill_manual(values=c("#984EA3","#0077ad"))+
    theme_ipsum_rc(grid="XY",axis_title_just="m")+
    theme(text=element_text(size=12, family = "Roboto Condensed"),legend.position="none",
          aspect.ratio=2.5/1, axis.text.y =element_blank(),#element_text(hjust=0,color="black"),
          axis.title.x = element_text(size=20),
          panel.border = element_rect(colour = "black", fill=NA, size=1.5))
  
  
  if(isMainFig)
    ggsave(file=paste0(filename,gsub(" ","",AttrLabel),"OLD.pdf"), width = 12, height = 6)
  else
    ggsave(gg,file=paste0(filename,gsub(" ","",AttrLabel),"SIOLD.pdf"), width = 12, height = 6)
}


# Main effect sizes
GetMainEffectSizesActualWeights <- function(profiles,savedata,r){
  Coeffs <- matrix(NA,r,2)
  
  lev <- levels(profiles$ScenarioType)
  if (levels(profiles$ScenarioType)[1]=="") lev <- levels(profiles$ScenarioType)[2:8]
  lev <- lev[c(3,2,5,1,7,6)]
  
  # For intervention
  profiles$BC.weights <- calcWeightsActual(as.factor(profiles$Intervention),paste0(profiles$CrossingSignal,'',profiles$Barrier,'',profiles$PedPed))
  lm.Int <- lm(Saved ~as.factor(Intervention), data=profiles, weights = BC.weights)
  lm.Int.se <- coeftest(lm.Int, cluster.vcov(lm.Int, cluster = profiles$UserID))[,2]
  Coeffs[1,1] <- lm.Int$coefficients[[2]]
  Coeffs[1,2] <- lm.Int.se[[2]]
  
  # For relationship to vehicle (no need for weighting)
  ## Consider only 'no legality' (CrossingSignal==0) and 'passengers vs. pedestrians' (PedPed==0)
  profile.Relation <- profiles[which(profiles$CrossingSignal==0 & profiles$PedPed==0),]
  profile.Relation$BC.weights <- calcWeightsActual(as.factor(profile.Relation$Barrier),profile.Relation$Intervention)
  
  lm.Rel <- lm(Saved ~as.factor(Barrier), data=profile.Relation, weights = BC.weights)
  lm.Rel.se <- coeftest(lm.Rel, cluster.vcov(lm.Rel, cluster = profile.Relation$UserID))[,2]
  Coeffs[2,1] <- lm.Rel$coefficients[[2]]
  Coeffs[2,2] <- lm.Rel.se[[2]]
  
  # Legality (no need for weighting)
  ## Exclude 'no legality' (CrossingSignal!=0) and consider only 'pedestrians vs. pedestrians' (PedPed==1)
  profile.Legality <- profiles[which(profiles$CrossingSignal!=0 & profiles$PedPed==1),]
  profile.Legality$BC.weights <- calcWeightsActual(as.factor(profile.Legality$CrossingSignal),profile.Legality$Intervention)
  lm.Leg <- lm(Saved ~as.factor(CrossingSignal), data=profile.Legality, weights = BC.weights)
  lm.Leg.se <- coeftest(lm.Leg, cluster.vcov(lm.Leg, cluster = profile.Legality$UserID))[,2]
  Coeffs[3,1] <- lm.Leg$coefficients[[2]]
  Coeffs[3,2] <- lm.Leg.se[[2]]
  
  # Six factors (gender, fitness, Social Status, age, utilitarianism, age, and species)
  ## Extract data subsets and run regression for each
  for(i in 1:6){
    Temp <- profiles[which(profiles$ScenarioType==lev[i] & profiles$ScenarioTypeStrict==lev[i]),]
    Temp$BC.weights <- calcWeightsActual(as.factor(Temp$AttributeLevel),paste0(Temp$Intervention,'',Temp$CrossingSignal,'',Temp$Barrier,'',Temp$PedPed))
    
    lm.Temp <- lm(Saved ~ as.factor(AttributeLevel), data=Temp, weights = BC.weights)
    lm.Temp.se <- coeftest(lm.Temp, cluster.vcov(lm.Temp, cluster = Temp$UserID))[,2]
    Coeffs[i+3,1] <- lm.Temp$coefficients[[2]]
    Coeffs[i+3,2] <- lm.Temp.se[[2]]
    
    # Save to a data frame
    if(savedata){
      var.name <- paste("profile",gsub(" ","",lev[i]),sep=".")
      assign(var.name,Temp)
    }
  }
  return(Coeffs)
}

# New functions -- Revise and resubmit
PreferredLevel <- list(Age = "Young",
                       Fitness = "Fit",
                       `Social Status` = "High",
                       Species = "Hoomans",
                       Gender = "Female",
                       Utilitarian = "More",
                       Random = "Rand")

NonPreferredLevel <- list(Age = "Old",
                          Fitness = "Fat",
                          `Social Status` = "Low",
                          Species = "Pets",
                          Gender = "Male",
                          Utilitarian = "Less",
                          Random = "Rand")

PreferredLevelF <- function(sType){
  return(as.character(unlist(PreferredLevel[sType])))
}


RecodeColumns <- function(profiles){
  profiles[,PreferredCol:= PreferredLevelF(as.character(ScenarioType))]
  profiles[,PreferredIsChosen:= ifelse(!xor(AttributeLevel==PreferredCol,Saved),TRUE,FALSE)]
  profiles[,StayIsChosen:= ifelse(!xor(Intervention==0,Saved),TRUE,FALSE)]
  profiles[,PedestriansIsChosen:= ifelse(!xor(Barrier==0,Saved),TRUE,FALSE)]
  profiles[,LegalIsChosen:= ifelse(!xor(CrossingSignal==1,Saved),TRUE,FALSE)]
  return(profiles)
}

# Prepare demographic columns
PrepareDemColumns <- function(profiles){
  profiles[,Review_gender:=gsub("default",NA,Review_gender)]
  profiles[,Review_gender:=gsub("others",NA,Review_gender)]
  profiles[,Review_gender:= factor(Review_gender,levels=c("female","male"))]
  
  profiles[,Review_income:=gsub("default",NA,Review_income)]
  profiles[,Review_income:=gsub("over10000","above100000",Review_income)]
  profiles[,Review_income:= factor(Review_income,levels=c("under5000","5000","10000","15000","25000",
                                                          "35000","50000","80000","above100000"))]
  profiles[,Review_ContinuousIncome:= case_when(Review_income == "under5000"~ 2500,
                                                Review_income == "5000"~7500,
                                                Review_income == "10000"~12500,
                                                Review_income == "15000"~20000,
                                                Review_income == "25000"~30000,
                                                Review_income == "35000"~42500,
                                                Review_income == "50000"~65000,
                                                Review_income == "80000"~90000,
                                                Review_income == "above100000"~150000)]
  profiles[,Review_ContinuousIncome.z:=as.numeric(scale(Review_ContinuousIncome))]
  
  profiles[,Review_education:=gsub("default",NA,Review_education)]
  profiles[,Review_education:= factor(Review_education,levels=c("underHigh","high","vocational","college",
                                                                "bachelor","graduate","others"))]
  profiles[,Review_CollegeEducated:= ifelse(Review_education=="others",NA,
                                            ifelse(Review_education %in% c("college","bachelor","graduate"),1,0))]
  
  
  profiles[,Review_age:=ifelse(Review_age>15 & Review_age<75,Review_age,NA)]
  profiles[,Review_age.z:=as.numeric(scale(Review_age))]
  
  profiles[,Review_political.z:=as.numeric(scale(Review_political))]
  
  profiles[,Review_religious.z:=as.numeric(scale(Review_religious))]
  
  return(profiles)
}




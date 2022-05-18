##### Presetting ######
  rm(list = ls()) # Clean variable
  memory.limit(150000)

##### Load Packages #####
  ## Check whether the installation of those packages is required from basic
  Package.set <- c("tidyverse","readxl")
  for (i in 1:length(Package.set)) {
    if (!requireNamespace(Package.set[i], quietly = TRUE)){
      install.packages(Package.set[i])
    }
  }
  ## Load Packages
  lapply(Package.set, library, character.only = TRUE)
  rm(Package.set,i)


##### Input data #####
  MusFib.df <- read_excel("C:/Users/YSL/Downloads/cachexia_muscle-area-fiber-counts.xlsx", 
                                          sheet = "skeletal muscle sample raw data")

##### Preprocess the dataframe #####
  ## Group
  Anno.df <- data.frame(Samples = colnames(MusFib.df),
                        Groups = c("Severe","Severe","Severe","Medium","Mild","Medium","Severe","Severe","Mild","Medium","Mild","Severe"))

##### Create dataframe for ggplot ##### 
  FreDis.df <- MusFib.df %>% pivot_longer(cols = 1:ncol(MusFib.df), 
                                          names_to = "Samples", 
                                          values_to = "Area") %>% left_join(Anno.df)


##### Basic Plot #####
  #### Basic Density plot ####
  ggplot(FreDis.df, aes(x = Area, colour= Samples)) + geom_density()
  ggplot(FreDis.df, aes(x = Area, colour= Groups)) + geom_density()
  ggplot(FreDis.df, aes(x = Area, colour= Groups, group = Samples)) + geom_density(kernel = "gaussian") # https://r-charts.com/distribution/density-plot-ggplot2/
  ggplot(FreDis.df, aes(x = Area, colour= Groups, group = Samples)) + 
         geom_density() + geom_rug(aes(min_dist= 400))
  qplot(x=Area, data=FreDis.df, geom='density', fill=as.factor(Samples),
        alpha=I(0.5), position='identity', color=I('black'),
        y=..count../sum(..count..))

  ## Beautify plot
    P_Den <- ggplot(FreDis.df, aes(x = Area, colour = Groups, group = Samples)) + geom_density()
    P_Den
    P_Den + theme_classic() + # White background
            theme(axis.line = element_line(colour = "black", 
                                            size = 1, linetype = "solid")) + # Change the line type and color of axis lines
            theme(axis.text.x = element_text(face="bold", color="black", 
                                             size=14, angle=0),
                  axis.text.y = element_text(face="bold", color="black", 
                                             size=14, angle=0)) +  # Change the appearance and the orientation angle
            ggtitle("Frequency distribution")+ # Change the main title and axis labels
            xlab("Muscle fiber size") + ylab("% Relative Frequency") +
            theme(
            plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5),
            axis.title.x = element_text(color="black", size=16, face="bold"),
            axis.title.y = element_text(color="black", size=16, face="bold") # Change the color, the size and the face of  the main title, x and y axis labels
            )+
            scale_x_continuous(breaks=seq(0,6500,500)) + # Setting the tick marks on an axis
            theme(legend.title = element_text(colour="black", size=15, face="bold")) + # legend title
            theme(legend.text = element_text(colour="black", size=12, face="bold")) + # legend labels
            theme(legend.position = c(0.8, 0.8)) + #  legend.position 
            theme(legend.key.size = unit(1, 'cm')) -> P_Den2
    P_Den2
    
    #### Basic dotplot ####
    P_Dot <-  ggplot(FreDis.df, aes(x=Samples, y= Area)) + geom_point()
    P_Dot
    
    # #error# Discrete value supplied to continuous scale
    # ggplot(FreDis.df) + geom_density(aes(x=Area, colour=Groups, group = Samples)) +
    # geom_point(aes(x=Samples, y= Area))
    
    #### Basic Hisplot ####
    ## Stack
    P_His <- ggplot(FreDis.df) + geom_histogram(aes(x = Area, colour = Groups, fill= Samples),
                                                binwidth = 400)
    P_His
    P_His2 <- qplot(x=Area, data=FreDis.df, geom='histogram', fill=as.factor(Samples), 
                    alpha=I(0.5), binwidth=400, position='identity')
    P_His2
    ## Overlay
    P_His3  <- ggplot(FreDis.df, aes(x=Area,  fill=Samples)) + 
      geom_histogram( aes(y = ..density..), color="#e9ecef", alpha=0.6, position = 'identity', binwidth = 400) +
      # scale_fill_manual(values=c("#69b3a2", "#404080")) +
      labs(fill="")
    P_His3
  
    #### Overlay different type of Figures ####
    ggplot(FreDis.df, aes(x=Area,  fill=Samples))  +
      geom_density(size = 0.8, alpha = 0.5) + 
      geom_histogram( aes(y = ..density..), color="#e9ecef", alpha=0.3, position = 'identity', binwidth = 400)
    
    ##
    FreDis_S.df <- FreDis.df[FreDis.df$Samples=="F88",]
    P_OvDenHis_S <- ggplot(data = FreDis_S.df,
                    mapping = aes(x = Area, fill=as.factor(Samples)))
    P_OvDenHis_S + geom_histogram(mapping = aes(y = ..density..), alpha = 0.5, binwidth=400) +
                   geom_density(size = 0.8, alpha = 0.4)
    rm(FreDis_S.df)  
##### Density Plot #####  
  #### Set the bin and classify different range for Area ####
    FreDis.df <- FreDis.df[!is.na(FreDis.df$Area),]
    MS_Max = FreDis.df$Area  %>% max(na.rm = T)
    MS_Min = FreDis.df$Area  %>% min(na.rm = T)
    
    Bin = 400
    
    Max_Range = floor(MS_Max/Bin)+1 # floor(): Round down
    
    FreDis.df$Range <- 1
    for (i in 1:Max_Range) {
      for (j in 1:nrow(FreDis.df)) {
        #if(FreDis.df$Area[j] %in% range(i*400,(i+1)*400)){
        if((i-1)*Bin <= FreDis.df$Area[j] && FreDis.df$Area[j] < (i)*Bin ){
            
          FreDis.df$Range[j] <- i
        }else{
          FreDis.df$Range[j] <- FreDis.df$Range[j]
        }
      }
    }
    #### Group the sample by range of bin for each type of sample ####
    Tem.df <-  FreDis.df %>% group_by(Samples) %>% 
                             count(Range) %>% 
                             right_join(FreDis.df) %>%
                             rename(Count=n)  # colnames(Tem.df)[3] <- "Count" 
    Tem.df <- Tem.df %>% group_by(Samples) %>% 
                         count(Samples) %>% 
                         right_join(Tem.df) %>%
                         data.frame(., Fre=.$Count/.$n)
    FreDis.df <- Tem.df %>% group_by(Range,Fre,Samples) %>% 
                            summarise(avg = mean(Area)) %>% 
                            left_join(.,FreDis.df, by=c("Samples","Range" ))
  
    rm(Tem.df)

    #### Density plot overlay with dotplot by samples group by bin range ####
    #library(ggpubr)
    P_OvDenPoint <- FreDis.df %>%
                    ggplot( mapping =aes(x=avg, y=Fre,color=Groups,fill=Groups ,
                                         group=Samples, shape= Samples))  +
                    geom_point(aes(x=avg, y=Fre, color=Groups ))+
                    scale_shape_manual(values = seq(1:nrow(Anno.df))) + # https://www.datanovia.com/en/blog/ggplot-point-shapes-best-tips/
                    geom_smooth(se=F,method = "loess")+
                    ggtitle("Full_data")
    P_OvDenPoint
    
    #### Add text annotation to each line  ####
     #### At Max y  ####
      # https://stackoverflow.com/questions/71162252/adding-the-maximum-peak-value-in-ggplot-for-geom-smoth
      # create the smooth and retain rows with max of smooth, using slice_max
      sm_max = FreDis.df %>% distinct(.,Range,Fre,Samples,avg,Groups) %>% group_by(Samples) %>%
              mutate(smooth =predict(loess(Fre~as.numeric(avg), span=.5))) %>% 
              slice_max(order_by = smooth)
      ## Plot
      P_OvDenPoint + geom_text(data = sm_max, aes(y=smooth, label=paste0(sm_max$Samples,": ",round(smooth,4)), color=Groups), size=4, face="bold") +
          theme_classic() + # White background
          theme(axis.line = element_line(colour = "black", 
                                         size = 1, linetype = "solid")) + # Change the line type and color of axis lines
          theme(axis.text.x = element_text(face="bold", color="black", 
                                           size=14, angle=0),
                axis.text.y = element_text(face="bold", color="black", 
                                           size=14, angle=0)) +  # Change the appearance and the orientation angle
          ggtitle("Frequency distribution")+ # Change the main title and axis labels
          xlab("Muscle fiber size") + ylab("% Relative Frequency") +
          theme(
            plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5),
            axis.title.x = element_text(color="black", size=16, face="bold"),
            axis.title.y = element_text(color="black", size=16, face="bold") # Change the color, the size and the face of  the main title, x and y axis labels
          )+
          scale_x_continuous(breaks=seq(0,6500,500)) + # Setting the tick marks on an axis
          theme(legend.title = element_text(colour="black", size=15, face="bold")) + # legend title
          theme(legend.text = element_text(colour="black", size=12, face="bold")) + # legend labels
          theme(legend.position = c(0.9, 0.6)) + #  legend.position 
          theme(legend.key.size = unit(0.8, 'cm')) -> P_OvDenPoint_Max
      P_OvDenPoint_Max
    
    
     #### At Max y in center ####
      Deriv.df = FreDis.df %>% distinct(.,Range,Fre,Samples,avg,Groups) %>% 
                               group_by(Samples) %>% mutate(smooth =predict(loess(Fre~as.numeric(avg), span=.5))) 
      Samples.set <- Anno.df$Samples
      
      ## Using Calculus to Find Extreme Values
      for (i in 1:length(Samples.set)) {
        if(i==1){
          Deriv2.df <- Deriv.df[Deriv.df$Samples == Samples.set[i], ]
          model <- smooth.spline(x = Deriv2.df$avg, y = Deriv2.df$smooth)
          Y0 <- predict(model, x = seq(0.1,max(Deriv2.df$avg),length=50), deriv=0)
          Y1 <- predict(model, x = seq(0.1,max(Deriv2.df$avg),length=50), deriv=1)
          Y2 <- predict(model, x = seq(0.1,max(Deriv2.df$avg),length=50), deriv=2)
          DerivSR.df <- data.frame(x=Y1$x, y=Y0$y ,d1=Y1$y, d2=Y2$y)
          DerivSR.df <- DerivSR.df[DerivSR.df$d2 < 0,]
          
          for (j in 1:nrow(DerivSR.df)) {
            if(DerivSR.df$d1[j] == min(DerivSR.df$d1) )
              Deriv2.df$x = DerivSR.df$x[j]
              Deriv2.df$y = DerivSR.df$y[j]
            
          }
        }else{
            DerivS.df <- Deriv.df[Deriv.df$Samples == Samples.set[i], ]
            model <- smooth.spline(x = DerivS.df$avg, y = DerivS.df$smooth)
            Y0 <- predict(model, x = seq(0.1,max(DerivS.df$avg),length=50), deriv=0)
            Y1 <- predict(model, x = seq(0.1,max(DerivS.df$avg),length=50), deriv=1)
            Y2 <- predict(model, x = seq(0.1,max(DerivS.df$avg),length=50), deriv=2)
            DerivSR.df <- data.frame(x=Y1$x, y=Y0$y ,d1=Y1$y, d2=Y2$y)
            DerivSR.df <- DerivSR.df[DerivSR.df$d2 < 0,]
            
            for (j in 1:nrow(DerivSR.df)) {
              if(DerivSR.df$d1[j] == min(DerivSR.df$d1) )
                DerivS.df$x = DerivSR.df$x[j]
                DerivS.df$y = DerivSR.df$y[j]
            }
            
            Deriv2.df <- rbind(Deriv2.df, DerivS.df)
            }
      }
      Deriv.df <- Deriv2.df
      rm(DerivS.df, DerivSR.df, model, Y0,Y1,Y2,Deriv2.df)
      
      sm_max2 = FreDis.df %>% group_by(Samples) %>%
        mutate(smooth =predict(loess(Fre~as.numeric(avg), span=.5))) %>% 
        slice_max(order_by = smooth)
      Deriv.df <- left_join( sm_max2,Deriv.df[, c("Range", "Samples", "x","y")], by = c("Range", "Samples") )%>% distinct(.,Range,Samples,avg,Groups,.keep_all=T)
      rm(sm_max2)
      ## Plot
      P_OvDenPoint + geom_text(data = Deriv.df, aes(x=x,y=smooth, label=paste0(Deriv.df$Samples,": ",round(smooth,4)), 
                                                        color=Groups), size=4, face="bold") +
        theme_classic() + # White background
        theme(axis.line = element_line(colour = "black", 
                                       size = 1, linetype = "solid")) + # Change the line type and color of axis lines
        theme(axis.text.x = element_text(face="bold", color="black", 
                                         size=14, angle=0),
              axis.text.y = element_text(face="bold", color="black", 
                                         size=14, angle=0)) +  # Change the appearance and the orientation angle
        ggtitle("Frequency distribution")+ # Change the main title and axis labels
        xlab("Muscle fiber size") + ylab("% Relative Frequency") +
        theme(
          plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5),
          axis.title.x = element_text(color="black", size=16, face="bold"),
          axis.title.y = element_text(color="black", size=16, face="bold") # Change the color, the size and the face of  the main title, x and y axis labels
        )+
        scale_x_continuous(breaks=seq(0,6500,500)) + # Setting the tick marks on an axis
        theme(legend.title = element_text(colour="black", size=15, face="bold")) + # legend title
        theme(legend.text = element_text(colour="black", size=12, face="bold")) + # legend labels
        theme(legend.position = c(0.9, 0.6)) + #  legend.position 
        theme(legend.key.size = unit(0.8, 'cm')) -> P_OvDenPoint_Max2
      P_OvDenPoint_Max2
      
      

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


##### input data #####
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
    p_Den <- ggplot(FreDis.df, aes(x = Area, colour = Groups, group = Samples)) + geom_density()
    p_Den
    p_Den + theme_classic() + # White background
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
            theme(legend.key.size = unit(1, 'cm')) -> p_Den2
    p_Den2
    
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
  
##### Density Plot #####  

############################################################################################################################

# Test






# ## 
# library(UsingR)
# hist(Galton$parent, freq = FALSE)
# x <- seq(64, 74, length.out=100)
# y <- with(Galton, dnorm(x, mean(parent), sd(parent)))
# lines(x, y, col = "red")


## 
p <- ggplot(data = FreDis.df,  mapping = aes(
              x = Area, 
              color = Samples))
p + geom_line(stat = "density")

##
dfs <- FreDis.df[FreDis.df$Samples=="F88",]
p <- ggplot(data = dfs,
            mapping = aes(x = Area, fill=as.factor(Samples)))
p + geom_histogram(mapping = aes(y = ..density..), alpha = 0.4, binwidth=400) +
  geom_density(size = 1.1, alpha = 0.6)

############################################################################################################################

## KLC
# generate dataframe for ploting  
to_plot <- data.frame(
  Distance = output.df$`mean(Distance)`,
  Density = output.df$Frequency/sum(output.tb$Frequency)
) %>% unique()

plt <- to_plot %>%
  ggplot(aes(x=Distance, y=Density)) +
  geom_smooth(method = "gam", se=TRUE, fill="#E0E0E0", color="red", size=0.5) +
  geom_point(aes(x=Distance, y=Density), shape=21, color="#4F4F4F", fill="#4F4F4F", size=0.5)+
  ggtitle("Full_data") +
  ylim(0,0.03)+
  geom_vline(xintercept = 1, color = "#5B5B5B", linetype="dotted", size=0.6)+
  geom_vline(xintercept = 2, color = "#5B5B5B", linetype="dotted", size=0.6)+
  theme_classic()
plot(plt)

############################################################################################################################
## Main
df <- MusFib.df
df <- pivot_longer(df, cols = 1:ncol(df), 
                   names_to = "Muscle_fiber_samples", values_to = "Muscle_fiber_size") %>%
  left_join(Anno.df)

df <- df[!is.na(df$Muscle_fiber_size),]
MS_Max = df$Muscle_fiber_size  %>% max(na.rm = T)
MS_Min = df$Muscle_fiber_size  %>% min(na.rm = T)

Bin = 400

Max_Range = round(MS_Max/Bin)+1

df$Range <- 1
for (i in 0:Max_Range+1) {
  for (j in 1:nrow(df)) {
    #if(df$Muscle_fiber_size[j] %in% range(i*400,(i+1)*400)){
    if(i*Bin <= df$Muscle_fiber_size[j] && df$Muscle_fiber_size[j] < (i+1)*Bin ){
        
      df$Range[j] <- i+1
    }else{
      df$Range[j] <- df$Range[j]
    }
    
  }
  
}

## Anno 2
Anno2.df <- data.frame(Muscle_fiber_samples = rep(Anno.df$Muscle_fiber_samples,each =max(df$Range)),
                                                  Range= seq(1:max(df$Range)))
# 
# dfTTT <- left_join(Anno2.df,df)
# dfTTT[is.na(dfTTT[, "Muscle_fiber_size"]), "Muscle_fiber_size"] <- 0
# dfTTT <- left_join(dfTTT[,-4],Anno.df)
# 
# df <- dfTTT

df2 <- df %>% group_by(Muscle_fiber_samples) %>% 
              count(Range) %>% right_join(df)

colnames(df2)[3] <- "Count" 
df3 <- df2 %>% group_by(Muscle_fiber_samples) %>% 
       count(Muscle_fiber_samples) %>% right_join(df2)
df4 <- data.frame(df3, Fre=df3$Count/df3$n)

df5 <- df4 %>% group_by(Range,Fre,Muscle_fiber_samples) %>% summarise(avg = mean(Muscle_fiber_size))
df6 <- left_join(df5,Anno.df)
# ggplot(df2, aes(x=avg, fill= Muscle_fiber_samples)) +
#   geom_histogram() 

#library(ggpubr)
plt <- df6 %>%
  ggplot( mapping =aes(x=avg, y=Fre,color=Group,fill=Group ,
                       group=Muscle_fiber_samples, shape= Muscle_fiber_samples))  +
  geom_point(aes(x=avg, y=Fre, color=Group ))+
  scale_shape_manual(values = seq(1:nrow(Anno.df))) + # https://www.datanovia.com/en/blog/ggplot-point-shapes-best-tips/
  #show_point_shapes(aes(x=avg, y=Fre, color=Group, shape= Muscle_fiber_samples ))+
  geom_smooth(se=F,method = "loess")+
  ggtitle("Full_data") # +
  # ylim(0,0.03)+
  # geom_vline(xintercept = 1, color = "#5B5B5B", linetype="dotted", size=0.6)+
  # geom_vline(xintercept = 2, color = "#5B5B5B", linetype="dotted", size=0.6)+
  # theme_classic()
plt

# https://stackoverflow.com/questions/71162252/adding-the-maximum-peak-value-in-ggplot-for-geom-smoth
# create the smooth and retain rows with max of smooth, using slice_max
sm_max = df6 %>% group_by(Muscle_fiber_samples) %>%
  mutate(smooth =predict(loess(Fre~as.numeric(avg), span=.5))) %>% 
  slice_max(order_by = smooth)

## Find max in center
Deriv.df = df6 %>% group_by(Muscle_fiber_samples) %>%
  mutate(smooth =predict(loess(Fre~as.numeric(avg), span=.5))) 
Muscle_fiber_samples.set <- Anno.df$Muscle_fiber_samples
model <- smooth.spline(x = Deriv.df$avg, y = Deriv.df$smooth)
Y1 <- predict(model, x = seq(0.1,max(Deriv.df$avg),length=1000), deriv=1)
Y2 <- predict(model, x = seq(0.1,max(Deriv.df$avg),length=1000), deriv=2)

for (i in 1:length(Muscle_fiber_samples.set)) {
  if(i==1){
    Deriv2.df <- Deriv.df[Deriv.df$Muscle_fiber_samples == Muscle_fiber_samples.set[i], ]
    model <- smooth.spline(x = Deriv2.df$avg, y = Deriv2.df$smooth)
    Y0 <- predict(model, x = seq(0.1,max(Deriv2.df$avg),length=1000), deriv=0)
    Y1 <- predict(model, x = seq(0.1,max(Deriv2.df$avg),length=1000), deriv=1)
    Y2 <- predict(model, x = seq(0.1,max(Deriv2.df$avg),length=1000), deriv=2)
    DerivSR.df <- data.frame(x=Y1$x, y=Y0$y ,d1=Y1$y, d2=Y2$y)
    DerivSR.df <- DerivSR.df[DerivSR.df$d2 < 0,]
    
    for (j in 1:nrow(DerivSR.df)) {
      if(DerivSR.df$d1[j] == min(DerivSR.df$d1) && DerivSR.df$d2[j] < 0)
        Deriv2.df$x = DerivSR.df$x[j]
        Deriv2.df$y = DerivSR.df$y[j]
      
    }
  }else{
      DerivS.df <- Deriv.df[Deriv.df$Muscle_fiber_samples == Muscle_fiber_samples.set[i], ]
      model <- smooth.spline(x = DerivS.df$avg, y = DerivS.df$smooth)
      Y0 <- predict(model, x = seq(0.1,max(DerivS.df$avg),length=1000), deriv=0)
      Y1 <- predict(model, x = seq(0.1,max(DerivS.df$avg),length=1000), deriv=1)
      Y2 <- predict(model, x = seq(0.1,max(DerivS.df$avg),length=1000), deriv=2)
      DerivSR.df <- data.frame(x=Y1$x, y=Y0$y ,d1=Y1$y, d2=Y2$y)
      DerivSR.df <- DerivSR.df[DerivSR.df$d2 < 0,]
      
      for (j in 1:nrow(DerivSR.df)) {
        if(DerivSR.df$d1[j] == min(DerivSR.df$d1) && DerivSR.df$d2[j] < 0)
          DerivS.df$x = DerivSR.df$x[j]
          DerivS.df$y = DerivSR.df$y[j]
      }
      
      Deriv2.df <- rbind(Deriv2.df, DerivS.df)
      }
}

rm(DerivS.df, DerivSR.df, model, Y0,Y1,Y2)

Deriv_max.df <- left_join(sm_max,Deriv2.df[, c("Range", "Muscle_fiber_samples", "x","y")],by = c("Range", "Muscle_fiber_samples"))
                

# Plot, using the same smooth as above (default is loess, span set at set above)
# df6 %>%
#   ggplot(df6, mapping = aes(avg,Fre,  group = Muscle_fiber_samples, color = Group)) +
#   geom_point() +
#   geom_smooth(span=.5, se=F) + 
#   geom_point(data=sm_max, aes(y=smooth),color="black", size=5) + 
#   geom_text(data = sm_max, aes(y=smooth, label=paste0(sm_max$Muscle_fiber_samples,": ",round(smooth,1))), color="black")
# 

## sm_max
plt + geom_text(data = sm_max, aes(y=smooth, label=paste0(sm_max$Muscle_fiber_samples,": ",round(smooth,4)), color=Group), size=4, face="bold") -> plt2

plt2 + theme_classic() + # White background
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
  theme(legend.key.size = unit(0.8, 'cm')) -> plt3
plt3

## Deriv_max.df
plt + geom_text(data = Deriv_max.df, aes(x=x,y=smooth, label=paste0(Deriv_max.df$Muscle_fiber_samples,": ",round(smooth,4)), color=Group), size=4, face="bold") -> plt2

plt2 + theme_classic() + # White background
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
  theme(legend.key.size = unit(0.8, 'cm')) -> plt3
plt3

# plt + geom_text(data = df6, aes(y=smooth, label=sprintf( df6$Muscle_fiber_samples)), color="black")
# 
# plt + scale_y_continuous(breaks=seq(0,0.4,0.2)) + # Setting the tick marks on an axis
#       scale_y_continuous(limits = c(0, 0.3)) + 
#       annotate('text', x = df6$avg, y = df6$Fre,
#                label = sprintf( df6$Muscle_fiber_samples), vjust = 0)
# 
# # Text.df = as.data.frame(matrix(nrow=nrow(Anno.df),ncol=0))
# Text.df <- df6 %>% group_by(Muscle_fiber_samples) %>% summarise(Fre=max(Fre))
# Text.df <- df6 %>% group_by(Muscle_fiber_samples) %>% summarise(xmin=min(avg))
# 
# Text.df <- left_join(Text.df,df6)
# Text.df <-Text.df[Text.df$Range==1,] 
# 
# 
# 
# library(geomtextpath)
# plt + scale_y_continuous(breaks=seq(0,0.4,0.2)) + # Setting the tick marks on an axis
#   scale_y_continuous(limits = c(0, 0.3))+ 
#   annotate('text', x = Text.df$xmin, y = Text.df$Fre,
#            label = sprintf( Text.df$Muscle_fiber_samples), vjust = 0)
# 
# 
# ###############################################################################################################################
# ## https://stackoverflow.com/questions/54135969/how-to-put-the-legends-in-the-peaks-of-multiple-distributions-using-ggplot
# labels <-
#   lapply(split(df6, df6$Muscle_fiber_samples), function(x){
#     dens <- density(x$avg)  # compute density of each variable
#     
#     data.frame(y = max(dens$y),  # get maximum density of each variable
#                x = dens$x[which(dens$y == max(dens$y))],  # get corresponding x value
#                label = x$Muscle_fiber_samples[1])
#   })
# 
# 
# plt + scale_y_continuous(breaks=seq(0,0.4,0.2)) + # Setting the tick marks on an axis
#   scale_y_continuous(limits = c(0, 0.3))+  
#   geom_text(data = do.call(rbind, labels), aes(x = x, y = y, label = label), 
#             inherit.aes = F, 
#             nudge_y = 0.03) +
#   labs(fill="")
# 
# 
# ##
# #library(ggpubr)
# plt <- df6 %>%
#   ggplot( mapping =aes(x=avg, y=Fre,color=Muscle_fiber_samples,fill=Group ,
#                        group=Muscle_fiber_samples, shape= Group))  +
#   geom_point(aes(x=avg, y=Fre, color=Group ))+
#   scale_shape_manual(values = seq(1:nrow(Anno.df))) + # https://www.datanovia.com/en/blog/ggplot-point-shapes-best-tips/
#   #show_point_shapes(aes(x=avg, y=Fre, color=Group, shape= Muscle_fiber_samples ))+
#   geom_smooth(se=F)+
#   ggtitle("Full_data") # +
# plt 
# # ylim(0,0.03)+
# # geom_vline(xintercept = 1, color = "#5B5B5B", linetype="dotted", size=0.6)+
# # geom_vline(xintercept = 2, color = "#5B5B5B", linetype="dotted", size=0.6)+
# # theme_classic()
# 

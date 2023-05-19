
setwd('/Users/keiranmaskell/Desktop/P_syring/fitness_landscaper')
rm(list=ls())

source('init.R')
#library(xlsx)
#bac_fit_data <- read.xlsx("/Users/keiranmaskell/Downloads/For_Keiran/GA_MinScreen4_B3i1_2023-02-24 copy.xlsx", sheetIndex = 1)
# fake_data <- read.xlsx("/Users/keiranmaskell/Desktop/P_syring/fitness_landscaper/fake_data.xlsx", sheetIndex = 1)

# GA_master <- read.xlsx("/Users/keiranmaskell/Desktop/P_syring/Data/GA_master.xlsx", sheetIndex = 1)

# plant_fit_data <- read.xlsx("/Users/keiranmaskell/Desktop/P_syring/fitness_landscaper/fake_pidiq_data.xlsx",sheetIndex = 1)

# plant_fitness_master <- read.xlsx("/Users/keiranmaskell/Desktop/P_syring/Data/Plant_Fitness_master.xlsx",sheetIndex = 1)

# write.csv(GA_master, "/Users/keiranmaskell/Desktop/P_syring/Data/GA_master.csv", row.names=FALSE)
# write.csv(plant_fitness_master, "/Users/keiranmaskell/Desktop/P_syring/Data/Plant_fitness_master.csv", row.names=FALSE)

# GA_master <- read.csv("/Users/keiranmaskell/Desktop/P_syring/Data/GA_master.csv")
# plant_fitness_master <- read.csv("/Users/keiranmaskell/Desktop/P_syring/Data/Plant_fitness_master.csv")

# #GA_fitness <- data.frame(fake_data)
# #GA_fitness <- GA_fitness[1:64,]

# GA_fitness <- data.frame(GA_master)

source('fxn_repo.R')

data_list <- data_loader(bac_in_fp="/Users/keiranmaskell/Desktop/P_syring/Data/GA_master.xlsx",
            plant_in_fp="/Users/keiranmaskell/Desktop/P_syring/Data/Plant_Fitness_master.xlsx",
            bac_out_fp="/Users/keiranmaskell/Desktop/P_syring/Data/GA_master.csv",
            plant_out_fp="/Users/keiranmaskell/Desktop/P_syring/Data/Plant_fitness_master.csv")
GA_fitness <- data.frame(data_list[[1]])
Pidiq_fitness <- data.frame(data_list[[2]])

GA_fitness <- data_list[[1]]
Pidiq_fitness <- data_list[[2]]

#View(GA_fitness)


# #add log.CFU.cm2 column
# #GA_fitness <- GA_fitness[,'Colony.count']*10^(as.numeric(GA_fitness[,'Diln']))
# #bec <- log((as.numeric(GA_fitness[,'Colony.count'])*(1/as.numeric(GA_fitness[,'Diln']))*80)/1)


# GA_fitness <- data.frame(GA_fitness[,1:11], 'log.CFU.cm2'=
#                          ifelse(as.numeric(GA_fitness[,'Diln'])==0, log10((as.numeric(GA_fitness[,'Colony.count'])*80)/1)
#                                                              ,log10((as.numeric(GA_fitness[,'Colony.count'])*(1/as.numeric(10^(-1)*GA_fitness[,'Diln']))*80)/1)),
#                                                              GA_fitness[,12:ncol(GA_fitness)])


# GA_fitness[,'Colony.count'] <- as.numeric(GA_fitness[,'Colony.count'])
# GA_fitness[,'Diln'] <- as.numeric(GA_fitness[,'Diln'])


# #GA_fitness <- data.frame(GA_fitness[,1:15],'log.CFU.cm2'=log10(((GA_fitness[,'Colony.count']) * (1/(10^((-1)*GA_fitness[,'Diln']))) * (80))/1))
# GA_fitness <- data.frame(GA_fitness,'log.CFU.cm2'=log10(((GA_fitness[,'Colony.count']) * (1/(10^((-1)*GA_fitness[,'Diln']))) * (80))/1))
# GA_fitness


# #clean up NAs and weird values created by taking ln(0)
# GA_fitness_wNAs <- GA_fitness
# GA_fitness <- subset(GA_fitness, GA_fitness[,'log.CFU.cm2']!='-Inf')
# GA_fitness <- GA_fitness[complete.cases(GA_fitness[,c("Diln", "Colony.count","log.CFU.cm2")]),]
# row.names(GA_fitness) <- 1:nrow(GA_fitness)
# #GA_fitness[,'log.CFU.cm2']
# View(GA_fitness_wNAs)
# #View(GA_fitness)

#Pidiq_fitness <- data.frame(plant_fit_data)
Pidiq_fitness <- data.frame(Pidiq_fitness)

# #add arcsine transformed column
# Pidiq_fitness[,'Arcsine.transformed.data'] <- asin(sqrt(Pidiq_fitness[,'YellowedArea']/(Pidiq_fitness[,'YellowedArea'] + Pidiq_fitness[,'GreenArea'])))/1.507



GA_fitness <- prepare_GA_data(GA_fitness)
Pidiq_fitness <- prepare_Pidiq_data(Pidiq_fitness)

#18
length(GA_fitness)
names(GA_fitness)
GA_fitness$log.CFU.cm2

View(GA_fitness, title = "GA_fitness")
View(Pidiq_fitness, title = "Pidiq_fitness")

#bacterial fitness
#simple barplot
quartz()
barplot(GA_fitness$log.CFU.cm2, names.arg=GA_fitness$Inoculum, xlab = "Category", ylab = 'Value', main="Bacterial Fitness")

#barplot of means of log.CFU.cm2 with sds as arrows
result <- aggregate(log.CFU.cm2 ~ Inoculum + Flat, data = GA_fitness, FUN = function(x) c(mean = mean(x), sd = sd(x)))
means <- c(data.frame(result)[3][[1]][,1])
order_id <- order(result$Flat, means)
means <- means[order_id]
#means <- means[order(means)]
sd <- c(data.frame(result)[3][[1]][,2])
sd <- sd[order_id]
result <- data.frame(result)
result$Inoculum[result$Inoculum == "D36E::McDC[35]"] <- "[35]"
short_labels <- substr(result$Inoculum, 1, 4)
#the plot
flat_colours <- c("red", "blue", "green", "purple", "orange","magenta","darkgreen","darkblue")
bar_colours <- flat_colours[as.numeric(factor(result$Flat))]

quartz()
bar_centers <- barplot(means, plot = FALSE)
par(cex.axis=0.8, col.axis="black")
barplot(means, names.arg=short_labels, ylim=c(0,9), xlab = "Category", ylab = 'Value', col=bar_colours, las=2)
#axis(1, at = range(1:5), labels = paste0("*", range(1:5)), col.axis = 'red', las=2)
arrows(bar_centers, means - sd, bar_centers, means + sd, angle = 90, code = 3, length = 0.05)
#mtext(text="*", side=1, line=2.5, at=at, col='red', cex=0.8)
#points(log.CFU.cm2 ~ group(Inoculum,Flat), data = GA_fitness)
title(main = "Bacterial Fitness", cex.main = 0.8)


#Plant fitness
1/Pidiq_fitness$Arcsine.transformed.data
#simple barplot
barplot(Pidiq_fitness$Arcsine.transformed.data, names.arg=Pidiq_fitness$Inoculum, xlab = "Category", ylab = 'Value', main="Plant Disease",col=bar_colours, las=2)
barplot(1/Pidiq_fitness$Arcsine.transformed.data, names.arg=Pidiq_fitness$Inoculum, xlab = "Category", ylab = 'Value', main="Plant Fitness",col=bar_colours, las=2)



# Define the colors to use for each level of Flat
flat_colours <- c("red", "blue", "green", "purple", "orange","magenta",'darkgreen','darkblue')
# Map the colors to each level of Flat
bar_colours <- flat_colours[as.numeric(factor(result2$Flat))]
#barplot of means of inv(Arcsine.transformed(yellow/green)) with sds as arrows
result2 <- aggregate(Pidiq_fitness$Arcsine.transformed.data ~ Inoculum + Flat, data = Pidiq_fitness, FUN = function(x) c(mean = mean(x), sd = sd(x)))
means2 <- c(data.frame(result2)[3][[1]][,1])

order_id2 <- order(result2$Flat, means2)
means2 <- means2[order_id2]

sd2 <- c(data.frame(result2)[3][[1]][,2])
sd2 <- sd2[order_id2]

result2 <- data.frame(result2)
#the plot
#virulence
result2$Inoculum[result2$Inoculum == "D36E::McDC[35]"] <- "[35]"
short_labels <- substr(result2$Inoculum, 1, 4)
bar_centers2 <- barplot(means2, plot = FALSE)
par(cex.axis=0.8, col.axis="black")
barplot(means2, names.arg=short_labels, ylim=c(0,max(Pidiq_fitness$Arcsine.transformed.data)), xlab = "Category", ylab = 'Value',col = bar_colours, las=2)
arrows(bar_centers2, means2 - sd2, bar_centers2, means2 + sd2, angle = 90, code = 3, length = 0.05)
title(main = "Plant Symptom Severity", cex.main = 0.8)
#Fitness
result2$Inoculum[result2$Inoculum == "D36E::McDC[35]"] <- "[35]"
short_labels <- substr(result2$Inoculum, 1, 4)
barplot(1/means2, names.arg=short_labels, ylim=c(0,max(1/Pidiq_fitness$Arcsine.transformed.data)), xlab = "Category", ylab = 'Value',col = bar_colours, las=2)
bar_centers2 <- barplot(means2, plot = FALSE)
arrows(bar_centers2, 1/(means2 - sd2), bar_centers2, 1/(means2 + sd2), angle = 90, code = 3, length = 0.05)
title(main = "Plant Fitness", cex.main = 0.8)


par(mfrow = c(2, 1))
#the plot
barplot(means, names.arg=result$Strain, ylim=c(0,9), xlab = "Category", ylab = 'Value', col=result$Batch_Infection)
bar_centers <- barplot(means, plot = FALSE)
arrows(bar_centers, means - sd, bar_centers, means + sd, angle = 90, code = 3, length = 0.05)
title(main = "Bacterial Fitness", cex.main = 0.8)

#the plot
barplot(means2, names.arg=result2$Strain, ylim=c(0,10), xlab = "Category", ylab = 'Value')
bar_centers2 <- barplot(means2, plot = FALSE)
arrows(bar_centers, means2 - sd2, bar_centers2, means2 + sd2, angle = 90, code = 3, length = 0.05)
title(main = "Plant Fitness", cex.main = 0.8)





#plot only PASS and plot by flat


#plant fitness
Pidiq_fitness_pass <-Pidiq_fitness[Pidiq_fitness$PASS.FAIL=="PASS",]
#Pidiq_fitness_pass <-Pidiq_fitness[Pidiq_fitness$PASS.FAIL=="NA",]

quartz()
par(cex.axis=0.6, col.axis="black")
barplot(1/Pidiq_fitness_pass$Arcsine.transformed.data, names.arg=Pidiq_fitness_pass$Inoculum, xlab = "Category", ylab = 'Value', main="Plant Fitness", las=2)

# Define the colors to use for each level of Flat
flat_colours <- c("red", "blue", "green", "purple", "orange","magenta",'darkgreen','darkblue')
# Map the colors to each level of Flat
#barplot of means of inv(Arcsine.transformed(yellow/green)) with sds as arrows
result2 <- aggregate(Pidiq_fitness_pass$Arcsine.transformed.data ~ Inoculum + Flat, data = Pidiq_fitness_pass, FUN = function(x) c(mean = mean(x), sd = sd(x)))
means2 <- c(data.frame(result2)[3][[1]][,1])

bar_colours <- flat_colours[as.numeric(factor(result2$Flat))]

order_id2 <- order(result2$Flat, means2)
means2 <- means2[order_id2]

sd2 <- c(data.frame(result2)[3][[1]][,2])
sd2 <- sd2[order_id2]

result2 <- data.frame(result2)
#the plot
#virulence
quartz()
pdf(file='Plant_fitness_pass_barplot.pdf', width=5, height=5)
result2$Inoculum[result2$Inoculum == "D36E::McDC[35]"] <- "[35]"
short_labels <- substr(result2$Inoculum, 1, 4)
bar_centers2 <- barplot(means2, plot = FALSE)
par(cex.axis=0.8, col.axis="black")
barplot(means2, names.arg=short_labels, ylim=c(0,max(Pidiq_fitness_pass$Arcsine.transformed.data)), xlab = "Category", ylab = 'Value',col = bar_colours, las=2)
arrows(bar_centers2, means2 - sd2, bar_centers2, means2 + sd2, angle = 90, code = 3, length = 0.05)
title(main = "Plant Symptom Severity", cex.main = 0.8)
flatlist <- c(unique(result2$Flat))
mtext(text=flatlist, side=1, line=-15, at=seq(1,20,10), col='black', cex=1)
dev.off()


Pidiq_fitness2 <- Pidiq_fitness
Pidiq_fitness2[,'Arcsine.transformed.data'] <- asin(sqrt(Pidiq_fitness[,'GreenArea']/(Pidiq_fitness[,'YellowedArea'] + Pidiq_fitness[,'GreenArea'])))/1.507
Pidiq_fitness2_pass <-Pidiq_fitness2[Pidiq_fitness2$PASS.FAIL=="PASS",]

result2 <- aggregate(Pidiq_fitness2_pass$Arcsine.transformed.data ~ Inoculum + Flat, data = Pidiq_fitness2_pass, FUN = function(x) c(mean = mean(x), sd = sd(x)))
means2 <- c(data.frame(result2)[3][[1]][,1])

bar_colours <- flat_colours[as.numeric(factor(result2$Flat))]

order_id2 <- order(result2$Flat, means2)
means2 <- means2[order_id2]

sd2 <- c(data.frame(result2)[3][[1]][,2])
sd2 <- sd2[order_id2]

result2 <- data.frame(result2)

quartz()
pdf(file='Plant_fitness_pass_barplot2.pdf', width=5, height=5)
result2$Inoculum[result2$Inoculum == "D36E::McDC[35]"] <- "[35]"
short_labels <- substr(result2$Inoculum, 1, 4)
bar_centers2 <- barplot(means2, plot = FALSE)
par(cex.axis=0.8, col.axis="black")
barplot(means2, names.arg=short_labels, ylim=c(0,max(Pidiq_fitness2_pass$Arcsine.transformed.data)), xlab = "Category", ylab = 'Value',col = bar_colours, las=2)
arrows(bar_centers2, means2 - sd2, bar_centers2, means2 + sd2, angle = 90, code = 3, length = 0.05)
title(main = "Plant Fitness", cex.main = 0.8)
flatlist <- c(unique(result2$Flat))
mtext(text=flatlist, side=1, line=-15, at=seq(1,20,10), col='black', cex=1)
dev.off()


########
#bacterial fitness

Bac_fitness_pass <-GA_fitness[GA_fitness$QC=="PASS",]

quartz()
par(cex.axis=0.6, col.axis="black")
barplot(order(Bac_fitness_pass$log.CFU.cm2), names.arg=Bac_fitness_pass$Inoculum, xlab = "Category", ylab = 'Value', main="Bacterial Fitness", las=2)


# Define the colors to use for each level of Flat
flat_colours <- c("red", "blue", "green", "purple", "orange","magenta",'darkgreen','darkblue')
# Map the colors to each level of Flat
#barplot of means of inv(Arcsine.transformed(yellow/green)) with sds as arrows
result2 <- aggregate(Bac_fitness_pass$log.CFU.cm2 ~ Inoculum + Flat, data = Bac_fitness_pass, FUN = function(x) c(mean = mean(x), sd = sd(x)))
means2 <- c(data.frame(result2)[3][[1]][,1])

bar_colours <- flat_colours[as.numeric(factor(result2$Flat))]

order_id2 <- order(result2$Flat, means2)
means2 <- means2[order_id2]

sd2 <- c(data.frame(result2)[3][[1]][,2])
sd2 <- sd2[order_id2]

result2 <- data.frame(result2)
#the plot
#virulence
quartz()
pdf(file='Bacterial_fitness_pass_barplot.pdf', width=5, height=5)
result2$Inoculum[result2$Inoculum == "D36E::McDC[35]"] <- "[35]"
short_labels <- substr(result2$Inoculum, 1, 4)
bar_centers2 <- barplot(means2, plot = FALSE)
par(cex.axis=0.8, col.axis="black")
barplot(means2, names.arg=short_labels, ylim=c(0,max(Bac_fitness_pass$log.CFU.cm2)), xlab = "Category", ylab = 'Value',col = bar_colours, las=2)
arrows(bar_centers2, means2 - sd2, bar_centers2, means2 + sd2, angle = 90, code = 3, length = 0.05)
title(main = "Bacteria log CFU/cm2", cex.main = 0.8)
flatlist <- c(unique(result2$Flat))
mtext(text=flatlist[1], side=1, line=-15, at=3, col='red', cex=1)
mtext(text=flatlist[2], side=1, line=-15, at=12, col='blue', cex=1)

dev.off()

flatlist[1]





#boxplots instead

# quartz()
# data_test <- data.frame(
#   group1 = c(3, 5, 7, 8, 9, 10, 12),
#   group2 = c(4, 6, 8, 9, 10, 11, 13)
# )


#boxplot(result2$Pidiq_fitness.Arcsine.transformed.data)

#Disease severity (Pidiq)
Pidiq_fitness_pass$Treatment <- paste(Pidiq_fitness_pass$Inoculum,Pidiq_fitness_pass$Flat)
Pidiq_fitness_pass$Treatment <- factor(Pidiq_fitness_pass$Treatment)

quartz()
boxplot(Pidiq_fitness_pass$Arcsine.transformed.data ~ Pidiq_fitness_pass$Treatment, las=2, outline=FALSE)
stripchart(Pidiq_fitness_pass$Arcsine.transformed.data ~Pidiq_fitness_pass$Treatment,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 


#Plant Fitness (Pidiq inverse)
Pidiq_fitness2_pass$Treatment <- paste(Pidiq_fitness2_pass$Inoculum,Pidiq_fitness2_pass$Flat)
Pidiq_fitness2_pass$Treatment <- factor(Pidiq_fitness2_pass$Treatment)

quartz()
boxplot(Pidiq_fitness2_pass$Arcsine.transformed.data ~ Pidiq_fitness2_pass$Treatment, las=2, outline=FALSE)
stripchart(Pidiq_fitness2_pass$Arcsine.transformed.data ~Pidiq_fitness2_pass$Treatment,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 


#Bacterial Fitness (GA)
Bac_fitness_pass$Treatment <- paste(Bac_fitness_pass$Inoculum,Bac_fitness_pass$Flat)
Bac_fitness_pass$Treatment <- factor(Bac_fitness_pass$Treatment)

quartz()
boxplot(Bac_fitness_pass$log.CFU.cm2 ~ Bac_fitness_pass$Treatment, las=2, outline=FALSE)
stripchart(Bac_fitness_pass$log.CFU.cm2 ~Bac_fitness_pass$Treatment,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)   

# values <-Bac_fitness_pass$log.CFU.cm2
# categories1 <- unique(Bac_fitness_pass$Inoculum)
# categories2 <- unique(Bac_fitness_pass$Flat)
# Inocula <- Bac_fitness_pass$Inoculum
# Flatname <- Bac_fitness_pass$Flat
# grouped <- aggregate(values ~ Bac_fitness_pass$Inoculum + Bac_fitness_pass$Flat, data=Bac_fitness_pass)
# non_empty_groups <- grouped$values > 0
# #non_empty_combinations <- with(grouped, Bac_fitness_pass$Inoculum[grouped$value > 0] + Bac_fitness_pass$Flat[grouped$value > 0])
# non_empty_combinations <- with(grouped, paste(Bac_fitness_pass$Inoculum[grouped$value > 0], Bac_fitness_pass$Flat[grouped$value > 0], sep = "_"))
# 
# 
# quartz()
# 
# grouped$value
# 
# boxplot(Bac_fitness_pass$log.CFU.cm2 ~ Bac_fitness_pass$Inoculum + Bac_fitness_pass$Flat, data = Bac_fitness_pass, notch = TRUE,
#         xlab = "Inoculum and Flat Combinations", ylab = "Value", main = "Box Plot Example")
# 
# 
# points(x = rep(1:length(levels(Bac_fitness_pass$Inoculum)) * length(levels(Bac_fitness_pass$Flat)),
#                each = nrow(Bac_fitness_pass)),
#        y = Bac_fitness_pass$log.CFU.cm2,
#        pch = 20, col = "blue")
# 
# # adjust the x-axis labels
# axis(side = 1, at = 1:length(non_empty_combinations), labels = non_empty_combinations)
# 
# 
# 
# 
# boxplot(Bac_fitness_pass$log.CFU.cm2 ~ Bac_fitness_pass$Inoculum + Bac_fitness_pass$Flat, data=Bac_fitness_pass, xlab = "Strain", las=2, outline=TRUE)
# #points(Bac_fitness_pass$log.CFU.cm2 ~ Bac_fitness_pass$Inoculum, Bac_fitness_pass$Inoculum, col = "red", pch = 16)
# #points(Bac_fitness_pass$Inoculum[Bac_fitness_pass$Inoculum=="195"], Bac_fitness_pass$log.CFU.cm2[Bac_fitness_pass$Inoculum=="195"], col = "red", pch = 16)
# points(x = rep(1:length(Bac_fitness_pass$Inoculum), length(Bac_fitness_pass$Flat)),
#        Bac_fitness_pass$log.CFU.cm2,
#        pch = 20, col = "blue")
# View(Bac_fitness_pass)
# 
# 
# # create a sample dataframe
# df <- data.frame(identity = rep(c("A", "B", "C"), each = 10),
#                  rep = rep(1:10, 3),
#                  value = rnorm(30))
# 
# # create a boxplot of the data without plotting
# bp <- boxplot(value ~ identity, data = df, notch = TRUE)
# 
# # plot the data points on top of the boxes
# points(jitter(rep(1:3, each = 10)), df$value, col = "red", pch = 16)
# 
# # add labels and title to the plot
# axis(side = 1, at = 1:3, labels = unique(df$identity))
# xlab <- "Identity"
# ylab <- "Value"
# title(main = "Box Plot Example", xlab = xlab, ylab = ylab)
# legend("topleft", legend = "Data points", pch = 16, col = "red")
# 
# 
# 


#barplots for current experiment 05/18

View(GA_fitness)

Pidiq_fitness


#K017 first
#GA fitness
K017_d3_y <- GA_fitness$log.CFU.cm2[GA_fitness["Flat"]=="K017" & GA_fitness["Days.infected.at.time.of.harvest"]=="3"]
K017_d3_x1 <- GA_fitness$Inoculum[GA_fitness["Flat"]=="K017" & GA_fitness["Days.infected.at.time.of.harvest"]=="3"]
K017_d3_x2 <- GA_fitness$Plant[GA_fitness["Flat"]=="K017" & GA_fitness["Days.infected.at.time.of.harvest"]=="3"]
K017_d3_x3 <- GA_fitness$Treatment..plate[GA_fitness["Flat"]=="K017" & GA_fitness["Days.infected.at.time.of.harvest"]=="3"]

GA_fitness$log.CFU.cm2[GA_fitness["Flat"]=="K017" & GA_fitness["Days.infected.at.time.of.harvest"]=="3" & GA_fitness["Inoculum"]=="plusminus_PtoDC3000::EV_and_D36E::EV"& GA_fitness["Plant"]=="1" & GA_fitness["Treatment..plate."]=="3"]


#K17_d3_y <- aggregate(log.CFU.cm2 ~ Inoculum + Flat + Batch_infection, data = K017_d3, FUN = function(x) c(mean = mean(x), sd = sd(x)))

#K017_d3_x <- group(K017_d3$Inoculum)


#GA_fitness$Treatment <- paste(GA_fitness$Inoculum,Pidiq_fitness2_pass$Flat)

#first with all treatments (plates or columns) separate
quartz()
par(cex.axis=0.5, col.axis="black")
boxplot(K017_d3_y ~ K017_d3_x1 + K017_d3_x2 + K017_d3_x3, las=2, outline=FALSE,xlab=NULL,ylab='logCFU/cm^2', main="Flat K017 GA results at 3 dpi (separate treatments)")
stripchart(K017_d3_y ~K017_d3_x1 + K017_d3_x2 + K017_d3_x3,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)   

#now with all treatments (plates or columns) combined
quartz()
par(cex.axis=0.5, col.axis="black")
boxplot(K017_d3_y ~ K017_d3_x1, las=2, outline=FALSE, main="Flat K017 logCFU/cm2 at 3 dpi (aggregated by strain)")
stripchart(K017_d3_y ~ K017_d3_x1,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)  


#K017 Pidiq
Pidiq_fitness["DPI"] <- as.Date(Pidiq_fitness$Day, format="%m/%d/%Y") - as.Date(Pidiq_fitness$Day.Infected, format ="%m/%d/%Y")


K017_d3_y <- Pidiq_fitness$Arcsine.transformed.data[Pidiq_fitness$Flat=="K017" & Pidiq_fitness$DPI==3]

K017_d3_x1 <- Pidiq_fitness$Inoculum[Pidiq_fitness$Flat=="K017" & Pidiq_fitness$DPI==3]
K017_d3_x2 <- Pidiq_fitness$Plant[Pidiq_fitness$Flat=="K017" & Pidiq_fitness$DPI==3]
K017_d3_x3 <- Pidiq_fitness$Treatment[Pidiq_fitness$Flat=="K017" & Pidiq_fitness$DPI==3]



#first with all treatments (plates or columns) separate
quartz()
par(cex.axis=0.5, col.axis="black")
boxplot(K017_d3_y ~ K017_d3_x1 + K017_d3_x2 + K017_d3_x3, las=2, outline=FALSE,xlab=NULL,ylab='Arcsin(yellow/green)', main="Flat K017 Pidiq results at 3 dpi (separate treatments)")
stripchart(K017_d3_y ~K017_d3_x1 + K017_d3_x2 + K017_d3_x3,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)   

#now with all treatments (plates or columns) combined
quartz()
par(cex.axis=0.5, col.axis="black")
boxplot(K017_d3_y ~ K017_d3_x1, las=2, outline=FALSE, xlab=NULL, ylab='Arcsin(yellow/green)', main="Flat K017 Pidiq results at 3 dpi (aggregated by strain)")
stripchart(K017_d3_y ~ K017_d3_x1,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)  


quartz()
par(cex.axis=0.5, col.axis="black")
boxplot(K017_d3_y ~ K017_d3_x1 + K017_d3_x2, las=2, outline=FALSE, xlab=NULL, ylab='Arcsin(yellow/green)', main="Flat K017 Pidiq results at 3 dpi (aggregated by treatment but not rep(plant))")
stripchart(K017_d3_y ~ K017_d3_x1 + K017_d3_x2,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)  


quartz()
par(cex.axis=0.5, col.axis="black")
boxplot(K017_d3_y ~ K017_d3_x1 + K017_d3_x3, las=2, outline=FALSE, xlab=NULL, ylab='Arcsin(yellow/green)', main="Flat K017 Pidiq results at 3 dpi (aggregated by rep (plant))")
stripchart(K017_d3_y ~ K017_d3_x1 + K017_d3_x3,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)  




#Pidiq_fitness["DPI"] <- as.Date(Pidiq_fitness$Day, format="%m/%d/%Y") - as.Date(Pidiq_fitness$Day.Infected, format ="%m/%d/%Y")

#K018, K019, K020


K018_d3_y <- GA_fitness$log.CFU.cm2[GA_fitness["Flat"]=="K018" & GA_fitness["Days.infected.at.time.of.harvest"]=="3"]
K018_d3_x1 <- GA_fitness$Inoculum[GA_fitness["Flat"]=="K018" & GA_fitness["Days.infected.at.time.of.harvest"]=="3"]
K018_d3_x2 <- GA_fitness$Plant[GA_fitness["Flat"]=="K018" & GA_fitness["Days.infected.at.time.of.harvest"]=="3"]
K018_d3_x3 <- GA_fitness$Treatment..plate[GA_fitness["Flat"]=="K018" & GA_fitness["Days.infected.at.time.of.harvest"]=="3"]
K018_d3_x4 <- GA_fitness$Treatment..flatcol.[GA_fitness["Flat"]=="K018" & GA_fitness["Days.infected.at.time.of.harvest"]=="3"]


names(GA_fitness)

#GA_fitness$log.CFU.cm2[GA_fitness["Flat"]=="K018" & GA_fitness["Days.infected.at.time.of.harvest"]=="3" & GA_fitness["Inoculum"]=="plusminus_PtoDC3000::EV_and_D36E::EV"& GA_fitness["Plant"]=="1" & GA_fitness["Treatment..plate."]=="3"]


#K17_d3_y <- aggregate(log.CFU.cm2 ~ Inoculum + Flat + Batch_infection, data = K017_d3, FUN = function(x) c(mean = mean(x), sd = sd(x)))

#K017_d3_x <- group(K017_d3$Inoculum)


#GA_fitness$Treatment <- paste(GA_fitness$Inoculum,Pidiq_fitness2_pass$Flat)

#first with all treatments (plates or columns) separate
quartz()
par(cex.axis=0.5, col.axis="black")
boxplot(K018_d3_y ~ K018_d3_x1 + K018_d3_x2 + K018_d3_x3 +K018_d3_x4, las=2, outline=FALSE,xlab=NULL,ylab='logCFU/cm^2', main="Flat K018 GA results at 3 dpi (separate treatments)")
stripchart(K018_d3_y ~K018_d3_x1 + K018_d3_x2 + K018_d3_x3 + K018_d3_x4,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)   

#now with all treatments (plates or columns) combined
quartz()
par(cex.axis=0.5, col.axis="black")
boxplot(K018_d3_y ~ K018_d3_x1, las=2, outline=FALSE,xlab=NULL,ylab='logCFU/cm^2', main="Flat K018 GA results at 3 dpi (aggregated by strain)")
stripchart(K018_d3_y ~ K018_d3_x1,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)  

quartz()
par(cex.axis=0.5, col.axis="black")
boxplot(K018_d3_y ~ K018_d3_x1 + K018_d3_x3, las=2, outline=FALSE,xlab=NULL,ylab='logCFU/cm^2', main="Flat K018 GA results at 3 dpi (different plates)")
stripchart(K018_d3_y ~ K018_d3_x1 + K018_d3_x3,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 


quartz()
par(cex.axis=0.5, col.axis="black")
boxplot(K018_d3_y ~ K018_d3_x1 + K018_d3_x4, las=2, outline=FALSE,xlab=NULL,ylab='logCFU/cm^2', main="Flat K018 GA results at 3 dpi (different columns)")
stripchart(K018_d3_y ~ K018_d3_x1 + K018_d3_x4,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 


quartz()
par(cex.axis=0.5, col.axis="black")
boxplot(K018_d3_y ~ K018_d3_x2 + K018_d3_x3, las=2, outline=FALSE,xlab=NULL,ylab='logCFU/cm^2', main="Aggregate by column & plate (treatment)")
stripchart(K018_d3_y ~ K018_d3_x2 + K018_d3_x3,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)            



#K018 Pidiq


K018_d3_y <- Pidiq_fitness$Arcsine.transformed.data[Pidiq_fitness$Flat=="K018" & Pidiq_fitness$DPI==3]
K018_d3_x1 <- Pidiq_fitness$Inoculum[Pidiq_fitness$Flat=="K018" & Pidiq_fitness$DPI==3]
K018_d3_x2 <- Pidiq_fitness$Plant[Pidiq_fitness$Flat=="K018" & Pidiq_fitness$DPI==3]
K018_d3_x3 <- Pidiq_fitness$Treatment[Pidiq_fitness$Flat=="K018" & Pidiq_fitness$DPI==3]

quartz()
# par(cex.axis=0.5, col.axis="black")
# boxplot(K018_d3_y ~ K018_d3_x1 + K018_d3_x2 + K018_d3_x3, las=2, outline=FALSE,xlab=NULL,ylab='Arcsin(yellow/green)', main="Flat K018 Pidiq results at 3 dpi (separate treatments)")
# stripchart(K018_d3_y ~K018_d3_x1 + K018_d3_x2 + K018_d3_x3,              # Data
#            method = "jitter", # Random noise
#            pch = 19,          # Pch symbols
#            col = 4,           # Color of the symbol
#            vertical = TRUE,   # Vertical mode
#            add = TRUE)   

par(cex.axis=0.5, col.axis="black")
boxplot(K018_d3_y ~ K018_d3_x1, las=2, outline=FALSE,xlab=NULL,ylab='Arcsin(yellow/green)', main="Flat K018 Pidiq results at 3 dpi (aggregate by strain)")
stripchart(K018_d3_y ~K018_d3_x1,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)   


quartz()
par(cex.axis=0.5, col.axis="black")
boxplot(K018_d3_y ~ K018_d3_x1 + K018_d3_x2, las=2, outline=FALSE,xlab=NULL,ylab='Arcsin(yellow/green)', main="Flat K018 Pidiq results at 3 dpi (aggregate by strain - treatment)")
stripchart(K018_d3_y ~K018_d3_x1 + K018_d3_x2,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)   

par(cex.axis=0.5, col.axis="black")
boxplot(K018_d3_y ~ K018_d3_x1 + K018_d3_x3, las=2, outline=FALSE,xlab=NULL,ylab='Arcsin(yellow/green)', main="Flat K018 Pidiq results at 3 dpi (aggregate by strain - plant)")
stripchart(K018_d3_y ~K018_d3_x1 + K018_d3_x3,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 

par(cex.axis=0.5, col.axis="black")
boxplot(K018_d3_y ~ K018_d3_x2, las=2, outline=FALSE,xlab=NULL,ylab='Arcsin(yellow/green)', main="Flat K018 Pidiq results at 3 dpi (aggregate by treatment)")
stripchart(K018_d3_y ~K018_d3_x2,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 

par(cex.axis=0.5, col.axis="black")
boxplot(K018_d3_y ~ K018_d3_x2, las=2, outline=FALSE,xlab=NULL,ylab='Arcsin(yellow/green)', main="Flat K018 Pidiq results at 3 dpi (aggregate by plant)")
stripchart(K018_d3_y ~K018_d3_x2,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 



#K019
#GA fitness
K019_d3_y <- GA_fitness$log.CFU.cm2[GA_fitness["Flat"]=="K019" & GA_fitness["Days.infected.at.time.of.harvest"]=="3"]
K019_d3_x1 <- GA_fitness$Inoculum[GA_fitness["Flat"]=="K019" & GA_fitness["Days.infected.at.time.of.harvest"]=="3"]
K019_d3_x2 <- GA_fitness$Plant[GA_fitness["Flat"]=="K019" & GA_fitness["Days.infected.at.time.of.harvest"]=="3"]
K019_d3_x3 <- GA_fitness$Treatment..plate[GA_fitness["Flat"]=="K019" & GA_fitness["Days.infected.at.time.of.harvest"]=="3"]
K019_d3_x4 <- GA_fitness$Treatment..flatcol.[GA_fitness["Flat"]=="K019" & GA_fitness["Days.infected.at.time.of.harvest"]=="3"]

#GA_fitness$log.CFU.cm2[GA_fitness["Flat"]=="K017" & GA_fitness["Days.infected.at.time.of.harvest"]=="3" & GA_fitness["Inoculum"]=="plusminus_PtoDC3000::EV_and_D36E::EV"& GA_fitness["Plant"]=="1" & GA_fitness["Treatment..plate."]=="3"]

quartz()
par(cex.axis=0.5, col.axis="black")
boxplot(K019_d3_y ~ K019_d3_x1 + K019_d3_x2 + K019_d3_x3 +K019_d3_x4, las=2, outline=FALSE,xlab=NULL,ylab='logCFU/cm^2', main="Flat K019 GA results at 3 dpi (separate treatments)")
stripchart(K019_d3_y ~K019_d3_x1 + K019_d3_x2 + K019_d3_x3 + K019_d3_x4,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)   

#now with all treatments (plates or columns) combined
par(cex.axis=0.5, col.axis="black")
boxplot(K019_d3_y ~ K019_d3_x1, las=2, outline=FALSE,xlab=NULL,ylab='logCFU/cm^2', main="Flat K019 GA results at 3 dpi (aggregated by strain)")
stripchart(K019_d3_y ~ K019_d3_x1,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)  

par(cex.axis=0.5, col.axis="black")
boxplot(K019_d3_y ~ K019_d3_x1 + K019_d3_x3, las=2, outline=FALSE,xlab=NULL,ylab='logCFU/cm^2', main="Flat K019 GA results at 3 dpi (different plates)")
stripchart(K019_d3_y ~ K019_d3_x1 + K019_d3_x3,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 


par(cex.axis=0.5, col.axis="black")
boxplot(K019_d3_y ~ K019_d3_x1 + K019_d3_x4, las=2, outline=FALSE,xlab=NULL,ylab='logCFU/cm^2', main="Flat K019 GA results at 3 dpi (different columns)")
stripchart(K019_d3_y ~ K019_d3_x1 + K019_d3_x4,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 


#error here - fix master data frame
# par(cex.axis=0.5, col.axis="black")
# boxplot(K019_d3_y ~ K019_d3_x2 + K019_d3_x3, las=2, outline=FALSE,xlab=NULL,ylab='logCFU/cm^2', main="Aggregate by column & plate (treatment)")
# stripchart(K019_d3_y ~ K019_d3_x2 + K019_d3_x3,              # Data
#            method = "jitter", # Random noise
#            pch = 19,          # Pch symbols
#            col = 4,           # Color of the symbol
#            vertical = TRUE,   # Vertical mode
#            add = TRUE)   


#K019 Pidiq

K019_d3_y <- Pidiq_fitness$Arcsine.transformed.data[Pidiq_fitness$Flat=="K019" & Pidiq_fitness$DPI==3]
K019_d3_x1 <- Pidiq_fitness$Inoculum[Pidiq_fitness$Flat=="K019" & Pidiq_fitness$DPI==3]
K019_d3_x2 <- Pidiq_fitness$Plant[Pidiq_fitness$Flat=="K019" & Pidiq_fitness$DPI==3]
K019_d3_x3 <- Pidiq_fitness$Treatment[Pidiq_fitness$Flat=="K019" & Pidiq_fitness$DPI==3]



quartz()
# par(cex.axis=0.5, col.axis="black")
# boxplot(K019_d3_y ~ K019_d3_x1 + K019_d3_x2 + K019_d3_x3, las=2, outline=FALSE,xlab=NULL,ylab='Arcsin(yellow/green)', main="Flat K019 Pidiq results at 3 dpi (separate treatments)")
# stripchart(K019_d3_y ~K019_d3_x1 + K019_d3_x2 + K019_d3_x3,              # Data
#            method = "jitter", # Random noise
#            pch = 19,          # Pch symbols
#            col = 4,           # Color of the symbol
#            vertical = TRUE,   # Vertical mode
#            add = TRUE)   

par(cex.axis=0.5, col.axis="black")
boxplot(K019_d3_y ~ K019_d3_x1, las=2, outline=FALSE,xlab=NULL,ylab='Arcsin(yellow/green)', main="Flat K019 Pidiq results at 3 dpi (aggregate by strain)")
stripchart(K019_d3_y ~K019_d3_x1,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)   


quartz()
par(cex.axis=0.5, col.axis="black")
boxplot(K019_d3_y ~ K019_d3_x1 + K019_d3_x2, las=2, outline=FALSE,xlab=NULL,ylab='Arcsin(yellow/green)', main="Flat K019 Pidiq results at 3 dpi (aggregate by strain - treatment)")
stripchart(K019_d3_y ~K019_d3_x1 + K019_d3_x2,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)   

par(cex.axis=0.5, col.axis="black")
boxplot(K019_d3_y ~ K019_d3_x1 + K019_d3_x3, las=2, outline=FALSE,xlab=NULL,ylab='Arcsin(yellow/green)', main="Flat K019 Pidiq results at 3 dpi (aggregate by strain - plant)")
stripchart(K019_d3_y ~K019_d3_x1 + K019_d3_x3,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 

par(cex.axis=0.5, col.axis="black")
boxplot(K019_d3_y ~ K019_d3_x2, las=2, outline=FALSE,xlab=NULL,ylab='Arcsin(yellow/green)', main="Flat K019 Pidiq results at 3 dpi (aggregate by treatment)")
stripchart(K019_d3_y ~K019_d3_x2,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 

par(cex.axis=0.5, col.axis="black")
boxplot(K019_d3_y ~ K019_d3_x2, las=2, outline=FALSE,xlab=NULL,ylab='Arcsin(yellow/green)', main="Flat K019 Pidiq results at 3 dpi (aggregate by plant)")
stripchart(K019_d3_y ~K019_d3_x2,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 


#K020

#K019
#GA fitness
K020_d3_y <- GA_fitness$log.CFU.cm2[GA_fitness["Flat"]=="K020" & GA_fitness["Days.infected.at.time.of.harvest"]=="3"]
K020_d3_x1 <- GA_fitness$Inoculum[GA_fitness["Flat"]=="K020" & GA_fitness["Days.infected.at.time.of.harvest"]=="3"]
K020_d3_x2 <- GA_fitness$Plant[GA_fitness["Flat"]=="K020" & GA_fitness["Days.infected.at.time.of.harvest"]=="3"]
K020_d3_x3 <- GA_fitness$Treatment..plate[GA_fitness["Flat"]=="K020" & GA_fitness["Days.infected.at.time.of.harvest"]=="3"]
K020_d3_x4 <- GA_fitness$Treatment..flatcol.[GA_fitness["Flat"]=="K020" & GA_fitness["Days.infected.at.time.of.harvest"]=="3"]

#GA_fitness$log.CFU.cm2[GA_fitness["Flat"]=="K017" & GA_fitness["Days.infected.at.time.of.harvest"]=="3" & GA_fitness["Inoculum"]=="plusminus_PtoDC3000::EV_and_D36E::EV"& GA_fitness["Plant"]=="1" & GA_fitness["Treatment..plate."]=="3"]

quartz()
par(cex.axis=0.5, col.axis="black")
boxplot(K020_d3_y ~ K020_d3_x1 + K020_d3_x2 + K020_d3_x3 +K020_d3_x4, las=2, outline=FALSE,xlab=NULL,ylab='logCFU/cm^2', main="Flat K020 GA results at 3 dpi (separate treatments)")
stripchart(K020_d3_y ~K020_d3_x1 + K020_d3_x2 + K020_d3_x3 + K020_d3_x4,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)   

#now with all treatments (plates or columns) combined
par(cex.axis=0.5, col.axis="black")
boxplot(K020_d3_y ~ K020_d3_x1, las=2, outline=FALSE,xlab=NULL,ylab='logCFU/cm^2', main="Flat K020 GA results at 3 dpi (aggregated by strain)")
stripchart(K020_d3_y ~ K020_d3_x1,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)  

par(cex.axis=0.5, col.axis="black")
boxplot(K020_d3_y ~ K020_d3_x1 + K020_d3_x3, las=2, outline=FALSE,xlab=NULL,ylab='logCFU/cm^2', main="Flat K020 GA results at 3 dpi (different plates)")
stripchart(K020_d3_y ~ K020_d3_x1 + K020_d3_x3,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 


par(cex.axis=0.5, col.axis="black")
boxplot(K020_d3_y ~ K020_d3_x1 + K020_d3_x4, las=2, outline=FALSE,xlab=NULL,ylab='logCFU/cm^2', main="Flat K020 GA results at 3 dpi (different columns)")
stripchart(K020_d3_y ~ K020_d3_x1 + K020_d3_x4,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 


par(cex.axis=0.5, col.axis="black")
boxplot(K020_d3_y ~ K020_d3_x2 + K020_d3_x3, las=2, outline=FALSE,xlab=NULL,ylab='logCFU/cm^2', main="Aggregate by column & plate (treatment)")
stripchart(K020_d3_y ~ K020_d3_x2 + K020_d3_x3,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)   


#K020 Pidiq

K020_d3_y <- Pidiq_fitness$Arcsine.transformed.data[Pidiq_fitness$Flat=="K020" & Pidiq_fitness$DPI==3]
K020_d3_x1 <- Pidiq_fitness$Inoculum[Pidiq_fitness$Flat=="K020" & Pidiq_fitness$DPI==3]
K020_d3_x2 <- Pidiq_fitness$Plant[Pidiq_fitness$Flat=="K020" & Pidiq_fitness$DPI==3]
K020_d3_x3 <- Pidiq_fitness$Treatment[Pidiq_fitness$Flat=="K020" & Pidiq_fitness$DPI==3]



quartz()
# par(cex.axis=0.5, col.axis="black")
# boxplot(K020_d3_y ~ K020_d3_x1 + K020_d3_x2 + K020_d3_x3, las=2, outline=FALSE,xlab=NULL,ylab='Arcsin(yellow/green)', main="Flat K020 Pidiq results at 3 dpi (separate treatments)")
# stripchart(K020_d3_y ~K020_d3_x1 + K020_d3_x2 + K020_d3_x3,              # Data
#            method = "jitter", # Random noise
#            pch = 19,          # Pch symbols
#            col = 4,           # Color of the symbol
#            vertical = TRUE,   # Vertical mode
#            add = TRUE)   

par(cex.axis=0.5, col.axis="black")
boxplot(K020_d3_y ~ K020_d3_x1, las=2, outline=FALSE,xlab=NULL,ylab='Arcsin(yellow/green)', main="Flat K020 Pidiq results at 3 dpi (aggregate by strain)")
stripchart(K020_d3_y ~K020_d3_x1,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)   


quartz()
par(cex.axis=0.5, col.axis="black")
boxplot(K020_d3_y ~ K020_d3_x1 + K020_d3_x2, las=2, outline=FALSE,xlab=NULL,ylab='Arcsin(yellow/green)', main="Flat K020 Pidiq results at 3 dpi (aggregate by strain - treatment)")
stripchart(K020_d3_y ~K020_d3_x1 + K020_d3_x2,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)   

par(cex.axis=0.5, col.axis="black")
boxplot(K020_d3_y ~ K020_d3_x1 + K020_d3_x3, las=2, outline=FALSE,xlab=NULL,ylab='Arcsin(yellow/green)', main="Flat K020 Pidiq results at 3 dpi (aggregate by strain - plant)")
stripchart(K020_d3_y ~K020_d3_x1 + K020_d3_x3,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 

par(cex.axis=0.5, col.axis="black")
boxplot(K020_d3_y ~ K020_d3_x2, las=2, outline=FALSE,xlab=NULL,ylab='Arcsin(yellow/green)', main="Flat K020 Pidiq results at 3 dpi (aggregate by treatment)")
stripchart(K020_d3_y ~K020_d3_x2,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 

par(cex.axis=0.5, col.axis="black")
boxplot(K020_d3_y ~ K020_d3_x2, las=2, outline=FALSE,xlab=NULL,ylab='Arcsin(yellow/green)', main="Flat K020 Pidiq results at 3 dpi (aggregate by plant)")
stripchart(K020_d3_y ~K020_d3_x2,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 






# #K17_d3_y <- aggregate(log.CFU.cm2 ~ Inoculum + Flat + Batch_infection, data = K017_d3, FUN = function(x) c(mean = mean(x), sd = sd(x)))

# #K017_d3_x <- group(K017_d3$Inoculum)


# #GA_fitness$Treatment <- paste(GA_fitness$Inoculum,Pidiq_fitness2_pass$Flat)

# #first with all treatments (plates or columns) separate
# quartz()
# par(cex.axis=0.5, col.axis="black")
# boxplot(K017_d3_y ~ K017_d3_x1 + K017_d3_x2 + K017_d3_x3, las=2, outline=FALSE,xlab=NULL,ylab='logCFU/cm^2', main="Flat K017 GA results at 3 dpi (separate treatments)")
# stripchart(K017_d3_y ~K017_d3_x1 + K017_d3_x2 + K017_d3_x3,              # Data
#            method = "jitter", # Random noise
#            pch = 19,          # Pch symbols
#            col = 4,           # Color of the symbol
#            vertical = TRUE,   # Vertical mode
#            add = TRUE)   

# #now with all treatments (plates or columns) combined
# quartz()
# par(cex.axis=0.5, col.axis="black")
# boxplot(K017_d3_y ~ K017_d3_x1, las=2, outline=FALSE, main="Flat K017 logCFU/cm2 at 3 dpi (aggregated by strain)")
# stripchart(K017_d3_y ~ K017_d3_x1,              # Data
#            method = "jitter", # Random noise
#            pch = 19,          # Pch symbols
#            col = 4,           # Color of the symbol
#            vertical = TRUE,   # Vertical mode
#            add = TRUE)  
















#fix - add unrepresented treatments and address NaNs in "empty" to re-enable zint


#network representation using Jaccard dissimilarity
#this xlsx file contains the presence/absence matrix for strains in the metaclone populations (analogous to alleles in the metaclone populations)
presabs_data <- read.xlsx('/Users/keiranmaskell/Downloads/For_Keiran/Validation_TopBottom_grid.xlsx', sheetIndex=2)
presabs_df <- data.frame(presabs_data)

View(presabs_df)

#Example df for bacterial fitness - used for building - template for plot without having its value plotted
bac_fitness_data <- read.xlsx('/Users/keiranmaskell/Desktop/P_syring/fitness_landscaper/datasheets/Bacterial_fitness_demo.xlsx', sheetIndex=1)
bac_fitness_df <- data.frame(bac_fitness_data)
#View(bac_fitness_df)
#Make similar to the above


#now need an index vector, and multiple fitness vectors
Fitness_indices <- data.frame("Metapop" = c('PtoDC3000::EV','D36E::McDC[35]','D36E::EV','245','272','286','290','154','21','6','195','255','220'))
row.names(Fitness_indices) <- Fitness_indices[,1]

#need Batch_infection to discriminate between groups (duplicates)

#pass and fail
Bac_result <- aggregate(log.CFU.cm2 ~ Inoculum + Flat + Batch_infection, data = GA_fitness, FUN = function(x) c(mean = mean(x), sd = sd(x)))

Plant_result <- aggregate(Pidiq_fitness$Arcsine.transformed.data ~ Inoculum + Flat + Batch_Infection, data = Pidiq_fitness, FUN = function(x) c(mean = mean(x), sd = sd(x)))

#pass only
Bac_result <- aggregate(log.CFU.cm2 ~ Inoculum + Flat + Batch_infection, data = Bac_fitness_pass, FUN = function(x) c(mean = mean(x), sd = sd(x)))

Plant_result <- aggregate(Pidiq_fitness_pass$Arcsine.transformed.data ~ Inoculum + Flat + Batch_Infection, data = Pidiq_fitness_pass, FUN = function(x) c(mean = mean(x), sd = sd(x)))



#
#bec <- c(unique(Bac_result$Inoculum))
#bac_fitness_df[,1][bac_fitness_df[,1] %in% bec]
#


bac_group <- interaction(Bac_result$Flat, Bac_result$Batch_infection)
grouped_vals <- split(Bac_result, bac_group)
#remove empty groups
df_rows <- sapply(grouped_vals, nrow)
non_empty_dfs <- grouped_vals[df_rows >0]

plant_group <- interaction(Plant_result$Flat, Plant_result$Batch_Infection)
grouped_vals2 <- split(Plant_result, plant_group)
#remove empty groups
df_rows2 <- sapply(grouped_vals2, nrow)
non_empty_dfs2 <- grouped_vals2[df_rows2 >0]

#first_vals_bacfit <- split(result, duplicated(result$Inoculum))$'FALSE'
#other_vals_bacfit <- split(result, duplicated(result$Inoculum))$'TRUE'

#grouped_rows <- split(result, result$Inoculum)
#grouped_rows

#length(duplicated(result$Inoculum))
#length(result$Inoculum)


#merge(data.frame(non_empty_dfs$K010.1), Fitness_indices, by = "row.names", all = TRUE)

empty <- c()
for(i in 1:length(non_empty_dfs)){
  #print(i)
  row.names(non_empty_dfs[[i]]) <- non_empty_dfs[[i]]$Inoculum
  matched_df <- merge(data.frame(non_empty_dfs[[i]]), Fitness_indices, by = "row.names", all = TRUE)
  empty <- cbind(empty,matched_df$log.CFU.cm2[,'mean'])
}
row.names(empty) <- row.names(Fitness_indices)
empty

View(matched_df)

#merge(data.frame(non_empty_dfs2$K010.1), Fitness_indices, by = "row.names", all = TRUE)

#pass and fail
empty2 <- c()
for(i in 1:length(non_empty_dfs2)){
  #print(i)
  row.names(non_empty_dfs2[[i]]) <- non_empty_dfs2[[i]]$Inoculum
  matched_df2 <- merge(data.frame(non_empty_dfs2[[i]]), Fitness_indices, by = "row.names", all = TRUE)
  empty2 <- cbind(empty2,matched_df2$Pidiq_fitness.Arcsine.transformed.data[,'mean'])
}
row.names(empty2) <- row.names(Fitness_indices)
empty2


#pass only
empty2 <- c()
for(i in 1:length(non_empty_dfs2)){
  #print(i)
  row.names(non_empty_dfs2[[i]]) <- non_empty_dfs2[[i]]$Inoculum
  matched_df2 <- merge(data.frame(non_empty_dfs2[[i]]), Fitness_indices, by = "row.names", all = TRUE)
  empty2 <- cbind(empty2,matched_df2$Pidiq_fitness_pass.Arcsine.transformed.data[,'mean'])
}
row.names(empty2) <- row.names(Fitness_indices)
empty2




#matched_df <- merge(non_empty_dfs$K009.1, Fitness_indices, by = "row.names", all = TRUE)
#row.names(non_empty_dfs$K009.1) <- non_empty_dfs$K009.1$Inoculum


#empty[,1][complete.cases(empty[,1])]
#data.frame()

#for(i in empty[1:nrow(empty),1]){
#  print(i)
#}

#template

library(rgl)
library(htmlwidgets)
rgl.init()
open3d()
# Create sample data with different z-coordinates
x <- c(1, 2, 3, 4, 5)
y <- c(1, 2, 3, 4, 5)
z1 <- c(1, 2, 3, 4, 5)
z2 <- c(2, 3, 4)
z3 <- c(3, 4, 5, 6, 7, 8)

# Pad the shorter vectors with NA
n <- length(x)
#x1 <- c(x, rep(NA, n - length(x1)))
#y1 <- c(y1, rep(NA, n - length(y1)))
z1 <- c(z1, rep(NA, n - length(z1)))

#x2 <- c(x2, rep(NA, n - length(x2)))
#y2 <- c(y2, rep(NA, n - length(y2)))
z2 <- c(z2, rep(NA, n - length(z2)))

#x3 <- c(x3, rep(NA, n - length(x3)))
#y3 <- c(y3, rep(NA, n - length(y3)))
z3 <- c(z3, rep(NA, n - length(z3)))

# Create a 3D scatterplot for each set of (x, y, z) coordinates
plot1 <- plot3d(x, y, z1, col = "black")
points3d(x, y, z2, col = "red")
points3d(x, y, z3, col = "blue")
scene1 <- scene3d(plot1)
rglwidget(scene1)


##
#multiple surfaces template
# Generate sample data for surfaces
x <- seq(-10, 10, length.out = 100)
y <- seq(-10, 10, length.out = 100)
z1 <- outer(x, y, function(x, y) sin(sqrt(x^2 + y^2))/sqrt(x^2 + y^2))
z2 <- outer(x, y, function(x, y) sin(sqrt((x-2)^2 + y^2))/sqrt((x-2)^2 + y^2))
z3 <- outer(x, y, function(x, y) sin(sqrt((x+2)^2 + y^2))/sqrt((x+2)^2 + y^2))

# Create rgl plot window
open3d()

# Loop through surfaces and add them to the plot
for (z in list(z1, z2, z3)) {
  surface3d(x, y, z, col = rainbow(10))
}

##


#get zeros for empty cells
presabs_df[is.na(presabs_df)] <- 0
#subset to trim off empty groups
presabs_df <- subset(presabs_df[1:13,])
#make 
rownames(presabs_df) <- presabs_df[,1]
#trim off the first column, containing only the group names
presabs_df <- subset(presabs_df[,2:length(presabs_df)])

# Remove empty rows
#presabs_df <- presabs_df[complete.cases(presabs_df), ]

library(vegan)
jac_dis <- vegdist(presabs_df, method='jaccard')

View(as.matrix(jac_dis))
#jaccard_dissimilarity <- as.dist(matrix(c(0, 0.2, 0.6, 0.8, 0.3, 0.1, 0.7, 0.9, 0.5, 0.4, 0.2, 0, 0.6, 0.8, 0.4, 0.3, 0.7, 0.9, 0.6, 0.4, 0.6, 0.8, 0.5, 0.6, 0), nrow = 5))


library(igraph)

# Convert the Jaccard dissimilarity distance matrix to a graph
#jaccard_graph <- graph_from_adjacency_matrix(as.matrix(jaccard_dissimilarity), mode = "undirected", weighted = TRUE)
jaccard_graph <- graph_from_adjacency_matrix(as.matrix(jac_dis), mode = "undirected", weighted = TRUE)
#View(jaccard_graph[1])
# Set the vertex names as row names of the distance matrix
#V(jaccard_graph)$name <- rownames(jaccard_dissimilarity)
V(jaccard_graph)$name <- rownames(as.matrix(jac_dis))
short_labels <- substr(V(jaccard_graph)$name, 1, 5)
V(jaccard_graph)$label <- short_labels
V(jaccard_graph)$label[2] <- "[35]"
V(jaccard_graph)$label[3] <- "36EV"

jav_dis_inverse <- ((1/jac_dis)/36)

jaccard_graph_inverse <- graph_from_adjacency_matrix(as.matrix(jav_dis_inverse), mode = "undirected", weighted = TRUE)
V(jaccard_graph_inverse)$name <- rownames(as.matrix(jav_dis_inverse))
short_labels <- substr(V(jaccard_graph_inverse)$name, 1, 5)
V(jaccard_graph_inverse)$label <- short_labels
V(jaccard_graph_inverse)$label[2] <- "[35]"
V(jaccard_graph_inverse)$label[3] <- "36EV"

# Customize the graph layout
layout <- layout_with_fr(jaccard_graph)
layout <- layout_with_fr(jaccard_graph, weights = E(jaccard_graph)$weight, maxiter =1000)
layout2 <- layout_with_fr(jaccard_graph_inverse)
layout2 <- layout_with_fr(jaccard_graph_inverse, weights = E(jaccard_graph_inverse)$weight, maxiter =10000)


# Plot the graph
x11()
plot(jaccard_graph, layout = layout, vertex.size = 20,
     vertex.label.color = "black",
     vertex.label.cex = 0.5,
     vertex.label = V(jaccard_graph)$label,
     vertex.label.dist=0.3,
     edge.width = E(jaccard_graph)$weight*5,
     edge.arrow.size = 0.3)
pdf(file='jaccardnetwork.pdf', width=5, height=5)
plot(jaccard_graph, layout = layout, vertex.size = 20,
     vertex.label.color = "black",
     vertex.label.cex = 0.5,
     vertex.label = V(jaccard_graph)$label,
     vertex.label.dist=0.3,
     edge.width = E(jaccard_graph)$weight*5,
     edge.arrow.size = 0.3)
dev.off()

# Plot the graph
x11()
plot(jaccard_graph_inverse, layout = layout2, vertex.size = 20,
     vertex.label.color = "black",
     vertex.label.cex = 0.5,
     vertex.label = V(jaccard_graph)$label,
     vertex.label.dist=0.3,
     edge.width = E(jaccard_graph)$weight*5,
     edge.arrow.size = 0.3)
pdf(file='jaccardnetwork_inverse.pdf', width=5, height=5)
plot(jaccard_graph_inverse, layout = layout2, vertex.size = 20,
     vertex.label.color = "black",
     vertex.label.cex = 0.5,
     vertex.label = V(jaccard_graph)$label,
     vertex.label.dist=0.3,
     edge.width = E(jaccard_graph)$weight*5,
     edge.arrow.size = 0.3)
dev.off()


#library(pdftools)
#library(png)
## Load the image file
#pdf_file <- 'jaccardnetwork.pdf'
#png_file <- 'jaccardnetwork.png'
#system(paste('sips -s format png', pdf_file, '--out', png_file))
##pdf_convert(pdf_file, format = 'png', pages = 1, dpi = 300, filenames = png_file)
#img <- readPNG(png_file)
#
#
#node_df <- data.frame(ID = V(jaccard_graph)$name, 
#                      X_cord = layout[,1], 
#                      Y_cord = layout[,2], 
#                      Z_cord = bac_fitness_df[,2])

node_df <- data.frame(ID = V(jaccard_graph_inverse)$name, 
                      X_cord = layout2[,1], 
                      Y_cord = layout2[,2], 
                      Z_cord = bac_fitness_df[,2])

#edge_df <- get.edgelist(jaccard_graph)

# Calculate distances between points
dist_mat <- as.matrix(dist(node_df[,c("X_cord", "Y_cord")]))
max_dist <- max(dist_mat)



rgl.init()
open3d(windowRect=c(50,50,800,800))
# Create a 3D scatter plot of the network
plot3d(node_df$X_cord, node_df$Y_cord, node_df$Z_cord, rep(0, nrow(node_df)), type = "n")
#points3d(node_df$X_cord, node_df$Y_cord, node_df$Z_cord, col = "blue", size = 5)
for(i in 1:ncol(empty)){
  #print(i)
  #print(empty[,i])
  points3d(node_df$X_cord, node_df$Y_cord, empty[,i], col = "blue", size = 5)
}
#points3d(node_df$X_cord, node_df$Y_cord, rep(0, nrow(node_df)), col = "red", size = 5)
for(i in 1:ncol(empty2)){
  points3d(node_df$X_cord, node_df$Y_cord, 30*empty2[,i], col = "red", size = 5)
}

# Loop over each pair of points and draw a line with width proportional to distance
for(i in 1:(nrow(node_df)-1)){
  for(j in (i+1):nrow(node_df)){
    dist_ij <- dist_mat[i,j]
    norm_dist_ij <- dist_ij/max_dist # Normalize distance
    line_width <- 1 + 4*norm_dist_ij # Scale the line width from 1 to 10 based on normalized distance
    segments3d(c(node_df$X_cord[i],node_df$X_cord[j]), c(node_df$Y_cord[i],node_df$Y_cord[j]), c(0,0),
               col = "lightblue", lwd = line_width)
  }
}

points3d(node_df$X_cord, node_df$Y_cord,
         rep(0, nrow(node_df)),
         col = "red", size = 10)

text3d(node_df$X_cord, node_df$Y_cord, rep(0, nrow(node_df)),
       texts = node_df$ID, cex = 1.2, adj = c(1, 1), bg= "transparent")




Plantfitness_scaler <- 10

Zcordtest <- rowMeans(empty, na.rm=TRUE)
Zcordplant <- rowMeans(Plantfitness_scaler*empty2, na.rm=TRUE)

Zcordtest[is.nan(Zcordtest)]


flat_colours <- c("magenta", "blue", "green", "purple", "gold","dodgerblue","darkred")

library(rgl)
rgl.init()
#next version
open3d(windowRect=c(50,50,1000,1000))
# Create a 3D scatter plot of the network
plot1 <- plot3d(node_df$X_cord, node_df$Y_cord, Zcordplant, rep(0, nrow(node_df)),
       xlab = "nodespace x", ylab='nodespace y', zlab='relative fitness (GA or Pidiq)', type = "n")
#points3d(node_df$X_cord, node_df$Y_cord, node_df$Z_cord, col = "blue", size = 8)
points3d(node_df$X_cord, node_df$Y_cord, rep(0.0, nrow(node_df)), col = "red", size = 10)

#GA points
for(i in 1:ncol(empty)){
  #print(i)
  #print(empty[,i])
  points3d(node_df$X_cord, node_df$Y_cord, empty[,i], col = flat_colours[i], size = 5)
}


#Pidiq point
for(i in 1:ncol(empty2)){
  points3d(node_df$X_cord, node_df$Y_cord, Plantfitness_scaler*empty2[,i], col = flat_colours[i], pch=3, size=5)
}


#GA network
# Loop over each pair of points and draw a line with width proportional to distance
for(i in 1:(nrow(node_df)-1)){
  for(j in (i+1):nrow(node_df)){
    dist_ij <- dist_mat[i,j]
    norm_dist_ij <- dist_ij/max_dist # Normalize distance
    line_width <- 1 + 0.5*norm_dist_ij # Scale the line width from 1 to 10 based on normalized distance
    segments3d(c(node_df$X_cord[i],node_df$X_cord[j]), c(node_df$Y_cord[i],node_df$Y_cord[j]), c(Zcordtest[i],Zcordtest[j]),
               col = "darkblue", lwd = line_width)
  }
}

#Pidiq network
# Loop over each pair of points and draw a line with width proportional to distance
for(i in 1:(nrow(node_df)-1)){
  for(j in (i+1):nrow(node_df)){
    dist_ij <- dist_mat[i,j]
    norm_dist_ij <- dist_ij/max_dist # Normalize distance
    line_width <- 1 + 0.5*norm_dist_ij # Scale the line width from 1 to 10 based on normalized distance
    segments3d(c(node_df$X_cord[i],node_df$X_cord[j]), c(node_df$Y_cord[i],node_df$Y_cord[j]), c(Zcordplant[i],Zcordplant[j]),
               col = "gold", lwd = line_width)
  }
}


text3d(node_df$X_cord, node_df$Y_cord, Zcordtest,
       texts = node_df$ID, cex = 0.5, adj= c(1,1), bg = "transparent")

text3d(node_df$X_cord, node_df$Y_cord, Zcordplant,
       texts = node_df$ID, cex = 0.5, adj= c(1,1), bg = "transparent")

text3d(node_df$X_cord, node_df$Y_cord, rep(-0.8, nrow(node_df)),
        texts = node_df$ID, cex = 1.2, adj= c(1,1), bg = "transparent")



#xyz <- matrix(c(node_df$X_cord, node_df$Y_cord, node_df$Z_cord),nrow = 13,ncol=3)
#x <- matrix(node_df$X_cord,nrow = 13,ncol=1)
#y <- matrix(node_df$Y_cord,nrow = 13,ncol=1)
#z <- matrix(node_df$Z_cord,nrow = 13,ncol=1)

#surface3d(matrix(x,y,z,nrow=13,ncol=3), alpha=0.3)

#10 * (1:nrow(z)) 



#####



library(akima)

#interpolate a bacterial fitness surface
# Generate a regular grid based on the range of X and Y coordinates
xseq <- seq(min(node_df$X_cord), max(node_df$X_cord), length.out = 70)
yseq <- seq(min(node_df$Y_cord), max(node_df$Y_cord), length.out = 70)
grid <- expand.grid(x = xseq, y = yseq)

# Interpolate the Z values for each point in the grid
zint <- interp(x = node_df$X_cord, y = node_df$Y_cord, z = Zcordtest, 
               xo = xseq, yo = yseq, linear = FALSE, extrap=TRUE,
               jitter = FALSE, jitter.iter=6, jitter.random =TRUE)$z

#zint <- interp(x = node_df$X_cord, y = node_df$Y_cord, z = node_df$Z_cord, 
 #             linear = FALSE, extrap=FALSE,
 #              jitter = FALSE, jitter.iter=6, jitter.random =TRUE)$z

#akima.spl <- with(node_df, interp(x = node_df$X_cord, y = node_df$Y_cord, z = node_df$Z_cord,nx=70, ny=70, linear=FALSE))$z
#akimat <- matrix(akima.spl, ncol=length(xseq), byrow =TRUE)
# Reshape the interpolated Z values into a matrix
zmat <- matrix(zint, ncol = length(xseq), byrow = TRUE)

#surface3d(x = xseq, y = yseq, z = zmat, col='purple', alpha = 0.6)


#surface3d(x =xseq, y=yseq, z=akimat, col='green', alpha = 0.3)

#interpolate a plant fitness surface
zint2 <- interp(x = node_df$X_cord, y = node_df$Y_cord, z = Zcordplant, 
               xo = xseq, yo = yseq, linear = FALSE, extrap=TRUE,
               jitter = FALSE, jitter.iter=6, jitter.random =TRUE)$z
zmat2 <- matrix(zint2, ncol = length(xseq), byrow = TRUE)

col_list <- c('purple','green')
zlist <- list(zmat,zmat2)
for (z in 1:length(zlist)) {
  surface3d(x = xseq, y = yseq, zlist[[z]], col = col_list[z], alpha=0.6)
}
#surface3d(x = xseq, y = yseq, z = zmat, col='purple', alpha = 0.6)
#surface3d(x = xseq, y = yseq, z = zmat2, col='green', alpha = 0.6)




for(i in 1:(nrow(node_df)-1)){
  for(j in (i+1):nrow(node_df)){
    dist_ij <- dist_mat[i,j]
    norm_dist_ij <- dist_ij/max_dist # Normalize distance
    line_width <- 0 + 0.1*log(500*norm_dist_ij) # Scale the line width from 1 to 10 based on normalized distance
    segments3d(c(node_df$X_cord[i],node_df$X_cord[j]), c(node_df$Y_cord[i],node_df$Y_cord[j]), c(0,0),
               col = "darkorange", lwd = line_width)
  }
}

for(i in 1:(nrow(node_df))){
  line_width <- 1 
  segments3d(c(node_df$X_cord[i],node_df$X_cord[i]), c(node_df$Y_cord[i],node_df$Y_cord[i]), c(Zcordtest[i],4.6),
               col = "lightgray", lwd = line_width)
}

#plant version
for(i in 1:(nrow(node_df))){
  line_width <- 1 
  segments3d(c(node_df$X_cord[i],node_df$X_cord[i]), c(node_df$Y_cord[i],node_df$Y_cord[i]), c(Zcordplant[i],5.5),
             col = "lightgray", lwd = line_width)
}

#writeWebGL("DC36E_top8_bottom8_fitLS.html")
scene1 <- scene3d(plot1)
rglwidget(scene1)

library(htmlwidgets)

widget <- rglwidget()
saveWidget(widget, '0505_coupledfitness_DC36E_top8_bottom8_fitLS.html')



#should filter by PASS / FAIL criteria
#and play with interpolation parameters
#also need better barplots
#next is contour plots 








# nodedfordrd <- node_df[order(node_df$X_cord),]
# secnodedfordfd<- nodedfordrd[order(nodedfordrd$Y_cord),]
# 
# 
# secnodedfordfd<- secnodedfordfd[order(secnodedfordfd$X_cord,secnodedfordfd$Y_cord,Zcordplant),]
# 
# contour(secnodedfordfd$X_cord, secnodedfordfd$Y_cord,Zcordplant)

# renv::install("reshape2")
# library(reshape2)



# Zvals <- as.numeric(Zcordplant)
# 
# secnodedfordfd$Z_cord <- Zvals
# 
# Z_matrix <- acast(secnodedfordfd, Y_cord ~ X_cord, value.var = "Z_cord")


# contour(seq(min(secnodedfordfd$X_cord), max(secnodedfordfd$X_cord), length.out = ncol(Z_matrix)),
#         seq(min(secnodedfordfd$Y_cord), max(secnodedfordfd$Y_cord), length.out = nrow(Z_matrix)),
#         Z_matrix,
#         main = "Contour Plot",
#         xlab = "X_cord",
#         ylab = "Y_cord")
# 
# renv::install("fields")
# library(fields)
# 
# zmin <- min(Z_matrix, na.rm = TRUE)
# zmax <- max(Z_matrix, na.rm = TRUE)
# 
# contour(seq(min(secnodedfordfd$X_cord), max(secnodedfordfd$X_cord), length.out = ncol(Z_matrix)),
#         seq(min(secnodedfordfd$Y_cord), max(secnodedfordfd$Y_cord), length.out = nrow(Z_matrix)),
#         Z_matrix,
#         main = "Contour Plot",
#         xlab = "X_cord",
#         ylab = "Y_cord",
#         zlim = c(zmin, zmax))
# 
# 
# dev.new(width = 10, height = 10)
# contour(seq(min(secnodedfordfd$X_cord), max(secnodedfordfd$X_cord), length.out = ncol(Z_matrix)),
#               seq(min(secnodedfordfd$Y_cord), max(secnodedfordfd$Y_cord), length.out = nrow(Z_matrix)),
#               Z_matrix,
#               main = "Contour Plot",
#               xlab = "X_cord",
#               ylab = "Y_cord",
#               zlim = c(zmin, zmax))
# 
# image.plot(Z_matrix,
#            x = seq(min(secnodedfordfd$X_cord), max(secnodedfordfd$X_cord), length.out = ncol(Z_matrix)),
#            y = seq(min(secnodedfordfd$Y_cord), max(secnodedfordfd$Y_cord), length.out = nrow(Z_matrix)),
#            col = colorRampPalette(c("white", "blue"))(100),
#            zlim = c(min(Z_matrix), max(Z_matrix)),
#            main = "Contour Plot",
#            xlab = "X_cord",
#            ylab = "Y_cord",
#            add.colorbar = TRUE)
# 
# x <- 1:10
# y <- x^2
# plot(x, y)



my_palette <- colorRampPalette(c("purple", "green"))


# extract X and Y coordinates from nodedf
x <- node_df$X_cord
y <- node_df$Y_cord

# normalize X and Y coordinates
#x_norm <- scale(x, center=TRUE, scale=TRUE)
#y_norm <- scale(y, center=TRUE, scale=TRUE)

scale_values <- function(x){(x-min(x))/(max(x)-min(x))}
x_norm <- scale_values(x)
y_norm <- scale_values(y)

quartz()

#plant
filled.contour(zmat2, color.palette = my_palette, key.title="Relative Fitness", main="Plant Fitness Landscape", xlim = c(-0.1, 1.1), ylim = c(-0.1,1.1), plot.axes = {
  axis(1)
  axis(2)
  contour(zmat2, add = TRUE, lwd = 2)
  points(x_norm, y_norm, col='orange',pch=16,cex=4)
  text(x_norm, y_norm, labels=node_df$ID,col='black',font=2,cex=1.0)
}
)
quartz.save("Plant_fitness_contour_0505.jpg", type = "jpg")
dev.off()

quartz()
#open3d(windowRect=c(50,50,800,800))

#bacteria

filled.contour(zmat, color.palette = my_palette, key.title="Relative Fitness", main="Bacterial Fitness Landscape", xlim = c(-0.1, 1.1), ylim = c(-0.1,1.1), plot.axes = {
  axis(1)
  axis(2)
  contour(zmat, add = TRUE, lwd = 2)
  points(x_norm, y_norm, col='orange',pch=16,cex=4)
  text(x_norm, y_norm, labels=node_df$ID,col='black',font=2,cex=1.0)
}
)
quartz.save("Bac_fitness_contour0505.jpg", type = "jpg")
dev.off()

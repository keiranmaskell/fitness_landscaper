#function_repo

data_loader <- function(bac_in_fp, plant_in_fp, bac_out_fp, plant_out_fp){

    GA_master <- read.xlsx(bac_in_fp,sheetIndex = 1)
    plant_fitness_master <- read.xlsx(plant_in_fp,sheetIndex = 1)

    write.csv(GA_master, bac_out_fp, row.names=FALSE)
    write.csv(plant_fitness_master, plant_out_fp, row.names=FALSE)

    GA_master <- read.csv(bac_out_fp)
    plant_fitness_master <- read.csv(plant_out_fp)

    GA_fitness <- data.frame(GA_master)
    Pidiq_fitness <- data.frame(plant_fitness_master)

    return(list(GA_fitness, Pidiq_fitness))
}


    #add log.CFU.cm2 column
    #GA_fitness <- GA_fitness[,'Colony.count']*10^(as.numeric(GA_fitness[,'Diln']))
    #bec <- log((as.numeric(GA_fitness[,'Colony.count'])*(1/as.numeric(GA_fitness[,'Diln']))*80)/1)


    # GA_fitness <- data.frame(GA_fitness[,1:11], 'log.CFU.cm2'=
    #                      ifelse(as.numeric(GA_fitness[,'Diln'])==0, log10((as.numeric(GA_fitness[,'Colony.count'])*80)/1)
    #                                                          ,log10((as.numeric(GA_fitness[,'Colony.count'])*(1/as.numeric(10^(-1)*GA_fitness[,'Diln']))*80)/1)),
    #                                                          GA_fitness[,12:ncol(GA_fitness)])


    # GA_fitness[,'Colony.count'] <- as.numeric(GA_fitness[,'Colony.count'])
    # GA_fitness[,'Diln'] <- as.numeric(GA_fitness[,'Diln'])


    # #GA_fitness <- data.frame(GA_fitness[,1:15],'log.CFU.cm2'=log10(((GA_fitness[,'Colony.count']) * (1/(10^((-1)*GA_fitness[,'Diln']))) * (80))/1))
    # GA_fitness <- data.frame(GA_fitness,'log.CFU.cm2'=log10(((GA_fitness[,'Colony.count']) * (1/(10^((-1)*GA_fitness[,'Diln']))) * (80))/1))
    # GA_fitness


prepare_GA_data <- function(GA_df){

#to do: 
# - group using GA dates
# - logcfu type to discriminate between log equations for different core sizes or weights, and bead beating liquid volumes

#add log.CFU.cm2 column
#GA_fitness <- GA_fitness[,'Colony.count']*10^(as.numeric(GA_fitness[,'Diln']))
#bec <- log((as.numeric(GA_fitness[,'Colony.count'])*(1/as.numeric(GA_fitness[,'Diln']))*80)/1)


#why is this broken?
# GA_fitness1 <- data.frame(GA_df[,1:ncol(GA_df)], 'log.CFU.cm2'=
#                          ifelse(as.numeric(GA_df[,'Diln'])==0, log10((as.numeric(GA_df[,'Colony.count'])*80)/1)
#                                                              ,log10((as.numeric(GA_df[,'Colony.count'])*(1/as.numeric(10^(-1)*GA_df[,'Diln']))*80)/1)))


GA_df[,'Colony.count'] <- as.numeric(GA_df[,'Colony.count'])
GA_df[,'Diln'] <- as.numeric(GA_df[,'Diln'])

GA_fitness <- data.frame(GA_df[,1:ncol(GA_df)],'log.CFU.cm2'=log10(((GA_df[,'Colony.count']) * (1/(10^((-1)*GA_df[,'Diln']))) * (80))/1))

#GA_fitness$log.CFU.cm2



#same
#GA_fitness <- data.frame(GA_fitness,'log.CFU.cm2'=log10(((GA_fitness[,'Colony.count']) * (1/(10^((-1)*GA_fitness[,'Diln']))) * (80))/1))



#clean up NAs and weird values created by taking ln(0)
#GA_fitness_wNAs <- GA_fitness
GA_fitness <- subset(GA_fitness, GA_fitness[,'log.CFU.cm2']!='-Inf')
GA_fitness <- subset(GA_fitness, GA_fitness[,'log.CFU.cm2']!='NaN')
GA_fitness <- GA_fitness[complete.cases(GA_fitness[,c("Diln", "Colony.count","log.CFU.cm2")]),]
row.names(GA_fitness) <- 1:nrow(GA_fitness)
#GA_fitness[,'log.CFU.cm2']
#View(GA_fitness_wNAs)
#View(GA_fitness)

return(GA_fitness)

}

prepare_Pidiq_data <- function(Pidiq_df){

    Pidiq_fitness <- Pidiq_df

    Pidiq_fitness[,'Arcsine.transformed.data'] <- asin(sqrt(Pidiq_fitness[,'YellowedArea']/(Pidiq_fitness[,'YellowedArea'] + Pidiq_fitness[,'GreenArea'])))/1.507

    return(Pidiq_fitness)
}
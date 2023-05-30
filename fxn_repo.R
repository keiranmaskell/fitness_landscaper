#function_repo

# data_loader <- function(bac_in_fp, plant_in_fp, bac_out_fp, plant_out_fp){

#     GA_master <- read.xlsx(bac_in_fp,sheetIndex = 1)
#     plant_fitness_master <- read.xlsx(plant_in_fp,sheetIndex = 1)

#     write.csv(GA_master, bac_out_fp, row.names=FALSE)
#     write.csv(plant_fitness_master, plant_out_fp, row.names=FALSE)

#     GA_master <- read.csv(bac_out_fp)
#     plant_fitness_master <- read.csv(plant_out_fp)

#     GA_fitness <- data.frame(GA_master)
#     Pidiq_fitness <- data.frame(plant_fitness_master)

#     return(list(GA_fitness, Pidiq_fitness))


# }


data_loader <- function(in_fp, out_fp, sheetnum){

    master <- read.xlsx(in_fp,sheetIndex = sheetnum)
    #plant_fitness_master <- read.xlsx(plant_in_fp,sheetIndex = 1)

    write.csv(master, out_fp, row.names=FALSE)
    #write.csv(plant_fitness_master, plant_out_fp, row.names=FALSE)

    master <- read.csv(out_fp)
    #plant_fitness_master <- read.csv(plant_out_fp)

    master <- data.frame(master)
    #Pidiq_fitness <- data.frame(plant_fitness_master)

    return(master)


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
GA_df[,'core.number'] <- as.numeric(GA_df[,'Diln'])

library(stringr)

GA_df[,'core.size'] <- as.numeric(GA_df[,'Diln'])

#GA_fitness_test <- GA_fitness
#GA_fitness <- GA_fitness_test
for(i in 1:nrow(GA_df)){
    GA_df[i,'core.size'] <- as.numeric(str_extract_all(GA_df[i,'core.size'], "\\d+"))
}
GA_df[,'core.size'] <- as.numeric(GA_df[,'core.size'])
#GA_fitness['core.size']
#GA_fitness[360:nrow(GA_fitness),'core.size']



GA_fitness <- data.frame(GA_df[,1:ncol(GA_df)],'log.CFU.cm2'=log10(((GA_df[,'Colony.count']) * (1/(10^((-1)*GA_df[,'Diln']))) * (40))/1))
GA_fitness <- data.frame(GA_df[,1:ncol(GA_df)],'log.CFU.cm2'=log10(((GA_df[,'Colony.count']) * (1/(10^((-1)*GA_df[,'Diln']))) * (GA_df[,'bead.beating.volume..µL.']))))


names(GA_fitness)

log10(GA_fitness[,'Colony.count']*(1/(10^((-1)*GA_fitness[,'Diln'])))*((GA_fitness[,'bead.beating.volume..µL.'])/5)/(GA_fitness[,'core.number']*2*pi*(((GA_fitness[,'core.size'])/10)/2)^2))
#GA_fitness$log.CFU.cm2

#GA_fitness <- data.frame(GA_df[,1:ncol(GA_df)],'log.CFU.cm2'=log10(((GA_df[,'Colony.count']) * (1/(10^((-1)*GA_df[,'Diln']))) * (80))/1))



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
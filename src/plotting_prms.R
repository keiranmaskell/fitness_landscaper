#plotting prms 

prms.update <- function(prms, null_base=FALSE){

  if(null_base==T){
    flat_id <- "K0049"
    type <- "all"
    GA <- TRUE
    Pidiq <- TRUE
    GA_master_fp <- 'data/Infection_results_master'
    Pidiq_master_fp <- 'data/Infection_results_master'
  }
    
    return(prms)   
}

make.prms <- function(flat_id = 'K0049',
                        type= 'single_flat',
                        GA = TRUE,
                        Pidiq = FALSE,
                        GA_master_fp = 'data/Infection_results_master',
                        GA_sheet = 1,
                        Pidiq_master_fp = 'data/Infection_results_master',
                        Pidiq_sheet = 2
                      ){
  

  inputs <- as.list(environment())
  prms <- prms.update(inputs)
  prms[order(names(prms))] ## order for consistency
  return(prms)
}







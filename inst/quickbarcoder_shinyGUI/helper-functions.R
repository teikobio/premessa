auto_barcode <- function(samples.metadata,
                         strata.vars,
                         slots.per.batch = 18,
                         rep.threshold = 100){

    ##### Setup Batch Simulation #####
    ## Set strata group variable
    samples.metadata <- samples.metadata %>% unite('strata.group',all_of(strata.vars),sep='_')
    strata.tab <- unique(samples.metadata[,c('subject.id','strata.group')]) %>%
        dplyr::mutate(N=table(samples.metadata$subject.id),batch=NA)

    # global proportions
    global.props <- data.frame(table(strata.tab$strata.group)) %>%
        dplyr::rename(strata.group=Var1,gfreq=Freq) %>%
        dplyr::mutate(gfreq=gfreq/sum(gfreq))




    simbatch <- function(strata.tab,slots.per.batch,global.props){
        batch=0
        available.ids <- 1:nrow(strata.tab)

        while (anyNA(strata.tab$batch)){
            slots.remaining <- slots.per.batch
            batch = batch+1

            while (any(strata.tab$N[available.ids]<=slots.remaining)){

                # randomly select a sample and place it in batch
                rand.ind <- sample(1:length(available.ids),1)
                sel.id <- available.ids[rand.ind]
                strata.tab$batch[sel.id] <- batch
                slots.remaining = slots.remaining-strata.tab$N[sel.id]

                available.ids<-available.ids[-rand.ind] # Remove from available
            }
        }

        # Calculate proportionality score
        batch.freq <- strata.tab %>%
            dplyr::group_by(strata.group,batch) %>%
            dplyr::summarize(freq=n()) %>%
            dplyr::mutate(freq = freq/sum(freq))

        strata.tab <- data.frame(strata.tab) %>% merge(global.props,by='strata.group') %>%
            merge(batch.freq,by=c('strata.group','batch')) %>%
            dplyr::mutate(diff = abs(gfreq-freq))
    }




    #### Simulate batches many times and find best configuration ####
    #################################################################
    # Initialize
    best.score <- Inf
    set.seed(8) # For reproducibility within a given cohort
    failedreps=0

    # Simulate
    while(failedreps<=rep.threshold){
        sim.tab <- simbatch(strata.tab,slots.per.batch,global.props)
        prop.score <- sum(sim.tab$diff)

        # If the proportionality score improves, save that configuration
        if(prop.score<best.score){
            best.score <- prop.score
            best.batch <- sim.tab
            failedreps <- 0
        } else {
            failedreps <- failedreps+1
        }
    }

    # convert batch to naming convention
    best.batch$PID <- sprintf('P%02d',best.batch$batch)

    # merge with metadata
    samples.metadata <- merge(best.batch[,c('subject.id','PID')],samples.metadata,by='subject.id')

    # order and assign pop
    samples.metadata <- samples.metadata[order(samples.metadata$PID),] %>%
        group_by(PID) %>%
        mutate(pop = paste0('pop',row_number(PID)),.after=PID)


    return(samples.metadata)
}

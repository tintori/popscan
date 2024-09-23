#' Takes a growth curve and finds the peak, calculating hours from feeding to starvation
#'
#' This function accepts a table with a "sd" value (standard deviation of pixel intensity) for each image captured in a time course. It finds the peak and calculates the time of starvation.
#' @param sd.table A table with one row per image captured, with standard deviation of pixel intensity already calculated.
#' @param exp.design A table with one row per culture/plate/petri dish. Each column adds info about how to group, facet, color, etc the data points.
#' @param inspect.each Allows you to inspect each trace by eye to make sure the min and max points of the growth curve are correctly IDed, and edit the "trim_before" and "trim_after" specifications, if not.
#' @param save.to Location to save the new meta file, now with peak and valley values, and edited trim_before and trim_afters. Defaults to "./tmp_meta_with_time_to_starve.csv"
#' @export
#' @examples
#' timeToStarve()

timeToStarve <- function(sd.table, exp.design, 
                         inspect.each = F, save.to = "tmp_meta_with_time_to_starve.csv"){
    library(dplyr)
    library(ggplot2)
    
    # find the max, min before the max, and time to starve
    sd.table.traits = sd.table %>% left_join(exp.design)
    
    if("trim.before" %in% colnames(sd.table.traits)){
        sd.table.traits = sd.table.traits %>%
            mutate(sd=replace(sd, tm(timepoint)<tm(trim.before), 0))
    }
    if("trim.after" %in% colnames(sd.table.traits)){
        sd.table.traits = sd.table.traits %>%
            mutate(sd=replace(sd, tm(timepoint)>tm(trim.after), 0))
    }
    
    sd.table.traits = sd.table.traits %>% 
        # Find max sd min, max, values, and timepoints for such
        group_by(grouper, time.fed) %>%
        summarise(
            sd_peak_value=max(sd[tm(timepoint)>(tm(timepoint[1])+1)], na.rm = T),
            sd_time_of_peak=tm(timepoint[which(sd==sd_peak_value&tm(timepoint)>(tm(timepoint[1])+1))][1]),
            sd_valley_value=min(min(sd[which(tm(timepoint)<sd_time_of_peak)]), sd_peak_value, na.rm = T),
            sd_time_of_valley=tm(tail(timepoint[which(sd==sd_valley_value & tm(timepoint)<=sd_time_of_peak)],n=1))
        ) %>%
        mutate(hours_to_starve = as.numeric(difftime(sd_time_of_peak, tm(time.fed), units = "hours")))
    
    # If option to inspect each by eye is selected, run through each sample
    if(inspect.each==T){
        for(tmp.row in 1:nrow(sd.table.traits)){
            # Set the counter to FALSE
            ok.to.go.before = FALSE
            ok.to.go.after = FALSE
            tmp.grouper = sd.table.traits$grouper[tmp.row]
            tmp.trim_before = exp.design$trim.before[which(exp.design$grouper==tmp.grouper)]
            tmp.trim_after = exp.design$trim.after[which(exp.design$grouper==tmp.grouper)]
            
            while(ok.to.go.before==FALSE){
                # Plot the trace, highlighting what is currently ID'ed as 
                tmp.min = sd.table.traits$sd_time_of_valley[tmp.row]
                tmp.max = sd.table.traits$sd_time_of_peak[tmp.row]
                p = ggplot()+
                    geom_line(data = sd.table %>% filter(grouper==tmp.grouper), 
                              aes(x=tm(timepoint), y=sd), color="gray")+
                    geom_line(data = sd.table %>% filter(grouper==tmp.grouper, tm(timepoint)<=tmp.max, tm(timepoint)>=tmp.min), 
                              aes(x=tm(timepoint), y=sd), color="black")+
                    labs(title=tmp.grouper, x="time", y="worm density")
                print(p)
                
                usr.input = readline(prompt = "RESPONSE NEEDED: Change the trim_before value? YY-MM-DD_HH-MM, or [enter] for no change.   ")
                if(usr.input==""){
                    ok.to.go.before=TRUE
                    next
                }
                if(usr.input!=""){
                    # check if the format is right
                    while(is.na(tm(usr.input))){                
                        usr.input = readline(prompt = "TRY AGAIN: The format must be YY-MM-DD_HH-MM (or [enter] for no change).   ")
                    }
                    # Once the format is confirmed, re-do the min biz
                    sd.patch = sd.table %>% filter(grouper==tmp.grouper, 
                                                   tm(timepoint)>tm(usr.input))
                    if(!is.na(tmp.trim_after)){ sd.patch = sd.patch %>% filter(tm(timepoint)<tm(tmp.trim_after)) } 
                    
                    # Rerun the peak and valley finding lines
                    sd.patch = sd.patch %>% 
                        group_by(grouper, time.fed) %>%
                        # Find max sd min, max, values, and timepoints for such
                        summarise(
                            sd_peak_value=max(sd[tm(timepoint)>(tm(timepoint[1])+1)], na.rm = T),
                            sd_time_of_peak=tm(timepoint[which(sd==sd_peak_value&tm(timepoint)>(tm(timepoint[1])+1))][1]),
                            sd_valley_value=min(min(sd[which(tm(timepoint)<sd_time_of_peak)]), sd_peak_value, na.rm = T),
                            sd_time_of_valley=tm(tail(timepoint[which(sd==sd_valley_value & tm(timepoint)<=sd_time_of_peak)],n=1))
                        ) %>%
                        mutate(hours_to_starve = as.numeric(difftime(sd_time_of_peak, tm(time.fed), units = "hours")))
                    
                    # Put these new peak and valley values into the traits table
                    sd.table.traits[tmp.row,c("sd_peak_value", "sd_time_of_peak", "sd_valley_value", "sd_time_of_valley", "hours_to_starve")] = 
                        sd.patch[1,c("sd_peak_value", "sd_time_of_peak", "sd_valley_value", "sd_time_of_valley", "hours_to_starve")]
                    # Put the new trim_befoe value into the meta table
                    if(!is.na(tm(usr.input))){exp.design[which(exp.design$grouper==tmp.grouper),"trim.before"] = usr.input}
                }
            }
            
            while(ok.to.go.after==FALSE){
                # Plot the trace, highlighting what is currently ID'ed as 
                tmp.min = sd.table.traits$sd_time_of_valley[tmp.row]
                tmp.max = sd.table.traits$sd_time_of_peak[tmp.row]
                p = ggplot()+
                    geom_line(data = sd.table %>% filter(grouper==tmp.grouper), 
                              aes(x=tm(timepoint), y=sd), color="gray")+
                    geom_line(data = sd.table %>% filter(grouper==tmp.grouper, tm(timepoint)<=tmp.max, tm(timepoint)>=tmp.min), 
                              aes(x=tm(timepoint), y=sd), color="black")+
                    labs(title=tmp.grouper, x="time", y="worm density")
                print(p)
                
                usr.input = readline(prompt = "RESPONSE NEEDED: Change the trim_after value? YY-MM-DD_HH-MM, or [enter] for no change.   ")
                if(usr.input==""){
                    ok.to.go.after=TRUE
                    next
                }
                if(usr.input!=""){
                    # check if the format is right
                    while(is.na(tm(usr.input))){                
                        usr.input = readline(prompt = "TRY AGAIN: The format must be YY-MM-DD_HH-MM (or [enter] for no change).   ")
                    }
                    # Once the format is confirmed, re-do the min biz
                    sd.patch = sd.table %>% filter(grouper==tmp.grouper, 
                                                   tm(timepoint)<tm(usr.input))
                    if(!is.na(tmp.trim_before)){ sd.patch = sd.patch %>% filter(tm(timepoint)>tm(tmp.trim_before)) } 
                    
                    # Rerun the peak and valley finding lines
                    sd.patch = sd.patch %>% 
                        group_by(grouper, time.fed) %>%
                        # Find max sd min, max, values, and timepoints for such
                        summarise(
                            sd_peak_value=max(sd[tm(timepoint)>(tm(timepoint[1])+1)], na.rm = T),
                            sd_time_of_peak=tm(timepoint[which(sd==sd_peak_value&tm(timepoint)>(tm(timepoint[1])+1))][1]),
                            sd_valley_value=min(min(sd[which(tm(timepoint)<sd_time_of_peak)]), sd_peak_value, na.rm = T),
                            sd_time_of_valley=tm(tail(timepoint[which(sd==sd_valley_value & tm(timepoint)<=sd_time_of_peak)],n=1))
                        ) %>%
                        mutate(hours_to_starve = as.numeric(difftime(sd_time_of_peak, tm(time.fed), units = "hours")))
                    
                    # Put these new peak and valley values into the traits table
                    sd.table.traits[tmp.row,c("sd_peak_value", "sd_time_of_peak", "sd_valley_value", "sd_time_of_valley", "hours_to_starve")] = 
                        sd.patch[1,c("sd_peak_value", "sd_time_of_peak", "sd_valley_value", "sd_time_of_valley", "hours_to_starve")]
                    # Put the new trim_after value into the meta table
                    if(!is.na(tm(usr.input))){exp.design[which(exp.design$grouper==tmp.grouper),"trim.after"] = usr.input}
                }
            }
        }
    }
    
    # Collapse the files
    exp.design = exp.design %>% select(setdiff(colnames(exp.design), c("time.fed", "sd_peak_value", "sd_time_of_peak", "sd_valley_value", "sd_time_of_valley", "hours_to_starve"))) %>% 
        left_join(sd.table.traits, by = "grouper")
        
    # Save/return the files
    write.csv(exp.design, file = save.to, quote = F, row.names = F)
    return(exp.design)
}


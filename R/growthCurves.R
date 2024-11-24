#' Convert pixel intensity tables to growth curves
#'
#' This function opens tables with pixel intensity information, and calculates standard deviation, to represent worm density.
#' @param exp.design A table with one row per culture/plate/petri dish. Each column adds info about how to group, facet, color, etc the data points.
#' @param remove.minimum For each pixel intensity, option to subtract the smallest frequency in the series from all images in the series. This should reduce background and improve contrast. Defaults to FALSE.
#' @param save.to Location where standard deviation file will be saved. Defaults to "./tmp_standard_deviations.csv".
#' @import dplyr
#' @export
#' @examples
#' growthCurves()

growthCurves <- function(exp.design, 
                         start.from = NA, patch.up = NA, 
                         remove.minimum = F,
                         save.to = "./tmp_standard_deviations.csv",
                         skip.filtereds = TRUE){
    library(dplyr)

    # Load data
    print("LOADING DATA")
    
    histos.df = data.frame()
    for(tmp.row in match(unique(exp.design$crop.name), exp.design$crop.name)){
        
        if(skip.filtereds){
            if(exp.design$filterer[tmp.row] == FALSE){next}
        }
 
        histo_folder = exp.design$path.to.crop[tmp.row]
        print(histo_folder)
        
        # Reconstruct the directory name and collect all pixel intensity table files.
        if(substr(histo_folder,nchar(histo_folder),nchar(histo_folder))=="/"){histo_folder = substr(histo_folder, 1, nchar(histo_folder)-1)}
        histo_folder = paste0(histo_folder, "_hists")
        histo_files=list.files(histo_folder, pattern = "*txt")
        if(length(histo_files)==0){
            print("no files here")
            next
        }
        
        tmp.df = data.frame()
        for(histo_file in histo_files){
            hf_path=paste0(histo_folder, "/", histo_file)
            tmp=read.csv(hf_path, sep="\t", row.names = 1)
            name_str=strsplit(histo_file, "\\.")[[1]]
            name_str=name_str[-which(name_str%in%c("tiff", "txt"))]
            tp_str=paste(name_str[(length(name_str)-1):length(name_str)], collapse = "_")
            name_str=paste(name_str[1:(length(name_str)-2)], collapse = ".")
            tmp$timepoint=tp_str
            tmp$sample=name_str
            tmp.df=rbind(tmp.df, tmp)
        }
        if(remove.minimum){
            tmp.df =  tmp.df %>%
                group_by(Value) %>%
                mutate(min_count = min(Count),
                       new_count = Count - min_count) %>% 
                select(Value, new_count, timepoint, sample) %>%
                rename("Count"="new_count")
        }
        histos.df = rbind(histos.df, tmp.df)
    }
    histos.tb = as_tibble(histos.df)
    # Get rid of crop-error black pixels
    histos.tb = histos.tb %>%  
        mutate(Count = replace(Count, Value==0 & Count >0, 0))
    rm(histos.df, tmp.df, tmp)
    
    # Calculate sds
    print(paste0("CALCULATING SDs FOR EACH TIMEPOINT - this might take ~", ceiling((nrow(histos.tb)/8000)/60) ," minutes? Depending on your computer"))
    print(Sys.time())
    summary.sd <- histos.tb %>% 
        mutate(run_tp = 0) %>% 
        group_by(sample, timepoint) %>%
        summarise(
            sd = sd(rep(Value,Count), na.rm = T)
        ) %>% 
        mutate(crop.name = sample) %>%
        left_join(by="crop.name", exp.design)
    if(skip.filtereds){summary.sd <- summary.sd %>% filter(filterer == TRUE)}
    print(Sys.time())
    
    # Get the NA rows back in there
    summary.sd = full_join(summary.sd, exp.design %>% select(intersect(c("grouper", "colorer", "filterer", "facet_wrap", "time.fed"), colnames(exp.design))))
    
    if(!is.na(save.to)){write.csv(summary.sd, file = save.to, quote = F, row.names = F)}
    return(summary.sd)
}


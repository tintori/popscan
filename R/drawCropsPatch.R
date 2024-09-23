
drawCrops <- function(exp.design, 
                      start.from=NA, patch.up=NA, 
                      save.to="./tmp_meta_with_crop_coords.csv",
                      path.to.ij.jar = "/Applications/ImageJ.app/Contents/Java/ij.jar"){
    if(!file.exists(path.to.ij.jar)){ quit("ERROR: I can't find the ij.jar file you specified.")}
    for(tmp.row in match(unique(exp.design$scan.prefix), exp.design$scan.prefix)){
        # If "start.from", skip everything until the start.from prefix matches
        if(!is.na(start.from)){
            start.row = match(start.from, exp.design$scan.prefix)
            if(tmp.row < start.row){next}
        }
        # If "patch.up", skip everything except the one patch.from prefix
        if(!is.na(patch.up)){
            patch.row = match(patch.up, exp.design$scan.prefix)
            if(tmp.row != patch.row){next}
        }
        
        # Locate the file series, and then locate the last file in the series
        tmp.scan.path = exp.design$scan.dir[tmp.row]
        tmp.scan.name = exp.design$scan.prefix[tmp.row]
        tmp.scan.file = list.files(path = tmp.scan.path, pattern = exp.design$scan.prefix[tmp.row])
        tmp.scan.file = tmp.scan.file[length(tmp.scan.file)]
        tmp.crops = unique(exp.design[which(exp.design$scan.prefix==tmp.scan.name),"scan.position"])
        
        for(tmp.crop in tmp.crops){
            if(exp.design[which(exp.design$scan.position==tmp.crop & exp.design$scan.prefix==tmp.scan.name),
                          "filterer"]==FALSE){
                print(paste0("Skipping ", tmp.scan.name, " crop ", tmp.crop, " because it was annotated to be skipped"))
                next
            }
            print(paste0("Setting crop coordinates for crop \'", tmp.crop, "\' out of ", length(tmp.crops), " (", tmp.scan.name, ")" ))
            tmp.command = paste0("java -jar ", path.to.ij.jar, " --console -macro ~/Dropbox/postdoc/072_scanner_method_PAPER/04_scripts/popscan/imageJ/define_circles.txt '", tmp.scan.path, "/", tmp.scan.file, " ", tmp.crop, "'")
            system(tmp.command, ignore.stdout = TRUE, intern=TRUE, ignore.stderr = TRUE)
            tmp.log = read.csv2("tmp_wormscanR_imageJ_log.txt", header = F)
            exp.design[which(exp.design$scan.prefix == exp.design$scan.prefix[tmp.row] & 
                                 exp.design$scan.position==tmp.crop),"crop.coords"] = 
                tmp.log[nrow(tmp.log),1]
        }
    }
    system("rm tmp_wormscanR_imageJ_log.txt")
    write.csv(exp.design, file = save.to, quote = F, row.names = F)
    return(exp.design)
}


#' Define crop coordinates
#'
#' This function opens scanner images in imageJ, prompts you to draw the cropping circle, and saves the coordinates.
#' @param exp.design A table with one row per culture/plate/petri dish. Each column adds info about how to group, facet, color, etc the data points.
#' @param start.from If you started this process earlier and are continuing mid-way, specify the scan.prefix to start with.
#' @param patch.up If you did this process already but want to change the crop for one plate, specify the scan.prefix of the plate.
#' @param save.to Location where crop coordinates will be saved.
#' @param path.to.ij.jar Location of imageJ jar file. Defaults to "/Applications/ImageJ.app/Contents/Java/ij.jar".
#' @export
#' @examples
#' drawCrops()

drawCrops <- function(exp.design, 
                      start.from=NA, patch.up=NA, 
                      save.to="./tmp_meta_with_crop_coords.csv",
                      path.to.ij.jar = "/Applications/ImageJ.app/Contents/Java/ij.jar"){
    # Check that imageJ is there
    if(!file.exists(path.to.ij.jar)){ quit("ERROR: I can't find the ij.jar file you specified.")}
    # Make imageJ macro
    write_ij_script(which.script = "draw_crops")
    path.to.macro = paste0(find.package("popscan"), "/define_circles.txt")
    
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
            tmp.command = paste0("java -jar ", path.to.ij.jar, " --console -macro ", path.to.macro, " '", tmp.scan.path, "/", tmp.scan.file, " ", tmp.crop, "'")
            system(tmp.command, ignore.stdout = TRUE, intern=TRUE, ignore.stderr = TRUE)
            tmp.log = read.csv2("tmp_wormscanR_imageJ_log.txt", header = F)
            if(length(strsplit(tmp.log[nrow(tmp.log),1], split = " ")[[1]]) != 4){break}
            exp.design[which(exp.design$scan.prefix == exp.design$scan.prefix[tmp.row] & 
                                 exp.design$scan.position==tmp.crop),"crop.coords"] = 
                tmp.log[nrow(tmp.log),1]
        }
    }
    system("rm tmp_wormscanR_imageJ_log.txt")
    write.csv(exp.design, file = save.to, quote = F, row.names = F)
    return(exp.design)
}


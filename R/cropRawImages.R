#' Crop raw scanner images
#'
#' This function cleans up the metadata table, and allows you to specify roles for columns.
#' @param exp.design A table with one row per culture/plate/petri dish. Each column adds info about how to group, facet, color, etc the data points.
#' @param crop.coords Column of the design table that contains cropping coordinates. Defaults to "crop.coords"
#' @param start.from If you started this process earlier and are continuing mid-way, specify the scan.prefix to start with.
#' @param patch.up If you did this process already but want to change the crop for one plate, specify the scan.prefix of the plate.
#' @param path.to.ij.jar Location of imageJ jar file. Defaults to "/Applications/ImageJ.app/Contents/Java/ij.jar".
#' @export
#' @examples
#' cropRawImages()

cropRawImages <- function(exp.design, 
                          crop.coords = "crop.coords", start.from=NA, patch.up=NA,
                          path.to.ij.jar = "/Applications/ImageJ.app/Contents/Java/ij.jar"){
    if(!file.exists(path.to.ij.jar)){ quit("ERROR: I can't find the ij.jar file you specified.")}
    counter=0 # only used for "start.from" option
    for(tmp.scan in unique(exp.design$scan.prefix)){
        
        # If "start.from", skip everything until the start.from prefix matches
        if(!is.na(start.from)){
            if(tmp.scan == start.from){counter = counter +1}
            if(counter == 0) {next}
        }
        
        # If "patch.up", skip everything except the one patch.from prefix
        if(!is.na(patch.up)){
            if(tmp.scan != patch.up){next}
        }
        
        print(tmp.scan)
        
        # Get coordinates from metadata file
        tmp.row = which(exp.design$scan.prefix==tmp.scan)[1]
        tmp.infolder = exp.design[tmp.row,"scan.dir"]
        tmp.coordsets = ""
        for(tmp.crop in exp.design$scan.position[which(exp.design$scan.prefix==tmp.scan)]){
            if(exp.design[which(exp.design$scan.position==tmp.crop & 
                                exp.design$scan.prefix==tmp.scan),
                          "filterer"]==FALSE){
                print(paste0("Skipping ", tmp.scan, " crop ", tmp.crop, " because it was annotated to be skipped"))
                next
            }
            tmp.coordsets = paste(tmp.coordsets, tmp.crop, 
                                  exp.design$crop.coords[which(exp.design$scan.prefix==tmp.scan & 
                                                                   exp.design$scan.position==tmp.crop)], 
                                  collapse = " ")
        }
        
        # Make the imageJ command and run
        tmp.command = paste0("java -jar ", path.to.ij.jar, " --console -macro ./popscanR/imageJ/crop_circles.txt '",
                             tmp.infolder, " ",
                             tmp.scan, " ",
                             tmp.coordsets, "'")
        system(tmp.command, ignore.stdout = TRUE, intern=TRUE, ignore.stderr = TRUE)
    }
}

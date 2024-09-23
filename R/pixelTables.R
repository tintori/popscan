#' Convert images to pixel intensity frequency tables
#'
#' This function opens (cropped) scanner images in imageJ and calculates how many pixels there are of each pixel intensity (from white to black). Returns a table.
#' @param exp.design A table with one row per culture/plate/petri dish. Each column adds info about how to group, facet, color, etc the data points.
#' @param start.from If you started this process earlier and are continuing mid-way, specify the scan.prefix to start with.
#' @param patch.up If you did this process already but want to change the crop for one plate, specify the scan.prefix of the plate.
#' @param path.to.ij.jar Location of imageJ jar file. Defaults to "/Applications/ImageJ.app/Contents/Java/ij.jar".
#' @export
#' @examples
#' pixelTables()

pixelTables <- function(exp.design, 
                        start.from=NA, patch.up=NA, 
                        path.to.ij.jar = "/Applications/ImageJ.app/Contents/Java/ij.jar"){
    if(!file.exists(path.to.ij.jar)){ quit("ERROR: I can't find the ij.jar file you specified.")}
    counter=0 # only used for "start.from" option
    for(tmp.row in match(unique(exp.design$path.to.crop), exp.design$path.to.crop)){
        tmp.scan = exp.design$scan.prefix[tmp.row]
        tmp.folder = exp.design$path.to.crop[tmp.row]
        
        # If "start.from", skip everything until the start.from prefix matches
        if(!is.na(start.from)){
            if(tmp.scan == start.from){counter = counter +1}
            if(counter == 0) {next}
        }
        
        # If "patch.up", skip everything except the one patch.from prefix
        if(!is.na(patch.up)){
            if(tmp.scan != patch.up){next}
        }
        
        if(exp.design$filterer == FALSE){next}
        print(tmp.folder)
        tmp.command = paste0("java -jar ", path.to.ij.jar, " --console -macro ./popscanR/imageJ/tabulate_pix_int.txt '",
                             tmp.folder, "'")
        system(tmp.command, ignore.stdout = TRUE, intern=TRUE, ignore.stderr = TRUE)
    }
    
}

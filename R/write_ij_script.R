#' Create ImageJ macro scripts
#'
#' This function generates the macro scripts needed for imageJ functions. It writes them to a findable location.
#' @param which.script Which script would you like to create? Options: "pix_int", "draw_crops", crop_images", or "all"
#' @export


write_ij_script <- function(which.script){
    ij.script.dir = find.package("popscan")
    
    if(which.script %in% c("pix_int", "all")){
        script.path = paste0(ij.script.dir, "/tabulate_pix_int.txt")
        sink(script.path)
        cat('//ImageJ macro that accepts cropped scanner images and converts them into tables of pixel intensity frequencies.

print("Herewego");

// Parse settings out of input

in_folder = getArgument;

// Clear lagging "/" from folder name
if(substring(in_folder, in_folder.length-1, in_folder.length)=="/"){
	in_folder = substring(in_folder, 0, in_folder.length-1);
}

out_folder = in_folder+"_hists";
File.makeDirectory(out_folder);

print("Making histogram of all images from "+in_folder);
print("Putting them in "+out_folder);

// Open images and histogram them

files_in =getFileList(in_folder);

for(i=0; i<(files_in.length); i++){
	sample = files_in[i];
	print(sample);
	out_file = out_folder+"/"+sample+".txt";
	print(out_file);
	open(in_folder+"/"+sample);

	nBins = 256;
	run("Clear Results");
	row = 0;
	getHistogram(values, counts, nBins);
	for (j=0; j<nBins; j++) {
		setResult("Value", row, values[j]);
		setResult("Count", row, counts[j]);
		row++;
	 }
	updateResults();

	saveAs("Results", out_file);

	close();
}

print("Done :)");

run("Quit");
')
        sink()
    }

    if(which.script %in% c("draw_crops", "all")){
        script.path = paste0(ij.script.dir, "/define_circles.txt")
        sink(script.path)
        cat('//ImageJ macro that allows user to manually set the crop areas for each set of images in their dataset.
        
setOption("ExpandableArrays", true);
ins = getArgument;
in_list = split(ins," ");
infile = in_list[0];
crop_name = in_list[1];

open(infile);

setTool("oval");
waitForUser("Drag selection oval over crop region for plate "+crop_name+" on this image then click \'OK\'");
getSelectionBounds(x, y, width, height);

print(x, y, width, height);
selectWindow("Log");
saveAs("Text", "tmp_wormscanR_imageJ_log.txt");
close();
run("Quit");

')
        sink()
    }

    if(which.script %in% c("crop_images", "all")){
        script.path = paste0(ij.script.dir, "/crop_circles.txt")
        sink(script.path)
        cat('//ImageJ macro that uses established crop coordinates to make new folders of cropped images from raw scanner data.
        
print("Herewego");
setOption("ExpandableArrays", true);


// Parse settings out of input

keyword = "";
ins = getArgument;
in_list = split(ins," ");
event_nums = (((in_list.length)-2)/5);

if(in_list.length>5){
	in_folder = in_list[0];
	if(substring(in_folder, in_folder.length-1, in_folder.length)=="/")
	in_folder = substring(in_folder, 0, in_folder.length-1);
	keyword=in_list[1];
	print("Processing all samples from "+in_folder+"/ that begin with \""+keyword+"\"");
}else{
	exit ("Error: No arguments. Please supply \"[in_folder] [keyword] [x1] [y1] [w1] [h1] [x2] [y2]....\" ");
}

name = newArray();
x = newArray();
y = newArray();
w = newArray();
h = newArray();

for(event_num=0; event_num<event_nums; event_num++){
	name[event_num] = in_list[event_num*5+2];
	x[event_num] = in_list[event_num*5+3];
	y[event_num] = in_list[event_num*5+4];
	w[event_num] = in_list[event_num*5+5];
	h[event_num] = in_list[event_num*5+6];
}

print("names, xs, ys, widths, and heights:");
Array.print(name);
Array.print(x);
Array.print(y);
Array.print(w);
Array.print(h);


// Crop images and save

out_folder = in_folder+"_cropped";
File.makeDirectory(out_folder);
files_in =getFileList(in_folder);


for(i=0; i<(files_in.length); i++){			// i is every single file, including those that dont match
sample = files_in[i];
print(sample);
if(substring(sample, 0, Math.min(keyword.length, sample.length))==keyword){		// if it matches
    print("file accepted");
    
    open(in_folder+"/"+sample);
    rename("orig");
    for (j=0; j<(x.length); j++){		// j is however objects there are to be cropped out of the scan
        print("crop "+name[j]);
        File.makeDirectory(out_folder+"/"+name[j]+"_"+keyword);
        makeRectangle(x[j], y[j], w[j], h[j]);
        run("Duplicate...", "title=crop");
        makeOval(0,0,w[j],h[j]);
        run("Clear Outside");
        saveAs("Tiff", out_folder+"/"+name[j]+"_"+keyword+"/"+name[j]+"_"+sample);
        close();
    }
    close();
}
    }

print("Done :)");

run("Quit");
')
        sink()
    }

}

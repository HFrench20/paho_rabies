### FUNCTION FOR PARTIAL MATCHING
### MATCHING VILLAGE NAMES APPROXIMATELY USING agrep# approximate pattern matching -ace!!
approxmatch <- function(x, y, d_increment, max_d){
possmatchindex=0; possmatchname=character(0); min_possmatches=100000;
for (i in 1:length(x)){
	done=0
	d=0
	min_possmatches[i]=100000
	while (done==0) {
		(indices=agrep(x[i],as.character(y),max.distance=d)) #agrep is the approx matching function
		(possmatchindex[i]=ifelse(length(indices)==1,indices,NA))#save
		(possmatchname[i]=ifelse(length(indices)==1,as.character(y[indices]),NA))#save
			#index of name if there is a single match
		done=ifelse(length(indices)==1,1,ifelse(d>=max_d,1,0))#tell loop to 
			#move on to next i when either found a single approx. match 
			#OR d very big (and matching not reliable)
		if (length(indices)>0 & length(indices) <min_possmatches[i]) 
			{min_possmatches[i]=length(indices)}#record
			#min number of approx matches reached -closest get to a single match
		(d=d+d_increment) #increment of d which is the matching 'distance'
		}
	i=i+1
}
data.frame(DataNames=x, MasterNames=possmatchname, MasterNameIndex=possmatchindex, minpossmatches=min_possmatches)
}
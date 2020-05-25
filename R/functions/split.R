
#makes k data splits
split <- function(k,data){

	#data = read.csv("./data.csv",header=T)

	#Randomize First !!
	rand = sample(nrow(data))

	ret = list()
	size = nrow(data) %/% k
	a = 1
	
	for(i in 1:k){
		ret[[i]] = data[rand[a : i*size],]
		a = i*size + 1
	}

	remainder = nrow(data) %% k
	rand_rem = sample(remainder)
	
	if(remainder != 0)
	  for(i in 1:remainder)
		  rbind( ret[[rand[i]]], data[rand[size*k + i]])
	  

	return(ret)
}
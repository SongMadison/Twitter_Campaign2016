myValidJSON <- function(data.json){
  
  #validate()
  #Test if a string contains valid JSON. Characters vectors will be 
  #collapsed into a single string
  
  validateIds <- unlist(lapply(data.json, function(x) validate(x)))
  a <- table(validateIds) ##FALSE  TRUE :12 126686 
  message(names(a), " : ", a)
  data.json <- data.json[which(validateIds == TRUE)] 
  return (data.json)
}

myToJSON <- function(data.json){
  
  #validate()
  #Test if a string contains valid JSON. Characters vectors will be 
  #collapsed into a single string
  
  validateIds <- unlist(lapply(data.json, function(x) validate(x)))
  a <- table(validateIds) ##FALSE  TRUE :12 126686 
  message(names(a)[1]," : ", a[1], ", ", names(a)[2]," : ", a[2])
  data.json <- data.json[which(validateIds == TRUE)] 
  return (sprintf("[%s]", paste(data.json, collapse = ',')))
}

# input:json.data is a list of {  }. having the following format.
#  ouput: '[ {},{} ]'  -- add ',' and []

myConvertJson <- function(json.data){
  json.data <- paste0( json.data[1:(length(json.data)-1)], ",")
  json.data[length(json.data)] <- paste0(json.data[length(json.data)], "]")
  json.data[1] <- paste0("[",json.data[1])
  return(json.data)
}


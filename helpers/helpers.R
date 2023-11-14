fromList <- function(input){
  elements <- unique(unlist(input))
  data <- unlist(lapply(input, function(x){x <- as.vector(match(elements, x))}))
  data[is.na(data)] <- as.integer(0); data[data != 0] <- as.integer(1)
  data <- data.frame(matrix(data, ncol = length(input), byrow = F))
  data <- data[which(rowSums(data) !=0), ]
  names(data) <- names(input)
  return(data)
}

# Function to calculate intersection
calculate_intersection <- function(combination, upset_data) {
  cols <- upset_data %>%
    filter(if_all(combination,~.x == 1)) %>%
    filter(if_all(colnames(cols)[!colnames(cols) %in% combination],~.x==0))
  
   intersect = nrow(cols) 
   return(intersect)
}


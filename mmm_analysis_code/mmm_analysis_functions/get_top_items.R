# Function to get the top items in any vector

get.top.items <- function(col,word.id.vec,word.stem.table,n.get){
  col.order <- rev(order(col))
  pos.top <- col.order[1:n.get]
  ids.top <- word.id.vec[pos.top]
  stems.top <- word.stem.table[ids.top,1]
  return(stems.top)
}

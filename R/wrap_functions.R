#' Calculate distance between a point and references on a line
#' 
#' Taking into account that the line wraps.
#' 
#' @param from the value we're measuring from
#' @param to one or more values to which we're measuring
#' @param size the length of the line
#' 
#' @return a vector of distances to the values in "to"
#' 
#' @export 
#' 
wrapped_distance <- function(from, to, size) {
  # centres: 20, 90
  # pixel: 2
  # map size 100
  # 90 to 2 is 12 pixels
  # 2 to 20 is 18 pixels
  # middle is normally 50.
  # if we want to make the middle 2,
  # we need to shift everything from
  # 52 onwards to the negatives side.
  # so... everything beyond 52 becomes itself -100?
  # seems right.
  # what if we are on the other side?
  # centres: 5, 90
  # pixel: 99
  # map size 100
  # 90 to 99 is 9 pixels
  # 99 to  5 is 6 pixels
  # middle is normally 50.
  # if we want to make the middle 99,
  # we need to shift everything from
  # 49 backwards to the extra positive side?
  # so... everything before 49 becomes itself + 100?
  # seems right. so we need to decide first if
  # we're working on the left or right side of the map.
  if (from == ceiling(size/2)) {
    # we're in the centre, no need to adjust
    dists <- to - from
  }
  if (from < ceiling(size/2)) {
    # we're before half
    right_end <- from + floor(size/2)
    aux <- to
    to_move <- aux > right_end
    aux[to_move] <- aux[to_move] - size
    dists <- aux - from
  }
  if (from > ceiling(size/2)) {
    # we're after half
    left_end <- from - floor(size/2)
    aux <- to
    to_move <- aux < left_end
    aux[to_move] <- aux[to_move] + size
    dists <- aux - from
  }
  return(dists)
}

#' Calculate wrapped ranges along a line.
#' 
#' Taking into account that the line wraps.
#' 
#' @param i the centre of the range
#' @param spread the width of the range to each side of i
#' @param size the length of the line
#' 
#' @return a vector of values within the range
#' 
#' @export 
#' 
wrapped_range <- function(i, spread, size) {
  left <- i - spread
  if (left < 1) {
    left <- size + left
  }
  # wrap
  right <- i + spread
  if (right > size) {
    right <- right - size
  }
  if (right > left) {
    output <- left:right
  } else {
    output <- c(left:size, 1:right)      
  }
  return(output)
}


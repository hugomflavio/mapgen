#' Generate tectonic plates
#' 
#' @param n_plates number of plates on the map
#' @param map_x map x dimension in tiles
#' @param map_y map y dimension in tiles
#' 
#' @return a list containing plate information, vector information, and the map
#' 
#' @export
#' 
gen_plates <- function(n_plates, map_x, map_y = map_x) {
  plates <- data.frame(id = 1:n_plates,
                         centre_x = round(runif(n = n_plates, 1, map_x)),
                         centre_y = round(runif(n = n_plates, 1, map_y)),
                         force_x = runif(n = n_plates, -1, 1),
                         force_y = runif(n = n_plates, -1, 1))

  plates$force_scaled_x <- plates$centre_x + plates$force_x * (map_x/5)
  plates$force_scaled_y <- plates$centre_y + plates$force_y * (map_y/5)

  map <- expand.grid(x = 1:map_x, y = 1:map_y)

  map$plate <- apply(map, 1, function(r) {
    x_dists <- abs(wrapped_distance(r["x"], plates$centre_x, map_x))
    y_dists <- abs(wrapped_distance(r["y"], plates$centre_y, map_y))
    return(which.min(sqrt(x_dists^2 + y_dists^2)))
  })

  map$force_x <- plates$force_x[map$plate]
  map$force_y <- plates$force_y[map$plate]
  map$centre_x <- plates$centre_x[map$plate]
  map$centre_y <- plates$centre_y[map$plate]

  part1 <- plates[, c("id", "centre_x", "centre_y")]
  part2 <- plates[, c("id", "force_scaled_x", "force_scaled_y")]
  colnames(part1) <- colnames(part2) <- c("id", "x", "y")
  vectors <- rbind(part1, part2)

  return(list(plates = plates, vectors = vectors, map = map))
}

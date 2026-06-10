#' Find Hamiltonian Cycle
#'
#' Finds a Hamiltonian cycle in an undirected graph using backtracking.
#' A Hamiltonian cycle visits every vertex exactly once and returns to the starting vertex.
#'
#' @param graf An integer square adjacency matrix representing an undirected graph.
#'   A value of 1 at position [i,j] indicates an edge between vertex i and vertex j.
#'   The matrix must be symmetric.
#'
#' @return An integer vector of length n+1 representing the Hamiltonian cycle,
#'   where n is the number of vertices. The first and last elements are equal
#'   (the cycle returns to the starting vertex). Returns NULL if no Hamiltonian
#'   cycle exists.
#'
#' @examples
#' graf <- matrix(as.integer(c(
#'   0,1,0,1,
#'   1,0,1,0,
#'   0,1,0,1,
#'   1,0,1,0
#' )), nrow = 4)
#' cykl_hamiltona(graf)
#'
#' @useDynLib EulerianandHamiltonianCyclesinR, .registration = TRUE
#' @export
cykl_hamiltona <- function(graf) {
  if (!is.matrix(graf)) stop("Graf musi być macierza.")
  if (!is.integer(graf)) graf <- matrix(as.integer(graf), nrow = nrow(graf))
  
  wynik <- .Call("hammilton", graf)
  
  if (wynik[1] == 0) {
    message("Nie znaleziono cyklu Hamiltona.")
    return(NULL)
  }
  return(wynik)
}

#' Find Eulerian Cycle
#'
#' Finds an Eulerian cycle in a directed graph using Hierholzer's algorithm.
#' An Eulerian cycle visits every edge exactly once and returns to the starting vertex.
#'
#' @param graf An integer square adjacency matrix representing a directed graph.
#'   A value of 1 at position [i,j] indicates a directed edge from vertex i to vertex j.
#'
#' @return An integer vector representing the Eulerian cycle. The length of the vector
#'   equals the number of directed edges plus one. The first and last elements are equal
#'   (the cycle returns to the starting vertex). Returns NULL if no Eulerian cycle exists.
#'
#' @examples
#' graf <- matrix(as.integer(c(
#'   0,1,0,1,
#'   1,0,1,0,
#'   0,1,0,1,
#'   1,0,1,0
#' )), nrow = 4)
#' cykl_eulera(graf)
#'
#' @export
cykl_eulera <- function(graf) {
  if (!is.matrix(graf)) stop("Graf musi być macierza.")
  if (!is.integer(graf)) graf <- matrix(as.integer(graf), nrow = nrow(graf))
  
  # Konwersja macierzy → lista sąsiedztwa (wymagana przez euler.c)
  lista_sasiedztwa <- lapply(1:nrow(graf), function(i) {
    as.integer(which(graf[i, ] != 0))
  })
  
  wynik <- .Call("euler", lista_sasiedztwa)
  
  if (length(wynik) == 0) {
    message("Nie znaleziono cyklu Eulera.")
    return(NULL)
  }
  return(wynik)
}
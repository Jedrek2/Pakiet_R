# Eulerian and Hamiltonian cycles in R

An R package for finding Eulerian and Hamiltonian cycles in graphs using C algorithms.

## Description

`Cykle` provides two functions:
- `cykl_eulera()` — finds an Eulerian cycle using Hierholzer's algorithm
- `cykl_hamiltona()` — finds a Hamiltonian cycle using backtracking

## Installation

```r
install.packages("devtools")
devtools::install_github("Jedrek2/Eulerian_and_Hamiltonian_cycles_in_R")
```

## Usage

### Eulerian Cycle

```r
library(Cykle)

graf <- matrix(as.integer(c(
  0,1,0,1,
  1,0,1,0,
  0,1,0,1,
  1,0,1,0
)), nrow = 4)

cykl_eulera(graf)
#> [1] 1 2 3 4 1 2 3 4 1
```

### Hamiltonian Cycle

```r
cykl_hamiltona(graf)
#> [1] 1 2 3 4 1
```

### No cycle found

```r
graf_bez <- matrix(as.integer(c(
  0,1,0,0,
  1,0,0,0,
  0,0,0,1,
  0,0,1,0
)), nrow = 4)

cykl_hamiltona(graf_bez)
#> Nie znaleziono cyklu Hamiltona.
#> NULL
```

## Requirements

- R >= 4.0.0
- C compiler (Rtools on Windows)
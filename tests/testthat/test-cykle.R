test1 <- matrix(as.integer(c(
  0,1,0,1,
  1,0,1,0,
  0,1,0,1,
  1,0,1,0
)), nrow = 4)

test_that("Czy zwraca wektor długości n+1", {
  wynik <- cykl_hamiltona(test1)
  expect_equal(length(wynik), 5)
})

test_that("Czy cykl zaczyna się i kończy w tym samym wierzchołku", {
  wynik <- cykl_hamiltona(test1)
  expect_equal(wynik[1], wynik[length(wynik)])
})

test_that("Czy każdy wierzchołek występuje dokładnie raz", {
  wynik <- cykl_hamiltona(test1)
  expect_equal(sort(unique(wynik[-length(wynik)])), 1:4)
})

test_that("Czy zwraca NULL gdy cykl nie istnieje", {
  test2 <- matrix(as.integer(c(
    0,1,0,0,
    1,0,0,0,
    0,0,0,1,
    0,0,1,0
  )), nrow = 4)
  expect_null(cykl_hamiltona(test2))
})

test_that("Co gdy nieprawidłowe wejście", {
  expect_error(cykl_hamiltona(c(1,2,3)))
})

# --- Testy cykl_eulera ---

test_that("Czy zwraca wektor długości liczba krawędzi+1", {
  wynik <- cykl_eulera(test1)
  liczba_krawedzi <- sum(test1)
  expect_equal(length(wynik), liczba_krawedzi + 1)
})

test_that("Czy zaczyna i kończy w tym samym wierzchołku", {
  wynik <- cykl_eulera(test1)
  expect_equal(wynik[1], wynik[length(wynik)])
})

test_that("Co gdy nieprawidłowe wejście", {
  expect_error(cykl_eulera(c(1,2,3)))
})









# --- Duże grafy ---

# Graf cykliczny 100 wierzchołków (każdy połączony z następnym i poprzednim)
graf_duzy_hamilton <- matrix(0L, nrow = 100, ncol = 100)
for (i in 1:99) {
  graf_duzy_hamilton[i, i+1] <- 1L
  graf_duzy_hamilton[i+1, i] <- 1L
}
graf_duzy_hamilton[1, 100] <- 1L
graf_duzy_hamilton[100, 1] <- 1L

test_that("cykl_hamiltona działa dla grafu 100 wierzchołków", {
  wynik <- cykl_hamiltona(graf_duzy_hamilton)
  expect_equal(length(wynik), 101)
  expect_equal(wynik[1], wynik[101])
  expect_equal(sort(unique(wynik[-101])), 1:100)
})

# Graf pełny 10 wierzchołków (każdy z każdym)
graf_pelny <- matrix(1L, nrow = 10, ncol = 10)
diag(graf_pelny) <- 0L

test_that("cykl_hamiltona działa dla grafu pełnego 10 wierzchołków", {
  wynik <- cykl_hamiltona(graf_pelny)
  expect_equal(length(wynik), 11)
  expect_equal(wynik[1], wynik[11])
  expect_equal(sort(unique(wynik[-11])), 1:10)
})


graf_bez_hamilton <- matrix(0L, nrow = 10, ncol = 10)
for (i in 1:6) {
  for (j in 7:10) {
    graf_bez_hamilton[i, j] <- 1L
    graf_bez_hamilton[j, i] <- 1L
  }
}
expect_null(cykl_hamiltona(graf_bez_hamilton))

# Graf Eulera - każdy wierzchołek ma stopień parzysty
# Graf cykliczny 50 wierzchołków z dodatkowymi krawędziami (stopień 4)
graf_duzy_euler <- matrix(0L, nrow = 50, ncol = 50)
for (i in 1:49) {
  graf_duzy_euler[i, i+1] <- 1L
  graf_duzy_euler[i+1, i] <- 1L
}
graf_duzy_euler[1, 50] <- 1L
graf_duzy_euler[50, 1] <- 1L

for (i in seq(1, 48, by = 2)) {
  graf_duzy_euler[i, i+2] <- 1L
  graf_duzy_euler[i+2, i] <- 1L
}

test_that("cykl_eulera działa dla dużego grafu", {
  wynik <- cykl_eulera(graf_duzy_euler)
  expect_equal(wynik[1], wynik[length(wynik)])
  liczba_krawedzi <- sum(graf_duzy_euler)
  expect_equal(length(wynik), liczba_krawedzi + 1)
})

test_that("cykl_eulera dla grafu cyklicznego 50 wierzchołków zawiera wszystkie krawędzie", {
  wynik <- cykl_eulera(graf_duzy_euler)
  # Każda krawędź powinna być odwiedzona dokładnie raz
  # Liczba przejść = liczba krawędzi skierowanych
  expect_equal(length(wynik) - 1, sum(graf_duzy_euler))
})





# Graf pełny 12 wierzchołków
graf_pelny_12 <- matrix(1L, nrow = 12, ncol = 12)
diag(graf_pelny_12) <- 0L

test_that("cykl_hamiltona działa dla grafu pełnego 12 wierzchołków", {
  wynik <- cykl_hamiltona(graf_pelny_12)
  expect_equal(length(wynik), 13)
  expect_equal(wynik[1], wynik[13])
  expect_equal(sort(unique(wynik[-13])), 1:12)
})

# Brak cyklu Hamiltona - mały graf żeby skończyło się w rozsądnym czasie
test_that("cykl_hamiltona zwraca NULL dla grafu bez cyklu (10 wierzchołków)", {
  graf_bez <- matrix(0L, nrow = 10, ncol = 10)
  for (i in 1:6) {
    for (j in 7:10) {
      graf_bez[i, j] <- 1L
      graf_bez[j, i] <- 1L
    }
  }
  expect_null(cykl_hamiltona(graf_bez))
})
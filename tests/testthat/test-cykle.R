test1 <- matrix(as.integer(c(
  0,1,0,1,
  1,0,1,0,
  0,1,0,1,
  1,0,1,0
)), nrow = 4)

test_that("Czy zwraca wektor dlugosci n+1", {
  wynik <- cykl_hamiltona(test1)
  expect_equal(length(wynik), 5)
})

test_that("Czy cykl zaczyna sie i konczy w tym samym wierzcholku", {
  wynik <- cykl_hamiltona(test1)
  expect_equal(wynik[1], wynik[length(wynik)])
})

test_that("Czy kazdy wierzcholek wystepuje dokladnie raz", {
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

test_that("Co gdy nieprawidlowe wejscie", {
  expect_error(cykl_hamiltona(c(1,2,3)))
})

# --- Testy cykl_eulera ---

test_that("Czy zwraca wektor dlugosci liczba krawedzi+1", {
  wynik <- cykl_eulera(test1)
  liczba_krawedzi <- sum(test1)
  expect_equal(length(wynik), liczba_krawedzi + 1)
})

test_that("Czy zaczyna i konczy w tym samym wierzcholku", {
  wynik <- cykl_eulera(test1)
  expect_equal(wynik[1], wynik[length(wynik)])
})

test_that("Co gdy nieprawidlowe wejscie", {
  expect_error(cykl_eulera(c(1,2,3)))
})









# --- Duze grafy ---

# Graf cykliczny 100 wierzcholkow (kazdy polaczony z nastepnym i poprzednim)
graf_duzy_hamilton <- matrix(0L, nrow = 100, ncol = 100)
for (i in 1:99) {
  graf_duzy_hamilton[i, i+1] <- 1L
  graf_duzy_hamilton[i+1, i] <- 1L
}
graf_duzy_hamilton[1, 100] <- 1L
graf_duzy_hamilton[100, 1] <- 1L

test_that("cykl_hamiltona dziala dla grafu 100 wierzcholkow", {
  wynik <- cykl_hamiltona(graf_duzy_hamilton)
  expect_equal(length(wynik), 101)
  expect_equal(wynik[1], wynik[101])
  expect_equal(sort(unique(wynik[-101])), 1:100)
})

# Graf pelny 10 wierzcholkow (kazdy z kazdym)
graf_pelny <- matrix(1L, nrow = 10, ncol = 10)
diag(graf_pelny) <- 0L

test_that("cykl_hamiltona dziala dla grafu pelnego 10 wierzcholkow", {
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

# Graf Eulera - kazdy wierzcholek ma stopien parzysty
# Graf cykliczny 50 wierzcholkow z dodatkowymi krawedziami (stopien 4)
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

test_that("cykl_eulera dziala dla duzego grafu", {
  wynik <- cykl_eulera(graf_duzy_euler)
  expect_equal(wynik[1], wynik[length(wynik)])
  liczba_krawedzi <- sum(graf_duzy_euler)
  expect_equal(length(wynik), liczba_krawedzi + 1)
})

test_that("cykl_eulera dla grafu cyklicznego 50 wierzcholkow zawiera wszystkie krawedzie", {
  wynik <- cykl_eulera(graf_duzy_euler)
  # Kazda krawedz powinna byc odwiedzona dokladnie raz
  # Liczba przejsc = liczba krawedzi skierowanych
  expect_equal(length(wynik) - 1, sum(graf_duzy_euler))
})





# Graf pelny 12 wierzcholkow
graf_pelny_12 <- matrix(1L, nrow = 12, ncol = 12)
diag(graf_pelny_12) <- 0L

test_that("cykl_hamiltona dziala dla grafu pelnego 12 wierzcholkow", {
  wynik <- cykl_hamiltona(graf_pelny_12)
  expect_equal(length(wynik), 13)
  expect_equal(wynik[1], wynik[13])
  expect_equal(sort(unique(wynik[-13])), 1:12)
})

# Brak cyklu Hamiltona - maly graf zeby skonczylo sie w rozsadnym czasie
test_that("cykl_hamiltona zwraca NULL dla grafu bez cyklu (10 wierzcholkow)", {
  graf_bez <- matrix(0L, nrow = 10, ncol = 10)
  for (i in 1:6) {
    for (j in 7:10) {
      graf_bez[i, j] <- 1L
      graf_bez[j, i] <- 1L
    }
  }
  expect_null(cykl_hamiltona(graf_bez))
})
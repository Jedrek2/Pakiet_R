#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>
#include <stdlib.h>

// Funkcja pomocnicza: sprawdza czy wierzchołek 'v' może być dodany do ścieżki
bool bezpieczny(int v, SEXP graf, int* sciezka, int pozycja, int n) {
    // Sprawdź czy istnieje krawędź między ostatnim wierzchołkiem a 'v'
    // graf jest macierzą sąsiedztwa
    if (INTEGER(graf)[sciezka[pozycja - 1] + v * n] == 0) return false;

    // 2. Sprawdź czy 'v' już występuje w ścieżce
    for (int i = 0; i < pozycja; i++) {
        if (sciezka[i] == v) return false;
    }
    return true;
}

// Funkcja rekurencyjna
bool rekurencja_hammilton(SEXP graf, int* sciezka, int pozycja, int n) {
    if (pozycja == n) {
        // Sprawdź czy ostatni wierzchołek łączy się ze startowym jeśli wszystkie wierzchołki są już w ścieżce
        return (INTEGER(graf)[sciezka[pozycja - 1] + sciezka[0] * n] == 1);
    }

    for (int v = 1; v < n; v++) {
        if (bezpieczny(v, graf, sciezka, pozycja, n)) {
            sciezka[pozycja] = v;
            if (rekurencja_hammilton(graf, sciezka, pozycja + 1, n)) return true;
            //usuń wierzchołek jeśli nie prowadzi do rozwiązania
            sciezka[pozycja] = -1;
        }
    }
    return false;
}

// Funkcja główna wywoływana z R
SEXP hammilton(SEXP graf) {
    int n = INTEGER(getAttrib(graf, R_DimSymbol))[0];
    
    // Alokacja pamięci dla ścieżki
    int* sciezka = (int*)malloc(n * sizeof(int));
    for (int i = 0; i < n; i++) sciezka[i] = -1;
    
    sciezka[0] = 0; // Zaczynamy od wierzchołka 0
    
    SEXP wynik = PROTECT(allocVector(INTSXP, n + 1));
    int* p_wynik = INTEGER(wynik);
    
    if (rekurencja_hammilton(graf, sciezka, 1, n)) {
        for (int i = 0; i < n; i++) p_wynik[i] = sciezka[i] + 1; // Konwersja na R
        p_wynik[n] = sciezka[0] + 1; // Domknięcie cyklu
    } else {
        // Zwraca wektor z zerem, jeśli nie znaleziono
        p_wynik[0] = 0; 
    }
    
    free(sciezka);
    UNPROTECT(1);
    return wynik;
}


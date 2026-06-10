#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>

// Struktura pomocnicza dla listy sąsiedztwa
typedef struct Node {
    int numer;
    struct Node* next;
} Node;

SEXP euler(SEXP graf) {
    int n = length(graf);
    
    // 1. Kopiowanie danych z R do struktur C (tablica list)
    Node** grafC = (Node**)malloc(n * sizeof(Node*));
    int wierzcholki_ilosc = 0;
    
    for (int i = 0; i < n; i++) {
        SEXP sasiedzi = VECTOR_ELT(graf, i);
        int len = length(sasiedzi);
        wierzcholki_ilosc += len;
        grafC[i] = NULL;
        int* p_sasiedzi = INTEGER(sasiedzi);
        for (int j = len - 1; j >= 0; j--) {
            Node* new_node = (Node*)malloc(sizeof(Node));
            new_node->numer = p_sasiedzi[j] - 1; // Z R do C (zmiana indeksowania)
            new_node->next = grafC[i];
            grafC[i] = new_node;
        }
    }

    // 2. Algorytm Hierholzera
    //Zaczynamy od wierzchołka 0, chodzimy po grafie, wrzucamy odwiedzone wierzchołki na stosi usuwamy za sobą krawęź żeby nie móc wykorzystać jej dwa razy.
    //Jeśli wierzchołek nie ma już sąsiadów to z parzystości stopni to znaczy że jest to początkowy wierzchołek, 
    //dodajemy go do listy wynikowej i cofamy się do poprzedniego i znowu albo dodajemy go do 
    //listy wynikowej i cofamy się albo (*) idziemy dalej jeśli jest taka możliwość usuwając krawędzie po których przeszliśmy (jeśli już nie będziemy już mogli iść dalej będzie to dokładnie ten wierzchołek).
    // Można powiedzieć że algorytm na początku wybiera jeden główny cykl idąc od wierzchołka 0 do wierzchołka 0, potem wpisuje na listę wynikową 
    //wierzchołki z tego cylku od końsa i podpina inne cykle powstałe w wyniku cofania się do poprzeniego wierzchołka i rozpoczynanie w nim nowego cyklu (*).
    int* stos = (int*)malloc((wierzcholki_ilosc + n) * sizeof(int));
    int* wynik = (int*)malloc((wierzcholki_ilosc + 1) * sizeof(int));
    int stos_ptr = 0;
    int wynik_ptr = 0;

    stos[stos_ptr++] = 0; // Zaczynamy od wierzchołka 0

    while (stos_ptr > 0) {
        int u = stos[stos_ptr - 1];
        if (grafC[u] != NULL) {
            Node* temp = grafC[u];  //jeśli wierzchołek ma sąsiada to idziemy do tego sąsiada
            int v = temp->numer;
            grafC[u] = temp->next;
            free(temp);
            stos[stos_ptr++] = v;
        } else {
            // Jeśli nie ma sąsiadów, dodaj do cyklu
            wynik[wynik_ptr++] = u + 1; // Powrót do indeksowania R
            stos_ptr--;
        }
    }

    // 3. Konwersja wyniku do R
    SEXP wynik2 = PROTECT(allocVector(INTSXP, wynik_ptr));
    int* p_wynik2 = INTEGER(wynik2);
    for (int i = 0; i < wynik_ptr; i++) {
        p_wynik2[i] = wynik[wynik_ptr - 1 - i]; // Odwracamy kolejność
    }

    free(stos);
    free(wynik);
    free(grafC);
    UNPROTECT(1);
    return wynik2;
}



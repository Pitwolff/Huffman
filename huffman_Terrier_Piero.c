#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

// compression

struct ab {struct ab* fg; unsigned char valeur; int frequence; struct ab* fd;};
typedef struct ab ab;

struct ab2 {struct ab2* fg; unsigned char valeur; struct ab2* fd;};
typedef struct ab2 ab2;

struct tas {ab** data; int taille; int capacite;};
typedef struct tas tas;

struct liste {unsigned char val; struct liste* suivant;};
typedef struct liste liste;

struct buffer {unsigned char val; int taille;};
typedef struct buffer buffer;

struct couple {ab2* arb; liste* octets;};
typedef struct couple couple;

int freq(ab* arbre) {
    return arbre->frequence;
}

int gauche(int i) {
    return 2 * i + 1;
}

int droite(int i) {
    return 2 * i + 2;
}

int pere(int i) {
    return (i - 1) / 2;
}

void echanger(ab** data, int i, int j) {
    ab* temp = data[i];
    data[i] = data[j];
    data[j] = temp;
}

void remonter(ab** data, int i) {
    if (i != 0 && freq(data[pere(i)]) > freq(data[i])) {
        echanger(data, i, pere(i));
        remonter(data, pere(i));
    }
}

void redescendre(tas* t, int i) {
    int imin = i;
    if (gauche(i) < t->taille && freq(t->data[gauche(i)]) < freq(t->data[imin]))
    imin = gauche(i);
    if (droite(i) < t->taille && freq(t->data[droite(i)]) < freq(t->data[imin]))
    imin = droite(i);
    if (i != imin) {
        echanger(t->data, i, imin);
        redescendre(t, imin);
    }
}

void rendre_tas(tas* t) {
    for (int i = (t->taille - 2) / 2; i != -1; i -= 1) {
        redescendre(t, i);
    }
}

tas* creer_tas(ab** data, int taille, int capacite) {
    tas* res = malloc(sizeof(tas));
    res->data = data;
    res->taille = taille;
    res->capacite = capacite;
    rendre_tas(res);
    return res;
}

void inserer(tas* t, ab* arbre) {
    assert(t->taille < t->capacite);
    t->data[t->taille] = arbre;
    remonter(t->data, t->taille);
    t->taille += 1;
}

ab* extraire_min(tas* t) {
    assert(t->taille > 0);
    ab* res = t->data[0];
    t->taille -= 1;
    echanger(t->data, 0, t->taille);
    redescendre(t, 0);
    return res;
}

ab* creer_arbre(tas* t) {
    assert(t->taille != 0);
    while(t->taille > 1) {
        ab* min1 = extraire_min(t);
        ab* min2 = extraire_min(t);
        ab* noeud = malloc(sizeof(ab));
        noeud->fg = min1;
        noeud->fd = min2;
        noeud->valeur = 0;
        noeud->frequence = freq(min1) + freq(min2);
        inserer(t, noeud); 
    }
    ab* res = t->data[0];
    free(t->data);
    free(t);
    return res;
}

int* frequences_de(char* nomfichier) {
    FILE* fichier = fopen(nomfichier, "rb");
    if (fichier == NULL) {
        printf("Impossible d'ouvrir le fichier");
        exit(0);
    }
    int* frequences = malloc(sizeof(int) * 256);
    for (int i = 0; i != 256; i += 1) {
        frequences[i] = 0;
    }
    unsigned char octet;
    while(fread(&octet, 1, 1, fichier) == 1) {
        frequences[octet] += 1;
    }
    fclose(fichier);
    return frequences;
}

tas* tas_de(int* frequences) {
    int taille = 0;
    for (int i = 0; i != 256; i += 1) {
        if (frequences[i] != 0) taille += 1;
    }
    ab** data = malloc(sizeof(ab*) * taille);
    int j = 0;
    for (int i = 0; i != 256; i += 1) {
        if (frequences[i] != 0) {
            ab* feuille = malloc(sizeof(ab));
            feuille->fg = NULL;
            feuille->fd = NULL;
            feuille->valeur = i;
            feuille->frequence = frequences[i];
            data[j] = feuille;
            j += 1;
        }
    }
    free(frequences);
    return creer_tas(data, taille, taille);
}

void vider(buffer* b, FILE* fichier) {
    assert(fwrite(&b->val, 1, 1, fichier) == 1);
    b->val = 0;
    b->taille = 0;
}

void ecrire(FILE* fichier, liste* l, buffer* b) {
    if (l == NULL) return;
    b->taille += 1;
    b->val = (b->val << 1) | l->val;
    if (b->taille == 8)
    vider(b, fichier);
    ecrire(fichier, l->suivant, b);
}

liste* aux(liste* entree, liste* res) {
    if (entree == NULL) return res;
    liste* new_case = malloc(sizeof(liste));
    new_case->val = entree->val;
    new_case->suivant = res;
    return aux(entree->suivant, new_case);
}

liste* reverse(liste* l) {
    return aux(l, NULL);
}

void lib_mem(liste* l) {
    if (l == NULL) return;
    lib_mem(l->suivant);
    free(l);
}

void parcours_profondeur(ab* arbre, liste** codes, liste* chemin) {
    if (arbre->fg == NULL) {
        codes[arbre->valeur] = reverse(chemin);
        free(arbre);
    }
    else {
        liste* new_case1 = malloc(sizeof(liste));
        new_case1->val = 0;
        new_case1->suivant = chemin;
        parcours_profondeur(arbre->fg, codes, new_case1);
        free(new_case1);
        liste* new_case2 = malloc(sizeof(liste));
        new_case2->val = 1;
        new_case2->suivant = chemin;
        parcours_profondeur(arbre->fd, codes, new_case2);
        free(new_case2);
        free(arbre);
    }
}

liste** codes_de(ab* arbre) {
    liste** codes = malloc(sizeof(liste*) * 256);
    for (int i = 0; i != 256; i += 1) {
        codes[i] = NULL;
    }
    parcours_profondeur(arbre, codes, NULL);
    return codes;
} 

char* nom_compression(char* nomfichier) {
    int n = strlen(nomfichier);
    char* nom_new_fichier = malloc(sizeof(char) * (n + 10));
    for (int i = 0; i != n; i += 1) {
        nom_new_fichier[i] = nomfichier[i];
    }
    nom_new_fichier[n] = '_';
    nom_new_fichier[n + 1] = 'c';
    nom_new_fichier[n + 2] = 'o';
    nom_new_fichier[n + 3] = 'm';
    nom_new_fichier[n + 4] = 'p';
    nom_new_fichier[n + 5] = '.';
    nom_new_fichier[n + 6] = 'b';
    nom_new_fichier[n + 7] = 'i';
    nom_new_fichier[n + 8] = 'n';
    nom_new_fichier[n + 9] = '\0';
    return nom_new_fichier;
}

void serialiser(ab* arbre, FILE* fichier) {
    if (arbre->fg == NULL) {
        int n = 1;
        assert(fwrite(&n, 1, 1, fichier) == 1);
        assert(fwrite(&(arbre->valeur), 1, 1, fichier) == 1);
    }
    else {
        int n = 0;
        assert(fwrite(&n, 1, 1, fichier) == 1);
        serialiser(arbre->fg, fichier); 
        serialiser(arbre->fd, fichier);
    }
}

void compresser(char* nomfichier) {
    char* nom_new_fichier = nom_compression(nomfichier);
    FILE* new_fichier = fopen(nom_new_fichier, "wb");
    ab* arbre = creer_arbre(tas_de(frequences_de(nomfichier)));
    serialiser(arbre, new_fichier);
    liste** codes = codes_de(arbre);
    buffer b = {.val = 0, .taille = 0};
    FILE* fichier = fopen(nomfichier, "rb");
    unsigned char octet;
    while (fread(&octet, 1, 1, fichier) == 1) {
        ecrire(new_fichier, codes[octet], &b);
    }
    fclose(fichier);
    for (int i = 0; i != 256; i += 1) {
        lib_mem(codes[i]);
    }
    free(codes);
    liste l = {.val = 0, .suivant = NULL};
    unsigned char n = b.taille;
    for (int i = 0; i != 8 - n; i += 1) {
        ecrire(new_fichier, &l, &b);
    }
    assert(fwrite(&n, 1, 1, new_fichier) == 1);
    fclose(new_fichier);
    free(nom_new_fichier);
}

// decompression

couple parcourir(ab2* arbre1, ab2* arbre2, liste* l) {
    if (arbre2->fg == NULL) {
        liste* new_case = malloc(sizeof(liste));
        new_case->val = arbre2->valeur;
        couple res = parcourir(arbre1, arbre1, l);
        new_case->suivant = res.octets;
        res.octets = new_case;
        return res;
    }
    if (l == NULL) {
        couple res = {.arb = arbre2, .octets = NULL};
        return res;
    }
    if (l->val == 0) {
        return parcourir(arbre1, arbre2->fg, l->suivant);
    }
    return parcourir(arbre1, arbre2->fd, l->suivant);
}

liste* aux2(int nb, int taille, liste* res) {
    if (taille == 0) return res;
    liste* new_case = malloc(sizeof(liste));
    new_case->val = nb & 1;
    new_case->suivant = res;
    return aux2(nb >> 1, taille - 1, new_case);
}

liste* liste_de(int nb, int taille) {
    return aux2(nb, taille, NULL);
}

void ecrire_liste(FILE* fichier, liste* l) {
    if (l == NULL) return;
    assert(fwrite(&(l->val), 1, 1, fichier) == 1);
    ecrire_liste(fichier, l->suivant);
    free(l);
}

char* nom_decompression(char* nomfichier) {
    int n = strlen(nomfichier);
    char* nom_new_fichier = malloc(sizeof(char) * (n - 1));
    int ind_point = -1;
    for (int i = n - 10; i != -1; i -= 1) {
        if (nomfichier[i] == '.') {
            ind_point = i;
            break;
        }
    }
    if (ind_point == -1) {
        for (int i = 0; i != n - 9; i += 1) {
            nom_new_fichier[i] = nomfichier[i];
        }
        nom_new_fichier[n - 9] = '_';
        nom_new_fichier[n - 8] = 'd';
        nom_new_fichier[n - 7] = 'e';
        nom_new_fichier[n - 6] = 'c';
        nom_new_fichier[n - 5] = 'o';
        nom_new_fichier[n - 4] = 'm';
        nom_new_fichier[n - 3] = 'p';
        nom_new_fichier[n - 2] = '\0';
        return nom_new_fichier;
    }
    for (int i = 0; i != ind_point; i += 1) {
        nom_new_fichier[i] = nomfichier[i];
    }
    nom_new_fichier[ind_point] = '_';
    nom_new_fichier[ind_point + 1] = 'd';
    nom_new_fichier[ind_point + 2] = 'e';
    nom_new_fichier[ind_point + 3] = 'c';
    nom_new_fichier[ind_point + 4] = 'o';
    nom_new_fichier[ind_point + 5] = 'm';
    nom_new_fichier[ind_point + 6] = 'p';
    for (int i = ind_point; i != n - 9; i += 1) {
        nom_new_fichier[i + 7] = nomfichier[i];
    }
    nom_new_fichier[n - 2] = '\0';
    return nom_new_fichier;
}

ab2* deserialiser(FILE* fichier) {
    unsigned char octet;
    assert(fread(&octet, 1, 1, fichier) == 1);
    if (octet == 1) {
        assert(fread(&octet, 1, 1, fichier) == 1);
        ab2* feuille = malloc(sizeof(ab2));
        feuille->fg = NULL;
        feuille->fd = NULL;
        feuille->valeur = octet;
        return feuille;
    }
    ab2* noeud = malloc(sizeof(ab2));
    noeud->valeur = 0;
    noeud->fg = deserialiser(fichier);
    noeud->fd = deserialiser(fichier);
    return noeud;
}

void lib_arb(ab2* arbre) {
    lib_arb(arbre->fg);
    lib_arb(arbre->fd);
    free(arbre);
}

void decompresser(char* nomfichier) {
    char* nom_new_fichier = nom_decompression(nomfichier);
    FILE* new_fichier = fopen(nom_new_fichier, "wb");
    FILE* fichier = fopen(nomfichier, "rb");
    ab2* arbre1 = deserialiser(fichier);
    ab2* arbre2 = arbre1;
    unsigned char octet1;
    unsigned char octet2;
    unsigned char octet3;
    assert(fread(&octet1, 1, 1, fichier) == 1);
    assert(fread(&octet2, 1, 1, fichier) == 1);
    while (fread(&octet3, 1, 1, fichier) == 1) {
        liste* l = liste_de(octet1, 8);
        couple arbre_liste = parcourir(arbre1, arbre2, l);
        lib_mem(l);
        arbre2 = arbre_liste.arb;
        ecrire_liste(new_fichier, arbre_liste.octets);
        octet1 = octet2;
        octet2 = octet3;
    }
    liste* l = liste_de(octet1 >> (8 - octet2), octet2);
    couple arbre_liste = parcourir(arbre1, arbre2, l);
    lib_mem(l);
    free(arbre1);
    ecrire_liste(new_fichier, arbre_liste.octets);
    fclose(fichier);
    fclose(new_fichier);
    free(nom_new_fichier);
}

int main(int argc, char* args[]) {
    assert(argc == 3);
    if (strcmp(args[1], "compresser") == 0)
    compresser(args[2]);
    else if (strcmp(args[1], "decompresser") == 0)
    decompresser(args[2]);
    else printf("Le premier argument doit être compresser ou decompresser, suivi du nom du fichier");
    return 0;
}
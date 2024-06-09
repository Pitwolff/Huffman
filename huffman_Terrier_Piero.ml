(*compression*)

type ab = F of int * int | N of ab * int * ab;;
type ab2 = F2 of int | N2 of ab2 * ab2;;
type heap = V | N of heap * ab * heap;;

let freq arbre = match arbre with
| F(_, f) -> f
| N(_, f, _) -> f;;

let rec inserer tas arbre = match tas with
| V -> N(V, arbre, V)
| N(fg, arb, fd) when freq arb <= freq arbre -> N(fd, arb, inserer fg arbre)
| N(fg, arb, fd) -> N(fd, arbre, inserer fg arb);;

let rec extraire_min tas = match tas with
| V -> failwith "tas vide"
| N(V, arb, V) -> V, arb
| N(V, arb, fd) -> let new_tas, new_arb = extraire_min fd in N(V, new_arb, new_tas), arb
| N(fg, arb, V) -> let new_tas, new_arb = extraire_min fg in N(new_tas, new_arb, V), arb
| N(N(fgg, arbg, fgd), arb, N(fdg, arbd, fdd)) when freq arbg <= freq arbd ->
let new_tas, new_arb = extraire_min (N(fgg, arbg, fgd)) in N(new_tas, new_arb, N(fdg, arbd, fdd)), arb
| N(fg, arb, fd) -> let new_tas, new_arb = extraire_min fd in N(fg, new_arb, new_tas), arb;;

let rec creer_arbre tas = match tas with 
| V -> failwith "tas vide"
| N(V, arbre, V) -> arbre
| _ -> let tas1, arb1 = extraire_min tas in 
let tas2, arb2 = extraire_min tas1 in 
creer_arbre (inserer tas2 (N(arb1, freq arb1 + freq arb2 , arb2)));;

let frequences_de nom_fichier = 
let fichier = open_in_bin nom_fichier in
let frequences = Array.make 256 0 in
begin try 
while true do
let octet = input_byte fichier in
frequences.(octet) <- frequences.(octet) + 1 done
with End_of_file -> () end;
close_in fichier; frequences;;

let tas_de frequences = 
let tas = ref V in
for i = 0 to 255 do
if frequences.(i) <> 0 
then tas := inserer !tas (F(i, frequences.(i))) done;
!tas;;

let vider buffer fichier = 
if !(snd buffer) = 8 then begin
output_byte fichier !(fst buffer);
fst buffer := 0;
snd buffer := 0 end;;

let rec ecrire fichier l buffer = match l with
| [] -> ()
| a::q -> incr (snd buffer);
fst buffer := (!(fst buffer) lsl 1) lor a;
vider buffer fichier; ecrire fichier q buffer;;

let codes_de arbre =
let codes = Array.make 256 [] in
let rec parcours_profondeur arb chemin = match arb with
| F(octet, _) -> codes.(octet) <- chemin
| N(fg, _, fd) -> parcours_profondeur fg (0::chemin); parcours_profondeur fd (1::chemin)
in parcours_profondeur arbre [];
Array.map (fun l -> List.rev l) codes;;

let compresser nomfichier =
let nom_new_fichier = nomfichier ^ "_comp.bin" in
let new_fichier = open_out_bin nom_new_fichier in
let arbre = creer_arbre (tas_de (frequences_de nomfichier)) in
let rec serialiser abr = match abr with
| F(octet, _) -> output_byte new_fichier 1; output_byte new_fichier octet;
| N(fg, _, fd) -> output_byte new_fichier 0; serialiser fg; serialiser fd;
in serialiser arbre;
let codes = codes_de arbre in
let buffer = (ref 0, ref 0) in
let fichier = open_in_bin nomfichier in
begin try
while true do
let octet = input_byte fichier in
ecrire new_fichier codes.(octet) buffer done;
with End_of_file -> () end;
close_in fichier;
let n = !(snd buffer) in 
ecrire new_fichier (List.init (8 - n) (fun x -> 0)) buffer;
output_byte new_fichier n;
close_out new_fichier;;

(*décompression*)

let parcourir arbre1 arbre2 l = 
let rec aux arbre l2 res = match arbre, l2 with
| F2(octet), _ -> aux arbre1 l2 (octet::res)
| _, [] -> arbre, List.rev res
| N2(fg, fd), a::q -> if a = 1 then aux fg q res else aux fd q res (*L'arbre a été construit à l'envers*)
in aux arbre2 l [];;

let liste_de nb taille =
let rec aux n taille2 l = 
if taille2 = 0 then l 
else aux (n lsr 1) (taille2 - 1) ((1 land n)::l)
in aux nb taille [];;

let rec ecrire_liste fichier l = match l with 
| [] -> ()
| a::q -> output_byte fichier a; ecrire_liste fichier q;;

let nom_decompression nomfichier =
let s = String.sub nomfichier 0 (String.length nomfichier - 9) in
match String.rindex_opt s '.' with
| Some indice_point -> let sans_extension = String.sub s 0 indice_point 
and extension = String.sub s indice_point (String.length s - indice_point) in
sans_extension ^ "_decomp" ^ extension
| None -> s ^ "_decomp";;

let decompresser nomfichier = 
let nom_new_fichier = nom_decompression nomfichier in
let new_fichier = open_out_bin nom_new_fichier in 
let fichier = open_in_bin nomfichier in
let rec deserialiser () = 
let noeud_ou_feuille = input_byte fichier in
if noeud_ou_feuille = 1 then F2(input_byte fichier)
else N2(deserialiser (), deserialiser ()) (*OCaml lit de la droite vers la gauche*)
in let arbre1 = deserialiser () in
let arbre2 = ref arbre1 in
let octet1 = ref (input_byte fichier) and octet2 = ref (input_byte fichier) in
begin try
while true do
let octet3 = input_byte fichier in
let l = liste_de !octet1 8 in
let arbre, octets = parcourir arbre1 !arbre2 l in
arbre2 := arbre; ecrire_liste new_fichier octets;
octet1 := !octet2; octet2 := octet3 done
with End_of_file -> () end;
let l = liste_de (!octet1 lsr (8 - !octet2)) !octet2 in
let octets = snd (parcourir arbre1 !arbre2 l) in
ecrire_liste new_fichier octets;
close_out new_fichier;
close_in fichier;;

(*utilisation des fonctions*)

let main () =
let args =  Sys.argv in
if Array.length args <> 3 then failwith "Il doit y avoir deux arguments : compresser ou decompresser et le nom du fichier"
else if args.(1) = "compresser" then compresser args.(2)
else if args.(1) = "decompresser" then decompresser args.(2)
else failwith "Il doit y avoir deux arguments : compresser ou decompresser et le nom du fichier";;

main ();;
let rec allCharsInWords chars words =
  match chars with
  | [] -> true
  | h::t -> 
      words |> List.exists (fun (w:string) -> w.Contains(h.ToString())) && allCharsInWords t words


let  verificarPalabra (listaA: _ list) (listaB: _ list)  =
  let A = List.sort listaA
  let B = List.sort listaB
  List.forall2 (fun elem1 elem2 -> elem1 = elem2) A B


let checkWord (words:string list) (letters: char list) =
  if (allCharsInWords letters words) then

    if List.exists (fun (word:string) -> (verificarPalabra letters (Seq.toList word))) words then
      printfn "Palabra Adivinada - los pinta de morado o algo"
    else
      printfn "Siga adivinando la palabra - Los deja en azul"
  else 
    printfn "Palabra Incorrecta - los pinta de nuevo de gris"
    
// printfn "%b" (allCharsInWords ['a'; 'b';'d';'c'] ["hello"; "abdc"] )

// printfn "%b" (verificarPalabra ['a'; 'c';'b';'d'] (Seq.toList "abcd") )



checkWord ["hello"; "abdc"] ['a'; 'c';'d']


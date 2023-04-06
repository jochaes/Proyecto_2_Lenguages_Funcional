// //test

// let palabras = [['h';'o';'l';'a']
//                 ['c';'a';'s';'a']
//                 ['c';'a';'m';'a']
//                 ['h';'o';'y';'o']
//                 ['a';'s';'a';'m']
//                 ['a';'s';'a';'c']]
                

// let objetivo = ['c';'a';'s';'a']
// let objetivo1 = ['c';'a';'m';'a']
// let objetivo2 = ['m';'a';'s';'a']
// let objetivo3 = ['y';'o']


let palabras = [['O'; 'Q'; 'P'; 'L'; 'C']
                ['I'; 'A'; 'E'; 'N'; 'A']
                ['T'; 'T'; 'M'; 'M'; 'S']
                ['A'; 'A'; 'J'; 'L'; 'A']
                ['P'; 'A'; 'N'; 'U'; 'A']]
//PAN
let objetivo = ['P'; 'A'; 'N']
let objetivo1 = ['P'; 'A'; 'T'; 'I'; 'O']
let objetivo2 = ['C'; 'A'; 'S'; 'A']
let objetivo3 = ['A'; 'L'; 'M'; 'A']
let objetivo4 = ['T'; 'E'; 'L']                




(*
    Esta función recibe un elemento e y una lista lista y 
    devuelve un valor booleano indicando si el elemento se
    encuentra en la lista o no. Para lograr esto, primero se 
    mapea cada elemento de la lista a un valor booleano que 
    indica si es igual a e. 

    Luego se reduce la lista de valores booleanos utilizando una 
    función lógica OR, lo que devuelve true si se encontró el 
    elemento e en la lista o false en caso contrario.
*)
let miembro e lista =
    lista
    |> List.map (fun x -> x = e)
    |> List.reduce (fun x y -> x || y)
    
(*
    Esta función recibe dos índices i y j, y una matriz matrix de 
    listas de elementos del mismo tipo (en este caso, listas de caracteres). 

    Devuelve el elemento correspondiente a la posición (i,j) de la matriz, si existe. 

    Si alguno de los índices es mayor o igual a la longitud de la matriz en la 
    dimensión correspondiente, la función devuelve el carácter '_' como valor predeterminado.
*)
let getElement i j (matrix: char list list)=
    if matrix.Length > i then
        if matrix[i].Length > j then
            matrix[i][j]
        else
            '_'
    else
        '_'

(*
    Esta función recibe dos índices i y j, un carácter letter y 
    una matriz matrix de listas de caracteres. 
    Devuelve la posición de la primera ocurrencia del carácter 
    letter en la matriz, 
    empezando por la posición (i,j) y avanzando horizontalmente 
    desde izquierda a derecha y 
    verticalmente desde arriba hacia abajo. 
    Si no se encuentra ninguna ocurrencia del carácter, la función 
    devuelve (-1,-1).
*)
let rec findFirstLetterPosition (i:int) (j:int) (letter:char) (matrix: _ list list) =    
    //printfn "Buscar Letra: (%d,%d)" i j

    if matrix[i][j] = letter then
        (i,j)
    elif (i = matrix.Length-1) && (j = matrix[i].Length-1) then
        (-1,-1)
    else
        if j = matrix[i].Length-1 then
            findFirstLetterPosition (i+1) 0 letter matrix
        else
            findFirstLetterPosition i (j+1) letter matrix
            
                           
(*
    Esta función recibe una posición (x,y) y una matriz matrix 
    de listas de caracteres. 
    Devuelve una lista con las posiciones de los vecinos horizontales 
    de (x,y) en la matriz.
*)
let vecinos_aux (posicion: (int*int)) (matrix: _ list list) =
    let i,j = fst(posicion),snd(posicion)
    //printfn "Posicion: (%d,%d)" i j
    //printfn  "Vecinos: %A" [(i,j+1); (i+1,j+1); (i+1,j); (i+1,j-1); (i,j-1); (i-1,j-1); (i-1,j); (i-1,j+1)]
    //left, upper_left_corner, up , upper_right_corner, right, lower_right_corner, down, lower_left_corner
    [(i,j-1); (i-1,j-1); (i-1,j); (i-1,j+1); (i,j+1); (i+1,j+1); (i+1,j); (i+1,j-1)]

    // right, lower_right_corner, down, lower_left_corner, left, upper_left_corner, up, upper_right_corner
    //[(i,j+1); (i+1,j+1); (i+1,j); (i+1,j-1); (i,j-1); (i-1,j-1); (i-1,j); (i-1,j+1)]

(*
    Esta función recibe una posición (x,y) y una matriz matrix 
    de listas de caracteres. 
    Devuelve una lista con las posiciones de los vecinos 
    horizontales de (x,y) en la 
    matriz que se encuentran dentro de los límites de la 
    matriz.
*)
let vecinos (posicion: (int*int)) (matrix: _ list list) =
    (vecinos_aux posicion matrix)
    |> List.filter (fun x -> if ((fst(x) >= 0)&&(fst(x) < matrix.Length)) && ((snd(x) < (matrix[0].Length))&&(snd(x) >= 0)) then true else false) 

(*
    Esta función recibe una lista de posiciones ruta y una 
    matriz matrix de listas de caracteres. 
    Devuelve una lista con las extensiones de ruta, es decir, 
    las posibles posiciones a las que 
    se puede avanzar a partir de la última posición en ruta. 
    Para hacer esto, primero se obtiene la lista de vecinos de 
    la última posición en ruta. 
    Luego se filtran aquellos vecinos que ya se encuentran en ruta. 
    Finalmente, se agrega cada vecino restante al inicio de ruta 
    para obtener una nueva lista de posiciones.
*)
let extender (ruta: _ list) (matrix: _ list list) =
    //printfn "Extender: %A" ruta
    (vecinos ruta.Head matrix)
    |> List.map (fun x -> if (miembro x ruta) then [] else x::ruta)
    |> List.filter (fun x -> x <> [])

(*
    Esta función recibe una posición (i,j), un carácter needed y 
    una matriz matrix de listas de caracteres. 
    Devuelve un valor booleano indicando si el elemento en la 
    posición (i,j) de la matriz es igual a needed.
*)
let verificarCorrespondencia (posicion:(int*int)) needed (matrix: _ list list) =
    // printfn "Needed: %A" needed
    // printfn "Elemento: %A" (getElement (fst(posicion)) (snd(posicion)) matrix)

    if (getElement (fst(posicion)) (snd(posicion)) matrix) = needed then
        true
    else
        false
(*
    La función prof_aux es la implementación de la búsqueda en profundidad. 
    Toma una lista de rutas, una posición de origen, un objetivo, un índice y 
    una matriz como entrada. 
    La función selecciona la primera ruta en la lista de rutas, la extiende y 
    agrega las nuevas rutas a la lista. 
    Si la ruta extendida alcanza el objetivo, se devuelve la ruta. 
    Si no, se selecciona la siguiente ruta en la lista y se repite el proceso. 
    Si no hay más rutas en la lista, la búsqueda falla y devuelve una lista vacía.
*)
let rec prof_aux (rutas: _ list list) (origin:('a*'b)) (goal:_ list) index (matrix: _ list list) =
    printfn "\nRutas: \n%A" rutas.Head
    if rutas = [] then
        []
    elif (rutas.Head.Length = goal.Length)  then
        List.rev rutas.Head

        (*List.append
        ([List.rev rutas.Head])
        (prof_aux rutas.Tail
             (findFirstLetterPosition (fst(origin)) (snd(origin)+1) goal[0] matrix) 
             goal 0 matrix)*)

    elif (verificarCorrespondencia rutas.Head.Head (goal[index]) matrix) then
        prof_aux (List.append (extender rutas.Head matrix) rutas.Tail) origin goal (index+1) matrix
    else
        if snd(rutas.Head.Head) < matrix[0].Length then
            printfn "Siguiente letra: %A" 
            prof_aux [[findFirstLetterPosition (fst(origin)) (snd(origin)+1) goal[0] matrix]]
                (findFirstLetterPosition (fst(origin)) (snd(origin)+1) goal[0] matrix)
                goal 0 matrix
        else
            prof_aux [[findFirstLetterPosition (fst(origin)+1) 0 goal[0] matrix]]
                (findFirstLetterPosition (fst(origin)+1) 0 goal[0] matrix)
                goal 0 matrix

(*
    Finalmente, la función prof toma un objetivo y una matriz como 
    entrada y devuelve la ruta que se encontró para ese objetivo utilizando la búsqueda en profundidad.
*)  
let prof (goal:char list) (matrix: _ list list) =
    let posicionArranque = findFirstLetterPosition 0 0 goal[0] matrix
    prof_aux [[posicionArranque]] posicionArranque goal 0 matrix




//Pan - CASA
printfn "%A" (prof objetivo palabras)

// //Patio - Cama
printfn "%A" (prof objetivo1 palabras)

// //Casa -  MASA
// printfn "%A" (prof objetivo2 palabras)

// //Alma - YO
//printfn "%A" (prof objetivo3 palabras)

// //Tel
//printfn "%A" (prof objetivo4 palabras)

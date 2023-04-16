namespace PalabrasEnredadas


open System
open Models
open Android.Views
open Android.Widget
open Android.Graphics
open System.Threading.Tasks



module GameLogic =

    

    // **************************************************************** Generador de matrices de Palabras Enredadas ****************************************************************

    module Array2D = 
        let toListofLists (arr2d: 'a [,]) =
            [ yield! [arr2d.GetLowerBound(0)..arr2d.GetUpperBound(0)] |> List.map(fun i -> arr2d.[i,*] |> List.ofArray) ]

    //Genera una matriz de NxN con N=gridSize con las palabras que se le pasan como parámetro
    let generateWordSearch (words: string list) (gridSize: int) =
        let rand = System.Random()
        let directionVectors = [(0, 1); (1, 0); (1, 1); (0, -1); (-1, 0); (-1, -1); (-1, 1); (1, -1)]
        let matrix = Array2D.create gridSize gridSize ' '

        let rec tryPlaceWord(word: string) =
            let x = rand.Next(0, gridSize)
            let y = rand.Next(0, gridSize)
            let direction = directionVectors.[rand.Next(0, directionVectors.Length)]
            if x + fst(direction) * (word.Length-1) >= gridSize || y + snd(direction) * (word.Length-1) >= gridSize ||
               x + fst(direction) * (word.Length-1) < 0 || y + snd(direction) * (word.Length-1) < 0 then
                tryPlaceWord(word)
            else
                let mutable success = true
                for i in 0..(word.Length-1) do
                    if matrix.[x + fst(direction)*i, y + snd(direction)*i] <> ' ' && matrix.[x + fst(direction)*i, y + snd(direction)*i] <> word.[i] then
                        success <- false
                if success then
                    for i in 0..(word.Length-1) do
                        matrix.[x + fst(direction)*i, y + snd(direction)*i] <- word.[i]
                else
                    tryPlaceWord(word)

        for word in words do
            tryPlaceWord(word)

        for i in 0..(gridSize-1) do
            for j in 0..(gridSize-1) do
                if matrix.[i, j] = ' ' then
                    matrix.[i, j] <- (char)(rand.Next(65, 91))
                
        matrix


    //Busca la palabra más larga para generar la matriz
    let getBiggestWordLength (words: string list) =
        let mutable biggestWordLength = 0
        for word in words do
            if word.Length > biggestWordLength then
                biggestWordLength <- word.Length
        biggestWordLength

    //Reviza si una palabra tiene un lenght menor a un numero
    let checkWordLength (word: string) (length: int) =
        word.Length < length

    //Toma un string y lo devuelve como un array de caracteres
    let stringToCharList (word: string) =
        Seq.toList word


    // **************************************************************** Encontrar Palabra en Matriz ****************************************************************
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
            if matrix.[i].Length > j then
                matrix.[i].[j]
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

        if matrix.[i].[j] = letter then
            (i,j)
        elif (i = matrix.Length-1) && (j = matrix.[i].Length-1) then
            (-1,-1)
        else
            if j = matrix.[i].Length-1 then
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
        |> List.filter (fun x -> if ((fst(x) >= 0)&&(fst(x) < matrix.Length)) && ((snd(x) < (matrix.[0].Length))&&(snd(x) >= 0)) then true else false) 

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

    //Una funcion que verifique que todos los elementos de una lista de caracteres esten en otra lista de caracteres
    let  verificarPalabra (listaA: _ list) (listaB: _ list)  (matrix: _ list list)=
      let A = List.sort listaA
      let B = List.sort ( List.map (fun x -> getElement (fst(x)) (snd(x)) matrix) listaB)
      List.forall2 (fun elem1 elem2 -> elem1 = elem2) A B
  

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
        //printfn "\nRutas: %A" rutas

        //Si rutas es vacio entonces busca otra posicion de inicio
        if rutas = [] then
            //printfn "No hay mas rutas"
            if snd(origin) + 1 < matrix.[0].Length then
                        printfn "Origin: (%d,%d)" (fst(origin)) (snd(origin))
                        prof_aux [[findFirstLetterPosition (fst(origin)) (snd(origin)+1) goal.[0] matrix]]
                            (findFirstLetterPosition (fst(origin)) (snd(origin)+1) goal.[0] matrix)
                            goal 0 matrix
            else
                prof_aux [[findFirstLetterPosition (fst(origin)+1) 0 goal.[0] matrix]]
                    (findFirstLetterPosition (fst(origin)+1) 0 goal.[0] matrix)
                    goal 0 matrix

        //Si retorna (-1,-1) entonces no encontro otro inicio 
        elif (fst(rutas.Head.Head) = -1) && (snd(rutas.Head.Head) = -1) then
          []
        //Si el tamaño de la ruta es igual al tamaño del objetivo entonces retorna la ruta
        elif (rutas.Head.Length = goal.Length) && (verificarPalabra goal rutas.Head matrix) then
            List.rev rutas.Head

            (*List.append
            ([List.rev rutas.Head])
            (prof_aux rutas.Tail
                 (findFirstLetterPosition (fst(origin)) (snd(origin)+1) goal[0] matrix) 
                 goal 0 matrix)*)

        //Si la posicion actual es igual a la letra del objetivo entonces sigue buscando por esa ruta
        elif (verificarCorrespondencia rutas.Head.Head (goal.[index]) matrix) then
            //printfn "buscando %A" goal[(index)]
            prof_aux (List.append (extender rutas.Head matrix) rutas.Tail) origin goal (index+1) matrix
        else
            //printfn "buscando %A" goal[(index)]
            //prof_aux (List.append (extender rutas.Head matrix) rutas.Tail) origin goal (index) matrix
            prof_aux rutas.Tail origin goal index matrix

    (*
        Finalmente, la función prof toma un objetivo y una matriz como 
        entrada y devuelve la ruta que se encontró para ese objetivo utilizando la búsqueda en profundidad.
    *)  
    let prof (goal:char list) (matrix: _ list list) =
        let posicionArranque = findFirstLetterPosition 0 0 goal.[0] matrix
        prof_aux [[posicionArranque]] posicionArranque goal 0 matrix


    // **************************************************************** Utilitarias ****************************************************************


    //Reviza si las letras que el usaurio ha elegido se encuentran en alguna palabra
    let rec allCharsInWords letters words =
      match letters with
      | [] -> true
      | h::t -> 
          words |> List.exists (fun (w:string) -> w.Contains(h.ToString())) && allCharsInWords t words
    
    //Se encarga de revizar sio dos palabras son iguales
    let  wordExists(gameWord: _ list) (playerWord: _ list)  =
      let A = List.sort gameWord
      let B = List.sort playerWord
      if A.Length = B.Length then (List.forall2 (fun elem1 elem2 -> elem1 = elem2) A B)
      else
        false


    let colorTrail (positions: (int * int) list) (gameMatrix: TableLayout) ( color: Android.Graphics.Color ) =
        //let mutable color = Android.Graphics.Color.Brown
        //match state with
        //| "correct" -> color   <- Android.Graphics.Color.Green
        //| "incorrect" -> color <- Android.Graphics.Color.White
        //| "ok" -> color <- Android.Graphics.Color.White
        //| _ -> printfn "Incorrect Option"

        List.iter (fun x ->
            let (fila:TableRow) =  gameMatrix.GetChildAt( fst(x) ):?> TableRow
            let (celda:TextView) = fila.GetChildAt( snd(x) ) :?> TextView
            celda.SetBackgroundColor(color)
        ) positions

    //Borra una palabra de una lista de palabras
    //Esto para borrar la palabra cuando la encuentra
    let rec deleteWord (word:string) (wordList: string list) =
        match wordList with
        | [] -> []
        | h::t when (Seq.sort h).ToString() = (Seq.sort word).ToString() -> t
        | h::t -> h :: deleteWord word t

    //Se encarga de revizar que la palabra que el usuario digitó se encuentra en la lista de letras
    //Cambia el color de las letras de la matriz segun corresponda
    let checkWord (words: byref<string list>) (letters: byref<char list>) (positions:byref< (int * int) list>) (gameMatrix: TableLayout) =
      printfn "Comparando las letras: %A" letters
      printfn "Posiciones: %A" positions
      printfn "Para ver si existen en: %A" words
      let lettersAux = letters
      let wordsAux = words

      //Si las letras existen en alguna palabra
      if (allCharsInWords lettersAux wordsAux) then
        //Reviza si esa letras conforman una palabra, si ya alcanzó el length
        if List.exists (fun (word:string) -> (wordExists lettersAux (Seq.toList word))) wordsAux then
          printfn "Palabra Adivinada - los pinta de verde"
          colorTrail positions gameMatrix Android.Graphics.Color.Green
          //let sinPalabra = deleteWord (System.String(lettersAux |> List.toArray)) wordsAux
          printfn "Sin Palabra "
          words       <- deleteWord (System.String(lettersAux |> List.toArray)) wordsAux //Elimina la palabra de la lista
          letters     <- []                                                              //Elimina las letras 
          positions   <- []                                                              //Elimina las posiciones
          true
        else
          printfn "Siga adivinando la palabra - Los deja en azul"
          true
      else 
        printfn "Palabra Incorrecta - los pinta de nuevo de blanco"
        colorTrail positions gameMatrix Android.Graphics.Color.White//Cambia al color original 
        letters   <- []                                             //Elimina las letras 
        positions <- []                                             //Elimina las posiciones
        false


    //FUNCION QUE RESUELVE PALABRAS ENREDADAS AUTOMATICAMENTE
    let solvePalabrasEnredadas (gameMatrix: char list list) (wordList: byref<string list>) (gameTableMatrix: TableLayout) =
        printfn"Iniciando proceso de resolver"

        for element in wordList do
            let mutable wordListaux = wordList
            let mutable goal = ( stringToCharList element )
            let mutable solution = [] 
            try
                solution <- prof  goal gameMatrix
                wordList <- deleteWord (System.String( goal |> List.toArray)) wordListaux
                colorTrail solution gameTableMatrix Android.Graphics.Color.Purple
            with
            | :? System.IndexOutOfRangeException -> printfn "No se encontro la palabra"
            | :? System.ArgumentException -> printfn "The index was outside the range of elements in the list."

            





    // printfn "%b" (allCharsInWords ['a'; 'b';'d';'c'] ["hello"; "abdc"] )
    
    // printfn "%b" (verificarPalabra ['a'; 'c';'b';'d'] (Seq.toList "abcd") )
   
    //let words = ["ALMA"; "PATIO"; "CASA";"TELEFONO"; "COCHE";"ALAJUELA";"LAGO"]
    //let matrix = generateWordSearch words 10 |> Array2D.toListofLists
    //printfn "%A" matrix


    
    //let solution = prof ( stringToCharList "ALAJUELA" ) matrix
    //printfn "%A" solution




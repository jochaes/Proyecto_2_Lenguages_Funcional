namespace PalabrasEnredadas


open System
open Models
open Android.Views
open Android.Widget
open Android.Graphics
open System.Threading.Tasks
open System


module GameLogic =

    

    // **************************************************************** Generador de matrices de Palabras Enredadas ****************************************************************

    module Array2D = 
        let toListofLists (arr2d: 'a [,]) =
            [ yield! [arr2d.GetLowerBound(0)..arr2d.GetUpperBound(0)] |> List.map(fun i -> arr2d.[i,*] |> List.ofArray) ]

    //Función Generada por ChatGPT
    (*
        Función que se encarga de generar una matriz de Sopa de Letras NxN con un conjunto de palabras
        words: La lista de palabras que van a estar en la sopa de letras
        gridSize: El tamaño de la matriz

        Esta función fue generada por ChatGpt, cómo respuest de la siguiente solicitud:

        I need an F# function that can generate a word search matrix, given a set of different length words

        Chat:
        Here's an explanation of how this function works:
        
        The function takes a list of words as input.
        The function creates an empty 20x20 word search matrix using Array2D.create.
        The function defines a tryPlaceWord function that attempts to place a word on the matrix in a random position and direction.
        The function then loops through the list of words, calling tryPlaceWord for each word.
        After placing all the words, the function fills in the remaining spaces in the matrix with random letters.

        Finally, the function returns the completed matrix.
        Note that this function places words randomly on the matrix, so you may get different results each time you run it with the same input list.
        You can adjust the gridSize parameter to create larger or smaller word search matrices.

    *)
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


    (*
        No está en uso, la matriz siempre es de 10x10 para que se vea bien en movil

        Toma una lista de estrings y retorna el size de la palabra más larga
        words: Lista de Strings con palabras que se van a agregar a la matriz

    *)
    let getBiggestWordLength (words: string list) =
        let mutable biggestWordLength = 0
        for word in words do
            if word.Length > biggestWordLength then
                biggestWordLength <- word.Length
        biggestWordLength


    //Reviza si una palabra tiene un lenght menor a un numero
    (*
        No está en uso

        Verifica que una palabra tenga un length concreto
        word: Palabra a verificar
        length: Tamaño que debe tener la palabra
    *)
    let checkWordLength (word: string) (length: int) =
        word.Length < length

    (*
        Toma un string y l ocnvierte a una sequencia
        word: Palabra a convertir en secuencia
    *)
    let stringToCharList (word: string) =
        Seq.toList word


    // **************************************************************** Encontrar Palabra en Matriz ****************************************************************
    (*
        Esta funciíon verifica si un elemento concreto ya se encuentra o no
        en una lista. 

        e: elemento a buscar en la lista
        lista: lista con las diferentes "rutas"

    *)
    let miembro e lista =
        lista
        |> List.map (fun x -> x = e)
        |> List.reduce (fun x y -> x || y)
    
    (*
        Devuelve el elemento (i,j) de la matriz,
        Si el elemento solicitado está fuera de ij devuelve _
        matrix: Matriz de la sopa de letras
        i: fila
        j: columna

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
        Esta funcion se encarga de buscar la primera aparición,
        de la primera letra, de la palabra que se está buscando.

        Iniciadno desde un lugar específico de la matriz.
        Esto por que si en una instancia no encunetra la palabra,
        debe buscar otra letra para tratar de buscar la palabra.

        i: fila de inicio
        j: columna de inicio
        letter: Letra a buscar
        matrix: matriz de la sopa de letras 
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
        Devuelve una lista con las posiciones de los vecinos  
        de (x,y) en la matriz.

        Los vecinos van en el siguiente orden:
        //left, upper_left_corner, up , upper_right_corner, right, lower_right_corner, down, lower_left_corner

        posicion: Lugar de donde se van a sacar los vecinos
        matrix 

    *)
    let vecinos_aux (posicion: (int*int)) =
        let i,j = fst(posicion),snd(posicion)

        //left, upper_left_corner, up , upper_right_corner, right, lower_right_corner, down, lower_left_corner
        [(i,j-1); (i-1,j-1); (i-1,j); (i-1,j+1); (i,j+1); (i+1,j+1); (i+1,j); (i+1,j-1)]

    (*
        Esta función recibe una posición (x,y) y una matriz matrix 
        de listas de caracteres. 
        Devuelve una lista con las posiciones de los vecinos de (x,y) dentro de la matrix

        Filtra todos aquellos vecinos que no estéb dentro de los limites 
    *)
    let vecinos (posicion: (int*int)) (matrix: _ list list) =
        (vecinos_aux posicion)
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

        ruta: Ruta que se quiere extender
        matrix: Matriz de l asopa de letras 
    *)
    let extender (ruta: _ list) (matrix: _ list list) =
        //printfn "Extender: %A" ruta
        (vecinos ruta.Head matrix)
        |> List.map (fun x -> if (miembro x ruta) then [] else x::ruta)
        |> List.filter (fun x -> x <> [])

    (*
        Devuelve un valor booleano indicando si el elemento en la 
        posición (i,j) de la matriz es igual al que se necesita.
        posicion: posicion a verificar
        needed: caracter
        matrix: matriz de la sopa de letras
    *)
    let verificarCorrespondencia (posicion:(int*int)) needed (matrix: _ list list) =
        // printfn "Needed: %A" needed
        // printfn "Elemento: %A" (getElement (fst(posicion)) (snd(posicion)) matrix)

        if (getElement (fst(posicion)) (snd(posicion)) matrix) = needed then
            true
        else
            false

    
    (*
        Función que verifica que todos los elementode una lista de caracteres esten en otrras lisa de caracteres
        Esto para verificar que cuando encuentre un "camino" que aparte de teber el mismo size que la palabra,
        también sea la palabra

        ListaA: Palabra
        ListaB: "camino" secuencia de caracteres
        matrix: matriz de la sopa de letras
    *)
    let  verificarPalabra (listaA: _ list) (listaB: _ list)  (matrix: _ list list)=
      let A = List.sort listaA
      let B = List.sort ( List.map (fun x -> getElement (fst(x)) (snd(x)) matrix) listaB)
      List.forall2 (fun elem1 elem2 -> elem1 = elem2) A B
  

    (*
        La función prof_aux es la implementación de la búsqueda en profundidad. 
        La función selecciona la primera ruta en la lista de rutas, la extiende y 
        agrega las nuevas rutas a la lista. 
        Si la ruta extendida alcanza el objetivo, se devuelve la ruta. 
        Si no, se selecciona la siguiente ruta en la lista y se repite el proceso. 
        Si no hay más rutas en la lista, la búsqueda falla y devuelve una lista vacía.

        rutas: Lista de "rutas"
        origin: punto inicial para buscar la palabra
        goal: Palabra objetivo a encontrar en la matriz
        index: indice de la letra que se está verificando en el momento
        matrix: matriz en donde va a buscar la palabra
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

        //Si la posicion actual es igual a la letra del objetivo entonces sigue buscando por esa ruta
        elif (verificarCorrespondencia rutas.Head.Head (goal.[index]) matrix) then
            //printfn "buscando %A" goal[(index)]
            prof_aux (List.append (extender rutas.Head matrix) rutas.Tail) origin goal (index+1) matrix
        else
            //printfn "buscando %A" goal[(index)]
            //prof_aux (List.append (extender rutas.Head matrix) rutas.Tail) origin goal (index) matrix
            prof_aux rutas.Tail origin goal index matrix

    (*
        Esta función devuelve la ruta que se encontró para ese objetivo utilizando la búsqueda en profundidad.

        obetivo: Lista de caracteres de la palabra a localizar en la matriz
        matrix: Matriz en dónde va a buscar la palabra

    *)  
    let prof (goal:char list) (matrix: _ list list) =
        let posicionArranque = findFirstLetterPosition 0 0 goal.[0] matrix
        prof_aux [[posicionArranque]] posicionArranque goal 0 matrix


    // **************************************************************** Utilitarias ****************************************************************


    //Reviza si las letras que el usaurio ha elegido se encuentran en alguna palabra
    (*
      
        Esta función reviza en que palabra existen las letras que el usuario ha eligido hasta el momento.
        letters: Lista de caracteres
        words: Lista de palabras
    *)
    let rec allCharsInWords letters words =
      match letters with
      | [] -> true
      | h::t -> 
          words |> List.exists (fun (w:string) -> w.Contains(h.ToString())) && allCharsInWords t words
    
    //Se encarga de revizar si dos palabras son iguales
    (*
        Esta función reviza si dos palabras son iguales,
        esto porque el usuairo puede elegir las palabras al azar,
        y al comparar puede generar errores.

        gameWord: Palabra del juego
        playerWord: Palabra que ha elejido el usuario

    *)
    let  wordExists(gameWord: _ list) (playerWord: _ list)  =
      let A = List.sort gameWord
      let B = List.sort playerWord
      if A.Length = B.Length then (List.forall2 (fun elem1 elem2 -> elem1 = elem2) A B)
      else
        false

    (*
        Colorea una celda de un color especifico
        position: Posicion de la celda
        gameMatryx: Layout en donde esta la celda
        color: color que se va a pintar

    *)
    let colorCell ( position: (int*int)) (gameMatrix: TableLayout) ( color: Android.Graphics.Color ) =
        let (fila:TableRow) =  gameMatrix.GetChildAt( fst(position) ):?> TableRow
        let (celda:TextView) = fila.GetChildAt( snd(position) ) :?> TextView
        celda.SetBackgroundColor(color)

    (*
        Colorea una secuencia de posiciones de un colos especifico
        positions: Lista de posiciones
        gameMatryx: Layout en donde esta la celda
        color: color que se van a pintar

    *)
    let colorTrail (positions: (int * int) list) (gameMatrix: TableLayout) ( color: Android.Graphics.Color ) =
        //let mutable color = Android.Graphics.Color.Brown
        //match state with
        //| "correct" -> color   <- Android.Graphics.Color.Green
        //| "incorrect" -> color <- Android.Graphics.Color.White
        //| "ok" -> color <- Android.Graphics.Color.White
        //| _ -> printfn "Incorrect Option"

        List.iter (fun x ->
            colorCell x gameMatrix color
        ) positions

    (*
        Estas 2 funciones se encargan de elimnar la palabra que ya se encomntró de la list de palabras
        listEqual: Verifica que 2 listas sean iguales
        deleteWord: Recorre una lista de palabras y las compara con la palabra que debe borrar de la lista

    *)
    let listEqual list list2 = List.forall2 (fun elem1 elem2 -> elem1 = elem2) list list2 
    let deleteWord (word : string) (wordList : string list) =
      let removeStr = List.sort ( word.ToCharArray() |> List.ofArray ) 
      //wordList |> List.filter (fun (s:string) ->  List.sort ( s.ToCharArray() |> List.ofArray ) <> removeStr)
      wordList |> List.filter ( fun (s:string) -> not (listEqual (List.sort ( s.ToCharArray() |> List.ofArray ) ) removeStr ))
    
    (*
        Se encarga de revizar que la palabra que el usuario digitó se encuentra en la lista de letras
        Cambia el color de las letras de la matriz segun corresponda
        words: Lista de palabras
        letters: Letras que ha elegido el usuario
        posiitons: posiciones de las letras
        gameMatrix: Table layout de la matrix UI
        
    *)
    let checkWord (words: byref<string list>) (letters: byref<char list>) (positions:byref< (int * int) list>) (gameMatrix: TableLayout) (context) =
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

        let toast = Toast.MakeText(context, "Esa palabra no existe", ToastLength.Short)
        toast.SetGravity(GravityFlags.Center,0,0)
        toast.Show()
        false


    
    (*
      FUNCION QUE RESUELVE PALABRAS ENREDADAS AUTOMATICAMENTE

      gameMatrix: Matrix del juego
      wordList: Lista de palabras que quedan de encontrar
      gameTableMatrix: Elemento UI que se va a actualizar
    *)
    let solvePalabrasEnredadas (gameMatrix: char list list) (wordList: byref<string list>) (gameTableMatrix: TableLayout) =
        printfn"Iniciando proceso de resolver"

        //Va palabra por palabra
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


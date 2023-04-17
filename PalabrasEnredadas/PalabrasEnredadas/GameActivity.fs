namespace PalabrasEnredadas

open System
open System.Collections.Generic
open System.Linq
open System.Text

open Android.App
open Android.Content
open Android.OS
open Android.Runtime
open Android.Views
open Android.Widget
open Newtonsoft.Json
open Models
open GameLogic

[<Activity (Label = "GameActivity")>]
type GameActivity() =

    inherit Activity()

    //Variables
    let mutable fromChooseWords: string = ""
    let mutable wordList : GameModel = {words = [""]}

    //Lista de letras que el usuario va presionando 
    let mutable letterList : char list = []
    //Posiicones de las letras que el usuario va presionando
    let mutable letterPositions : (int*int) list = []

    //Funciones
    (*
        Actualiza el TextView que indica las letras que faltan por encontrar
    *)
    let updateWordsTextView (wordsList: string list) (textView: TextView) =
        printfn "Actualizando TV con: %A" wordList
        textView.Text <- ""
        List.iter (fun x -> textView.Text <- textView.Text + " - " + x) wordsList

    (*
        Función que se ejecuta cada vez que el usuairo presiona una letra desde el UI

    *)
    let tableAux (position: (int* int)) (letter: char) (gameMatrix: TableLayout) (wordsTextView: TextView) (context)=

        letterList <- List.append letterList [letter]               //Guarda la letra 
        letterPositions <- List.append letterPositions [position]   //Guarda la posicion de la letra

        printfn "************************************"
        printfn "Vamos a revizar: letra(%A) posicion(%A)" letter position

        checkWord &wordList.words &letterList &letterPositions gameMatrix context |> ignore //Reviza la palabra 
        updateWordsTextView wordList.words  wordsTextView                                   //Actualiza el TV que muestra las palabras restantes

        //if not(checkWord &wordList.words &letterList &letterPositions gameMatrix) then
        //    letterList <- []
        //    letterPositions <- []
        //else
        //    wordList.words <- deleteWord ( System.String(letterList |> List.toArray) ) wordList.words
        //    updateWordsTextView wordList.words  wordsTextView
        printfn "************************************"

        if wordList.words = [] then Models.showAlertAsync "Juego Terminado" "Has Encontrado las Palabras Enredadas" context //Si ya no quedan palabras el juego ha termiando 




    (*
        Función que se encarga de crear la Matriz en el UI
        matrix: MAtriz de sopa de letras
        tableLayout: El layout donde va a construir la matriz
        wordsTextView: Cartel que muestra las letras restanets
        context: Este Activity

        Esta función también fue generada por ChatGPT, con la  siguiente solicitud:
        How will you represent a NxM char matrix in the activity's UI and when you touch one of the matrix's boxes the box will change it's color to blue?

        Chat:
        In this example, we use a TableLayout to represent the character matrix.
        We first create a 2D array to store the cells of the matrix, and then loop
        through the rows and columns to create the TextView cells, setting their
        properties and adding a click handler to change their background color to blue when they are clicked.
        
        Finally, we return the TableLayout along with the array of cells so
        that we can access and manipulate them later on.
        
        You can then add the TableLayout to your existing Axel by calling the
        createMatrixUI function and adding the returned TableLayout to your existing UI layout using the AddView method,
    *)
    let addMatrixToTable (matrix: char list list) ( tableLayout: TableLayout) (wordsTextView:TextView) (context)=
        
        let numRows = matrix.Length
        let numCols = matrix.[0].Length

        for numRows = 0 to (numRows - 1) do
            let tableRow = new TableRow(tableLayout.Context)
            for col = 0 to (numCols - 1) do

                let cellValue = matrix.[numRows].[col]
                let tableCell = new TextView(tableLayout.Context)

                
                tableCell.SetBackgroundColor(Android.Graphics.Color.White)
                tableCell.SetTextColor(Android.Graphics.Color.Black)
                tableCell.SetText(cellValue.ToString(), TextView.BufferType.Normal)
                tableCell.SetPadding(20,20,20,20)
                tableCell.Gravity <- GravityFlags.CenterVertical
    
                //Añade la función de verificar a cada celda
                tableCell.Click.Add( fun args ->
                    
                    tableCell.SetBackgroundColor(Android.Graphics.Color.Blue)
                    tableAux (numRows, col) cellValue tableLayout wordsTextView context

                )

                tableRow.AddView(tableCell)

            tableLayout.AddView(tableRow)

               
    override x.OnCreate(bundle) =
        
        base.OnCreate (bundle)
        // Create your application here
        x.SetContentView(Resources3.Layout.Game)
        x.ActionBar.Hide()

        

        //Recibe un valor de Choose Game
        wordList <- JsonConvert.DeserializeObject<GameModel>(x.Intent.GetStringExtra("GameModel"))
        printfn "Iniciando el juego con las palabras: %A" wordList

        //Components
        let words_tv = x.FindViewById<TextView>(Resources3.Id.g_allWords_tv) 
        let gameMatrix = x.FindViewById<TableLayout>(Resources3.Id.g_Matrix_tl)
        let solve_Btn = x.FindViewById<Button>(Resources3.Id.g_Solve_btn)

        //Llene el Text View con las palabras
        updateWordsTextView wordList.words words_tv


        //Actions

        //Agrega una matriz de 10x10 con las palabras elegidas por el usuario 
        let matrix = generateWordSearch wordList.words 10 |> Array2D.toListofLists
        addMatrixToTable matrix gameMatrix words_tv x


        //Ejecuta la funcionalidad de resolver Palabras Enredadas
        solve_Btn.Click.Add( fun _ ->
            printfn "BOTON INICIANDO "
            solvePalabrasEnredadas matrix &wordList.words gameMatrix

            updateWordsTextView wordList.words  words_tv
            if wordList.words = [] then Models.showAlertAsync "Juego Terminado" "Ha Ganado el Juego" x
        )

        //Cambiar el color de la celda [0,1]
        //let fila =  gameMatrix.GetChildAt(0) :?> TableRow
        //let celda = fila.GetChildAt(1) :?> TextView
        //celda.SetText("H", TextView.BufferType.Normal)



        

    

        
    


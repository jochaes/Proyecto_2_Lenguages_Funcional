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
    //Functions

    //Crea la matriz del Juego
    let addMatrixToTable (matrix: char list list) ( tableLayout: TableLayout) =
        
        //let numRows = matrix.GetLength(0)
        //let numCols = matrix.GetLength(1)

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
                //tableCell.SetForegroundGravity(GravityFlags.CenterVertical)
                tableCell.Gravity <- GravityFlags.CenterVertical
    

                tableCell.Click.Add( fun args ->
                    tableCell.SetBackgroundColor(Android.Graphics.Color.Blue)
                )

                tableRow.AddView(tableCell)

            tableLayout.AddView(tableRow)

               
    override x.OnCreate(bundle) =
        
        base.OnCreate (bundle)
        // Create your application here
        x.SetContentView(Resources3.Layout.Game)
        x.ActionBar.Hide()
        //ActionBarNavigationMode.SetHasNavigationBar(x, false);

        //Recibe un valor de Choose Game
        wordList <- JsonConvert.DeserializeObject<GameModel>(x.Intent.GetStringExtra("GameModel"))
        printfn "Iniciando el juego con las palabras: %A" wordList

        //Components
        let words_tv = x.FindViewById<TextView>(Resources3.Id.g_allWords_tv) 
        let gameMatrix = x.FindViewById<TableLayout>(Resources3.Id.g_Matrix_tl)
        let solve_Btn = x.FindViewById<Button>(Resources3.Id.g_Solve_btn)
        //Llene el Text View con las palabras
        words_tv.Text <- ""
        List.iter (fun x -> words_tv.Text <- words_tv.Text + " - " + x) wordList.words


        //Actions

        //Agrega una matriz de 10x10 con las palabras elegidas por el usuario 
        let matrix = generateWordSearch wordList.words 10 |> Array2D.toListofLists
        addMatrixToTable matrix gameMatrix

        //Ejecuta la funcionalidad de resolver Palabras Enredadas
        
        //let context = x.Application.ApplicationContext
        solve_Btn.Click.Add( fun _ -> (Models.showAlert "hola" "adios" x)  )


        //Cambiar el color de la celda [0,1]
        //let fila =  gameMatrix.GetChildAt(0) :?> TableRow
        //let celda = fila.GetChildAt(1) :?> TextView
        //celda.SetText("H", TextView.BufferType.Normal)



        

    

        
    


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


[<Activity (Label = "ChooseWordsActivity")>]
type ChooseWordsActivity() =
  inherit Activity()

  let mutable fromMain: GreetingModel  = {greeting = ""}

  let mutable words: string = ""
  //let mutable wordList : string list = []

  let mutable modelWordList : GameModel = { words = [""] }

  

  override x.OnCreate(bundle) =
    base.OnCreate (bundle)

    // Create your application here
    x.SetContentView (Resources2.Layout.ChooseWords)

    //Recibe un GreetingModel desde el main
    fromMain <-  JsonConvert.DeserializeObject<GreetingModel>(x.Intent.GetStringExtra("Greeting"))
    printfn "from main: %A" fromMain

    

    //Componentes
    let title_tv = x.FindViewById<TextView>(Resources2.Id.cw_title_tv)

    let words_tv = x.FindViewById<TextView>(Resources2.Id.cw_words_tv)

    let wordsInput_et = x.FindViewById<EditText>(Resources2.Id.cw_wordsInput_et)

    let startGame_btn = x.FindViewById<Button>(Resources2.Id.cw_StartGame_btn)


    //Actions
    wordsInput_et.EditorAction.Add (
        fun args ->
            
            args.Handled <- false
            if args.ActionId = InputMethods.ImeAction.Done then
               args.Handled <- true
               //Agrega la palabra al modelo
               modelWordList.words <- List.append modelWordList.words [wordsInput_et.Text]
               words <- words + wordsInput_et.Text + ", "
               words_tv.Text <- words
               
               printfn "Palabra Nueva: %s" wordsInput_et.Text
               printfn "Las Palabras son: %A" modelWordList.words
               
               wordsInput_et.Text <- ""
    )

    startGame_btn.Click.Add( fun args ->

        let intent = new Intent(x, typedefof<GameActivity>)
        intent.PutExtra("GameModel", JsonConvert.SerializeObject(modelWordList))|> ignore
        x.StartActivity(intent)
        
    )

       


    


    
    




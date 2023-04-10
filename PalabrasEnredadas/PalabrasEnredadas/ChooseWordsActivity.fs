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


[<Activity (Label = "ChooseWordsActivity")>]
type ChooseWordsActivity() =
  inherit Activity()

  let mutable fromMain: string  = ""
  let mutable words: string = ""
  let mutable wordList : string list = []

  override x.OnCreate(bundle) =
    base.OnCreate (bundle)

    // Create your application here
    x.SetContentView (Resources2.Layout.ChooseWords)

    //Recibe un valor de main
    fromMain <- x.Intent.GetStringExtra("Greeting")

    //Componentes
    let title_tv = x.FindViewById<TextView>(Resources2.Id.cw_title_tv)

    let words_tv = x.FindViewById<TextView>(Resources2.Id.cw_words_tv)

    let wordsInput_et = x.FindViewById<EditText>(Resources2.Id.cw_wordsInput_et)

    wordsInput_et.EditorAction.Add (
        fun args ->
            
            args.Handled <- false
            if args.ActionId = InputMethods.ImeAction.Done then
               args.Handled <- true
               wordList <- List.append wordList [wordsInput_et.Text]
               words <- words + wordsInput_et.Text + ", "
               words_tv.Text <- words
               
               printfn "Palabra Nueva: %s" wordsInput_et.Text
               printfn "Las Palabras son: %A" wordList
               
               wordsInput_et.Text <- ""
    )

       


    


    
    




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

  //Variables
  let mutable fromMain: GreetingModel  = {greeting = ""}
  let mutable modelWordList : GameModel = { words = [""] }

  let mutable wordPosition  = 0
  let mutable wordUpdate = false

  

  override x.OnCreate(bundle) =
    base.OnCreate (bundle)

    // Create your application here
    x.SetContentView (Resources2.Layout.ChooseWords)
    x.ActionBar.Hide()

    //Recibe un GreetingModel desde el main
    fromMain <-  JsonConvert.DeserializeObject<GreetingModel>(x.Intent.GetStringExtra("Greeting"))
    printfn "from main: %A" fromMain

    

    //Componentes
    let wordsInput_et = x.FindViewById<EditText>(Resources2.Id.cw_wordsInput_et)
    let startGame_btn = x.FindViewById<Button>(Resources2.Id.cw_StartGame_btn)
    let words_list_lv =  x.FindViewById<ListView>(Resources2.Id.cw_words_lv)


    //Actions
    //List View
    //Crea el adapter que carga las palabras en el List view
    let adapter = new ArrayAdapter<string>(x, Android.Resource.Layout.SimpleListItem1, new List<string>())
    words_list_lv.Adapter <- adapter


    words_list_lv.ItemLongClick.Add(
        fun args ->
            let word = adapter.GetItem(args.Position)            
            let toast = Toast.MakeText(x, "Palabra " + word + " eliminada.", ToastLength.Short)
            adapter.Remove(word)
            toast.Show()
    )

    //Cuando da click sobre un item lo carga para modificarlo
    words_list_lv.ItemClick.Add (
        fun args ->
            wordPosition <- args.Position
            wordUpdate <- true
            wordsInput_et.Text <- adapter.GetItem(args.Position)   
    )
  
 
    //Cuando presiona enter agrega una nueva palabra a la lista 
    wordsInput_et.EditorAction.Add (
        fun args ->
            
            args.Handled <- false
            if args.ActionId = InputMethods.ImeAction.Done then
               args.Handled <- true

               // Bloque para modificar una palabra seleccionada del listview
               if wordUpdate then
                    if wordsInput_et.Text.Length = 0 then
                        adapter.Remove(adapter.GetItem(wordPosition))    
                    else
                        adapter.Insert(wordsInput_et.Text, wordPosition)
                        adapter.Remove(adapter.GetItem(wordPosition+1))
                    wordUpdate <- false
                    
                else
                   //Agrega la palabra al modelo
                   //modelWordList.words <- List.append modelWordList.words [wordsInput_et.Text]

                   //Agrega la palabra a la lista de List View mediante el adapter
                   adapter.Insert(wordsInput_et.Text,0)
                   printfn "Palabra Nueva: %s" wordsInput_et.Text
               wordsInput_et.Text <- ""
    )
    
    //Envia las palabras al juego para iniciarse
    startGame_btn.Click.Add( fun args ->

        let intent = new Intent(x, typedefof<GameActivity>)

        
        //Pasa todas las palabras del adapter a la lista
        //Todas las palabras las envia en mayuscula
        modelWordList.words <- []
        for i = 0 to adapter.Count - 1 do
            modelWordList.words <- List.append modelWordList.words [adapter.GetItem(i).ToUpper()]


        intent.PutExtra("GameModel", JsonConvert.SerializeObject(modelWordList))|> ignore
        x.StartActivity(intent)
        
    )

       


    


    
    




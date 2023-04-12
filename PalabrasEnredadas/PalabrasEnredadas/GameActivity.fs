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
open Models
open Newtonsoft.Json

[<Activity (Label = "GameActivity")>]
type GameActivity() =
  inherit Activity()

  //Variables
  let mutable fromChooseWords: string = ""
  let mutable wordList : GameModel = {words = [""]}


  override x.OnCreate(bundle) =
    base.OnCreate (bundle)
    // Create your application here
    x.SetContentView(Resources3.Layout.Game)

    //Recibe un valor de Choose Game
    wordList <- JsonConvert.DeserializeObject<GameModel>(x.Intent.GetStringExtra("GameModel"))
    printfn "Iniciando el juego con las palabras: %A" wordList
    


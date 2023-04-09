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

  override x.OnCreate(bundle) =
    base.OnCreate (bundle)
    // Create your application here
    let mutable greet: string  = ""


    x.SetContentView (Resources2.Layout.ChooseWords)

    greet <- x.Intent.GetStringExtra("Greeting")

    let label = x.FindViewById<TextView>(Resources.Id.textView1)
    label.Text <- greet
    
    




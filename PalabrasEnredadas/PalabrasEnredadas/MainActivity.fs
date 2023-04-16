namespace PalabrasEnredadas

open System

open Android.App
open Android.Content
open Android.OS
open Android.Runtime
open Android.Views
open Android.Widget
open Newtonsoft.Json

open Models

[<Activity (Label = "PalabrasEnredadas", MainLauncher = true, Icon = "@mipmap/icon")>]
type MainActivity () =
    inherit Activity ()

    

    override this.OnCreate (bundle) =

        base.OnCreate (bundle)

        // Set our view from the "main" layout resource
        
        this.SetContentView (Resources.Layout.Main)
        this.ActionBar.Hide()
        let var : GreetingModel = {greeting = "Hola"}
            

        // Get our button from the layout resource, and attach an event to it
        let button = this.FindViewById<Button>(Resources.Id.myButton)
        button.Click.Add (fun args -> 
            //button.Text <- sprintf "%d clicks!" count
            //count <- count + 1

            let intent = new Intent(this, typedefof<ChooseWordsActivity>)
            intent.PutExtra("Greeting", JsonConvert.SerializeObject(var))|> ignore
            this.StartActivity(intent)
        )


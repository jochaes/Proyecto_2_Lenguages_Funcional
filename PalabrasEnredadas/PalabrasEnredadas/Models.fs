namespace PalabrasEnredadas

open Android.App
open Android.Content
open Android.Widget




//Models and aux functionality 
module Models =

    
    //Mensaje de Saludo desde Main Activity to Choose Words 
    type GreetingModel = {
        mutable greeting: string
    }

    //Modelo para enviar la lista de palabras desde ChooseWords a Game
    type GameModel = {
        mutable words: string list
    }


    //Muestra un mensaje en pantalla y devuelve true o false
    //let context = Android.App.Application.Context

    let showAlert (title: string) (message: string) (context) =
        let builder = new AlertDialog.Builder(context)
        builder.SetTitle(title) |> ignore
        builder.SetMessage(message) |>ignore
        builder.SetCancelable(false) |>ignore
        builder.SetPositiveButton("OK", (fun dialog _ -> printfn "Ok %A" dialog )) |>ignore
        builder.SetNegativeButton("Cancel", (fun dialog _ -> printfn "Nope %A" dialog )) |>ignore
        let alert = builder.Create()
        alert.Show() |> ignore
        
       








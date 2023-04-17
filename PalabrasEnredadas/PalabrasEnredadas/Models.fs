namespace PalabrasEnredadas

open Android.App
open Android.Content
open Android.Widget




//Modelos que utiliza la palicación 
module Models =

    
    //Mensaje de Saludo desde Main Activity to Choose Words
    //Parta probar en envío de datos entre actividades
    type GreetingModel = {
        mutable greeting: string
    }

    //Modelo para enviar la lista de palabras desde ChooseWords a Game
    type GameModel = {
        mutable words: string list
    }

    (*
        Muestra un mensaje en la pantalla
        title: Titulo del mensaje
        message:
        context: El activity desde donde se está creando
    *)
    let showAlertAsync (title: string) (message: string) (context)  =
        
        let builder = new AlertDialog.Builder(context)
        builder.SetTitle(title) |> ignore
        builder.SetMessage(message) |>ignore
        builder.SetCancelable(false) |>ignore
        builder.SetPositiveButton("OK", (fun dialog _ -> printfn "OKOKOK" )) |>ignore
        let alert = builder.Create()
        alert.Show() |> ignore

     








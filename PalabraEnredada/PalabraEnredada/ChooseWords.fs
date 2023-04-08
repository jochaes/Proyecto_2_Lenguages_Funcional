namespace PalabraEnredada

open Fabulous
open Models
open Styles
open Fabulous.XamarinForms
open Xamarin.Forms


module ChooseWords =
    type Model = {
        wordList: string list
    }

    type Msg =
        | AddWord

    let init () =
        {wordList = []}

    let update (msg: Msg) (model: Model) ( word:string) =
        match msg with
        | AddWord -> {model with wordList = (List.append model.wordList [word])}

    //let view (model: Model) dispatch =
    //    View

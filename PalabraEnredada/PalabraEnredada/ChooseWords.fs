namespace PalabraEnredada


open Models
open Styles
open Fabulous
open Xamarin.Forms
open Fabulous.XamarinForms

open type View


module ChooseWords =
    type Model = {
        wordList: string list
        entryText: string
    }

    type Msg =
        | AddWord
        | TextChanged of string

    let init =
        {
            wordList = ["hola";"LOL"]
            entryText = ""
        } 

    let update msg model =
        match msg with
        | AddWord -> {model with wordList = (List.append model.wordList [model.entryText])}
        | TextChanged msg-> {model with entryText = msg}

    let view (model: Model) dispatch =
        ContentPage(
            Strings.PaginaElegirPalabras_Titulo,
            View.VStack(){
                View.Label($"Las palabras son: {model.wordList}").font(namedSize = NamedSize.Title).centerTextHorizontal()

                View.Entry("hola", TextChanged ).centerHorizontal()
                View.Button("Añadir", AddWord)
            }
        )
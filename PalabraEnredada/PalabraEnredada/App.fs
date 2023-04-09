namespace PalabraEnredada

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms.PlatformConfiguration
open Xamarin.Forms.PlatformConfiguration.iOSSpecific
open Xamarin.Forms


open type View

module App =

    type Model = {
        ChooseWordsModel : ChooseWords.Model option
    }

    type Msg =
        | GoChooseWords
        | ChooseWordsMsg of ChooseWords.Msg
       
    let init () = { ChooseWordsModel = None}

    let update msg model =
        match msg with
        | ChooseWordsMsg msg ->
            let newModel = ChooseWords.update msg model.ChooseWordsModel.Value
            { model with  ChooseWordsModel = Some newModel }
        | GoChooseWords ->
            let chooseWordsModel = ChooseWords.init
            { model with ChooseWordsModel = Some  chooseWordsModel}


    let view model dispatch =
        let onNavigationPageAppearing () =
            NavigationPage.SetPrefersLargeTitles(Xamarin.Forms.Application.Current.MainPage, true)
            
        let onMainPageCreated (page: BindableObject) =
            Page.SetUseSafeArea(page, false)

        let onGoChooseWords =
            dispatch (GoChooseWords)
 
        let mainPage =
            View.ContentPage(
                "PalabraEnredada",
                View.VStack() {
                    View.Label(Strings.PaginaInicio_Titulo)
                        .font(namedSize = NamedSize.Title)
                        .centerTextHorizontal()

                    (View.VStack() {
                        
                        View.Button(Strings.PaginaInicio_BtnJugar, onGoChooseWords)
               
                    }).centerVertical(expand = true)
                }
               )

        let ChooseWordsPage =
            model.ChooseWordsModel
            |> Option.map (fun m -> ChooseWords.view m (ChooseWordsMsg >> dispatch))

 
        View.NavigationPage(
            pages = [
                yield mainPage
                match ChooseWordsPage with None -> () | Some p -> yield p
            ]
        
        )
        

    let program = Program.stateful init update view

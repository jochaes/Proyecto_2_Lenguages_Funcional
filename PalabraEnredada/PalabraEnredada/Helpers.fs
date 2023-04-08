namespace PalabraEnredada

open System.IO
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

module Helpers =
    let displayAlert (title, message, cancel) =
        Application.Current.MainPage.DisplayAlert(title, message, cancel)
        |> Async.AwaitTask

    let displayAlertWithConfirm (title, message, accept, cancel) =
        Application.Current.MainPage.DisplayAlert(title = title, message = message, accept = accept, cancel = cancel)
        |> Async.AwaitTask


module Cmd =
    let performAsync asyncUnit = 
        Cmd.ofAsyncMsgOption (async {
            do! asyncUnit
            return None
        })
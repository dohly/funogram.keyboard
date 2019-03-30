﻿module Funogram.TestBot

open System.IO
open Funogram.Api
open Funogram.Types
open Funogram.Bot
open Funogram.Types
open FunHttp
open System.Net.Http
open Funogram.Keyboard
open Funogram.Keyboard.Inline
open DemoBot.Examples


[<Literal>]
let TokenFileName = "token"
let mutable botToken = "none"

let processMessageBuild config =

    let defaultText = """⭐️Keyboard demo bot:
    /calendar - Calendar keyboard example
    /flight - Reserve seats in Embraer E170 example
    /confirm - Confirm keyboard example"""

    let processResultWithValue (result: Result<'a, ApiResponseError>) =
        match result with
        | Ok v -> Some v
        | Error e ->
            printfn "Error: %s" e.Description
            None

    let processResult (result: Result<'a, ApiResponseError>) =
        processResultWithValue result |> ignore

    let botResult data = api config data |> Async.RunSynchronously
    let bot data = botResult data |> processResult
    let updateArrived ctx =
        let fromId = if ctx.Update.Message.IsSome then ctx.Update.Message.Value.From.Value.Id
                     else ctx.Update.CallbackQuery.Value.From.Id       
        let sendMessageFormatted text parseMode = (sendMessageBase (ChatId.Int(fromId)) text (Some parseMode) None None None None) |> bot
        let say s= sendMessageFormatted s ParseMode.Markdown
        let askForBirthday()=Calendar.show config fromId "When is your birthday?"
        let askForConfirm()=ConfirmKeyboard.show config fromId "Are you sure?"
        let answeredBithday=Calendar.handleUpdate config
        let askForSeats()=EmbraerE170Reservations.show config fromId "Please select your seats"
        let answeredConfirm=ConfirmKeyboard.handleUpdate config
        let selectedSeats=EmbraerE170Reservations.handleUpdate config
        let notHandled =
            processCommands ctx [
                cmd "/calendar"  (fun _ -> askForBirthday())
                cmd "/confirm"  (fun _ -> askForConfirm())
                cmd "/flight"  (fun _ -> askForSeats())
                answeredBithday (fun date->say (date.ToLongDateString()))
                answeredConfirm (fun b->say (sprintf "You have just pressed %s" (match b with | true -> "yes" | false -> "no")))                
                selectedSeats (fun seats->
                                          let selected=match seats with
                                                        |[]->"nothing"
                                                        |_->
                                                           seats
                                                           |>List.map( fun (r,s)->sprintf "%d%c" r s)
                                                           |>String.concat(";")

                                          selected
                                          |>sprintf "You've just reserved %s"
                                          |>say)
            ]
        if notHandled then             
            bot (sendMessage fromId defaultText)           
    updateArrived

let start token =
    (*
    * Set poxy
    *```fsharp
    * let handler = new HttpClientHadler ()
    * handler.Proxy <- createMyProxy ()
    * handler.UseProxy <- true
    * let config = { defaultConfig with Token = token
    *                                   Cleint = new HttpClient(handler, true) }
    *```
    *)
    let config = { defaultConfig with Token = token }
    let updateArrived = processMessageBuild config
    startBot config updateArrived None

[<EntryPoint>]
let main argv =
    printfn "Bot started..."
    let startBot = 
        if File.Exists(TokenFileName) then
            start (File.ReadAllText(TokenFileName))
        else
            printf "Please, enter bot token: "
            let token = System.Console.ReadLine()
            File.WriteAllText(TokenFileName, token)
            start token
    startBot |> Async.RunSynchronously
    0

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
open Funogram.Keyboard
open System


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

    let botResult data = apiUntyped config data |> Async.RunSynchronously
    let bot data = botResult data |> processResult
    let updateArrived ctx =
        let userId = if ctx.Update.Message.IsSome then ctx.Update.Message.Value.From.Value.Id
                     else ctx.Update.CallbackQuery.Value.From.Id       
        let sendMessageFormatted text parseMode = (sendMessageBase (ChatId.Int(userId)) text (Some parseMode) None None None None) |> bot
        let say s= sendMessageFormatted s ParseMode.Markdown     
        let showKeyboard def=
                InlineKeyboard.show bot userId def
        let calendar()=Calendar.create  
                        "When is your birthday?" 
                        (fun (_,date)->say (date.ToLongDateString()))
        let seats flight=
            let flightId= DB.fligts.[flight]
            EmbraerE170Reservations.create 
                        flightId
                        (sprintf "Please select up to 4 seats for flight %A" flightId)
                        4
                        (fun (flightId, r)->
                                let selected=match r with
                                                        |[]->"nothing"
                                                        |_->
                                                           r
                                                           |>List.map( fun (r,s)->sprintf "%d%c" r s)
                                                           |>String.concat(";")

                                sprintf "You've just reserved %s on the flight %s" selected flightId                              
                                |>say)
                        (fun id->DB.reservationsTable.[id])
        let confirmKeyboard() = ConfirmKeyboard.create "Are you sure?"
                                  (fun (_,answer) -> match answer with
                                                    | true -> say ("You have just pressed yes")
                                                    | false -> say ("You have just pressed no"))
        let cmds=[
                cmd "/calendar"  (fun _ -> showKeyboard (calendar()))
                cmd "/flight"  (fun _ -> Random().Next(0,3)|>seats|>showKeyboard)
                cmd "/confirm"  (fun _ -> showKeyboard (confirmKeyboard()))
            ]
        let notHandled =
            processCommands ctx (cmds @ InlineKeyboard.getRegisteredHandlers())
        if notHandled then             
            bot (sendMessage userId defaultText)           
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

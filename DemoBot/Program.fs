module Funogram.TestBot

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
        let showKeyboard def=InlineKeyboard.show bot userId def
        let tryHandleKeyboard def=InlineKeyboard.tryHandleUpdate bot def
        let calendar=Calendar.create  
                        "When is your birthday?" 
                        (fun date->say (date.ToLongDateString()))
        let seats=EmbraerE170Reservations.create 
                        config 
                        "Please select your seats"
                        (fun seats->
                                          let selected=match seats with
                                                        |[]->"nothing"
                                                        |_->
                                                           seats
                                                           |>List.map( fun (r,s)->sprintf "%d%c" r s)
                                                           |>String.concat(";")

                                          selected
                                          |>sprintf "You've just reserved %s"
                                          |>say)
                        [(5,'A');(4,'C')]
        let confirmKeyboard = ConfirmKeyboard.create config "Are you sure?"
                                  (fun answer -> match answer with
                                                    | true -> say ("You have just pressed yes")
                                                    | false -> say ("You have just pressed no"))
                        
        let notHandled =
            processCommands ctx [
                cmd "/calendar"  (fun _ -> showKeyboard calendar)
                cmd "/flight"  (fun _ -> showKeyboard seats)
                cmd "/confirm"  (fun _ -> showKeyboard confirmKeyboard)
                tryHandleKeyboard calendar
                tryHandleKeyboard seats
                tryHandleKeyboard confirmKeyboard
            ]
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

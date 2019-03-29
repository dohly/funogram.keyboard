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


[<Literal>]
let TokenFileName = "token"
let mutable botToken = "none"

let processMessageBuild config =

    let defaultText = """⭐️Keyboard demo bot:
    /calendar - Calendar keyboard example
    /choice - Choice keyboard example"""


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
        
        let notHandled =
            processCommands ctx [
                cmd "/calendar"  (fun _ -> (Calendar.show fromId "Please select a date")|>bot)
            ]
        if notHandled then             
            let defaultMsg()=bot (sendMessage fromId defaultText)
            match ctx.Update.CallbackQuery with
            | Some q->
                       let handle handler confirmed=
                              optional {
                                let! r= handler q
                                return match r with
                                        |InlineKeyboard.Empty resp->resp|>bot
                                        |InlineKeyboard.Edited resp->resp|>bot
                                        |InlineKeyboard.Confirmed (d,resp)-> 
                                                     resp|>bot                                                     
                                                     (fromId,d)
                                                     |>confirmed
                              }
                       let hr=seq{
                                    yield handle Calendar.handleUpdate (fun (from,date)->sendMessage from (date.ToLongDateString())|>bot)
                                    //yield handle ChoiceControl.handleUpdate (AnsweredTarget>>FromWoman>>pushEvt)
                                }
                                |>Seq.choose(fun x->x)
                                |>Seq.tryHead
                       
                       match hr with
                       |Some r->r
                       |None -> defaultMsg()
            | None->defaultMsg()
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

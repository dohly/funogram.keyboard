﻿namespace Funogram.Keyboard.Inline

open Funogram.Types
open Funogram
open Funogram.RequestsTypes

module internal Constants=
     [<Literal>]
     let IGNORE="IGNORE"
     [<Literal>]
     let CONFIRM="CONFIRM"
     [<Literal>]
     let CHANGE_STATE="CHANGE_STATE"

type InlineButtonDefinition<'TState>=
      |ChangeState of string*'TState
      |Confirm of string*'TState
      |Ignore of string

type KeyboardBuilder<'TState>(kb:KeyboardDefinition<'TState>)=
    let inlineBtn btnType text (payload:string) =     
     {
      Text = text
      CallbackData = Some(sprintf "%s|%s|%s" kb.Id btnType payload)
      Url = None
      CallbackGame = None
      SwitchInlineQuery = None
      SwitchInlineQueryCurrentChat = None
     }
    let btn def=
        let toBtn t label value=value|>kb.Serialize|>inlineBtn t label
        match def with
            |ChangeState (text, s)->s|>toBtn Constants.CHANGE_STATE text
            |Confirm (text, s)->s|>toBtn Constants.CONFIRM text
            |Ignore (text)->inlineBtn Constants.IGNORE text ""
    member __.Change =ChangeState>>btn
    member __.Ignore =Ignore>>btn
    member __.Confirm=Confirm>>btn
    member __.YieldFrom(x:InlineKeyboardButton seq)= [x]
    member __.Yield(x:InlineKeyboardButton)= [[x]|>Seq.ofList]
    member __.Combine(a,b)=a@b
    member __.Delay(f)=f()
    member __.For(m,f) =m |> List.collect f
    member __.Bind(m,f) =m |> List.collect f
    member __.Zero()=[]

and KeyboardDefinition<'TState>={
    Id:string
    GetMessageText:'TState->string
    InitialState:'TState
    GetKeysByState:KeyboardBuilder<'TState>->'TState->seq<InlineKeyboardButton> list
    TryDeserialize:string->'TState option
    Serialize:'TState->string
    DoWhenConfirmed: string*'TState->unit
    DisableNotification:bool
    HideAfterConfirm:bool
}

 

[<AutoOpen>]
module InlineKeyboard=
 open Funogram.RequestsTypes
 open Bot
 open System.Collections.Concurrent

 [<Literal>]
 let private IGNORE="IGNORE"
 [<Literal>]
 let private CONFIRM="CONFIRM"
 [<Literal>]
 let private CHANGE_STATE="CHANGE_STATE"
 let private keyboardHandlers=ConcurrentDictionary<string,(UpdateContext->bool)>()
 let getRegisteredHandlers()=keyboardHandlers.Values|>Seq.toList

 type private HandleResult<'state>=
            |Edited of EditMessageTextReq
            |Empty of AnswerCallbackQueryReq
            |Confirmed of 'state*DeleteMessageReq

 //let private bot config data = Funogram.Api.api config data |> Async.RunSynchronously |> ignore      
    
 

     
 let private build buttons=      
     { InlineKeyboard =buttons }

 let private handleCallback (kb:KeyboardDefinition<'a>) (q:CallbackQuery)=
        let extractTypePayload (parts:string[])=
            if parts.[0]=kb.Id then Some (parts.[1], parts|>Array.skip(2)|>String.concat "|")
            else None
        let skip()=(Api.answerCallbackQueryBase(Some(q.Id)) None None None None)
        let delete()=Api.deleteMessage(q.Message.Value.Chat.Id)(q.Message.Value.MessageId)
        let edit newState=
            let keys=newState|>kb.GetKeysByState (KeyboardBuilder(kb))
            let text=newState|>kb.GetMessageText
            Api.editMessageTextBase
                     (Some(q.Message.Value.Chat.Id|>ChatId.Int)) 
                     (Some(q.Message.Value.MessageId))
                     None
                     (text)
                     None
                     None
                     (Some(keys|>build))
             
        let switch=
            function
            |(IGNORE, _)->skip()|>Empty|>Some
            |(CHANGE_STATE, s)->
                optional{
                 let! newState=kb.TryDeserialize s
                 return newState|>edit|>Edited
                }
            |(CONFIRM, s)->
                optional{
                 let! newState=kb.TryDeserialize s
                 keyboardHandlers.TryRemove(kb.Id)|>ignore
                 return (newState,delete())|>Confirmed
                }
            |_->None
        optional { 
         let! text=q.Data
         let parts=text.Split('|')
         let! typeAndPayload=extractTypePayload parts
         return! switch typeAndPayload
        }  
 
 let private tryHandleUpdate (bot:IBotRequest->unit) (kb:KeyboardDefinition<'a>) (ctx:UpdateContext)=
    let r=optional{            
            let! q=ctx.Update.CallbackQuery
            let! hr= handleCallback kb q      
            return match hr with
                    |Empty resp->resp|>bot|>ignore
                    |Edited resp->resp|>bot|>ignore
                    |Confirmed (state,resp)->let deleted=if kb.HideAfterConfirm then resp|>bot
                                             deleted|>ignore
                                             (kb.Id,state)|>kb.DoWhenConfirmed
           }
    r.IsNone
 let show (bot:IBotRequest->unit) toId (kb:KeyboardDefinition<'a>) =
        keyboardHandlers.[kb.Id]<-tryHandleUpdate bot kb
        let keys=kb.InitialState|>kb.GetKeysByState (KeyboardBuilder(kb))
        let markup=keys|>build|>Markup.InlineKeyboardMarkup
        let text=kb.InitialState|>kb.GetMessageText
        let req=Api.sendMessageMarkup toId text markup
        {req with DisableNotification=Some kb.DisableNotification}
        |>bot|>ignore
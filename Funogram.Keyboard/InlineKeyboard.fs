namespace Funogram.Keyboard.Inline

[<AutoOpen>]
module InlineKeyboard=
 open Funogram.Types
 open Funogram.RequestsTypes
 open Funogram 
 open Bot
 type Payload=string
 type KeyboardId=string
 type MessageText=string
 type StateToKeyboard<'a>='a->InlineKeyboardMarkup
 type StringToState<'a>=string->'a option
 type SendAnswer<'a>=RequestsTypes.IRequestBase<'a>->unit
 type HandleResult<'state>=
            |Edited of EditMessageTextReq
            |Empty of AnswerCallbackQueryReq
            |Confirmed of 'state*DeleteMessageReq 
 type InlineBtn<'state>=
      |ChangeState of string*'state
      |Confirm of string*'state
      |Ignore of string
 type KeyboardBuilder()=
    member __.YieldFrom(x:InlineKeyboardButton seq)= [x]
    member __.Yield(x:InlineKeyboardButton)= [[x]|>Seq.ofList]
    member __.Combine(a,b)=a@b
    member __.Delay(f)=f()
    member __.For(m,f) =m |> List.collect f
    member __.Bind(m,f) =m |> List.collect f
 let newKeyboard=KeyboardBuilder()
 [<Literal>]
 let private IGNORE="IGNORE"
 [<Literal>]
 let private CONFIRM="CONFIRM"
 [<Literal>]
 let private CHANGE_STATE="CHANGE_STATE"

 let private btn (id:KeyboardId) btnType text (payload:Payload) =     
     {
      Text = text
      CallbackData = Some(sprintf "%s|%s|%s" id btnType payload)
      Url = None
      CallbackGame = None
      SwitchInlineQuery = None
      SwitchInlineQueryCurrentChat = None
     }
 let buildButton keyboardId toString def =
        let toBtn t label value=value|>toString|>btn keyboardId t label
        match def with
        |ChangeState (text, s)->s|>toBtn CHANGE_STATE text
        |Confirm (text,s)->s|>toBtn CONFIRM text
        |Ignore text->btn keyboardId IGNORE text ""
 
 let build buttons=      
     { InlineKeyboard =buttons }

 let show<'a> (getKb:StateToKeyboard<'a>) text state notify toId= 
        let kb=state|>getKb
        let markup=kb|>Markup.InlineKeyboardMarkup
        let req=Api.sendMessageMarkup toId text markup
        {req with DisableNotification=Some notify}
 
 let handleUpdate keyboardId (tryParse:StringToState<'a>) (getKb:StateToKeyboard<'a>) text (q:CallbackQuery)=
        let extractTypePayload (parts:string[])=
            if parts.[0]=keyboardId then Some (parts.[1], parts.[2])
            else None
        let skip()=(Api.answerCallbackQueryBase(Some(q.Id)) None None None None)
        let delete()=Api.deleteMessage(q.Message.Value.Chat.Id)(q.Message.Value.MessageId)            
        let edit newState=
            let (kb)=newState|>getKb
            Api.editMessageTextBase
                     (Some(q.Message.Value.Chat.Id|>ChatId.Int)) 
                     (Some(q.Message.Value.MessageId))
                     None
                     (text)
                     None
                     None
                     (Some(kb))
             
        let switch=
            function
            |(IGNORE, _)->skip()|>Empty|>Some
            |(CHANGE_STATE, s)->
                optional{
                 let! newState=tryParse s
                 return newState|>edit|>Edited
                }
            |(CONFIRM, s)->
                optional{
                 let! newState=tryParse s
                 return (newState,delete())|>Confirmed
                }
            |_->None
        optional { 
         let! text=q.Data
         let parts=text.Split('|')
         let! typeAndPayload=extractTypePayload parts
         return! switch typeAndPayload
        }  
 
 let tryHandleUpdate config confirmed keyboardId (tryParse:StringToState<'a>) (getKb:StateToKeyboard<'a>) (ctx:UpdateContext)=
    let bot data = Funogram.Api.api config data |> Async.RunSynchronously |> ignore      
    let r=optional{
            let! q=ctx.Update.CallbackQuery
            let! msg=q.Message
            let! txt=msg.Text
            return! handleUpdate keyboardId tryParse getKb txt q      
            }|>Option.map(function
                    |Empty resp->resp|>bot
                    |Edited resp->resp|>bot
                    |Confirmed (state,resp)->resp|>bot  
                                             state|>confirmed)
    r.IsNone
      
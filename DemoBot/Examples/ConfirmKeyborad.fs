namespace Funogram.Keyboard
module ConfirmKeyborad=
    open System
    open System.Globalization   
    open Funogram.Keyboard.Inline
    [<Literal>]
    let private CONFIRM="CONFIRM"
    let private keyboard (keys:KeyboardBuilder<bool>) (b:bool)= 
               let X=keys.Ignore
               let B=keys.Change
               let OK=keys.Confirm               
               keys {
                      yield! [OK("Yes", true);   OK("No", false)] // (Yes) (No)
               }    
    
    let create botCfg text callback={
        Id=CONFIRM
        DisableNotification=false
        HideAfterConfirm=true
        InitialState=false
        GetMessageText=fun _->text
        BotConfig=botCfg
        Serialize=fun d-> d |> Convert.ToString
        GetKeysByState=keyboard
        TryDeserialize=fun d->match bool.TryParse d with
                                |true, dt->dt|>Some
                                |_->None
        DoWhenConfirmed=callback
    }    
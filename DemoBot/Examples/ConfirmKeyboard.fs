namespace Funogram.Keyboard
module ConfirmKeyborad=
    open System
    open Funogram.Keyboard.Inline
    
    let create botCfg text callback={
        Id="CONFIRM"
        DisableNotification=false
        HideAfterConfirm=true
        InitialState=false
        GetMessageText=fun _->text
        BotConfig=botCfg
        Serialize=fun d-> d |> Convert.ToString
        GetKeysByState = fun keys selected->
                                let OK=keys.Confirm               
                                keys {
                                  yield! [OK("Yes", true);   OK("No", false)] // (Yes) (No)
                                }    
        TryDeserialize=fun d->match bool.TryParse d with
                                |true, dt->dt|>Some
                                |_->None
        DoWhenConfirmed=callback
    }    
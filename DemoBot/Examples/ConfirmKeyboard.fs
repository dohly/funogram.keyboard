namespace DemoBot.Examples
module ConfirmKeyboard=
    open System
    open Funogram.Keyboard.Inline
    
    let create text callback={
        Id="CONFIRM"
        DisableNotification=false
        HideAfterConfirm=true
        InitialState=false
        GetMessageText=fun _->text
        Serialize=fun d-> d |> Convert.ToString
        GetKeysByState = fun keys _->
                                let OK=keys.Confirm               
                                keys {
                                  yield! [OK("Yes", true);   OK("No", false)] // (Yes) (No)
                                }    
        TryDeserialize=fun d->match bool.TryParse d with
                                |true, dt->dt|>Some
                                |_->None
        DoWhenConfirmed=callback
    }    
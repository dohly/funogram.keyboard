namespace DemoBot.Examples

open Funogram.Keyboard

module ConfirmKeyboard=
    let create text callback=ChoiceKeyboard.single 
                                    {
                                        Items=[(1,"Yes");(2,"No")]|>dict
                                        ItemsPerRow=2
                                        ConfirmButtonText=""
                                        CancelButton=Some""
                                    }
                                    "CONFIRM" 
                                    text
                                    (fun _ id->callback(id=1))
    //let create text callback={
    //    Id="CONFIRM"
    //    DisableNotification=false
    //    HideAfterConfirm=true
    //    InitialState=false
    //    GetMessageText=fun _->text
    //    Serialize=fun d-> d |> Convert.ToString
    //    GetKeysByState = fun keys _->
    //                            let OK=keys.Confirm               
    //                            keys {
    //                              yield! [OK("Yes", true);   OK("No", false)] // (Yes) (No)
    //                            }    
    //    TryDeserialize=fun d->match bool.TryParse d with
    //                            |true, dt->dt|>Some
    //                            |_->None
    //    DoWhenConfirmed=callback
    //}    
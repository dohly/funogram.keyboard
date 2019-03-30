namespace Funogram.Keyboard
module ConfirmKeyboard =
    open System
    open Funogram.Keyboard.Inline
    
    [<Literal>]
    let private CONFIRM="CONFIRM"
    let private serialize (b:bool)= b |> System.Convert.ToString  
    let private btn=buildButton CONFIRM serialize
    let private B=ChangeState>>btn    
    let private X=Ignore>>btn
    let private OK=Confirm>>btn
    let private keyboard :StateToKeyboard<bool>=
              fun selected-> 
               let kb=build(newKeyboard {
                          yield! [OK("yes", true);   OK("no", false)] // (yes) (no)
                        })
               kb
    
    let show cfg toId msg=
        InlineKeyboard.show cfg keyboard msg false true toId
    
    let handleUpdate cfg confirmed=
            let tryParse (d:string)=
                match bool.TryParse d with
                            |true, dt->dt|>Some
                            |_->None
            InlineKeyboard.tryHandleUpdate cfg confirmed CONFIRM tryParse keyboard
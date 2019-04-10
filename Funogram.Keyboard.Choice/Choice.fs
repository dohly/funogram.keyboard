namespace Funogram.Keyboard

open System.Collections.Generic
open System
module Choice=
    open Funogram.Keyboard.Inline
    type Settings={
        Items:IDictionary<int,string>
        ItemsPerRow:int
        ConfirmButtonText:string
        CancelButton:string option
    }
    let private toRows perRow=Seq.mapi(fun i k->(i,k))
                              >>Seq.groupBy(fun (i,k)->i/perRow)
                              >>Seq.toList
    let multiple limit cfg questionId text formatSelected callback
        :KeyboardDefinition<int list>={
        Id=questionId
        DisableNotification=false
        HideAfterConfirm=true
        InitialState=[]
        GetMessageText=fun _->text
        Serialize=List.map(sprintf "%d")>>String.concat "|"        
        TryDeserialize=fun seats->
                        try
                            if seats.Length=0 then Some []
                            else  seats.Split('|')|>Array.map(int)|>Array.toList|>Some
                        with
                        |_->None
        DoWhenConfirmed=callback
        GetKeysByState=
            fun keys selected->
               let B=keys.Change
               let OK=keys.Confirm
               let btn s=
                 let alreadySelected=List.contains s selected
                 let mutable text=cfg.Items.[s]
                 if (alreadySelected) then text<-formatSelected text
                 let newState=if alreadySelected then List.filter(fun x->x<>s) selected
                                                 else
                                                    let added=s::selected
                                                    match limit with
                                                        |Some l->added|>List.truncate(l)
                                                        |None->added
                 B(text, newState)               
               keys {
                      let rows=cfg.Items.Keys|>toRows cfg.ItemsPerRow                                
                      for (_,row) in rows do   
                           yield! row|>Seq.map(fun (_,k)->btn k)
                      let any=selected|>List.isEmpty|>not
                      if any then yield OK(cfg.ConfirmButtonText, selected)
                      if cfg.CancelButton.IsSome then yield OK(cfg.CancelButton.Value,[])
                   }
               
    }
    let single cfg questionId text callback
        :KeyboardDefinition<int option>={
        Id=questionId
        DisableNotification=false
        HideAfterConfirm=true
        InitialState=None
        GetMessageText=fun _->text
        Serialize=function
                    |Some x->x.ToString()
                    |None->"_"
        TryDeserialize=fun s->
                        match Int32.TryParse(s) with
                        |(true, v)->v|>Some|>Some
                        |_->None
        DoWhenConfirmed=fun (id, v)->v|>Option.map(callback id)|>ignore
        GetKeysByState=
            fun keys _->
               let OK=keys.Confirm
               keys {
                      let rows=cfg.Items|>toRows cfg.ItemsPerRow
                      for (_,row) in rows do   
                           yield! row|>Seq.map(fun (_,KeyValue(id,text))->
                                                        OK(text, id|>Some))                      
                   }
               
    }

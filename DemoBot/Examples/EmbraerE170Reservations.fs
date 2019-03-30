namespace DemoBot.Examples

[<RequireQualifiedAccess>]
module EmbraerE170Reservations=
    open Funogram.Keyboard.Inline
    type Seat=(int*char)
    let private seatToStr x=              
                        let (r,s)=x
                        sprintf "%d%c" r s
    let private strToSeat (s:string)=
                let letter=s|>Seq.rev|>Seq.toList|>List.head
                let row=int (s.Replace(letter.ToString(), ""))
                (row,letter)
    [<Literal>]
    let private E170="E170"   
    
    let create botCfg text callback :KeyboardDefinition<Seat list>={
        Id=E170
        DisableNotification=false
        HideAfterConfirm=true
        InitialState=[]
        GetMessageText=fun _->text
        BotConfig=botCfg
        Serialize=List.map(seatToStr)>>String.concat ";"        
        TryDeserialize=fun seats->
                        try
                            if seats.Length=0 then Some []
                            else  seats.Split(';')|>Array.map(strToSeat)|>Array.toList|>Some
                        with
                        |_->None
        DoWhenConfirmed=callback
        GetKeysByState=
            fun keys selectedSeats->
               let X=keys.Ignore
               let B=keys.Change
               let OK=keys.Confirm

               let seatsRow i=
                   List.map(fun s->(i,s))
                   >>List.map(fun s->
                                    match s with
                                    |(_,' ')->X(" ")
                                    |_->let alreadySelected=List.contains s selectedSeats
                                        let mutable text=seatToStr s
                                        if (alreadySelected) then text<-sprintf ">%s<" text
                                        let newState=if alreadySelected then List.filter(fun x->x<>s) selectedSeats
                                                     else s::selectedSeats
                                        B(text, newState)
                            )
               let businessClassRow i=['A';' ';' ';'D';'F']|>seatsRow i
               let economyClassRow i= ['A';'C';' ';'D';'F']|>seatsRow i

               build(keys {
                          for i in [1..2] do
                            yield! businessClassRow i
                          for i in [3..18] do
                            yield! economyClassRow i
                          let any=selectedSeats|>List.isEmpty|>not
                          if any then yield OK("Ready", selectedSeats)
                          yield OK("Cancel",[])
                        })
               
    }

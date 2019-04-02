namespace DemoBot.Examples

[<RequireQualifiedAccess>]
module DB=
    open System
    let fligts=Array.init 3 (fun _-> Guid.NewGuid())   
    let reserve c =List.map(fun x->(x,c))
    let reservationsTable=dict [
                (fligts.[0], reserve 'D' [3..2..13])
                (fligts.[1], reserve 'A' [1..2..10])
                (fligts.[2], reserve 'F' [2..2..15])            
             ]    

[<RequireQualifiedAccess>]
module EmbraerE170Reservations=
    open Funogram.Keyboard.Inline
    open System.Text.RegularExpressions
    open System
    open Newtonsoft.Json
    open System.Collections.Concurrent

    type Seat=(int*char)
    let private seatToStr x=              
                        let (r,s)=x
                        sprintf "%d%c" r s
    let private strToSeat (s:string)=
                let m=Regex("(?<Row>\d+)(?<Letter>.)").Match(s)
                let letter=m.Groups.["Letter"].Value.[0]
                let row=m.Groups.["Row"].Value |> int
                (row,letter)
    
    
    let create flightId text limit callback (getReserved:Guid->Seat list)
        :KeyboardDefinition<Seat list>={
        Id=flightId.ToString()
        DisableNotification=false
        HideAfterConfirm=true
        InitialState=[]
        GetMessageText=fun _->text
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
               let reservedBySomeoneElse=getReserved(flightId)
               let X=keys.Ignore
               let B=keys.Change
               let OK=keys.Confirm
               let canBeReserved s=
                 let alreadySelected=List.contains s selectedSeats
                 let mutable text=seatToStr s
                 if (alreadySelected) then text<-sprintf ">%s<" text
                 let newState=if alreadySelected then List.filter(fun x->x<>s) selectedSeats
                                                 else s::selectedSeats|>List.truncate(limit)
                 B(text, newState)
               let busy=X("X")
               let isSeatAlreadyReserved s=reservedBySomeoneElse|>List.contains(s)
               let seatsRow i=
                   List.map(fun s->(i,s))
                   >>List.map(fun s->
                                    match s with
                                    |(_,' ')->X(" ")
                                    |_->if isSeatAlreadyReserved(s) then busy
                                        else canBeReserved(s)
                            )
               let businessClassRow i=['A';' ';' ';'D';'F']|>seatsRow i
               let economyClassRow i= ['A';'C';' ';'D';'F']|>seatsRow i

               keys {
                          for i in [1..2] do
                            yield! businessClassRow i
                          for i in [3..18] do
                            yield! economyClassRow i
                          let any=selectedSeats|>List.isEmpty|>not
                          if any then yield OK("Ready", selectedSeats)
                          yield OK("Cancel",[])
                   }
               
    }

namespace DemoBot.Examples

[<RequireQualifiedAccess>]
module DB=
    let mutable private callCounter=0
    let mutable private reservationsTable=[(5,'D')]
    let getReservationsDB()=
                    callCounter<-callCounter+1
                    reservationsTable<-(callCounter,'A')::reservationsTable
                    reservationsTable

[<RequireQualifiedAccess>]
module EmbraerE170Reservations=
    open Funogram.Keyboard.Inline
    open System.Text.RegularExpressions

    type Seat=(int*char)
    let private seatToStr x=              
                        let (r,s)=x
                        sprintf "%d%c" r s
    let private strToSeat (s:string)=
                let m=Regex("(?<Row>\d+)(?<Letter>.)").Match(s)
                let letter=m.Groups.["Letter"].Value.[0]
                let row=m.Groups.["Row"].Value |> int
                (row,letter)

    [<Literal>]
    let private E170="E170"   
    
    let create botCfg text limit callback (getReserved:unit->Seat list)
        :KeyboardDefinition<Seat list>={
        Id=E170
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
               let reservedBySomeoneElse=getReserved()
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

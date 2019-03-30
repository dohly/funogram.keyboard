namespace Funogram.Keyboard
module Calendar=
    open System
    open System.Globalization   
    open Funogram.Keyboard.Inline

    [<Literal>]
    let private CALENDAR="CALENDAR"
    let private serialize (d:DateTime)=d.ToShortDateString()
    let private btn=buildButton CALENDAR serialize
    let private B=ChangeState>>btn    
    let private X=Ignore>>btn
    let private OK=Confirm>>btn
    let private keyboard :StateToKeyboard<DateTime>=
              fun d-> 
               let daysInMonth=DateTime.DaysInMonth(d.Year, d.Month)
               let fdow = int CultureInfo.CurrentCulture.DateTimeFormat.FirstDayOfWeek               
               let dow x=(int (new DateTime(d.Year, d.Month, x)).DayOfWeek)
               let firstDay=dow 1
               let lastDay=dow daysInMonth
               let diffStart = (7+(firstDay-fdow))%7
               let diffEnd=(7-lastDay)%7
               let btnLabel i=if i=d.Day then sprintf ">%d<" i
                              else i.ToString()
               let dayBtn x=
                    let i=x-diffStart                        
                    if (i>=1 && i<=daysInMonth) then
                        let dt=new DateTime(d.Year, d.Month,i)
                        B(btnLabel i, dt)
                    else
                        X(" ")
               let decade=
                let y=d.Year
                let start=y-y%10
                let e=start+10
                let s=sprintf "%d-%d" start e
                X(s)
               let year=X(d.Year.ToString())
               let month=CultureInfo.CurrentCulture.DateTimeFormat.GetMonthName(d.Month)|>X
               let Y=d.AddYears
               let M=d.AddMonths
               let kb=build(newKeyboard {
                          yield! [B("<<<", Y(-10));   decade;  B(">>>", Y(+10))] // (<<<) ( 2010-2020 ) (>>>)
                          yield! [B("<<",  Y(-1));    year;    B(">>",  Y(+1))]  // (<< ) (    2019   ) ( >>)
                          yield! [B("<",   M(-1));    month;   B(">",   M(+1))]  // ( < ) (  February ) ( > )
                          let weeks=[1..(daysInMonth+diffStart+diffEnd)]|>List.groupBy(fun x->(x-1)/7)
                          for (_,days) in weeks do   
                             yield! days|>List.map(dayBtn)
                          yield OK(sprintf "OK (%s)" (d.ToShortDateString()), d)
                        })
               kb
    
    let show cfg toId msg=
        InlineKeyboard.show cfg keyboard msg DateTime.Now true toId

    let handleUpdate cfg confirmed=
            let tryParse (d:string)=
                        match DateTime.TryParse d with
                            |true, dt->dt|>Some
                            |_->None
            InlineKeyboard.tryHandleUpdate cfg confirmed CALENDAR tryParse keyboard
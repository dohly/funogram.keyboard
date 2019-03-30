namespace Funogram.Keyboard
module Calendar=
    open System
    open System.Globalization   
    open Funogram.Keyboard.Inline
    [<Literal>]
    let private CALENDAR="CALENDAR"
    let private keyboard (keys:KeyboardBuilder<DateTime>) (d:DateTime)= 
               let X=keys.Ignore
               let B=keys.Change
               let OK=keys.Confirm
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
               build(keys {
                          yield! [B("<<<", Y(-10));   decade;  B(">>>", Y(+10))] // (<<<) ( 2010-2020 ) (>>>)
                          yield! [B("<<",  Y(-1));    year;    B(">>",  Y(+1))]  // (<< ) (    2019   ) ( >>)
                          yield! [B("<",   M(-1));    month;   B(">",   M(+1))]  // ( < ) (  February ) ( > )
                          let weeks=[1..(daysInMonth+diffStart+diffEnd)]|>List.groupBy(fun x->(x-1)/7)
                          for (_,days) in weeks do   
                             yield! days|>List.map(dayBtn)
                          yield OK(sprintf "OK (%s)" (d.ToShortDateString()), d)
               })
    
    
    let create botCfg text callback={
        Id=CALENDAR
        DisableNotification=false
        HideAfterConfirm=true
        InitialState=DateTime.Now
        GetMessageText=fun _->text
        BotConfig=botCfg
        Serialize=fun d->d.ToShortDateString()
        GetKeysByState=keyboard
        TryDeserialize=fun d->match DateTime.TryParse d with
                                |true, dt->dt|>Some
                                |_->None
        DoWhenConfirmed=callback
    }
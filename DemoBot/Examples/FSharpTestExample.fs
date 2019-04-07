namespace DemoBot.Examples

open Funogram.Keyboard.Inline
open System.Collections.Generic

module FSharpTestExample=
 open Funogram.Keyboard.Choice
 open System
 let private cfg items={
     Items=items|>dict
     ItemsPerRow=4
     ConfirmButtonText="OK"
     CancelButton="I don't know"|>Some
 }
 type QuestionId=string
 type SuggestedAnswer=int*string
 type TestData=(QuestionId*string*SuggestedAnswer list)
 type Question=SingleAnswer of TestData*int|MultipleAnswers of TestData*int list
 
 let questions= [|
     SingleAnswer (("F1","Whaat?",[(1,"C");(2,"W1");(3,"W2");(4,"W4")]), 1)
     MultipleAnswers (("F2", "WAAAT?", [(1,"C");(2,"W1");(3,"W2");(4,"C")]), [1;4])
   |]
 
 let show bot toid onCompleted=
    let testResults=Dictionary<string,bool>()    
    let registerAnswer (q,c)=testResults.[q]<-c
    let askN data correctIds onAnswer=
           let (id, text,items)=data
           let c=cfg items
           multiple None c id text (sprintf "[%s]") 
                    (fun (_, answer)->
                       let expected=Set.ofList correctIds
                       let actual=Set.ofList answer
                       let difference=Set.difference expected actual
                       onAnswer(id, difference=Set.empty))
          |>InlineKeyboard.show bot toid
    let ask1 data correctId onAnswer=
        let (id, text,items)=data    
        let c=cfg items
        single c id text
                 (fun _ answer-> onAnswer(id,answer=correctId))
        |>InlineKeyboard.show bot toid
    let ask=
        function
        |SingleAnswer (q,c)->ask1 q c
        |MultipleAnswers (q,c)->askN q c
    let processor=MailboxProcessor<int>.Start(fun inbox-> 
        // the message processing function
        let rec messageLoop() = async{       
            // read a message
            let! i = inbox.Receive()
            if i>=questions.Length then testResults|>onCompleted
            else ask questions.[i] (fun a->registerAnswer a
                                           i+1|>inbox.Post)
            // loop to top
            return! messageLoop()  
        }
        messageLoop() 
    )
    processor.Post(0)
        
    
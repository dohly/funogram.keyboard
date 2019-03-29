namespace Funogram
[<AutoOpen>]
module internal Optional=
  type OptionalBuilder() =
    member __.Bind(opt, binder) =
      match opt with
      | Some value -> binder value
      | None -> None
    member __.Return(value) =
      Some value
    member __.ReturnFrom(value) =
      __.Bind(value, Some)
    member __.Zero()=None
      
  let optional = new OptionalBuilder()
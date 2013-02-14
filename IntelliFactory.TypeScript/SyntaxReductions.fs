module internal IntelliFactory.TypeScript.SyntaxReductions

module A = Assembling
module S = Syntax

let (|CallMember|_|) m =
    match m with
    | S.CallMember (S.CallSignature (ps, ret))
    | S.FunctionMember (S.FunctionCallSignature (_, ps, ret)) -> Some (ps, ret)
    | _ -> None

let (|CallType|_|) (s: S.Type) =
    match s with
    | S.ObjectType ss ->
        let ok =
            ss
            |> Seq.forall (function
                | CallMember _ -> true
                | _ -> false)
        if ok then
            let variants =
                ss
                |> Seq.choose (|CallMember|_|)
                |> Seq.cache
            Some variants
        else
            None
    | _ -> None

type Node =
    | Node of A.Key * S.Type

let FunctionNode (fs: S.FunctionSignature) =
    let makeType ps ret =
        S.ObjectType [| S.CallMember (S.CallSignature (ps, ret)) |]
    match fs with
    | S.FunctionCallSignature (req, ps, ret) ->
        let ty = makeType ps ret
        Node (A.Call, ty)
    | S.FunctionSignature (id, req, ps, ret) ->
        let ty = makeType ps ret
        Node (A.Property id, ty)

let PropertyNode (p: S.PropertySignature) =
    match p with
    | S.PropertySignature (id, req, ty) ->
        Node (A.Property id, ty)

let ParameterNode (p: S.Parameter) =
    match p with
    | S.Parameter (id, ty) ->
        Node (A.Property id, ty)

let ReduceTypes (t1: S.Type) (t2: S.Type) =
    match t1, t2 with
    | S.ObjectType a, S.ObjectType b ->
        S.ObjectType (Seq.append a b)
    | _ -> t2

let MergeNodes (ns: seq<Node>) : seq<Node> =
    ns
    |> Seq.MergeDuplicates
        (fun (Node (id, _)) -> id)
        HashIdentity.Reference
        (fun (Node (id, t1)) (Node (_, t2)) ->
            Node (id, ReduceTypes t1 t2))

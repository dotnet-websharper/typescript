declare module A {
    var Id: number;
}

interface I1 {
    incr(x: number, y: string): number;
    a: number;
    b: string;
    c: any;
    d: number[];
    e: boolean;
    //type Type =
    //    | TAny
    //    | TNumber
    //    | TBoolean
    //    | TString
    //    | TVoid
    //    | TReference of TypeReference<Type>
    //    | TQuery of TypeQuery
    //    | TArray of Type
    //    | TObject of list<TypeMember<Type>>

}

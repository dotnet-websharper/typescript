declare function mapArray<A1,A2>(f: (x: A1) => A2, values: A1[]): A2[];

declare function f<T>(x: T): void;

//declare module A {
//    var nestedNumber: number;
//    export var x: I1;
//}

//declare var globalString: string;

//declare module B.X {
//    var deeplyNestedBoolean: boolean;
//}

//interface I1 {
//    incr(x: number, y: string): number;
//    withRest(x: number, y: boolean, ...rest: string[]): string;
//    a: number;
//    b: string;
//    c: any;
//    d: number[];
//    e: boolean;
//    [x: number]: string;
//}

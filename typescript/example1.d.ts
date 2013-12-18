
declare function mapArray<T1,T2>(f: (x: T1) => T2, values: T1[]): T2[];

declare module A {
    var nestedNumber: number;
}

declare var globalString: string;

declare module B.X {
    var deeplyNestedBoolean: boolean;
}

interface I1 {
    incr(x: number, y: string): number;
    withRest(x: number, ...rest: string[]): string;
    a: number;
    b: string;
    c: any;
    d: number[];
    e: boolean;
}

// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

/// <reference path="../contrib/DefinitelyTyped-1.0.1/_infrastructure/tests/typescript/0.9.5/lib.d.ts" />

declare module Test0001 {

    export interface I1 {
        incr(x: number, y: string): number;
        withRest(x: number, y: boolean, ...rest: string[]): string;
        a: number;
        b: string;
        c: any;
        d: number[];
        e: boolean;
        [x: number]: string;
    }

    export var x: I1;

    class MyBaseClass<T> {
        constructor(x: T)
    }

    class MyClass extends MyBaseClass<number> {
    }
}

//declare function mapArray<A1, A2>(f: (x: A1) => A2, values: A1[]): A2[];

//declare function f<T>(x: T): void;

//declare module A {
//    var nestedNumber: number;
//    export var x: I1;
//}

//declare var globalString: string;

//declare module B.X {
//    var deeplyNestedBoolean: boolean;
//}



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

var Test0001;
(function (A) {
    var Id;

    A.x = {
        a: 1,
        b: 'a',
        c: 0,
        d: [],
        e: true,
        incr: function (x, y) {
            return x + 1;
        },
        withRest: function (x, y) {
            var rest = [];
            for (var _i = 0; _i < (arguments.length - 2); _i++) {
                rest[_i] = arguments[_i + 2];
            }
            rest.push(x.toString());
            rest.push(y.toString());
            return rest.join(',');
        }
    };
})(Test0001 || (Test0001 = {}));

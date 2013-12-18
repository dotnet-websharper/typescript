// compiled from typescript
var A;
(function (A) {
    var Id;

    A.x = {
        a: 1,
        b: 'a',
        c: 0,
        d: [],
        e: true,
        incr: function (x, y) {
            return x;
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
})(A || (A = {}));

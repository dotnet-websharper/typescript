 module A {
	var Id: number;

	export var x : I1 = {
		a: 1,
		b: 'a',
		c: 0,
		d: [],
		e: true,
		incr: function(x: number, y: string) { return x; },
		withRest: function(x: number, y: boolean, ...rest: string[]) {
			rest.push(x.toString());
			rest.push(y.toString());
			return rest.join(',');
		}
	};
}

interface I1 {
    incr(x: number, y: string): number;
    withRest(x: number, y: boolean, ...rest: string[]): string;
    a: number;
    b: string;
    c: any;
    d: number[];
    e: boolean;
    [x: number]: string;
}

/*
    Allows jQuery Promises to interop with non-jQuery promises
*/
interface JQueryGenericPromise<T> {
    then<U>(onFulfill: (value: T) => U, onReject?: (reason: any) => U): JQueryGenericPromise<U>;
    then<U>(onFulfill: (value: T) => JQueryGenericPromise<U>, onReject?: (reason: any) => U): JQueryGenericPromise<U>;
    then<U>(onFulfill: (value: T) => U, onReject?: (reason: any) => JQueryGenericPromise<U>): JQueryGenericPromise<U>;
    then<U>(onFulfill: (value: T) => JQueryGenericPromise<U>, onReject?: (reason: any) => JQueryGenericPromise<U>): JQueryGenericPromise<U>;
}

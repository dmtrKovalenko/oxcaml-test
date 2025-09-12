const std = @import("std");
const print = std.debug.print;

fn fib(n: i32) i32 {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

const Foo = struct {
    foo: i32,
    bar: i32,
};

pub fn main() void {
    const n: i32 = 35;
    const x2 = Foo{ .foo = n, .bar = n };

    const result = fib(n);

    print("fibonacci({d}) = {d}\n", .{ x2.foo, result });
}


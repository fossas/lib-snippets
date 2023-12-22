use itertools::Itertools;
use pretty_assertions::assert_eq;

use snippets::parser::{bytes::Location, stack::*, Symbol};

/// Given some text, return a new location
/// for that text that models it appearing
/// immediately after the current content.
macro_rules! stack_loc {
    ($stack:ident, $text:expr) => {{
        // Offset by one more byte; pretend this is a newline or space.
        let byte_offset = $stack
            .last()
            .map(|e| e.location())
            .unwrap_or_default()
            .end_byte()
            + 1;

        Location::builder()
            .byte_offset(byte_offset)
            .byte_len($text.len())
            .build()
    }};
}

/// Push something on the stack.
///
/// Call with one of:
/// - `stack => enter_scope`
/// - `stack => exit_scope`
/// - `stack => {symbol}`
macro_rules! stack_push {
    ($stack:ident => enter_scope) => {
        $stack.enter(stack_loc!($stack, "{"))
    };
    ($stack:ident => exit_scope) => {
        $stack.exit(stack_loc!($stack, "}"))
    };
    ($stack:ident => $symbol:expr) => {
        $stack.push($symbol)
    };
}

/// Make a new symbol for the stack.
///
/// Call with one of:
/// - `stack => "name"`
macro_rules! mk_symbol {
    ($stack:ident => $name:expr) => {
        Symbol::<String>::new($name, stack_loc!($stack, $name))
    };
}

/// Assert the scope of the provided symbol.
macro_rules! assert_scope {
    ($stack:ident => $($scope:expr),* $(,)*) => {
        assert_eq!($stack.retrace().collect_vec(), vec![$($scope,)*])
    };
    ($stack:ident, $symbol:expr => $($scope:expr),* $(,)*) => {
        assert_eq!($stack.retrace_from($symbol).collect_vec(), vec![$($scope,)*])
    };
}

#[test]
fn smoke_scope() {
    crate::tracing::setup();
    let mut stack = Stack::default();

    // Set the package at the base level.
    // Note that it doesn't get a scope!
    let package = mk_symbol!(stack => "com.example.myapp");
    stack_push!(stack => &package);

    // Now we've come across the declaration for `SomeClass`...
    let some_class = mk_symbol!(stack => "SomeClass");
    stack_push!(stack => &some_class);
    stack_push!(stack => enter_scope);

    // Oh look, it has a method inside!
    let some_method = mk_symbol!(stack => "some_method");
    stack_push!(stack => &some_method);
    stack_push!(stack => enter_scope);

    // Now we've found another class declaration inside that method...
    let inner_class = mk_symbol!(stack => "InnerClass");
    stack_push!(stack => &inner_class);
    stack_push!(stack => enter_scope);

    // ... and a method inside that class too...
    let inner_method = mk_symbol!(stack => "inner_method");
    stack_push!(stack => &inner_method);
    stack_push!(stack => enter_scope);

    // Next, declare some variables to make sure they don't mess with
    // the scope parents incorrectly.
    let logger_var = mk_symbol!(stack => "logger:Logger");
    let today_var = mk_symbol!(stack => "today:Date");
    stack_push!(stack => &logger_var);
    stack_push!(stack => &today_var);

    // Check the scope. Remember that scope is reported inverted from declaration order.
    assert_scope!(
        stack =>
        &today_var,
        &logger_var,
        &inner_method,
        &inner_class,
        &some_method,
        &some_class,
        &package,
    );

    // Now let's pop the stack back up to just inside the `SomeClass` scope.
    stack_push!(stack => exit_scope); // pop out of inner_method
    stack_push!(stack => exit_scope); // pop out of InnerClass
    stack_push!(stack => exit_scope); // pop out of some_method

    // Remember that even though we've left the inner scope of `some_method`,
    // `some_method` is still visible. We're now in the scope that contains it.
    assert_scope!(
        stack =>
        &some_method,
        &some_class,
        &package,
    );

    // Now if we pop out of that to the base scope...
    stack_push!(stack => exit_scope);
    assert_scope!(
        stack =>
        &some_class,
        &package,
    );

    // ... and enter a new class and method, does that look right?
    // Note that `SomeClass` is a sibling of this class!
    let other_class = mk_symbol!(stack => "OtherClass");
    stack_push!(stack => &other_class);
    stack_push!(stack => enter_scope);
    assert_scope!(
        stack =>
        &other_class,
        &some_class,
        &package,
    );

    // Now, `OtherClass` has a constructor.
    //
    // Nothing else is declared inside it,
    // so its scope is immediately pushed then popped.
    //
    // Note that this means the `other_constructor` symbol
    // is left on the stack as a child of `other_class`.
    let other_constructor = mk_symbol!(stack => "constructor");
    stack_push!(stack => &other_constructor);
    stack_push!(stack => enter_scope);
    stack_push!(stack => exit_scope);

    // And same deal with `other_method`.
    let other_method = mk_symbol!(stack => "other_method");
    stack_push!(stack => &other_method);
    stack_push!(stack => enter_scope);
    stack_push!(stack => exit_scope);

    // How's the scope looking?
    // Remember that we're still inside `OtherClass`'s scope,
    // so the items in this scope should be visible.
    assert_scope!(
        stack =>
        &other_method,
        &other_constructor,
        &other_class,
        &some_class,
        &package,
    );

    // Now, let's say we finish parsing the file...
    stack_push!(stack => exit_scope); // pop out of `OtherClass`

    // Can we still get the right scope for arbitrary symbols?
    // Remember that the symbol doesn't appear in the stack reported for itself.
    assert_scope!(
        stack, &today_var =>
        &logger_var,
        &inner_method,
        &inner_class,
        &some_method,
        &some_class,
        &package,
    );
}

#[test]
fn retrace_from() {
    crate::tracing::setup();
    let mut stack = Stack::default();

    // Set the package at the base level.
    // Note that it doesn't get a scope!
    let package = mk_symbol!(stack => "com.example.myapp");
    stack_push!(stack => &package);

    // Now we've come across the declaration for `SomeClass`...
    let some_class = mk_symbol!(stack => "SomeClass");
    stack_push!(stack => &some_class);
    stack_push!(stack => enter_scope);
    stack_push!(stack => exit_scope);

    // And a sibling of it, `OtherClass`...
    let other_class = mk_symbol!(stack => "OtherClass");
    stack_push!(stack => &other_class);
    stack_push!(stack => enter_scope);
    stack_push!(stack => exit_scope);

    // Make sure the scope is set up as intended:
    assert_scope!(
        stack =>
        &other_class,
        &some_class,
        &package,
    );

    // Now, if we search for a symbol in the stack
    // and retrace the scope from there, ensure:
    // - It doesn't report things below it in the stack.
    // - It doesn't report itself.
    assert_scope!(stack, &some_class => &package);
}

# Rust Borrowing and References Guide

## The `&` Operator (Borrowing/Creating References)

### When to use `&`:
- **Function parameters** - when you want to borrow instead of taking ownership
- **Method calls** - when the method expects a reference  
- **Creating references from owned values**

```rust
// Function that borrows
fn print_length(s: &String) { 
    println!("{}", s.len()); 
}

let my_string = String::from("hello");
print_length(&my_string);  // & creates a reference
// my_string is still usable here
```

### Common `&` patterns:

```rust
// HashMap methods expect references to keys
map.get(&key)
map.contains_key(&key)

// Comparing values
if &my_value == &other_value

// Pattern matching with references
match &some_option {
    Some(value) => // value is &T
    None =>
}
```

## The `*` Operator (Dereferencing)

### When to use `*`:
- **Getting the value from a reference**
- **Modifying through mutable references**
- **Pattern matching to extract owned values**

```rust
// Dereferencing to get/modify the actual value
let x = 5;
let y = &x;        // y is &i32
let z = *y;        // z is i32 (copied value)

let mut count = 0;
let count_ref = &mut count;
*count_ref += 1;   // Dereference to modify the actual value
```

## Function Parameter Patterns

| Pattern | Syntax | Use Case |
|---------|--------|----------|
| Take ownership | `fn foo(s: String)` | Function owns the value, caller loses it |
| Borrow immutably | `fn foo(s: &String)` | Function reads the value, caller keeps it |
| Borrow mutably | `fn foo(s: &mut String)` | Function can modify, caller keeps ownership |

## Calling Functions - When to use `&`

```rust
// Function signatures tell you what to pass
fn take_ownership(s: String) { }
fn borrow_immutable(s: &String) { }
fn borrow_mutable(s: &mut String) { }

let mut my_string = String::from("hello");

take_ownership(my_string.clone());     // Pass owned value
borrow_immutable(&my_string);          // Use & to create reference
borrow_mutable(&mut my_string);        // Use &mut for mutable reference
```

## Common Scenarios Summary

### Use `&` when:
- Calling functions that expect references
- You want to keep ownership of your data
- Accessing HashMap with keys: `map.get(&key)`
- Comparing values: `&a == &b`

### Use `*` when:
- You have a reference but need the actual value
- Modifying through mutable references: `*mut_ref += 1`
- HashMap entry modifications: `*map.entry(key).or_insert(0) += 1`

### Use neither when:
- Passing owned values to functions that take ownership
- Working with values directly (not references)

## Quick Reference Guide

```rust
let value = 42;
let reference = &value;        // & creates reference
let copied_value = *reference; // * gets value from reference

// Function calls
some_func(value);      // Pass owned value
some_func(&value);     // Pass reference to value
some_func(*reference); // Pass dereferenced value (copy)

// Mutable references
let mut x = 5;
let mut_ref = &mut x;
*mut_ref = 10;         // * to modify through reference
```

## Key Insight

**`&` creates references, `*` uses references to get to the actual values!**
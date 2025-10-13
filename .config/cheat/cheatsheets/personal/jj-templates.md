# Jujutsu Templates - Quick Reference

A concise guide to Jujutsu's template language for customizing command output.

## Basic Syntax

Templates use functional syntax with method calls and operators:
- `object.method()` - Method calls
- `x ++ y` - Concatenation
- `if(condition, then, else)` - Conditionals
- `"string"` or `'string'` - String literals

## Core Commit Properties

### Basic Info
```
commit_id                    # Full commit ID
commit_id.short()           # Short commit ID (12 chars)
commit_id.shortest()        # Shortest unique prefix
change_id                   # Full change ID
change_id.short()          # Short change ID
description                # Full commit message
description.first_line()   # Subject line only
```

### People & Dates
```
author.name()              # Author name
author.email()             # Author email
author.timestamp()         # Author date
committer.name()           # Committer name
committer.timestamp()      # Commit date
author.timestamp().ago()   # Relative time (e.g., "2 days ago")
```

### Relationships
```
parents                    # Parent commits (list)
parents.len()             # Number of parents
parents.map(|c| c.commit_id().short())  # Parent IDs
bookmarks()               # Local and remote bookmarks
local_bookmarks()         # Local bookmarks only
remote_bookmarks()        # Remote bookmarks only
tags()                    # Tags pointing to commit
```

### Status Checks
```
empty()                   # True if no file changes
conflict()                # True if has merge conflicts
mine()                    # True if authored by current user
working_copies()          # Workspace name if working copy
current_working_copy()    # True if current workspace's working copy
immutable()               # True if immutable commit
divergent()               # True if change ID has multiple commits
hidden()                  # True if commit is hidden
root()                    # True if root commit
```

## Operators

### Arithmetic
```
x + y, x - y, x * y, x / y    # Math operations
x == y, x != y                # Equality
x < y, x > y, x <= y, x >= y  # Comparisons
```

### Logic
```
!x                        # Logical not
x && y                    # Logical and (short-circuit)
x || y                    # Logical or (short-circuit)
```

### String/Template
```
x ++ y                    # Concatenate strings/templates
```

## Essential Functions

### Formatting
```
if(condition, then, else)     # Conditional output
coalesce(a, b, c)            # First non-empty value
separate(sep, a, b, c)       # Join non-empty with separator
surround(prefix, suffix, content)  # Wrap non-empty content
label(style, content)        # Apply color/styling
```

### Layout
```
fill(width, content)         # Wrap text at width
indent(prefix, content)      # Indent non-empty lines
pad_start(width, content)    # Right-align with padding
pad_end(width, content)      # Left-align with padding
truncate_end(width, content) # Truncate with ellipsis
```

### String Processing
```
content.upper()              # Uppercase
content.lower()              # Lowercase
content.trim()               # Remove whitespace
content.contains("text")     # Check if contains text
content.starts_with("text")  # Check prefix
content.lines()              # Split into lines
```

### Lists
```
list.len()                   # List length
list.join(", ")             # Join with separator
list.map(|item| expression) # Transform each item
list.filter(|item| condition) # Filter items
```

## Common Patterns

### Basic Commit Info
```bash
# One-line format
jj log -T 'commit_id.short() ++ " " ++ description.first_line()'

# Multi-line with author
jj log -T 'commit_id.short() ++ " " ++ author.name() ++ "\n" ++ description.first_line()'

# Show only my commits
jj log -T 'if(mine(), commit_id.short() ++ " " ++ description.first_line(), "")'
```

### Status Indicators
```bash
# Show working copy and conflict status
jj log -T 'if(current_working_copy(), "@ ", "  ") ++ 
           if(conflict(), "⚠ ", "") ++ 
           commit_id.short() ++ " " ++ description.first_line()'

# Show empty commits
jj log -T 'if(empty(), "○ ", "● ") ++ commit_id.short() ++ " " ++ description.first_line()'
```

### Bookmark Information
```bash
# Show bookmarks if any
jj log -T 'commit_id.short() ++ " " ++ 
           if(bookmarks(), " (" ++ bookmarks().map(|b| b.name()).join(", ") ++ ")", "") ++ 
           " " ++ description.first_line()'

# Remote tracking status
jj log -T 'commit_id.short() ++ " " ++ 
           separate(" ", local_bookmarks().map(|b| b.name()), 
                         remote_bookmarks().map(|b| b.name() ++ "@" ++ b.remote().unwrap()))'
```

### Detailed Information
```bash
# Full commit details
jj log -T 'commit_id ++ "\n" ++ 
           "Author: " ++ author.name() ++ " <" ++ author.email() ++ ">\n" ++ 
           "Date: " ++ author.timestamp() ++ "\n\n" ++ 
           indent("    ", description)'

# Parent information
jj log -T 'commit_id.short() ++ " (" ++ 
           if(parents.len() > 1, "merge, ", "") ++ 
           parents.len() ++ " parent" ++ if(parents.len() != 1, "s", "") ++ ")"'
```

### Diff and Changes
```bash
# Show file changes
jj log -T 'commit_id.short() ++ " " ++ description.first_line() ++ "\n" ++ 
           diff().summary()'

# Diff stats
jj log -T 'commit_id.short() ++ " +" ++ diff().stat().total_added() ++ 
           " -" ++ diff().stat().total_removed() ++ " " ++ description.first_line()'
```

## Date Formatting

```bash
# Different date formats
author.timestamp().format("%Y-%m-%d")           # 2024-01-15
author.timestamp().format("%Y-%m-%d %H:%M")     # 2024-01-15 14:30
author.timestamp().format("%b %d, %Y")          # Jan 15, 2024
author.timestamp().ago()                        # 2 days ago
author.timestamp().utc()                        # Convert to UTC
```

## Config Aliases

Add to your config file:
```toml
[template-aliases]
# Short format
'short' = 'commit_id.short() ++ " " ++ description.first_line()'

# Format with author
'detailed' = '''
commit_id.short() ++ " " ++ author.name() ++ " " ++ 
author.timestamp().format("%Y-%m-%d") ++ "\n" ++ 
description.first_line()
'''

# Status indicators
'status' = '''
if(current_working_copy(), "@ ", "  ") ++ 
if(conflict(), "⚠ ", "") ++ 
if(empty(), "○ ", "● ")
'''

# Bookmark info
'refs' = '''
separate(" ", 
  bookmarks().map(|b| b.name()),
  tags().map(|t| "tag:" ++ t.name())
)
'''
```

## Advanced Examples

### Complex Status Line
```bash
jj log -T '
label(if(current_working_copy(), "working_copy", "commit"), 
  commit_id.short()
) ++ " " ++ 
separate(" ",
  if(conflict(), label("conflict", "conflict")),
  if(empty(), label("empty", "empty")),
  if(divergent(), label("divergent", "divergent"))
) ++ " " ++ 
description.first_line()
'
```

### Machine-Readable Output
```bash
# JSON format
jj log --no-graph -T '
"{" ++ 
"\"commit_id\": " ++ commit_id.escape_json() ++ ", " ++ 
"\"change_id\": " ++ change_id.escape_json() ++ ", " ++ 
"\"author\": " ++ author.name().escape_json() ++ ", " ++ 
"\"description\": " ++ description.escape_json() ++ 
"}"
'

# CSV format
jj log --no-graph -T 'commit_id ++ "," ++ change_id ++ "," ++ author.name() ++ "," ++ description.first_line()'
```

## Pro Tips

1. **Use `label()` for custom colors** - Define styles in config
2. **Combine with revsets** - `jj log -r 'mine()' -T template`
3. **Test incrementally** - Build complex templates step by step
4. **Use `separate()`** - Cleaner than manual conditionals for optional parts
5. **String escape** - Use `.escape_json()` for machine-readable output
6. **Debug colors** - Use `--color=debug` to see label structure
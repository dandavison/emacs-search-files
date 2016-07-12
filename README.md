##### An emacs interface for quickly narrowing down search results

This package provides a single interface for searching for patterns and file
names in the current project (git repository). The following key bindings are
available in the results buffer:

|       |                                    |
|-------|------------------------------------|
| RET   | Go to search hit                   |
| /     | Retain results matching a pattern  |
| C-u / | Discard results matching a pattern |


This package depends on https://github.com/dandavison/emacs-filter-results for
the filter/exclude mode buffer. You must `require` that package before
`require`ing this package.

###### Commands

|                                     |                                                     |
|-------------------------------------|-----------------------------------------------------|
| `search-files-thing-at-point`       | Search for occurrences of the current word          |
| `search-files-read-from-minibuffer` | Enter a regex and search for occurrences of it      |
| `search-files-by-name`              | Enter a regex and search for file names matching it |

With a prefix argument (`C-u`), search for _definitions_ of the word instead of
all occurrences (e.g. `def <word>` / `class <word>` in a python buffer, etc).

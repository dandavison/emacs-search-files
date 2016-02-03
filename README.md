##### An emacs interface to `git grep` and other file search tools

`search-files` uses `git grep` to search for patterns in the current git repo.

###### Commands

|                                     |                                               |
|-------------------------------------|-----------------------------------------------|
| `search-files-thing-at-point`       | Search for occurrences of the current word    |
| `search-files-read-from-minibuffer` | Enter a word and search for occurrences of it |

With a prefix argument (`C-u`), search for _definitions_ of the word instead of all occurrences (e.g. `def <word>` / `class <word>` in a python buffer, etc).


###### Key bindings in results buffer

|       |                                         |
|-------|-----------------------------------------|
| RET   | go to search hit                        |
| /     | filter results (retain matching lines)  |
| C-u / | filter results (exclude matching lines) |

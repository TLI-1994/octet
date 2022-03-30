# OCTET (OCaml TExt ediTor)

To install dependencies run

`opam switch --switch octet import ./requirements`

followed by

`opam switch octet`

Currently available key bindings:

    M-f                 Move forward one word
    M-b                 Move backward one word
    M-d                 Delete next word
    M-Backspace         Kill back to the beginning of a word
    C-e                 Move cursor to end of line
    C-a                 Move cursor to beginning of line
    C-n                 Autoformat current buffer
    C-x C-s             Switch between buffers
    C-x C-b             Switch to minibuffer
    C-x C-n             Execute command in the minibuffer
    C-x C-c             Exit

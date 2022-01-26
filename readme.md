# pomp - Simple Emacs pomodoro timer

_Author:_ Jade Michael Thornton<br>
_Version:_ 1.1.0<br>

Pomp is a simple and useful pomodoro-style timer.

### Usage

 M-x pomp or bind to a useful key.

 When pomp opens, it automatically begins a work session. There is nothing
 to do at this point except work!

### Keybindings

 | key   | binding             |
 |-------|---------------------|
 | W     | pomp-work           |
 | B     | pomp-break          |
 | Q     | pomp-quit           |
 | R     | pomp-reset          |
 | q     | quit-window         |
 | h     | describe-mode       |
 | ?     | describe-mode       |

### Customization

 To change timer duration:

       (setq pomp-seconds (* 25 60))     ; 25 minutes for work period
       (setq pomp-break-seconds (*5 60)) ; 5 minutes for break period


---
Converted from `pomp.el` by [_el2md_](https://gitlab.com/thornjad/el2md).
